# ---- INSTALLING PACKAGES, LOADING LIBRARIES AND FILES ----

packages <- c(
  "shiny",        # For Shiny app
  "rvest",        # For reading HTML
  "dplyr",        # Data manipulation
  "stringr",      # String manipulation
  "tm",           # Text mining
  "wordcloud",    # Wordcloud generation
  "RColorBrewer", # Color palettes
  "vader",        # VADER sentiment analysis
  "tidyr",        # Reshaping data
  "ggplot2"       # Plotting
)

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Installing missing package:", pkg))
    install.packages(pkg)
  } else {
    message(paste("Package already installed:", pkg))
  }
}

library(shiny)        # For Shiny App     
library(rvest)        # For reading HTML
library(dplyr)        # Data manipulation
library(stringr)      # String manipulation
library(tm)           # Text mining
library(wordcloud)    # For wordcloud generation
library(RColorBrewer) # Color palettes for wordcloud
library(vader)        # VADER sentiment analysis
library(tidyr)        # Reshaping data
library(ggplot2)      # Plotting

# Loading analysis functions and calling post, story, comment and DM analysis code from other R files
# Post and story likes are in the same R file called "instagram-analysis.R"
source("instagram_analysis.R")
source("comments_analysis.R")
source("comment_DM_analysis.R")

# ---- CREATING SHINY APP: UI ----

# Creating the UI - tabs for instructions, post likes, story likes, comments, and DMs
ui <- fluidPage(
  titlePanel("Instagram Data Viewer and Analyzer"),
  
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.tab_selected == 'Instructions'",
        selectInput("step", "To download data, follow these steps",
                    choices = paste("Step", 1:13),
                    selected = "Step 1")
      ),
      conditionalPanel(
        condition = "input.tab_selected != 'Instructions'",
        fileInput("zip_file", "Upload the ZIP file you downloaded from Instagram here:")
      )
    ),
    
    mainPanel(
      tabsetPanel(id = "tab_selected",
                  tabPanel("Data Download Instructions",
                           value = "Instructions",
                           imageOutput("screenshot"),
                           br(), br(), br(),
                           uiOutput("instructions")
                  ),
                  tabPanel("Post Likes",
                           value = "Post Likes",
                           uiOutput("post_missing_text"),
                           div(plotOutput("post_plot1"), style = "margin-bottom: 20px;"),
                           div(uiOutput("post_chi_text"), style = "margin-bottom: 20px;"),
                           div(plotOutput("post_plot2"), style = "margin-bottom: 20px;"),
                           div(plotOutput("post_plot3"), style = "margin-bottom: 40px;")
                  ),
                  tabPanel("Story Likes",
                           value = "Story Likes",
                           uiOutput("story_missing_text"),
                           div(plotOutput("story_plot1"), style = "margin-bottom: 20px;"),
                           div(uiOutput("story_chi_text"), style = "margin-bottom: 20px;"),
                           div(plotOutput("story_plot2"), style = "margin-bottom: 20px;"),
                           div(plotOutput("story_plot3"), style = "margin-bottom: 40px;")
                  ),
                  tabPanel("Comments",
                           value = "Comments",
                           uiOutput("comments_missing_text"),
                           conditionalPanel(
                             condition = "output.show_comment_tab",
                             plotOutput("comment_wordcloud"),
                             plotOutput("comment_sentiment")
                           )
                  ),
                  tabPanel("Direct Messages",
                           value = "Direct Messages",
                           uiOutput("dm_missing_text"),
                           conditionalPanel(
                             condition = "output.show_dm_tab",
                             uiOutput("dm_loading_text"),
                             plotOutput("dm_sentiment_plot")
                           )
                  )
      )
    )
  )
)

# ---- CREATING THE SHINY APP: SERVER ----

server <- function(input, output, session) {
  
  zip_uploaded <- reactiveVal(NULL) # This guarantees that every time a ZIP is uploaded we can force it to re-run
  
  observeEvent(input$zip_file, {
    zip_uploaded(Sys.time())
  })
  
# ---- INSTRUCTIONS ----
  
# Adding instructions on how to download data with pictures
# Loading in pictures from www folder
  output$screenshot <- renderImage({
    file_name <- switch(input$step,
                        "Step 1" = "www/StepOne.png",   "Step 2" = "www/StepTwo.png",
                        "Step 3" = "www/StepThree.png", "Step 4" = "www/StepFour.png",
                        "Step 5" = "www/StepFive.png",  "Step 6" = "www/StepSix.png",
                        "Step 7" = "www/StepSeven.png", "Step 8" = "www/StepEight.png",
                        "Step 9" = "www/StepNine.png",  "Step 10" = "www/StepTen.png",
                        "Step 11" = "www/StepEleven.png",  "Step 12" = "www/StepTwelve.png",
                        "Step 13" = "www/StepThirteen.png"
                        
    )
    list(src = file_name, contentType = 'image/png', width = 600)
  }, deleteFile = FALSE)
  
# Labeling steps 
  output$instructions <- renderUI({
    instr <- switch(input$step,
                    "Step 1" = "Log onto Instagram on your computer. Click on 'More' at the bottom left.",
                    "Step 2" = "Click on 'Your Activity'.",
                    "Step 3" = "Click on 'Download your information'.",
                    "Step 4" = "Click on the blue 'Continue' button.",
                    "Step 5" = "Click 'Download or transfer your information'.",
                    "Step 6" = "Select your Instagram account.",
                    "Step 7" = "Click 'Some of your information'.",
                    "Step 8" = "Select 'Comments', 'Likes', 'Story Interactions', and/or 'Messages'.",
                    "Step 9" = "Click 'Download to device'.",
                    "Step 10" = "Set the date to 'Last year'. Format: HTML. Click 'Create files' and then wait to be notified that your files are ready to download (~ 1 day).",
                    "Step 11" = "Once your files are ready to download, log back onto Instagram. Go to Accounts Center (follow steps 1-4 again). Click on 'Download' under 'Available Downloads'.",
                    "Step 12" = "Enter your Instagram password if prompted to.",
                    "Step 13" = "Save your ZIPPED files on your computer. And now your data has been successfully downloaded! Upload your ZIPPED file in any of the tabs."
    )
    HTML(paste("<p>", instr, "</p>"))
  })

# ---- FILE HANDLING ----
  
# Unzipping participant uploaded files 
  uploaded_files <- reactive({
    req(input$zip_file)
    zip_uploaded()
    
    # Make a brand new clean folder each time
    temp_dir <- tempfile("unzipped_")
    dir.create(temp_dir)
    
    unzip(input$zip_file$datapath, exdir = temp_dir)
    
    post_file <- list.files(temp_dir, pattern = "liked_posts\\.html$", full.names = TRUE, recursive = TRUE)[1]
    story_file <- list.files(temp_dir, pattern = "story_likes\\.html$", full.names = TRUE, recursive = TRUE)[1]
    comment_file <- list.files(temp_dir, pattern = "post_comments_1\\.html$", full.names = TRUE, recursive = TRUE)[1]
    inbox_folder <- list.dirs(temp_dir, recursive = TRUE, full.names = TRUE)
    inbox_folder <- inbox_folder[grepl("/inbox$", inbox_folder, ignore.case = TRUE)][1]
    
    if (identical(post_file, character(0))) post_file <- NULL
    if (identical(story_file, character(0))) story_file <- NULL
    if (identical(comment_file, character(0))) comment_file <- NULL
    if (identical(inbox_folder, character(0))) inbox_folder <- NULL
    
    list(
      post = post_file,
      story = story_file,
      comment = comment_file,
      inbox = inbox_folder
    )
  })

# ---- POST LIKES ----

# Checking to see if participant downloaded post likes
  output$post_missing_text <- renderUI({
    if (input$tab_selected != "Post Likes") return(NULL)
    
    post <- uploaded_files()$post
    if (is.null(post) || post == "" || !file.exists(post)) {
      return(HTML("<p style='color: red; font-weight: bold;'>You didn't download your post likes, so we can't analyze them!</p>"))
    } else {
      return(NULL)
    }
  })
  
# Showing plots for post likes and chi-square analysis of the post likes
# Calling plots from instagram_analysis.R
  post_data <- reactive({
    req(uploaded_files()$post)
    df <- analyze_post_likes(uploaded_files()$post)
    validate(need(!all(is.na(df$timestamp)), "No valid timestamps in liked_posts.html"))
    return(df)
  })
  
  output$post_plot1 <- renderPlot({ plot_post_daytype(post_data()) })
  output$post_plot2 <- renderPlot({ plot_post_dayofweek(post_data()) })
  output$post_plot3 <- renderPlot({ plot_post_month(post_data()) })
  output$post_chi_text <- renderUI({
    result <- chi_sq_post_likes(post_data())
    interpretation <- if (result$p.value < 0.05) { # Rewriting analysis for laypeople to understand
      "Yes — you tend to like posts at different times depending on whether it’s a weekday or a weekend. Your liking habits change based on the day."
    } else {
      "Nope — your post liking habits don’t seem to change based on whether it’s a weekday or a weekend. You like posts at similar times regardless of the day."
    }
    
    HTML(paste0(
      "<b>Question: </b>Do you tend to like Instagram posts at different times of day depending on whether it’s a weekday or a weekend?<br><br>",
      "<b>Chi-Square Test Result:</b><br>",
      "X-squared = ", round(result$statistic, 2),
      ", df = ", result$parameter,
      ", p = ", format.pval(result$p.value, digits = 3), "<br><br>",
      "<b>Interpretation:</b> ", interpretation
    ))
  })

# ---- STORY LIKES ----
  
# Checking to see if participant downloaded story likes
  output$story_missing_text <- renderUI({
    if (input$tab_selected != "Story Likes") return(NULL)
    
    story <- uploaded_files()$story
    if (is.null(story) || story == "" || !file.exists(story)) {
      return(HTML("<p style='color: red; font-weight: bold;'>You didn't download your story likes, so we can't analyze them!</p>"))
    } else {
      return(NULL)
    }
  })
  
# Showing plots for story likes and chi-square analysis of the story likes
# Calling plots from instagram_analysis.R
  story_data <- reactive({
    req(uploaded_files()$story)
    df <- analyze_story_likes(uploaded_files()$story)
    validate(need(!all(is.na(df$datetime)), "No valid timestamps in story_likes.html"))
    return(df)
  })
  
  output$story_plot1 <- renderPlot({ plot_story_daytype(story_data()) })
  output$story_plot2 <- renderPlot({ plot_story_dayofweek(story_data()) })
  output$story_plot3 <- renderPlot({ plot_story_month(story_data()) })
  output$story_chi_text <- renderUI({
    result <- chi_sq_story_likes(story_data())
    interpretation <- if (result$p.value < 0.05) { # Rewriting analysis for laypeople to understand
      "Yes — you tend to like stories at different times depending on whether it’s a weekday or a weekend. Your liking habits change based on the day."
    } else {
      "Nope — your story liking habits don’t seem to change based on whether it’s a weekday or a weekend. You like stories at similar times regardless of the day."
    }
    
    HTML(paste0(
      "<b>Question: </b>Do you tend to like Instagram stories at different times of day depending on whether it’s a weekday or a weekend?<br><br>",
      "<b>Chi-Square Test Result:</b><br>",
      "X-squared = ", round(result$statistic, 2), ", df = ", result$parameter,
      ", p = ", format.pval(result$p.value, digits = 3), "<br><br>",
      "<b>Interpretation:</b> ", interpretation
    ))
  })
  
# ---- COMMENTS ----
  
# Checking to see if participant downloaded comments
  output$comments_missing_text <- renderUI({
    if (input$tab_selected != "Comments") return(NULL)
    
    comment <- uploaded_files()$comment
    if (is.null(comment) || comment == "" || !file.exists(comment)) {
      return(HTML("<p style='color: red; font-weight: bold;'>You didn't download your comments, so we can't analyze them!</p>"))
    } else {
      return(NULL)
    }
  })
  
  output$show_comment_tab <- reactive({
    comment <- uploaded_files()$comment
    !is.null(comment) && comment != "" && file.exists(comment)
  })
  outputOptions(output, "show_comment_tab", suspendWhenHidden = FALSE)
  
  comment_data <- reactive({
    zip_uploaded()  # invalidate on new file
    comment_path <- uploaded_files()$comment
    if (is.null(comment_path) || comment_path == "" || !file.exists(comment_path)) return(NULL)
    analyze_comments(comment_path)
  })

  comment_word_freq <- reactive({
    zip_uploaded()
    data <- comment_data()
    if (is.null(data)) return(NULL)
    get_comment_word_freq(data)
  })
  
  comment_sentiments <- reactive({
    zip_uploaded()
    data <- comment_data()
    if (is.null(data)) return(NULL)
    get_sentiment_df(data)
  })

# Calling word cloud plot from comments_analysis.R  
  output$comment_wordcloud <- renderPlot({
    wf <- comment_word_freq()
    validate(
      need(!is.null(wf), "No comment data to show."),
      need(length(wf) > 0, "No valid words to show in wordcloud.")
    )
    plot_comment_wordcloud(wf)
  })
  
# Calling sentiment analysis plot from comments_analysis.R
  output$comment_sentiment <- renderPlot({
    df <- comment_sentiments()
    validate(
      need(!is.null(df), "No comment data to show."),
      need(nrow(df) > 0 && !all(is.na(df$score)), "No sentiment scores to show.")
    )
    plot_sentiment_distribution(df)
  })
  
# ---- DIRECT MESSAGES ----
  
# Checking to see if participant downloaded direct messages
  output$dm_missing_text <- renderUI({
    if (input$tab_selected != "Direct Messages") return(NULL)
    
    inbox <- uploaded_files()$inbox
    if (is.null(inbox) || inbox == "" || !file.exists(inbox)) {
      return(HTML("<p style='color: red; font-weight: bold;'>You didn't download your DMs, so we can't analyze them!</p>"))
    } else {
      return(NULL)
    }
  })
  
  output$show_dm_tab <- reactive({
    !is.null(uploaded_files()$inbox)
  })
  outputOptions(output, "show_dm_tab", suspendWhenHidden = FALSE)
  
  output$dm_loading_text <- renderUI({
    if (input$tab_selected != "Direct Messages") return(NULL)
    
    inbox <- uploaded_files()$inbox
    if (is.null(inbox) || !file.exists(inbox)) return(NULL)
    
    HTML("<p style='color: black; font-weight: bold;'>Wow, so many DMs! Thanks for your patience while we loaded everything!</p>")
  })  # Message while DMs load
  
# Calling sentiment analysis plot from comment_DM_analysis.R
  output$dm_sentiment_plot <- renderPlot({
    inbox <- uploaded_files()$inbox
    req(inbox, file.exists(inbox))
    plot_dm_sentiment_distribution(inbox)
  })
  
}

shinyApp(ui, server)