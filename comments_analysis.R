# ---- INSTALLING PACKAGES AND LOADING LIBRARIES ----

packages <- c(
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

library(rvest)        # For reading HTML
library(dplyr)        # Data manipulation
library(stringr)      # String manipulation
library(tm)           # Text mining
library(wordcloud)    # For wordcloud generation
library(RColorBrewer) # Color palettes for wordcloud
library(vader)        # VADER sentiment analysis
library(tidyr)        # Reshaping data
library(ggplot2)      # Plotting

# ---- COMMENTS DATA PREP ----

# Helper: returns an empty named numeric vector
named_numeric <- function(n) {
  structure(numeric(0), names = character(0))
}

# Function to analyze Instagram comments from HTML file
analyze_comments <- function(filepath) {
  html_data <- read_html(filepath)
  all_text <- html_data %>% html_text() # Load HTML and extract text
  lines <- str_split(all_text, "\n")[[1]] %>% str_trim() # Break into lines
  lines <- lines[lines != ""] # remove empty lines
  collapsed <- paste(lines, collapse = " ") # Collapse everything into one string (some HTML files are like this)
  raw_comments <- unlist(str_split(collapsed, "Comment")) # Split by "Comment"
  
  # Return empty tibble if no comments found
  if (length(raw_comments) < 2) return(tibble(comment = character(0)))
  
  raw_comments <- raw_comments[-1]  # Remove first element as it's empty
  
  # Extract comments, media owner, and time using regex
  comments_df <- tibble(
    comment = str_extract(raw_comments, "^[^M]+"),
    media_owner = str_extract(raw_comments, "Media Owner(.*?)Time") %>%
      str_replace_all("Media Owner|Time", "") %>% str_trim(),
    time = str_extract(raw_comments, "Time(.*)") %>%
      str_replace("Time", "") %>% str_trim()
  )
  
  return(comments_df)
}

# ---- CREATING A WORD CLOUD ----

# Get word frequency for wordcloud
get_comment_word_freq <- function(comments_df) {
  if (nrow(comments_df) == 0) return(named_numeric(0))
  
  comments_corpus <- VCorpus(VectorSource(comments_df$comment)) # Extract comments and create a VCorpus 
  
  # Clean the text
  clean_corpus <- comments_corpus %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords("en")) %>%
    tm_map(stripWhitespace)
  
  # Create term-document matrix
  tdm <- TermDocumentMatrix(clean_corpus)
  mat <- as.matrix(tdm)
  
  if (nrow(mat) == 0) return(named_numeric(0))
  
  word_freq <- sort(rowSums(mat), decreasing = TRUE)
  clean_names <- gsub("[^\x01-\x7F]", "", names(word_freq)) # Remove emojis
  valid <- nchar(clean_names) > 0
  names(word_freq)[valid] <- clean_names[valid] # Remove empty words
  
  word_freq_cleaned <- word_freq[valid] # # Reassign the cleaned words as names for the word frequencies
  return(word_freq_cleaned)
}

# ---- ANAYZING SENTIMENT OF COMMENTS ----

# Get sentiment scores for comments using VADER
get_sentiment_df <- function(comments_df) {
  if (nrow(comments_df) == 0) return(data.frame(sentiment_type = NA, score = NA))
  
  corpus <- VCorpus(VectorSource(comments_df$comment)) # Create a corpus from the comments
  
  # Clean the text
  clean <- corpus %>%
    tm_map(content_transformer(tolower)) %>% # Convert to lowercase
    tm_map(removePunctuation) %>% # Remove punctuation
    tm_map(removeNumbers) %>% # Remove numbers
    tm_map(removeWords, stopwords("en")) %>% # Remove common stopwords
    tm_map(stripWhitespace) # Remove extra whitespace
  
  cleaned_comments <- sapply(clean, as.character) # Extract the cleaned text into a new dataframe column
  cleaned_comments <- gsub("[^\x01-\x7F]", "", cleaned_comments) # Remove non-ASCII characters (including emojis)
  cleaned_comments <- trimws(cleaned_comments) # Remove extra white spaces (leading and trailing)
  cleaned_comments <- cleaned_comments[cleaned_comments != ""]
  
  if (length(cleaned_comments) == 0) return(data.frame(sentiment_type = NA, score = NA))
  
  sentiment_scores <- sapply(cleaned_comments, get_vader)  # Use sapply to apply get_vader on each comment
  sentiment_df <- data.frame(t(sentiment_scores), stringsAsFactors = FALSE) # Convert the result into a data frame
  sentiment_df$comment <- cleaned_comments
  sentiment_df <- sentiment_df %>%
    mutate(across(c(pos, neg, neu), as.numeric)) %>% # Convert sentiment columns to numeric
    pivot_longer(cols = c(pos, neu, neg), # Reshape the sentiment data into long format
                 names_to = "sentiment_type", 
                 values_to = "score") 
  
  return(sentiment_df)
}

# ---- PLOTTING SENTIMENT FOR COMMENTS ----
plot_sentiment_distribution <- function(sentiment_df) {
  sentiment_df <- sentiment_df %>%
    filter(sentiment_type != "neu") %>%  # Exclude neutral sentiment (aesthetic purposes)
    mutate(sentiment_type = dplyr::recode(sentiment_type,
                                          "pos" = "Positive",
                                          "neg" = "Negative"))
  
  ggplot(sentiment_df, aes(x = sentiment_type, y = score, fill = sentiment_type)) +
    geom_bar(stat = "identity", position = "stack", show.legend = FALSE) + # Using stat = "identity" for custom y-values
    scale_fill_manual(values = c("Positive" = "#117733", "Negative" = "#CC6677")) +
    labs(
      title = "The General 'Vibes' of Your Comments",
      x = NULL,  # Remove x-axis label
      y = "Total Sentiment Score (aka 'Vibes Score')"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5)  # Bold and center title
    )
}

# ---- PLOTTING WORD CLOUD FOR COMMENTS ----
plot_comment_wordcloud <- function(word_freq) {
  set.seed(123) 
  wordcloud(
    words = names(word_freq),
    freq = word_freq,
    min.freq = 2, # min.freq controls the minimum frequency a word must have in order to appear in the word cloud; adjust as needed
    max.words = 100,
    random.order = FALSE,
    rot.per = 0.25,
    colors = brewer.pal(8, "Dark2")
  )
  title(main = "These are Your Top Used Words in Your Comments!")
}
