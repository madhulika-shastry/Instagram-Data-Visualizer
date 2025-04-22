# ---- INSTALLING PACKAGES AND LOADING LIBRARIES ----

packages <- c(
  "rvest", "dplyr", "stringr", "purrr", "tibble",
  "tm", "vader", "tidyr", "text2vec", "Matrix", "ggplot2"
)

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Installing missing package:", pkg))
    install.packages(pkg)
  } else {
    message(paste("Package already installed:", pkg))
  }
}

library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)
library(tm)
library(vader)
library(tidyr)
library(text2vec)
library(Matrix)
library(ggplot2)

# ---- DMS DATA PREP ----

# Extract and clean DMs from inbox folder
analyze_dms <- function(inbox_folder) {
  # Get list of all folders (each DM thread)
  folders <- list.dirs(inbox_folder, full.names = TRUE, recursive = FALSE)
  
  # Function to read messages from message_1.html
  extract_from_folder <- function(folder_path) {
    html_path <- file.path(folder_path, "message_1.html")
    if (!file.exists(html_path)) return(NULL)
    
    page <- read_html(html_path)
    divs <- page %>% html_nodes("div") %>% html_text(trim = TRUE)  # Get all <div> elements
    divs <- divs[divs != ""] # Remove blanks
    divs <- divs[!str_detect(divs, "^https?://")] # Remove links (if you want to skip them)
    divs <- divs[!str_detect(divs, "\\d{1,2},? \\d{4}|\\d{1,2}:\\d{2} ?[ap]m")] # Remove timestamps 
    divs <- divs[!str_detect(divs, "^[A-Z][a-z]+( [A-Z][a-z]+)?$")] # Remove names (if lines are usually just a name)
    divs <- unique(divs) # De-duplicate
    
    tibble(message = divs, thread = basename(folder_path))
  }
  
  all_dms_df <- map_dfr(folders, extract_from_folder)
  return(all_dms_df)
}

# ---- CLEAN DMS FOR SENTIMENT ANALYSIS ----

# Clean DMs and remove near-duplicates
clean_dm_messages <- function(dm_df) {
  # Create a corpus from the DMs
  dm_corpus <- VCorpus(VectorSource(dm_df$message))
  
  # Clean the text
  dm_clean <- dm_corpus %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords("en")) %>%
    tm_map(stripWhitespace)
  
  # Extract clean text
  cleaned_dms <- tibble(message = sapply(dm_clean, as.character)) %>%
    mutate(message = gsub("https?://\\S+", "", message)) %>% # Remove URLs
    mutate(message = gsub("[^\x01-\x7F]", "", message)) %>% # Remove non-ASCII characters
    mutate(message = gsub("[\r\n\t]", " ", message)) %>% # Remove line breaks & tabs
    mutate(message = gsub("[[:punct:]]", "", message)) %>% # Remove punctuation
    mutate(message = gsub("[[:digit:]]", "", message)) %>% # Remove numbers
    mutate(message = trimws(message)) %>% # Trim extra spaces
    distinct(message, .keep_all = TRUE) # Remove duplicates based on cleaned message
  
  # Remove near-duplicates using cosine similarity
  texts <- cleaned_dms$message # text vector
  tokens <- word_tokenizer(tolower(texts)) # Tokenizer
  it <- itoken(tokens, progressbar = FALSE) # Create it
  vocab <- create_vocabulary(it, ngram = c(1L, 2L))
  vectorizer <- vocab_vectorizer(vocab)
  dtm <- create_dtm(it, vectorizer)
  tfidf <- TfIdf$new() # Create TF-IDF
  dtm_tfidf <- fit_transform(dtm, tfidf)
  cos_sim <- sim2(dtm_tfidf, method = "cosine", norm = "l2") # Compute cosine similarity
  
  to_remove <- c() # Filter near-duplicates (over 75% similar)
  for (i in 1:(nrow(cos_sim) - 1)) {
    if (i %in% to_remove) next
    sim_scores <- cos_sim[i, (i + 1):nrow(cos_sim)]
    dup_indices <- which(sim_scores >= 0.75) + i
    to_remove <- c(to_remove, dup_indices)
  }
  
  cleaned_dms_nodup <- cleaned_dms[-to_remove, ]
  cleaned_dms_nodup <- cleaned_dms_nodup[!grepl("https[a-zA-Z]+", cleaned_dms_nodup$message), ]
  return(cleaned_dms_nodup)
}

# ---- CONDUCT AND PLOT SENTIMENT ANALYSIS ----

# Create the sentiment plot directly
plot_dm_sentiment_distribution <- function(inbox_folder) {
  raw_dms <- analyze_dms(inbox_folder)
  cleaned <- clean_dm_messages(raw_dms)
  
  # Get VADER sentiment sentiment scores
  sentiment_scores <- sapply(cleaned$message, get_vader)
  sentiment_df <- as.data.frame(t(sentiment_scores), stringsAsFactors = FALSE)
  
  sentiment_df <- sentiment_df %>%
    mutate(pos = as.numeric(pos),
           neu = as.numeric(neu),
           neg = as.numeric(neg))
  
  dms_with_sentiment <- bind_cols(cleaned, sentiment_df) # Combine scores with messages
  
  # Reshape to long format and clean labels
  dm_long <- dms_with_sentiment %>%
    select(message, pos, neu, neg) %>%
    pivot_longer(cols = c(pos, neu, neg),
                 names_to = "sentiment_type",
                 values_to = "score") %>%
    filter(sentiment_type != "neu") %>%  # remove neutral
    mutate(sentiment_type = dplyr::recode(sentiment_type,
                                   "pos" = "Positive",
                                   "neg" = "Negative"))
  
  # Plot sentiment distribution
  ggplot(dm_long, aes(x = sentiment_type, fill = sentiment_type)) +
    geom_bar(aes(y = score), position = "stack", stat = "identity", show.legend = FALSE) +
    scale_fill_manual(values = c("Positive" = "#117733", "Negative" = "#CC6677")) +
    labs(title = "The General 'Vibes' of Your Direct Messages",
         x = NULL, y = "Total Sentiment Score (aka 'Vibes Score')") +
    theme_minimal() +
    theme(text = element_text(size = 12),
          plot.title = element_text(face = "bold", hjust = 0.5))
}