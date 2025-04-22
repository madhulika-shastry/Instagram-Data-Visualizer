# ---- INSTALLING PACKAGES, LOADING LIBRARIES AND FILES ----

packages <- c(
  "ggplot2",     # Plotting
  "dplyr",       # Data manipulation
  "rvest",       # For reading HTML
  "lubridate"    # For working with dates and times
)

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Installing missing package:", pkg))
    install.packages(pkg)
  } else {
    message(paste("Package already installed:", pkg))
  }
}

library(ggplot2)      # Plotting
library(dplyr)        # Data manipulation
library(rvest)        # For reading HTML
library(lubridate)    # For working with dates and times

# ---- POST LIKES DATA PREP ----

# Function to analyze Instagram post likes from HTML File and read timestamps
# We are analyzing data over the past year. 
analyze_post_likes <- function(filepath) {
  html_data <- read_html(filepath)
  timestamps <- html_data %>% 
    html_nodes("div._a6-p > div > div:nth-child(2)") %>% 
    html_text(trim = TRUE)
  dates <- mdy_hm(timestamps)
  df <- data.frame(timestamp = dates) %>%
    mutate(
      day_of_week = wday(timestamp, label = TRUE, abbr = FALSE),
      day_type = ifelse(wday(timestamp) %in% c(1, 7), "Weekend", "Weekday"), 
      hour = hour(timestamp),
      time_period = ifelse(hour >= 7 & hour < 20, "Day", "Night"), # Separating likes by time (Day is 7am to 8pm, Night is 8pm to 7am)
      month = month(timestamp, label = TRUE, abbr = TRUE) # Separating likes by month 
    )
  return(df)
}

# ---- STORY LIKES DATA PREP ----

# Function to analyze Instagram post likes from HTML File and read timestamps
analyze_story_likes <- function(filepath) {
  doc <- read_html(filepath)
  dates <- doc %>% html_nodes("div._a6-p > div > div") %>% html_text(trim = TRUE)
  df <- data.frame(datetime = mdy_hm(dates)) %>%
    mutate(
      weekday = wday(datetime, label = TRUE, abbr = FALSE),
      is_weekend = ifelse(weekday %in% c("Saturday", "Sunday"), "Weekend", "Weekday"), # Separating likes by day of the week
      hour = hour(datetime),
      time_of_day = ifelse(hour >= 7 & hour < 20, "Day", "Night"), # Separating likes by time (Day is 7am to 8pm, Night is 8pm to 7am)
      month = floor_date(datetime, "month") # Separating likes by month 
    )
  return(df)
}

# ---- PLOT FUNCTIONS FOR POSTS ----

# Plotting post likes by time of day and weekend vs weekday 
plot_post_daytype <- function(df) {
  df %>%
    count(day_type, time_period) %>%
    ggplot(aes(x = day_type, y = n, fill = time_period)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = n), position = position_dodge(0.8), vjust = -0.3) +
    scale_fill_manual(values = c("Day" = "#88CCEE", "Night" = "#AA4499")) +
    labs(title = "How Many Posts You've Liked on the Weekday Vs. Weekend!", x = "Day Type", y = "Likes", fill = "Time") + theme(plot.title = element_text(face = "bold", hjust = 0.5))
}

# Plotting post likes by days of the week 
plot_post_dayofweek <- function(df) {
  df %>% count(day_of_week) %>%
    ggplot(aes(x = day_of_week, y = n)) +
    geom_bar(stat = "identity", fill = "#117733") +
    geom_text(aes(label = n), vjust = -0.3) +
    labs(title = "How Many Posts You've Liked Per Day of the Week!", x = "Day", y = "Like Count") + theme(plot.title = element_text(face = "bold", hjust = 0.5))
}

# Plotting post likes by month
plot_post_month <- function(df) {
  df %>% count(month) %>%
    ggplot(aes(x = month, y = n)) +
    geom_bar(stat = "identity", fill = "#332288") +
    geom_text(aes(label = n), vjust = -0.3) +
    labs(title = "How Many Posts You've Liked Per Month!", x = "Month", y = "Like Count") + theme(plot.title = element_text(face = "bold", hjust = 0.5))
}

# ---- CHI SQUARE ANALYSIS FOR POSTS----

# Testing whether the time of day someone likes posts (Day vs Night) is related to type of day (Weekday vs Weekend)
chi_sq_post_likes <- function(df) {
  tbl <- table(df$day_type, df$time_period)
  chisq.test(tbl)
}

# ---- PLOT FUNCTIONS FOR STORY LIKES ----

# Plotting story likes by time of day and weekend vs weekday 
plot_story_daytype <- function(df) {
  df %>%
    count(is_weekend, time_of_day) %>%
    ggplot(aes(x = is_weekend, y = n, fill = time_of_day)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = n), position = position_dodge(0.8), vjust = -0.3) +
    scale_fill_manual(values = c("Day" = "#88CCEE", "Night" = "#AA4499")) +
    labs(
      title = "How Many Stories You've Liked on the Weekday Vs. Weekend!",
      x = "Day Type", y = "Likes", fill = "Time"
    ) + theme(plot.title = element_text(face = "bold", hjust = 0.5))
}

# Plotting story likes by days of the week 
plot_story_dayofweek <- function(df) {
  df %>% count(weekday) %>%
    ggplot(aes(x = weekday, y = n)) +
    geom_bar(stat = "identity", fill = "#117733") +
    geom_text(aes(label = n), vjust = -0.3) +
    labs(
      title = "How Many Stories You've Liked Per Day of the Week!",
      x = "Day of Week", y = "Like Count"
    ) + theme(plot.title = element_text(face = "bold", hjust = 0.5))
}

# Plotting story likes by month
plot_story_month <- function(df) {
  df %>% count(month) %>%
    ggplot(aes(x = month, y = n)) +
    geom_bar(stat = "identity", fill = "#332288") +
    geom_text(aes(label = n), vjust = -0.3) +
    labs(
      title = "How Many Stories You've Liked Per Month!",
      x = "Month", y = "Like Count"
    ) + theme(plot.title = element_text(face = "bold", hjust = 0.5))
}

# ---- CHI SQUARE ANALYSIS FOR STORY LIKES----

# Testing whether the time of day someone likes stories (Day vs Night) is related to type of day (Weekday vs Weekend)
chi_sq_story_likes <- function(df) {
  tbl <- table(df$is_weekend, df$time_of_day)
  chisq.test(tbl)
}
