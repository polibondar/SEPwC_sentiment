suppressPackageStartupMessages({
  library(sentimentr)
  library(tidytext)
  library(lubridate)
  library(dplyr)
  library(tidyr)
  library(argparse)
  library(ggpubr)
  library(syuzhet)
  library(stringr)
  library(ggplot2)
})

#using this function requires entering the file name for our data
load_data <- function(filename) {
  # preparing the data and giving the class to the "ID" column
  data <- read.csv(filename,
                   sep = ",",
                   header = TRUE,
                   stringsAsFactors = FALSE, colClasses = c("id" = "character"))
  # getting rid of html
  cleaned_data <- gsub("<[^>]+>", "", data$content)
  data$content <- cleaned_data
  # making sure the date format is year-month-date hour-minute-second
  date_time <- ymd_hms(data$created_at)
  data$created_at <- date_time
  data <- filter(data, language == "en")
  return(data)
}

#using this function requires entering data as the toot_data value
word_analysis <- function(toot_data, emotion) {
  word_data <- toot_data %>%
    #separating out the words- now we have a word column
    unnest_tokens(word, content) %>%
    group_by(id, created_at)
  nrc_emotion <- get_sentiments("nrc") %>%
    #by specifying the emotion we alter the output
    filter(sentiment == emotion)
  filtered_by_emotion <- word_data %>%
    inner_join(nrc_emotion, by = "word") %>%
    count(id, word, sentiment, sort = TRUE) %>%
    arrange(desc(n))
  #now we have a table with id, created_at, word, sentiment and quantities
  top_10_emotion_words <- filtered_by_emotion %>%
    #top 10 most common emotion words sorted in descending order
    ungroup() %>%
    slice_max(order_by = n, n = 10)
  return(top_10_emotion_words)
}

sentiment_analysis <- function(toot_data) {
  data_lexicons <- toot_data %>% #content to words
    unnest_tokens(word, content) %>%
    group_by(id, created_at)
  #analyse sentiments using the syuzhet library and NRC lexicon
  emotions <- get_nrc_sentiment(toot_data$content)
  #dataframe of the emotions
  emo_bar <- colSums(emotions)
  #total count of the emotions
  emo_sum <- data.frame(count = emo_bar,
                        emotion = names(emo_bar))
  ggplot(emo_sum, aes(x = reorder(emotion, -count),
                      y = count)) +
    geom_bar(stat = "identity")
  bing_word_counts <- data_lexicons %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sort = TRUE)
  bing_top_10_words_by_sentiment <- bing_word_counts %>%
    group_by(word) %>%
    slice_max(order_by = n, n = 10) %>%
    ungroup() %>%
    mutate(word = reorder(word, n))
  bing_top_10_words_by_sentiment %>%
    ggplot(aes(word, n, fill = sentiment)) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(y = "contribution to sentiment", x = NULL) +
    coord_flip()
  return(toot_data)
}

main <- function(args) {
  #content to words
  data_lexicons <- toot_data %>%
    unnest_tokens(word, content) %>%
    group_by(id, created_at)
  #analysing toots against bing lexicon
  toot_sentiment_bing <- data_lexicons %>%
    inner_join(get_sentiments("bing"),
               by = "word") %>%
    count(sentiment) %>%
    pivot_wider(names_from = sentiment,
                values_from = n,
                values_fill = 0) %>%
    mutate(sentiment = positive - negative)
  mutate(method = "bing") %>%
    select(id, created_at, sentiment, method)
  ggplot(toot_sentiment, aes(id, sentiment, fill = emotion)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~emotion, ncol = 2, scales = "free_x")
  #analysing toots against afinn lexicon
  toot_sentiment_afinn <- data_lexicons %>%
    inner_join(get_sentiments("afinn")) %>%
    group_by(id, created_at) %>%
    summarise(sentiment = sum(value))
  mutate(method = "afinn") %>%
    select(id, created_at, sentiment, method)
  #analysing toots against nrc lexicon
  toot_sentiment_nrc <- data_lexicons %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    count(sentiment) %>%
    pivot_wider(names_from = sentiment,
                values_from = n,
                values_fill = 0) %>%
    mutate(sentiment = positive - negative)
  mutate(method = "nrc") %>%
    select(id, created_at, sentiment, method)
  sentiment_data <- bind_rows(toot_sentiment_bing,
                              toot_sentiment_afinn,
                              toot_sentiment_nrc) #combining all the results
  filename <- args$filename
  output <- args$output
  emotion <- args$emotion
  data <- load_data(filename)
  word_data <- word_analysis(data, emotion)
  sentiment_data <- sentiment_analysis(data)
  plot_overall <- ggplot(word_data,
                         aes(x = reorder(word, n), y = n)) +
    geom_bar(stat = "identity") +
    labs(title = paste("Top Words for Emotion:", emotion),
         x = "Words", y = "Frequency") +
    theme_minimal()
  ggsave(output, plot = plot_overall, width = 8, height = 6)
  return(sentiment_data)
}


if (sys.nframe() == 0) {
  # main program, called via Rscript
  parser <- ArgumentParser(
    prog = "Sentiment Analysis",
    description = "Analyse toots for word and sentence sentiments"
                    )
  parser$add_argument("filename",
                   help="the file to read the toots from")
  parser$add_argument("--emotion",
                     default="anger",
    help="which emotion to search for")
  parser$add_argument('-v', '--verbose',
                     action='store_true',
    help="Print progress")
  parser$add_argument('-p', '--plot',
                     help="Plot something. Give the filename")
  args = parser$parse_args()  
  main(args)
}