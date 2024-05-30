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
})

load_data<-function(filename) {
  data <- read.csv(filename, sep = ",", header = TRUE, stringsAsFactors = FALSE, colClasses=c("id"="character")) # preparing the data and giving the class to the "ID" column
  cleaned_data <- gsub("<[^>]+>", "", data$content) # getting rid of html
  data$content <- cleaned_data
  date_time <- ymd_hms(data$created_at) # making sure the date format is year-month-date hour-minute-second
  data$created_at <- date_time
  data <- filter(data, language== "en")
    return(data)
}

word_analysis<-function(toot_data, emotion) {
  word_data <- toot_data %>%
        unnest_tokens(word, content) #separating out the words
  nrc_joy <- get_sentiments("nrc") %>%
    filter(sentiment == emotion)
  filtered_by_emotion <- word_data %>%
    inner_join(nrc_joy, by= "word") %>%
      count(word) %>%
  arrange (desc(n))
    return(filtered_by_emotion)
}

sentiment_analysis<-function(toot_data) {
  data_lexicons <- toot_data %>%
    unnest_tokens(word, content) %>%
  group_by(id)
  emotions <-get_nrc_sentiment(data$content) #analyse sentiments using the syuzhet library and NRC lexicon
  emo_bar <-colSums(emotions) #dataframe of the emotions
  emo_sum <- data.frame(count=emo_bar,emotion =names(emo_bar)) #total count of the emotions
  ggplot(emo_sum, aes(x = reorder(emotion,-count), y = count))+
    geom_bar(stat = 'identity')
  bing_word_counts <- data_lexicons %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sort = TRUE)
  bing_top_10_words_by_sentiment <- bing_word_counts %>%
    group_by(sentiment) %>%
    slice_max(order_by = n, n=10) %>%
    ungroup() %>%
    mutate(word=reorder(word,n))
  bing_top_10_words_by_sentiment %>%
    ggplot(aes(word, n, fill=sentiment)) +
    facet_wrap(~sentiment, scales= "free_y") +
    labs(y= "contribution to sentiment", x = NULL) +
    coord_flip()
    return(data)

}

main <- function(args) {

}


if(sys.nframe() == 0) {

  # main program, called via Rscript
  parser = ArgumentParser(
                    prog="Sentiment Analysis",
                    description="Analyse toots for word and sentence sentiments"
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

