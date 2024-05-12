suppressPackageStartupMessages({
library(sentimentr)
library(tidytext)
library(lubridate)
library(dplyr)
library(tidyr)
library(argparse)
library(ggpubr)
})

load_data<-function(filename) {
  data <- read.csv(filename, sep = ",", header = TRUE, stringsAsFactors = FALSE, colClasses=c("id"="character"))
  cleaned_data <- gsub("<[^>]+>", "", data$content)
  data$content <- cleaned_data
  date_time <- ymd_hms(data$created_at)
  data$created_at <- date_time
  data <- filter(data, language== "en")
    return(data)
}

word_analysis<-function(toot_data, emotion) {
    nrc_joy <- get_sentiments("nrc") %>%
      filter(sentiment == "joy")
    
   data %>%
      filter(book == "data") %>%
      inner_join(nrc_joy) %>%
      count(word, sort = TRUE)
    return(data)
}

sentiment_analysis<-function(toot_data) {

    return()

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
