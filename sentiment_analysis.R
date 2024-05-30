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
  data <- read.csv(filename, sep = ",", header = TRUE, stringsAsFactors = FALSE, colClasses=c("id"="character")) # preparing the data and giving the class to the "ID" column
  cleaned_data <- gsub("<[^>]+>", "", data$content) # getting rid of html
  data$content <- cleaned_data
  date_time <- ymd_hms(data$created_at) # making sure the date format is year-month-date hour-minute-second
  data$created_at <- date_time
  data <- filter(data, language== "en")
    return(data)
}

word_analysis<-function(toot_data, emotion) {
  tidy_data <- data() %>%
    group_by(id) %>% # so we have each comment as a group
    summarize()
    mutate(            # adding columns
      id = row_number())
    ungroup() %>% 
    unnest_tokens(word, text) #separating out the words
  
   nrc_joy <- get_sentiments("nrc") %>%
    filter(sentiment == "joy")
  data %>%
      filter(data) %>%
      inner_join(nrc_joy) %>%
      count(word, sort = TRUE)
  arrange (.data, desc(), .by_group = TRUE)
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

