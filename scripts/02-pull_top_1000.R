library(tidyverse)
library(rvest)
library(httr)

#Retrieving url and creating HTML
url <- "https://www.4icu.org/us/"
response <- GET(url)

rankings_html <- read_html(response)

rankings_text <- as.character(rankings_html)
writeLines(rankings_text, "rankings.html")

rankings_lines <- readLines("rankings.html")

#Retrieves text from the html
rankings_text <- paste(rankings_lines, collapse = "")

#Using REGEXP, patterns found by opening the html file with microsoft notepad
rank_pattern <- "<b>(\\d+)</b>"
university_pattern <- "<a href=\"/reviews/\\d+\\.htm\">(.*?)</a>"

#extracting using the patterns
college_rank <- str_extract_all(rankings_text, pattern = rank_pattern)[[1]]
universities <- str_extract_all(rankings_text, pattern = university_pattern)[[1]]

#fixing the strings created to include just the rank and university
rankings <- str_replace_all(college_rank, "<b>|</b>", "")
universities <- str_replace_all(universities, "<a href=\"/reviews/\\d+\\.htm\">|</a>", "")

#retrieves ranks of the top 1000, amherst ranked 60
r_1000 <- head(rankings, 1000)
u_1000 <- head(universities, 1000)

#creates a dataframe of the colleges with their respective rank
df <- data.frame(rank = r_1000, university = u_1000)

df$rank <- as.numeric(df$rank)

# fixing ranking error at rank 964
brooklyn_college_index <- which(df$university == "Brooklyn College")

df$rank[brooklyn_college_index] <- 964

#increments the ranks after the fix by 1
df$rank[(brooklyn_college_index + 1):nrow(df)] <- df$rank[(brooklyn_college_index + 1):nrow(df)] + 1

# Write Data
write_csv(df, "data/top_1000.csv")
