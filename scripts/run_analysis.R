#### Script to run the entire analysis ####

# Pull initial scorecard data via API as a json file
source("scripts/01-pull_scorecard_data.R")
# Web-scrape data on university rankings
source("scripts/02-pull_top_1000.R")
# Process the json file into a usable dataframe and combine with university rankings
source("scripts/03-process_scorecard_data.R")
# Filter the dataframe, create new variables, clean up names to prepare for modeling
source("scripts/04-create_modeling_df.R")