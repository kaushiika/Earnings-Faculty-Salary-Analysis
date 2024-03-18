# Code to pull scorecard data as .json file

# Load Libraries ----
libs <- c("dplyr", "jsonlite", "httr", "stringr")
invisible(lapply(libs, library, character.only = TRUE))

# My API key
my_api <- "h5uUj9BCmSgFB6dQAPZefe4sYXFQyCMiXDjTjTEI"

# Set URL
base_url <- "https://api.data.gov/ed/collegescorecard/v1/schools.json?"

# Apply filters to query
filters <- "&_" 

# Variables to select
vars <- c("id", 
          "school.name", 
          "school.state", 
          "latest.admissions.admission_rate.overall", 
          "latest.student.enrollment.all", 
          "latest.cost.attendance.academic_year",
          "latest.cost.tuition.in_state", 
          "latest.cost.tuition.out_of_state", 
          "school.faculty_salary", 
          "school.ft_faculty_rate", 
          "school.degrees_awarded.predominant_recoded", # Predominant degree awarded (recoded 0s and 4s)
          "school.main_campus", # Logical, 1 if this is the main campus and 0 if not
          "school.carnegie_size_setting", 
          "school.carnegie_undergrad", 
          "latest.earning.4_yrs_after_completion.not_working_not_enrolled.overall_count",
          "latest.earnings.1_yr_after_completion.not_working_not_enrolled.overall_count",
          "latest.earnings.4_yrs_after_completion.working_not_enrolled.overall_count",
          "latest.earnings.4_yrs_after_completion.median",
          "latest.earnings.1_yr_after_completion.median",
          "latest.student.demographics.student_faculty_ratio", 
          "latest.admissions.admission_rate.overall",
          "latest.admissions.sat_scores.average.overall",
          "school.ownership", #  identify whether the institution’s
          # governance structure is public, private nonprofit, or private for-profit.
          "school.religious_affiliation",
          "school.online_only",
          "school.tuition_revenue_per_fte",
          "school.instructional_expenditure_per_fte",
          "latest.cost.avg_net_price.public",
          "latest.cost.avg_net_price.private",
          "latest.completion.completion_rate_4yr_100nt",
          "latest.completion.completion_rate_4yr_150nt",
          "latest.student.retention_rate.four_year.full_time",
          "school.operating", # 1 if currently operating
          "school.carnegie_basic",
          "latest.student.size",
          "latest.aid.loan_principal"
          )

fields <- str_flatten(vars, ",")

# Query API
res <- list()

# Need to loop over different pages of output to make sure we get everything
for (i in 0:100){
  # print(i)
  results <- GET(paste0(base_url, filters,"fields=", fields, 
                        str_c("&_page=", i, "&_per_page=100&api_key=", my_api)))
  res[[i+1]] <- results
}

# Results as JSON
json_output <- unlist(lapply(res, function(x) rawToChar(x$content)))
# class(json_output)
# length(json_output)

# Remove empty pages
json_output <- json_output[1:66]

# Function to remove the metadata from each element of json_output vector
get_results <- function(x){
  return(str_split_1(x, ",\"results\":")[2])
}

json_output <- lapply(json_output, get_results)

# Collapse into a string
json_output <- paste0(json_output, collapse = " ")

# Write text file
file <- file("data/scorecard_raw.txt")
writeLines(json_output, file)
close(file)
