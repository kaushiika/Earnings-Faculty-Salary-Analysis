# Load libraries
libs <- c("stringr", "dplyr", "janitor", "readr", "tibble")
invisible(lapply(libs, library, character.only = TRUE))

# Load data
results <- readLines("data/scorecard_raw.txt")

# Get results into a dataframe ----

# Remove backslash and quotation marks (makes the regex easier)
results_adj <- str_replace_all(results, "[\\,\"]", " ")

vars <- c(
  "latest\\.admissions\\.admission_rate\\.overall",
  "latest\\.student\\.enrollment\\.all",
  "school\\.faculty_salary",
  "latest\\.cost\\.attendance\\.academic_year",
  "school\\.state",
  "school\\.faculty_salary", # The average faculty salary (AVGFACSAL) produces the average faculty
  # salary per month, by dividing the total salary outlays by the number of
  # months worked for all full-time, nonmedical instructional staff.
  "latest\\.cost\\.tuition\\.in_state",
  "latest\\.cost\\.tuition\\.out_of_state",
  "school\\.faculty_salary",
  "school\\.ft_faculty_rate",
  "school\\.degrees_awarded\\.predominant_recoded",
  "school\\.main_campus",
  "school\\.carnegie_size_setting",
  "school\\.carnegie_undergrad",
  # "latest\\.earning\\.4_yrs_after_completion\\.not_working_not_enrolled\\.overall_count",
  "latest\\.earnings\\.1_yr_after_completion\\.not_working_not_enrolled\\.overall_count",
  # "latest\\.earnings\\.4_yrs_after_completion\\.working_not_enrolled\\.overall_count",
  "latest\\.earnings\\.4_yrs_after_completion\\.median",
  "latest\\.earnings\\.1_yr_after_completion\\.median",
  "latest\\.student\\.demographics\\.student_faculty_ratio",
  "latest\\.admissions\\.admission_rate\\.overall",
  "latest\\.admissions\\.sat_scores\\.average.overall",
  "school\\.ownership",
  "school\\.religious_affiliation",
  "school\\.online_only",
  "school\\.tuition_revenue_per_fte",
  # The net tuition revenue per full-time equivalent (FTE) student (TUITFTE)
  # uses tuition revenue minus discounts and allowances, and divides that
  # by the number of FTE undergraduate and graduate students.
  "school\\.instructional_expenditure_per_fte",
  # Instructional expenditures per FTE student (INEXPFTE) uses instructional
  # expenditures divided by the number of FTE students. 
  "latest\\.cost\\.avg_net_price\\.public",
  "latest\\.cost\\.avg_net_price\\.private",
  "latest\\.completion\\.completion_rate_4yr_100nt",
  "latest\\.completion\\.completion_rate_4yr_150nt",
  "latest\\.student\\.retention_rate\\.four_year\\.full_time",
  "school\\.operating",
  "school\\.carnegie_basic",
  "latest\\.student.size",
  "latest\\.aid\\.loan_principal"
)

# Get data for each field
data <- list()
for (i in 1:length(vars)){
  pattern <- str_c(vars[i], "\\s*:\\s?([^\\s}]*)")
  new_col <- unlist(str_extract_all(results_adj, pattern))
  new_col <- str_trim(str_remove_all(new_col, str_c(vars[i], "\\s:")))
  # print(head(new_col))
  data[[vars[i]]] <- new_col
}

# Convert list data into a dataframe
df <- as_tibble(data) |>
  janitor::clean_names() 

# Need to extract the school name separately
uni_name <- unlist(str_extract_all(results, "(?<=school\\.name\":\")[^\\\"]*(?=\\\")"))
df$university <- str_trim(uni_name)


# Clean up dataframe ----

# Section is not finished, need to fix the null values and convert fields
# to proper types

numeric_fields <- c(
  "latest_admissions_admission_rate_overall",                               
  "latest_student_enrollment_all",
  "latest_student_size",
  "school_faculty_salary",
  "latest_cost_attendance_academic_year",
  "latest_cost_tuition_in_state",
  "latest_cost_tuition_out_of_state",
  "school_ft_faculty_rate",
  "school_degrees_awarded_predominant_recoded",
  "latest_earnings_1_yr_after_completion_not_working_not_enrolled_overall_count",
  "latest_earnings_4_yrs_after_completion_median",
  "latest_earnings_1_yr_after_completion_median",
  "latest_student_demographics_student_faculty_ratio",
  "latest_admissions_sat_scores_average_overall",
  "school_tuition_revenue_per_fte",
  "school_instructional_expenditure_per_fte",
  "latest_cost_avg_net_price_public",
  "latest_cost_avg_net_price_private",
  "latest_completion_completion_rate_4yr_100nt",
  "latest_completion_completion_rate_4yr_150nt",
  "latest_student_retention_rate_four_year_full_time",
  "latest_aid_loan_principal",
  "school_online_only"
)

clean_numeric_field <- function(x, data = df){
  print(x)
  ifelse(data[[x]] == "null", NA, as.numeric(data[[x]]))
}

df_numeric <- sapply(numeric_fields, clean_numeric_field)

df_clean <- df |>
  select(-all_of(numeric_fields)) |>
  cbind(df_numeric) |>
  mutate(school_main_campus = as.logical(as.integer(school_main_campus)),
         school_online_only = as.logical(as.integer(school_online_only)),
         school_operating = as.logical(as.integer(school_operating)),
         carnegie_size_setting_label = case_when(
           school_carnegie_size_setting == "-2" ~ "Not applicable",
           school_carnegie_size_setting == "0" ~ "Not classified",
           school_carnegie_size_setting == "1"	~ "Two-year, very small",
           school_carnegie_size_setting == "2" ~	"Two-year, small",
           school_carnegie_size_setting == "3" ~ "Two-year, medium",
           school_carnegie_size_setting == "4" ~ "Two-year, large",
           school_carnegie_size_setting == "5" ~ "Two-year, very large",
           school_carnegie_size_setting == "6" ~ "Four-year, very small, primarily nonresidential",
           school_carnegie_size_setting == "7" ~ "Four-year, very small, primarily residential",
           school_carnegie_size_setting == "8" ~ "Four-year, very small, highly residential",
           school_carnegie_size_setting == "9" ~ "Four-year, small, primarily nonresidential",
           school_carnegie_size_setting == "10" ~ "Four-year, small, primarily residential",
           school_carnegie_size_setting == "11" ~ "Four-year, small, highly residential",
           school_carnegie_size_setting == "12"	~ "Four-year, medium, primarily nonresidential",
           school_carnegie_size_setting == "13" ~	"Four-year, medium, primarily residential",
           school_carnegie_size_setting == "14" ~	"Four-year, medium, highly residential",
           school_carnegie_size_setting == "15" ~	"Four-year, large, primarily nonresidential",
           school_carnegie_size_setting == "16" ~	"Four-year, large, primarily residential",
           school_carnegie_size_setting == "17" ~	"Four-year, large, highly residential",
           school_carnegie_size_setting == "18"	~ "Exclusively graduate/professional"
         ),
         school_carnegie_basic_label = case_when(
           school_carnegie_basic == "-2" ~ "Not Applicable",
           school_carnegie_basic == "0" ~ "(Not Classified)",
           school_carnegie_basic == "1" ~ "Associate's Colleges: High Transfer-High Traditional",
           school_carnegie_basic == "2" ~ "Associate's Colleges: High Transfer-Mixed Traditional/Nontraditional",
           school_carnegie_basic == "3" ~ "Associate's Colleges: High Transfer-High Nontraditional",
           school_carnegie_basic == "4" ~ "Associate's Colleges: Mixed Transfer/Career & Technical-High Traditional",
           school_carnegie_basic == "5" ~ "Associate's Colleges: Mixed Transfer/Career & Technical-Mixed Traditional/Nontraditional",
           school_carnegie_basic == "6" ~ "Associate's Colleges: Mixed Transfer/Career & Technical-High Nontraditional",
           school_carnegie_basic == "7" ~ "Associate's Colleges: High Career & Technical-High Traditional",
           school_carnegie_basic == "8" ~ "Associate's Colleges: High Career & Technical-Mixed Traditional/Nontraditional",
           school_carnegie_basic == "9" ~ "Associate's Colleges: High Career & Technical-High Nontraditional",
           school_carnegie_basic == "10" ~ "Special Focus Two-Year: Health Professions",
           school_carnegie_basic == "11" ~ "Special Focus Two-Year: Technical Professions",
           school_carnegie_basic == "12" ~ "Special Focus Two-Year: Arts & Design",
           school_carnegie_basic == "13" ~ "Special Focus Two-Year: Other Fields",
           school_carnegie_basic == "14" ~ "Baccalaureate/Associate's Colleges: Associate's Dominant",
           school_carnegie_basic == "15" ~ "Doctoral Universities: Very High Research Activity",
           school_carnegie_basic == "16" ~ "Doctoral Universities: High Research Activity",
           school_carnegie_basic == "17" ~ "Doctoral/Professional Universities",
           school_carnegie_basic == "18" ~ "Master's Colleges & Universities: Larger Programs",
           school_carnegie_basic == "19" ~ "Master's Colleges & Universities: Medium Programs",
           school_carnegie_basic == "20" ~ "Master's Colleges & Universities: Small Programs",
           school_carnegie_basic == "21" ~ "Baccalaureate Colleges: Arts & Sciences Focus",
           school_carnegie_basic == "22" ~ "Baccalaureate Colleges: Diverse Fields",
           school_carnegie_basic == "23" ~ "Baccalaureate/Associate's Colleges: Mixed Baccalaureate/Associate's",
           school_carnegie_basic == "24" ~ "Special Focus Four-Year: Faith-Related Institutions",
           school_carnegie_basic == "25" ~ "Special Focus Four-Year: Medical Schools & Centers",
           school_carnegie_basic == "26" ~ "Special Focus Four-Year: Other Health Professions Schools",
           school_carnegie_basic == "27" ~ "Special Focus Four-Year: Research Institution",
           school_carnegie_basic == "28" ~ "Special Focus Four-Year: Engineering and Other Technology-Related Schools",
           school_carnegie_basic == "29" ~ "Special Focus Four-Year: Business & Management Schools",
           school_carnegie_basic == "30" ~ "Special Focus Four-Year: Arts, Music & Design Schools",
           school_carnegie_basic == "31" ~ "Special Focus Four-Year: Law Schools",
           school_carnegie_basic == "32" ~ "Special Focus Four-Year: Other Special Focus Institutions"
         ),
         ownership_label = case_when(
           school_ownership == 1 ~ "Public",
           school_ownership == 2 ~ "Private Nonprofit",
           school_ownership == 3 ~ "Private For-Profit"
         )) |>
  select(university, school_state, everything())

# Clean the names a little 
names(df_clean) <- str_remove(names(df_clean), "school_|latest_")

# Load 'top_1000.csv'
top_1000 <- read_csv("data/top_1000.csv")

df_clean <- df_clean |>
  left_join(top_1000, by = c("university")) 

sum(!is.na(df_clean$rank)) # 819 matches - may need to do some work to match the remaining names

# Write Data ----

write_rds(df_clean, "data/scorecard_df.rds")
