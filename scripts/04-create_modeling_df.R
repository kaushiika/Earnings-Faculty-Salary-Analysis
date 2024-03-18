# Script to create a cleaned dataframe for modeling

# Load Libraries ----
libs <- c("dplyr", "tidyr", "readr",
          "tibble", "stringr")

invisible(lapply(libs, library, character.only = TRUE))

# Load Data ----
df_raw <- read_rds("data/scorecard_df.rds")

# Clean Data ----

df_filt <- df_raw |>
  filter(
    # Only look at main campuses
    main_campus == TRUE, 
    # Remove online-only schools (high levels of NAs)
    online_only == FALSE, 
    # Remove Associates and Special Focus schools (also high levels of NAs)
    carnegie_basic %in% 14:23, 
  ) |>
  mutate(
    school_type = case_when(
      carnegie_basic %in% 15:17 ~ "Doctoral",
      carnegie_basic %in% 18:20 ~ "Masters",
      carnegie_basic %in% c(14, 21:23) ~ "Baccalaureate"
    ),
    avg_cost_comb = ifelse(is.na(cost_avg_net_price_public),
                           cost_avg_net_price_private, 
                           cost_avg_net_price_public),
    rank_ind = factor(ifelse(is.na(rank), 0, 1))
  ) |>
  select(university, 
         state, 
         # Possible response variables
         completion_rate_100nt = completion_completion_rate_4yr_100nt,
         completion_rate_150nt = completion_completion_rate_4yr_150nt,
         median_earnings_4yr = earnings_4_yrs_after_completion_median,
         median_earnings_1yr = earnings_1_yr_after_completion_median,
         retention_rate = student_retention_rate_four_year_full_time,
         # Possible predictors
         ownership_label,
         adm_rate = admissions_admission_rate_overall,
         student_size, 
         faculty_salary, 
         ft_faculty_rate,
         rank_ind,
         student_fac_ratio = student_demographics_student_faculty_ratio,
         tuition_revenue_per_fte, 
         instructional_expenditure_per_fte,
         carnegie_basic_label,
         carnegie_size_setting_label
         ) |>
  mutate(
    # Creating some logged transformations of variables (mostly $ variables)
    log_fac_salary = ifelse(faculty_salary == 0, 0, log2(faculty_salary)),
    log_earnings_4yr = log2(median_earnings_4yr),
    log_earnings_1yr = log2(median_earnings_1yr),
    log_revenue = log2(tuition_revenue_per_fte),
    log_instr_exp = log2(instructional_expenditure_per_fte)
  )

dim(df_filt)

df <- df_filt |>
  drop_na()

# Write Data
write_rds(df, "data/modeling_df.rds")
