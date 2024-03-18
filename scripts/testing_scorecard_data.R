# Load Libraries ----
libs <- c("httr", "tidyverse", "rscorecard")
invisible(lapply(libs, library, character.only = TRUE))

# My API key
my_api <- "h5uUj9BCmSgFB6dQAPZefe4sYXFQyCMiXDjTjTEI"

# Pull the data directly via url ----
URL <- "https://api.data.gov/ed/collegescorecard/v1/schools?"

get.data <- GET(URL, query=list(api_key = my_api))
res <- content(get.data)

# Split res into metadata and results
metadata <- res$metadata
data <- res$results # will take some work to figure this out

# Pull the data using rscorecard ----

# Set API
sc_key(my_api)

df_latest <- sc_init() |>
  # Pulling schools in New England 
  # sc_filter(stabbr == c('MA', "VT", "NH", "ME", "CT", "RI")) |>
  # I have selected a somewhat random collection of variables
  # There may be better fields to use! 
  sc_select(
    unitid, # Unit ID for institution
    instnm, # Institution name
    stabbr, # State postcode
    adm_rate, # Admission rate
    ug, # Enrollment of all undergraduate students
    costt4_a, # Average cost of attendance (academic year institutions)
    tuitionfee_in, # In-state tuition and fees
    tuitionfee_out, # Out-of-state tuition and fees
    avgfacsal, # Average faculty salary
    pftfac, # Proportion of faculty that is full-time
    sch_deg, # Predominant degree awarded (recoded 0s and 4s)
    main, # Flag for main campus
    ccsizset, # Carnegie Classification -- size and setting
        # -2	Not applicable
        # 0	(Not classified)
        # 1	Two-year, very small
        # 2	Two-year, small
        # 3	Two-year, medium
        # 4	Two-year, large
        # 5	Two-year, very large
        # 6	Four-year, very small, primarily nonresidential
        # 7	Four-year, very small, primarily residential
        # 8	Four-year, very small, highly residential
        # 9	Four-year, small, primarily nonresidential
        # 10	Four-year, small, primarily residential
        # 11	Four-year, small, highly residential
        # 12	Four-year, medium, primarily nonresidential
        # 13	Four-year, medium, primarily residential
        # 14	Four-year, medium, highly residential
        # 15	Four-year, large, primarily nonresidential
        # 16	Four-year, large, primarily residential
        # 17	Four-year, large, highly residential
        # 18	Exclusively graduate/professional
    ccugprof, # Carnegie Classification -- undergraduate profile
        # -2	Not applicable
        # 0	Not classified (Exclusively Graduate)
        # 1	Two-year, higher part-time
        # 2	Two-year, mixed part/full-time
        # 3	Two-year, medium full-time
        # 4	Two-year, higher full-time
        # 5	Four-year, higher part-time
        # 6	Four-year, medium full-time, inclusive, lower transfer-in
        # 7	Four-year, medium full-time, inclusive, higher transfer-in
        # 8	Four-year, medium full-time, selective, lower transfer-in
        # 9	Four-year, medium full-time , selective, higher transfer-in
        # 10	Four-year, full-time, inclusive, lower transfer-in
        # 11	Four-year, full-time, inclusive, higher transfer-in
        # 12	Four-year, full-time, selective, lower transfer-in
        # 13	Four-year, full-time, selective, higher transfer-in
        # 14	Four-year, full-time, more selective, lower transfer-in
        # 15	Four-year, full-time, more selective, higher transfer-in
    count_nwne_4yr, # Number of graduates not working and not enrolled 4 years after completing
    count_nwne_1yr, # Number of graduates not working and not enrolled 1 year after completing
    count_wne_4yr, # Number of graduates working and not enrolled 4 years after completing
    md_earn_wne_4yr, # Median earnings of graduates working and not enrolled 4 years after completing
    md_earn_wne_1yr, # Median earnings of graduates working and not enrolled 1 year after completing
    stufacr, # Undergraduate student to instructional faculty ratio
    adm_rate,
    sat_avg,
    control,
    relaffil,
    distanceonly,
    tuitfte,
    inexpfte,
    npt4_pub,
    npt4_priv,
    c100_4,
    c150_4,
    ret_ft4,
    curroper,
    ccbasic,
    ugds, # Might be the actual enrollment field? 
    debt_mdn # This is the median loan debt accumulated at the institution18 by all
    # student borrowers of federal loans19 who separate (i.e., either graduate
    # or withdraw) in a given fiscal year, measured at the point of separation
    ) |>
  # Pulling latest data, but you can select other years
  # However, can only select one year at a time
  sc_year("latest") |> 
  sc_get(debug = TRUE)

# Counting by 
df_latest |> 
  count(ccsizset)

# Grabbing schools listed as 'four-year' in this field
df_4y <- df_latest |> 
  filter(ccugprof %in% 5:15) 

# Look at some distributions
hist(df_4y$costt4_a)
hist(df_4y$avgfacsal)
hist(df_4y$adm_rate)
hist(df_4y$pftfac)

# Testing out combining data from different years
df_2020 <- sc_init() |>
  sc_filter(stabbr == 'MA') %>%
  sc_select(unitid, instnm, stabbr, adm_rate, ug,
            costt4_a, TUITIONFEE_IN, TUITIONFEE_OUT,
            AVGFACSAL, PFTFAC) |>
  sc_year(2020) |>
  sc_get() |>
  mutate(year = as.character(year))

df_2021 <- sc_init() |>
  sc_filter(stabbr == 'MA') %>%
  sc_select(unitid, instnm, stabbr, adm_rate, ug,
            costt4_a, TUITIONFEE_IN, TUITIONFEE_OUT,
            AVGFACSAL, PFTFAC) |>
  sc_year(2021) |>
  sc_get() |>
  mutate(year = as.character(year))

df <- bind_rows(df_2021, df_2020)
