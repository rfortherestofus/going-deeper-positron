# Load Packages -----------------------------------------------------------

library(tidyverse)
library(fs)
library(readxl)
library(janitor)

# Create Directories ------------------------------------------------------

dir_create("data-raw")

# Download Data -----------------------------------------------------------

# https://www.oregon.gov/ode/reports-and-data/students/Pages/Student-Enrollment-Reports.aspx

# download.file("https://www.oregon.gov/ode/reports-and-data/students/Documents/fallmembershipreport_20222023.xlsx",
#               mode = "wb",
#               destfile = "data-raw/fallmembershipreport_20222023.xlsx")
#
# download.file("https://www.oregon.gov/ode/reports-and-data/students/Documents/fallmembershipreport_20212022.xlsx",
#               mode = "wb",
#               destfile = "data-raw/fallmembershipreport_20212022.xlsx")
#
# download.file("https://www.oregon.gov/ode/reports-and-data/students/Documents/fallmembershipreport_20202021.xlsx",
#               mode = "wb",
#               destfile = "data-raw/fallmembershipreport_20202021.xlsx")
#
# download.file("https://www.oregon.gov/ode/reports-and-data/students/Documents/fallmembershipreport_20192020.xlsx",
#               mode = "wb",
#               destfile = "data-raw/fallmembershipreport_20192020.xlsx")
#
# download.file("https://www.oregon.gov/ode/reports-and-data/students/Documents/fallmembershipreport_20182019.xlsx",
#               mode = "wb",
#               destfile = "data-raw/fallmembershipreport_20182019.xlsx")

# Import, Tidy, and Clean Data -----------------------------------------------------

clean_enrollment_data <- function(excel_file, sheet_name) {
  read_excel(path = excel_file, sheet = sheet_name) |>
    clean_names() |>
    select(1, 3, 7:19) |>
    select(-contains("percent")) |>
    set_names(
      "district_institution_id",
      "school_institution_id",
      "american_indian_alaska_native",
      "asian",
      "native_hawaiian_pacific_islander",
      "black_african_american",
      "hispanic_latino",
      "white",
      "multi_racial"
    ) |>
    pivot_longer(
      cols = -c(district_institution_id, school_institution_id),
      names_to = "race_ethnicity",
      values_to = "number_of_students"
    ) |>
    mutate(
      race_ethnicity = case_when(
        race_ethnicity ==
          "american_indian_alaska_native" ~ "American Indian Alaska Native",
        race_ethnicity == "asian" ~ "Asian",
        race_ethnicity == "black_african_american" ~ "Black/African American",
        race_ethnicity == "hispanic_latino" ~ "Hispanic/Latino",
        race_ethnicity == "multiracial" ~ "Multi-Racial",
        race_ethnicity ==
          "native_hawaiian_pacific_islander" ~ "Native Hawaiian Pacific Islander",
        race_ethnicity == "white" ~ "White",
        race_ethnicity == "multi_racial" ~ "Multiracial"
      )
    ) |>
    mutate(number_of_students = parse_number(number_of_students)) |>
    group_by(district_institution_id, race_ethnicity) |>
    summarize(number_of_students = sum(number_of_students, na.rm = TRUE)) |>
    ungroup() |>
    group_by(district_institution_id) |>
    mutate(pct = number_of_students / sum(number_of_students)) |>
    ungroup() |>
    mutate(year = sheet_name)
}

enrollment_by_race_ethnicity_2022_2023 <-
  clean_enrollment_data(
    excel_file = "data-raw/fallmembershipreport_20222023.xlsx",
    sheet_name = "School 2022-23"
  )

enrollment_by_race_ethnicity_2021_2022 <-
  clean_enrollment_data(
    excel_file = "data-raw/fallmembershipreport_20212022.xlsx",
    sheet_name = "School 2021-22"
  )

enrollment_by_race_ethnicity <-
  bind_rows(
    enrollment_by_race_ethnicity_2021_2022,
    enrollment_by_race_ethnicity_2022_2023
  )
