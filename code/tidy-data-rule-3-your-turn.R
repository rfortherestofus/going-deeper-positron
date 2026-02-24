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

# Import Data -------------------------------------------------------------

enrollment_2022_2023 <- read_excel(
  path = "data-raw/fallmembershipreport_20222023.xlsx",
  sheet = "School 2022-23"
) |>
  clean_names()

# Tidy and Clean Data -----------------------------------------------------

enrollment_by_race_ethnicity_2022_2023 <-
  enrollment_2022_2023 |>
  select(
    district_institution_id,
    school_institution_id,
    x2022_23_american_indian_alaska_native:x2022_23_multi_racial
  ) |>
  select(-contains("percent")) |>
  pivot_longer(
    cols = -c(district_institution_id, school_institution_id),
    names_to = "race_ethnicity",
    values_to = "number_of_students"
  ) |>
  mutate(race_ethnicity = str_remove(race_ethnicity, pattern = "x2022_23_")) |>
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
  )

enrollment_by_race_ethnicity_2022_2023 |>
  mutate(race_ethnicity = str_remove(race_ethnicity, pattern = "x2022_23_"))

enrollment_by_race_ethnicity_2022_2023 |>
  mutate(race_ethnicity = str_remove(race_ethnicity, pattern = "x2022_23_")) |>
  mutate(
    race_ethnicity = recode(
      race_ethnicity,
      "american_indian_alaska_native" = "American Indian Alaska Native",
      "asian" = "Asian",
      "black_african_american" = "Black/African American",
      "hispanic_latino" = "Hispanic/Latino",
      "multiracial" = "Multi-Racial",
      "native_hawaiian_pacific_islander" = "Native Hawaiian Pacific Islander",
      "white" = "White",
      "multi_racial" = "Multiracial"
    )
  )

enrollment_by_race_ethnicity_2022_2023 |>
  mutate(race_ethnicity = str_remove(race_ethnicity, pattern = "x2022_23_")) |>
  mutate(
    race_ethnicity = if_else(
      race_ethnicity == "american_indian_alaska_native",
      true = "American Indian Alaska Native",
      false = race_ethnicity
    )
  ) |>
  mutate(
    race_ethnicity = if_else(
      race_ethnicity == "asian",
      true = "Asian",
      false = race_ethnicity
    )
  ) |>
  mutate(
    race_ethnicity = if_else(
      race_ethnicity == "black_african_american",
      true = "Black/African American",
      false = race_ethnicity
    )
  ) |>
  mutate(
    race_ethnicity = if_else(
      race_ethnicity == "hispanic_latino",
      true = "Hispanic/Latino",
      false = race_ethnicity
    )
  ) |>
  mutate(
    race_ethnicity = if_else(
      race_ethnicity == "multiracial",
      true = "Multi-Racial",
      false = race_ethnicity
    )
  ) |>
  mutate(
    race_ethnicity = if_else(
      race_ethnicity == "native_hawaiian_pacific_islander",
      true = "Native Hawaiian Pacific Islander",
      false = race_ethnicity
    )
  ) |>
  mutate(
    race_ethnicity = if_else(
      race_ethnicity == "white",
      true = "White",
      false = race_ethnicity
    )
  ) |>
  mutate(
    race_ethnicity = if_else(
      race_ethnicity == "multi_racial",
      true = "Multiracial",
      false = race_ethnicity
    )
  )

enrollment_by_race_ethnicity_2022_2023 |>
  mutate(race_ethnicity = str_remove(race_ethnicity, pattern = "x2022_23_")) |>
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
  )
