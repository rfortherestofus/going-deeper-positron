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

enrollment_2021_2022 <- read_excel(
  path = "data-raw/fallmembershipreport_20212022.xlsx",
  sheet = "School 2021-22"
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
  ) |>
  mutate(number_of_students = parse_number(number_of_students)) |>
  group_by(district_institution_id, race_ethnicity) |>
  summarize(number_of_students = sum(number_of_students, na.rm = TRUE)) |>
  ungroup() |>
  group_by(district_institution_id) |>
  mutate(pct = number_of_students / sum(number_of_students)) |>
  ungroup() |>
  mutate(year = YOURCODEHERE)

enrollment_by_race_ethnicity_2021_2022 <-
  enrollment_2021_2022 |>
  select(
    attending_district_institution_id,
    attending_school_institution_id,
    YOURCODEHERE:YOURCODEHERE
  ) |>
  rename(
    YOURCODEHERE = attending_district_institution_id,
    YOURCODEHERE = attending_school_institution_id
  ) |>
  select(-contains("percent")) |>
  pivot_longer(
    cols = -c(district_institution_id, school_institution_id),
    names_to = "race_ethnicity",
    values_to = "number_of_students"
  ) |>
  mutate(race_ethnicity = str_remove(race_ethnicity, pattern = YOURCODEHERE)) |>
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
  mutate(year = YOURCODEHERE)

enrollment_by_race_ethnicity <-
  bind_rows(YOURCODEHERE, YOURCODEHERE)
