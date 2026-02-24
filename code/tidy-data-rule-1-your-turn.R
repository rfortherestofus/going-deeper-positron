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
  )
