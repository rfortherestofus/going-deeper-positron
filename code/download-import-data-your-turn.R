# Load Packages -----------------------------------------------------------

library(tidyverse)
library(fs)
library(readxl)
library(janitor)

# Create Directories ------------------------------------------------------

dir_create("data-raw")

# Download Data -----------------------------------------------------------

# https://www.oregon.gov/ode/reports-and-data/students/Pages/Student-Enrollment-Reports.aspx

download.file(
  "https://www.oregon.gov/ode/reports-and-data/students/Documents/fallmembershipreport_20222023.xlsx",
  mode = "wb",
  destfile = "data-raw/fallmembershipreport_20222023.xlsx"
)

download.file(
  "https://www.oregon.gov/ode/reports-and-data/students/Documents/fallmembershipreport_20212022.xlsx",
  mode = "wb",
  destfile = "data-raw/fallmembershipreport_20212022.xlsx"
)

download.file(
  "https://www.oregon.gov/ode/reports-and-data/students/Documents/fallmembershipreport_20202021.xlsx",
  mode = "wb",
  destfile = "data-raw/fallmembershipreport_20202021.xlsx"
)

download.file(
  "https://www.oregon.gov/ode/reports-and-data/students/Documents/fallmembershipreport_20192020.xlsx",
  mode = "wb",
  destfile = "data-raw/fallmembershipreport_20192020.xlsx"
)

download.file(
  "https://www.oregon.gov/ode/reports-and-data/students/Documents/fallmembershipreport_20182019.xlsx",
  mode = "wb",
  destfile = "data-raw/fallmembershipreport_20182019.xlsx"
)

# Import Data -------------------------------------------------------------

enrollment_2022_2023 <-
  read_excel(
    path = "data-raw/fallmembershipreport_20222023.xlsx",
    sheet = "School 2022-23"
  ) |>
  clean_names()
