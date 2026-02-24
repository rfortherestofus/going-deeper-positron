# Load Packages -----------------------------------------------------------

library(tidyverse)
library(fs)
library(readxl)
library(janitor)

# Create Directories ------------------------------------------------------

dir_create("data-raw")

# Download Data -----------------------------------------------------------

# https://www.oregon.gov/ode/educator-resources/assessment/Pages/Assessment-Group-Reports.aspx

download.file(
  "https://www.oregon.gov/ode/educator-resources/assessment/Documents/TestResults2122/pagr_schools_math_tot_raceethnicity_2122.xlsx",
  mode = "wb",
  destfile = "data-raw/pagr_schools_math_tot_raceethnicity_2122.xlsx"
)

download.file(
  "https://www.oregon.gov/ode/educator-resources/assessment/Documents/TestResults2122/TestResults2019/pagr_schools_math_tot_raceethnicity_1819.xlsx",
  mode = "wb",
  destfile = "data-raw/pagr_schools_math_tot_raceethnicity_1819.xlsx"
)

download.file(
  "https://www.oregon.gov/ode/educator-resources/assessment/TestResults2018/pagr_schools_math_raceethnicity_1718.xlsx",
  mode = "wb",
  destfile = "data-raw/pagr_schools_math_raceethnicity_1718.xlsx"
)

download.file(
  "https://www.oregon.gov/ode/educator-resources/assessment/TestResults2017/pagr_schools_math_raceethnicity_1617.xlsx",
  mode = "wb",
  destfile = "data-raw/pagr_schools_math_raceethnicity_1617.xlsx"
)

download.file(
  "https://www.oregon.gov/ode/educator-resources/assessment/TestResults2016/pagr_schools_math_raceethnicity_1516.xlsx",
  mode = "wb",
  destfile = "data-raw/pagr_schools_math_raceethnicity_1516.xlsx"
)


# Import Data -------------------------------------------------------------

math_scores_2021_2022 <-
  read_excel(path = "data-raw/pagr_schools_math_tot_raceethnicity_2122.xlsx") |>
  clean_names()
