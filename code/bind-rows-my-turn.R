# Load Packages -----------------------------------------------------------

library(tidyverse)
library(fs)
library(readxl)
library(janitor)

# Create Directories ------------------------------------------------------

dir_create("data-raw")

# Download Data -----------------------------------------------------------

# https://www.oregon.gov/ode/educator-resources/assessment/Pages/Assessment-Group-Reports.aspx

# download.file("https://www.oregon.gov/ode/educator-resources/assessment/Documents/TestResults2122/pagr_schools_math_tot_raceethnicity_2122.xlsx",
#               mode = "wb",
#               destfile = "data-raw/pagr_schools_math_tot_raceethnicity_2122.xlsx")
#
# download.file("https://www.oregon.gov/ode/educator-resources/assessment/Documents/TestResults2122/TestResults2019/pagr_schools_math_tot_raceethnicity_1819.xlsx",
#               mode = "wb",
#               destfile = "data-raw/pagr_schools_math_tot_raceethnicity_1819.xlsx")
#
# download.file("https://www.oregon.gov/ode/educator-resources/assessment/TestResults2018/pagr_schools_math_raceethnicity_1718.xlsx",
#               mode = "wb",
#               destfile = "data-raw/pagr_schools_math_raceethnicity_1718.xlsx")
#
# download.file("https://www.oregon.gov/ode/educator-resources/assessment/TestResults2017/pagr_schools_math_raceethnicity_1617.xlsx",
#               mode = "wb",
#               destfile = "data-raw/pagr_schools_math_raceethnicity_1617.xlsx")
#
# download.file("https://www.oregon.gov/ode/educator-resources/assessment/TestResults2016/pagr_schools_math_raceethnicity_1516.xlsx",
#               mode = "wb",
#               destfile = "data-raw/pagr_schools_math_raceethnicity_1516.xlsx")

# Import Data -------------------------------------------------------------

math_scores_2021_2022 <-
  read_excel(path = "data-raw/pagr_schools_math_tot_raceethnicity_2122.xlsx") |>
  clean_names()

math_scores_2018_2019 <-
  read_excel(path = "data-raw/pagr_schools_math_tot_raceethnicity_1819.xlsx") |>
  clean_names()


# Tidy and Clean Data -----------------------------------------------------

third_grade_math_proficiency_2021_2022 <-
  math_scores_2021_2022 |>
  filter(student_group == "Total Population (All Students)") |>
  filter(grade_level == "Grade 3") |>
  select(academic_year, school_id, contains("number_level")) |>
  pivot_longer(
    cols = starts_with("number_level"),
    names_to = "proficiency_level",
    values_to = "number_of_students"
  ) |>
  mutate(proficiency_level = parse_number(proficiency_level)) |>
  mutate(number_of_students = parse_number(number_of_students)) |>
  mutate(
    pct = number_of_students / sum(number_of_students, na.rm = TRUE),
    .by = school_id
  )

third_grade_math_proficiency_2018_2019 <-
  math_scores_2018_2019 |>
  filter(student_group == "Total Population (All Students)") |>
  filter(grade_level == "Grade 3") |>
  select(academic_year, school_id, contains("number_level")) |>
  pivot_longer(
    cols = starts_with("number_level"),
    names_to = "proficiency_level",
    values_to = "number_of_students"
  ) |>
  mutate(proficiency_level = parse_number(proficiency_level)) |>
  mutate(number_of_students = parse_number(number_of_students)) |>
  mutate(
    pct = number_of_students / sum(number_of_students, na.rm = TRUE),
    .by = school_id
  )

bind_rows(
  third_grade_math_proficiency_2021_2022,
  third_grade_math_proficiency_2018_2019
)

third_grade_math_proficiency_2021_2022 |>
  bind_rows(third_grade_math_proficiency_2018_2019)
