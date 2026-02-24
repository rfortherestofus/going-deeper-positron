# Load Packages -----------------------------------------------------------

library(tidyverse)
library(janitor)
library(readxl)
library(fs)

# Create Directory --------------------------------------------------------

dir_create("data-raw")

# Download Data -----------------------------------------------------------

# download.file(url = "https://www.oregon.gov/ode/educator-resources/assessment/Documents/TestResults2122/pagr_schools_math_tot_raceethnicity_2122.xlsx",
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
  read_excel("data-raw/pagr_schools_math_tot_raceethnicity_2122.xlsx") |>
  clean_names()

third_grade_math_proficiency_2021_2022 <-
  math_scores_2021_2022 |>
  filter(student_group == "Total Population (All Students)") |>
  filter(grade_level == "Grade 3") |>
  select(academic_year, school_id, contains("number_level_")) |>
  pivot_longer(
    cols = contains("number_level_"),
    names_to = "proficiency_level",
    values_to = "number_of_students"
  )

third_grade_math_proficiency_2021_2022 |>
  mutate(
    proficiency_level = str_remove(proficiency_level, pattern = "number_level_")
  )

third_grade_math_proficiency_2021_2022 |>
  mutate(
    proficiency_level = recode(
      proficiency_level,
      "number_level_4" = "4",
      "number_level_3" = "3",
      "number_level_2" = "2",
      "number_level_1" = "1"
    )
  )

third_grade_math_proficiency_2021_2022 |>
  mutate(
    proficiency_level = if_else(
      proficiency_level == "number_level_4",
      true = "4",
      false = proficiency_level
    )
  ) |>
  mutate(
    proficiency_level = if_else(
      proficiency_level == "number_level_3",
      true = "3",
      false = proficiency_level
    )
  ) |>
  mutate(
    proficiency_level = if_else(
      proficiency_level == "number_level_2",
      true = "2",
      false = proficiency_level
    )
  ) |>
  mutate(
    proficiency_level = if_else(
      proficiency_level == "number_level_1",
      true = "1",
      false = proficiency_level
    )
  )

third_grade_math_proficiency_2021_2022 |>
  mutate(
    proficiency_level = case_when(
      proficiency_level == "number_level_4" ~ "4",
      proficiency_level == "number_level_3" ~ "3",
      proficiency_level == "number_level_2" ~ "2",
      proficiency_level == "number_level_1" ~ "1"
    )
  )

third_grade_math_proficiency_2021_2022 |>
  mutate(proficiency_level = parse_number(proficiency_level))
