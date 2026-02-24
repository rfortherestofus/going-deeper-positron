library(tidyverse)
library(here)
library(scales)

third_grade_math_proficiency <-
  read_rds(here("data/third_grade_math_proficiency.rds")) |>
  select(
    academic_year,
    school,
    school_id,
    district,
    proficiency_level,
    number_of_students
  ) |>
  mutate(
    is_proficient = case_when(
      proficiency_level >= 3 ~ TRUE,
      .default = FALSE
    )
  ) |>
  group_by(academic_year, school, district, school_id, is_proficient) |>
  summarize(number_of_students = sum(number_of_students, na.rm = TRUE)) |>
  ungroup() |>
  group_by(academic_year, school, district, school_id) |>
  mutate(
    percent_proficient = number_of_students /
      sum(number_of_students, na.rm = TRUE)
  ) |>
  ungroup() |>
  filter(is_proficient == TRUE) |>
  select(academic_year, school, district, percent_proficient) |>
  rename(year = academic_year) |>
  mutate(
    percent_proficient = case_when(
      is.nan(percent_proficient) ~ NA,
      .default = percent_proficient
    )
  ) |>
  mutate(
    percent_proficient_formatted = percent(percent_proficient, accuracy = 1)
  )

third_grade_math_proficiency |>
  write_rds("data/third_grade_math_proficiency_dichotomous.rds")
