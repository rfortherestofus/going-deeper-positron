# Load Packages -----------------------------------------------------------

library(tidyverse)
library(fs)

# Create Directory --------------------------------------------------------

dir_create("data")

# Download Data -----------------------------------------------------------

# download.file("https://github.com/rfortherestofus/going-deeper-v2/raw/main/data/third_grade_math_proficiency.rds",
#               mode = "wb",
#               destfile = "data/third_grade_math_proficiency.rds")

# Import Data -------------------------------------------------------------

third_grade_math_proficiency <-
  read_rds("data/third_grade_math_proficiency.rds") |>
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
  )

# Plot --------------------------------------------------------------------

third_grade_math_proficiency |>
  filter(year == "2021-2022") |>
  filter(district == "Portland SD 1J") |>
  ggplot(aes(x = percent_proficient, y = school)) +
  geom_col()

third_grade_math_proficiency |>
  filter(year == "2021-2022") |>
  filter(district == "Portland SD 1J") |>
  ggplot(aes(x = percent_proficient, y = reorder(school, percent_proficient))) +
  geom_col()

third_grade_math_proficiency |>
  filter(year == "2021-2022") |>
  filter(district == "Portland SD 1J") |>
  mutate(school = fct_reorder(school, percent_proficient)) |>
  ggplot(aes(x = percent_proficient, y = school)) +
  geom_col()
