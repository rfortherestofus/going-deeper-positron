# Load Packages -----------------------------------------------------------

library(tidyverse)
library(fs)
library(scales)
library(ggrepel)
library(marquee)

# Create Directory --------------------------------------------------------

dir_create("data")

# Download Data -----------------------------------------------------------

download.file(
  "https://github.com/rfortherestofus/going-deeper-positron/raw/main/data/third_grade_math_proficiency.rds",
  mode = "wb",
  destfile = "data/third_grade_math_proficiency.rds"
)

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
  rename(year = academic_year)


# Theme ------------------------------------------------------------------

theme_dk <- function() {
  theme_minimal(base_family = "IBM Plex Mono") +
    theme(
      axis.title = element_blank(),
      legend.position = "none",
      panel.grid = element_blank(),
      plot.title = element_marquee(width = 1),
      plot.title.position = "plot"
    )
}

# Plot --------------------------------------------------------------------

top_growth_school <-
  third_grade_math_proficiency |>
  filter(district == "Portland SD 1J") |>
  group_by(school) |>
  mutate(
    growth_from_previous_year = percent_proficient - lag(percent_proficient)
  ) |>
  ungroup() |>
  slice_max(
    order_by = growth_from_previous_year,
    n = 1
  ) |>
  pull(school)


plot_title <-
  marquee_glue(
    "{.orange **{top_growth_school}**} showed large growth 
        in math proficiency over the last two years"
  )

third_grade_math_proficiency |>
  filter(district == "Portland SD 1J") |>
  mutate(
    highlight_school = case_when(
      school == top_growth_school ~ "Y",
      .default = "N"
    )
  ) |>
  mutate(
    school = fct_relevel(
      school,
      top_growth_school,
      after = Inf
    )
  ) |>
  mutate(
    percent_proficient_formatted = percent(percent_proficient, accuracy = 1)
  ) |>
  mutate(
    percent_proficient_formatted = case_when(
      highlight_school == "Y" & year == "2021-2022" ~
        str_glue(
          "{percent_proficient_formatted} of students
          were proficient
          in {year}"
        ),
      highlight_school == "Y" & year == "2018-2019" ~
        percent_proficient_formatted
    )
  ) |>
  ggplot(
    aes(
      x = year,
      y = percent_proficient,
      color = highlight_school,
      group = school,
      label = percent_proficient_formatted
    )
  ) +
  geom_line() +
  geom_text_repel(
    hjust = 0,
    lineheight = 0.9,
    direction = "x",
    family = "IBM Plex Mono"
  ) +
  scale_color_manual(
    values = c(
      "Y" = "orange",
      "N" = "gray80"
    )
  ) +
  scale_x_discrete(
    expand = expansion(add = c(0, 0.5))
  ) +
  scale_y_continuous(
    labels = percent_format(),
    limits = c(0, 1)
    # expand = expansion(add = c(0.1, 0.2))
  ) +
  annotate(
    geom = "text",
    x = 2.02,
    y = 0.6,
    hjust = 0,
    lineheight = 0.9,
    color = "gray70",
    label = str_glue(
      "Each gray line
    represents one
    school"
    )
  ) +
  labs(
    title = plot_title
  ) +
  theme_dk()
