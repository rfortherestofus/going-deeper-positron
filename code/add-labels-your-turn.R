# Load Packages -----------------------------------------------------------

library(tidyverse)
library(fs)
library(scales)
library(ggrepel)

# Create Directory --------------------------------------------------------

dir_create("data")

# Download Data -----------------------------------------------------------

# download.file("https://github.com/rfortherestofus/going-deeper-v2/raw/main/data/enrollment_by_race_ethnicity.rds",
#               mode = "wb",
#               destfile = "data/enrollment_by_race_ethnicity.rds")

# Import Data -------------------------------------------------------------

enrollment_by_race_ethnicity <-
  read_rds("data/enrollment_by_race_ethnicity.rds") |>
  select(-district_institution_id) |>
  select(year, district, everything()) |>
  mutate(
    year = case_when(
      year == "School 2021-22" ~ "2021-2022",
      year == "School 2022-23" ~ "2022-2023",
    )
  ) |>
  mutate(pct_formatted = percent(pct, accuracy = 1))

# Plot --------------------------------------------------------------------

top_growth_district <-
  enrollment_by_race_ethnicity |>
  filter(race_ethnicity == "Hispanic/Latino") |>
  group_by(district) |>
  mutate(growth_from_previous_year = pct - lag(pct)) |>
  ungroup() |>
  drop_na(growth_from_previous_year) |>
  slice_max(order_by = growth_from_previous_year, n = 1) |>
  pull(district)

enrollment_by_race_ethnicity |>
  filter(race_ethnicity == "Hispanic/Latino") |>
  mutate(
    highlight_district = case_when(
      district == top_growth_district ~ "Y",
      .default = "N"
    )
  ) |>
  mutate(
    pct_formatted = case_when(
      highlight_district == "Y" & year == "2022-2023" ~ str_glue(
        "{pct_formatted} of students
                                                               were Hispanic/Latino
                                                               in {year}"
      ),
      highlight_district == "Y" & year == "2021-2022" ~ pct_formatted,
      .default = NA
    )
  ) |>
  mutate(district = fct_relevel(district, top_growth_district, after = Inf)) |>
  ggplot(aes(
    x = year,
    y = pct,
    group = district,
    color = highlight_district,
    label = pct_formatted
  )) +
  geom_line() +
  geom_text_repel(hjust = 0, lineheight = 0.9, direction = "x") +
  scale_color_manual(
    values = c(
      "N" = "grey90",
      "Y" = "orange"
    )
  ) +
  scale_y_continuous(labels = percent_format()) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  )
