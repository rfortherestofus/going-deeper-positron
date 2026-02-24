# Load Packages -----------------------------------------------------------

library(tidyverse)
library(fs)

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
  )

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
  mutate(district = fct_relevel(district, top_growth_district, after = Inf)) |>
  ggplot(aes(x = year, y = pct, group = district, color = highlight_district)) +
  geom_line() +
  scale_color_manual(
    values = c(
      "N" = "grey80",
      "Y" = "orange"
    )
  )
