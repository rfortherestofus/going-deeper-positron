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

enrollment_by_race_ethnicity |>
  filter(race_ethnicity == "Hispanic/Latino") |>
  ggplot(aes(x = year, y = pct, group = district)) +
  geom_line()
