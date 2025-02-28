
#command for observed event, 


# libraries

library(tidyverse)
library(here)
library(janitor)
library(readxl)

# load and clean data
acled_data <- read_csv(here("data", "ACLED_Africa_Regions.csv"))

drought_data <- read_csv(here("data", "Annual_SPEI_Africa_1980_2025.csv")) |>
  clean_names() |>
  filter(year >= 1997)


# group conflicts by year and summarize number conflicts per year for Mazowe admin zone

acled_mazowe <- acled_data |>
  filter(admin2 == "Mazowe")

mazowe_number_conflicts <- acled_mazowe |>
  group_by(year) |>
  summarise(number_conflicts = n())

ggplot(mazowe_number_conflicts, aes(x = year, y = number_conflicts)) +
  geom_point()


# 48 month plot Mazowe

drought_48mo_mazowe <- drought_data |>
  filter(shape_name == "Mazowe") |>
  select(year, spei_48_month) 

ggplot(data = drought_48mo_mazowe, aes(x = year, y = spei_48_month)) +
  geom_point()

# putting them together

mazowe_join <- right_join(mazowe_number_conflicts, drought_48mo_mazowe) |>
  mutate(number_conflicts = ifelse(is.na(number_conflicts), 0, number_conflicts))


drought_periods_mazowe <- mazowe_join %>%
  mutate(drought = spei_48_month < -1.5) %>%
  group_by(grp = cumsum(c(0, diff(drought)) * drought)) %>%
  filter(drought) %>%
  summarise(start = min(year), end = max(year)) %>%
  mutate(start = start - 4, end = end)  # Expand by half a year

ggplot(mazowe_join, aes(x = year, y = number_conflicts)) +
  geom_line(size = 1) +  
  geom_point(size = 2) +  
  geom_rect(data = drought_periods_mazowe, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), 
            fill = "red", alpha = 0.3, inherit.aes = FALSE) + 
  labs(title = "Conflict Trends with Drought Periods",
       x = "Year",
       y = "Number of Conflicts") +
  theme_minimal()


