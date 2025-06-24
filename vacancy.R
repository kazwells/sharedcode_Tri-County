#install libraries
library(tidycensus)
library(tidyverse)

#defining census API key
census_api_key("c4384d63f3d32a7ad13b1d29005bce56246dfa26", install = TRUE)

#years of interest
years <- c(2010:2019, 2021:2023)

#want ACS 1-year data
vacancy_data <- map_dfr(years, function(y) {
  get_acs(
    geography = "county",
    table = "B25004",             #vacancy code
    state = "CA",
    county = c("Santa Barbara", "Ventura", "San Luis Obispo"),
    year = y,
    survey = "acs1",
    output = "wide"
  ) %>%
    mutate(year = y)
})

View(vacancy_data)


vacancy_data <- vacancy_data %>%
  rename(vacancy = B25004_001E) %>%
  rename(county_name = NAME)
vacancy_data$county <- sub(",.*", "", vacancy_data$county_name)


graph <- ggplot(vacancy_data, aes(x = year, y = vacancy, color = county, group = county)) +
  geom_line(size = 1.2) +
  labs(
    title = "Vacancies (2010-2023)",
    x = "Year", y = "Number of Vacancies",
    color = "County"
  ) +
  scale_color_manual(values = c("San Luis Obispo County" = "maroon", 
                                "Santa Barbara County" = "gold2", 
                                "Ventura County" = "skyblue2")) +
  scale_y_continuous(labels = scales::label_comma())+
  theme_minimal()
graph


#standardized
vacancy_one <- vacancy_data %>%
  group_by(county) %>%
  mutate(
    base_value = vacancy[year == 2010],
    vacancy_std = vacancy / base_value
  ) %>%
  ungroup()

View(vacancy_one)

graph_one <- ggplot(vacancy_one, aes(x = year, y = vacancy_std, color = county, group = county)) +
  geom_line(size = 1.2) +
  labs(
    title = "Vacancies Over Time, (Standardized at 2010 = 1)",
    x = "Year", y = "Number of Vacancies",
    color = "County"
  ) +
  scale_color_manual(values = c("San Luis Obispo County" = "maroon", 
                                "Santa Barbara County" = "gold2", 
                                "Ventura County" = "skyblue2")) +
  scale_y_continuous(
    limits = c(0.6, 1.5),
    breaks = seq(0.6, 1.5, by = 0.3)
  )+
  scale_x_continuous(
    breaks =seq(2010,2024, by = 2)
  )+
  theme_minimal()
graph_one








