# libraries

library(dplyr)
library(ggplot2)
library(tidyr)


# defining file path to data

file_path <- "C:\\Users\\kazum\\Downloads\\annual_summary_for__1980-2023.csv"


# downlaoding data

permit_data <- read_csv(file_path)


# only looking at total units for now

permits_total_only <- permit_data %>%
  filter(Series == "Total Units")


# plot the data and save as an interactive .html file in the wd


# graph of gross number of housing permits
permits_i <- ggplotly(ggplot(permits_total_only, aes(x = Year, y = Permits, color = Location)) +
  geom_line(size = 1) +
  labs(title = "Total Building Permits Per Year",
       subtitle = "1980 - 2023",
       x = "Year",
       y = "Permit Numbers",
       color = "County") +
  theme_minimal())
permits_interactive <- ggplotly(permits_i)
htmlwidgets::saveWidget(permits_interactive, "permits_raw.html", selfcontained = TRUE)



permits_total_only_one <- permits_total_only %>%
  mutate(standard = ifelse(Location == "Ventura County, CA", Permits/4022
                           , ifelse(Location == "Santa Barbara County, CA", Permits/1111
                                    , Permits/1890
                           )))


# graph of housing permits starting with each county standardized at 1
permits_i_stan <- ggplotly(ggplot(permits_total_only_one, aes(x = Year, y = standard, color = Location)) +
  geom_line(size = 1) +
  labs(title = "Total Building Permits Per Year",
       subtitle = "1980 - 2023",
       x = "Year",
       y = "Permit Numbers",
       color = "County") +
  theme_minimal())

permits_stan_interactive <- ggplotly(permits_i_stan)
htmlwidgets::saveWidget(permits_stan_interactive, "permits_stan.html", selfcontained = TRUE)
