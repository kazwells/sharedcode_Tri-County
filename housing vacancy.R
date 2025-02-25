# libraries

library(ggplot2)
library(readxl)
library(tidyr)
library(tidyverse)
library(plotly)
library(grid)=


# downloading files from working directory

setwd("C:\\Users\\kazum\\OneDrive\\Doc, Pre\\Benzarti\\excel")
file_list <- list.files(pattern = "*.csv")  # looking for csv files


# merging all the downloaded files together

merged_data <- lapply(file_list, function(file) {
  data <- read_csv(file)  # reading data
  data$Year <- gsub("ACS.*?([0-9]{4}).*", "\\1", file)  # extract year from filename
  return(data)
}) %>% bind_rows()

head(merged_data)


# cleaning data

merged_data <- merged_data %>%
  filter(GEO_ID != "Geography")
merged_data$B25002_003E <- as.numeric(merged_data$B25002_003E)
merged_data$B25002_001E <- as.numeric(merged_data$B25002_001E)


# gross number of Housing Vacancies

housing_vancancy<-ggplot(merged_data, aes(x = Year, y = B25002_003E, color = NAME, group = NAME)) +
  geom_line() +
  labs(title = "Gross Number of Housing Vacancies",
       x = "Year",
       y = "Number",
       color = "County") +
  scale_x_discrete(breaks = seq(2010, 2024, 2)) +
  annotate("text", x = Inf, y = Inf, 
           label = format(Sys.time(), "%Y-%m-%d"), 
           hjust = 1.1, vjust = 1.1, size = 3, color = "gray")+
    theme_minimal()
  

# percentage of Vacant Housing

merged_data <- merged_data %>%
  mutate(fraction = B25002_003E/B25002_001E)

vacancy <- ggplot(merged_data, aes(x = Year, y = fraction*100, color = NAME, group = NAME)) +
  geom_line() +
  labs(title = "Percentage of Housing that is Vacant",
       x = "Year",
       y = "Percentage",
       color = "County") +
  scale_x_discrete(breaks = seq(2010, 2024, 2)) +
  annotate("text", x = Inf, y = Inf, 
           label = format(Sys.time(), "%Y-%m-%d"), 
           hjust = 1.1, vjust = 1.1, size = 3, color = "gray")+
theme_minimal()
vacancy


# set wd

setwd("C:\\Users\\kazum\\OneDrive\\Doc, Pre\\Benzarti\\HTML\\Zillow")


# creating interactive graphs and saving as .html files (uploaded to Github)

vacancy_interactive <- ggplotly(housing_vancancy)
htmlwidgets::saveWidget(vacancy_interactive, "vacancy_raw.html", selfcontained = TRUE)

vacancy_one <- ggplotly(vacancy)
htmlwidgets::saveWidget(vacancy_one, "vacancy_one.html", selfcontained = TRUE)



 # CABAL


































