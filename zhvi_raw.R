#required libraries
library(tidyverse)
library(lubridate)
library(plotly)
library(htmlwidgets)


#updating .csv file from Zillow
zillow_url <- "https://files.zillowstatic.com/research/public_csvs/zhvi/County_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv?t=1750783887https://files.zillowstatic.com/research/public_csvs/zhvi/County_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv"

#making data readable
temp_file <- tempfile(fileext = ".csv")
download.file(zillow_url, temp_file, mode = "wb")
zhvi_data <- read_csv(temp_file)

View(zhvi_data)

#selecting Tri-Counties
target_counties <- c("Santa Barbara County", "Ventura County", "San Luis Obispo County")

#filtering for specific counties, also reshaping from wide to long (one date variable)
zhvi_tri <- zhvi_data %>%
  filter(RegionName %in% target_counties, State == "CA") %>%
  select(RegionName, matches("^\\d{4}-\\d{2}")) %>%
  pivot_longer(cols = -RegionName, names_to = "Date", values_to = "ZHVI")


#changing format for date variable
zhvi_tri$Date <- as.Date(zhvi_tri$Date)
  
View(zhvi_tri)

#graph
zhvi_raw <- ggplot(zhvi_tri, aes(x = Date, y = ZHVI, color = RegionName, group = RegionName)) +
  geom_line(size = 1.2) +
  labs(
    title = "Zillow Home Value Index (All Homes)",
    x = "Date", y = "Home Value Index (USD)",
    color = "County"
  ) +
  scale_color_manual(values = c("San Luis Obispo County" = "maroon", 
                                "Santa Barbara County" = "gold2", 
                                "Ventura County" = "skyblue2")) +
  scale_x_date(
    date_breaks = "4 years",         
    date_labels = "%Y"                   
  ) +
  scale_y_continuous(labels = scales::label_comma())+
  theme_minimal()

#save as interactive graph
graph_raw <- ggplotly(zhvi_raw)

#set working directory
setwd("~/R/housing")

#save as .html file
saveWidget(graph_raw, "zhvi_raw.html", selfcontained = TRUE)








