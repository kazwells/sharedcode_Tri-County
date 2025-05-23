library(dplyr)       # allows for better/more intuitive coding
library(blscrapeR)   # scrapes BLS data
library(ggplot2)     # package for good graphics
library(shiny)       # package for uploading to website (not used)
library(plotly)      # makes graphs interactive


#set up the API keys from website

api_key <- "813f57df699445db829b6a694b6f9d9a"

series_id_SB <- "LAUCN060830000000003"

series_id_SLO <- "LAUCN060790000000003"

series_id_VTA <- "LAUCN061110000000003"


#fetch the latest data

bls_data_SB <- bls_api(
  seriesid = series_id_SB,
  startyear = 2014,  # Adjust the year range
  endyear = format(Sys.Date(), "%Y"),  # Current year
  registrationKey = api_key
)

bls_data_SLO <- bls_api(
  seriesid = series_id_SLO,
  startyear = 2014,  # Adjust the year range
  endyear = format(Sys.Date(), "%Y"),  # Current year
  registrationKey = api_key
)

bls_data_VTA <- bls_api(
  seriesid = series_id_VTA,
  startyear = 2014,  # Adjust the year range
  endyear = format(Sys.Date(), "%Y"),  # Current year
  registrationKey = api_key
)


#creating one dataset

bls_data <- bls_data_SB %>%
  bind_rows(bls_data_SLO) %>%
  bind_rows(bls_data_VTA)

bls_data <- bls_data %>%
  mutate(county = ifelse(seriesID == "LAUCN060790000000003", "San Luis Obispo",
                         ifelse(seriesID == "LAUCN061110000000003", "Ventura", "Santa Barbara")))


#format data

bls_data <- bls_data %>%
  mutate(
    month = as.numeric(sub("M", "", period)), 
    date = as.Date(paste(year, month, "01", sep = "-"))
  ) %>%
  arrange(date)


#plot data

graph_unemployment <- ggplot(bls_data, aes(x = date, y = value, color = county)) +
  geom_line() +
  labs(
    title = "BLS Unemployment Data",
    x = "Date",
    y = "Unemployment Rate (Percent)",
    color = "County"
  ) +
  scale_color_manual(values = c("San Luis Obispo" = "maroon", "Santa Barbara" = "gold2", "Ventura" = "skyblue2")) + 
  theme_minimal()


#make it interactive

final_unemployment <- ggplotly(graph_unemployment)

final_unemployment


#saving as html widget (to use in website)
setwd("~/R/Economic Indicators/output")

htmlwidgets::saveWidget(final_unemployment, "unemployment.html", selfcontained = TRUE)
