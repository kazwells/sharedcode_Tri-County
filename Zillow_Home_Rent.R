# load  libraries

library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)


# Zillow dataset URL (latest county-level ZHVI data)

zillow_csv_url <- "https://files.zillowstatic.com/research/public_csvs/zhvi/County_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv"


# file path for saving the CSV

file_path <- "C:\\Users\\kazum\\OneDrive\\Doc, Pre\\Benzarti\\excel\\zhvi_county.csv"

download.file(zillow_csv_url, destfile = file_path, mode = "wb")


# read the CSV file

zhvi_data <- read_csv(file_path)


# converting wide format to long format

zhvi_long <- zhvi_data %>%
  pivot_longer(cols = starts_with("20"), names_to = "Date", values_to = "ZHVI") %>%
  mutate(Date = as.Date(paste0(Date, "-01"), format = "%Y-%m-%d"))
zhvi_long <- zhvi_long %>%
  filter(State == "CA")


# Tri-counties1`  

selected_counties <- c("Ventura County", "Santa Barbara County", "San Luis Obispo County")
zhvi_filtered <- zhvi_long %>%
  filter(RegionName %in% selected_counties)


# plotting data

ggplot(zhvi_filtered, aes(x = Date, y = ZHVI, color = RegionName)) +
  geom_line(size = 1) +
  labs(title = "Zillow Home Value Index (ZHVI) Over Time",
       x = "Year",
       y = "ZHVI ($)",
       color = "County") +
  theme_minimal()

raw <- ggplot(zhvi_filtered, aes(x = Date, y = ZHVI, color = RegionName)) +
  geom_line(size = 1) +
  labs(title = "Zillow Home Value Index (ZHVI) Over Time",
       x = "Year",
       y = "ZHVI ($)",
       color = "County") +
  theme_minimal()


# setting working directory for a .html file

setwd <- ("C:\\Users\\kazum\\OneDrive\\Doc, Pre\\Benzarti\\HTML\\Zillow")


# saving graphs as html

raw_interactive <- ggplotly(raw)
htmlwidgets::saveWidget(raw_interactive, "Zillow_rent_raw.html", selfcontained = TRUE)



zhvi_one <- zhvi_filtered %>%
  mutate(standard = ifelse(RegionName == "Ventura County", ZHVI/257287.2
, ifelse(RegionName == "Santa Barbara County", ZHVI/267665.8
, ZHVI/205517.0
)))

one <- ggplot(zhvi_one, aes(x = Date, y = standard, color = RegionName)) +
  geom_line(size = 1) +
  labs(title = "Zillow Home Value Index (ZHVI) Over Time",
       x = "Year",
       y = "Ratio",
       subtitle = "Standardized at 1.31.2000",
       color = "County") +
  theme_minimal()

one_interactive <- ggplotly(one)
htmlwidgets::saveWidget(one_interactive, "Zillow_rent_raw.html", selfcontained = TRUE)



# RENT INDEX CODE

# Zillow dataset URL (latest county-level ZORI data)

zillow_csv_url <- "https://files.zillowstatic.com/research/public_csvs/zori/County_zori_uc_sfrcondomfr_sm_month.csv"


# file path for saving the CSV

file_path_zori <- "C:\\Users\\kazum\\OneDrive\\Doc, Pre\\Benzarti\\excel\\zori_county.csv"

download.file(zillow_csv_url, destfile = file_path_zori, mode = "wb")


# read the CSV file

zori_data <- read_csv(file_path)


# converting wide format to long format

zori_long <- zori_data %>%
  pivot_longer(cols = starts_with("20"), names_to = "Date", values_to = "ZORI") %>%
  mutate(Date = as.Date(paste0(Date, "-01"), format = "%Y-%m-%d"))
zori_long <- zori_long %>%
  filter(State == "CA")


# Tri-counties1`  

zori_filtered <- zori_long %>%
  filter(RegionName %in% selected_counties)


# plotting Zillow Home Value Index (ZHVI) Over Time

  ggplot(zori_filtered, aes(x = Date, y = ZORI, color = RegionName)) +
  geom_line(size = 1) +
  labs(title = "Zillow Observed Rent Index (ZORI) Over Time",
       x = "Year",
       y = "ZHVI ($)",
       color = "County") +
  theme_minimal()

raw_zori <- ggplot(zori_filtered, aes(x = Date, y = ZORI, color = RegionName)) +
  geom_line(size = 1) +
  labs(title = "Zillow Observed Rent Index (ZORI) Over Time",
       x = "Year",
       y = "ZHVI ($)",
       color = "County") +
  theme_minimal()


# setting working directory for a .html file

setwd <- ("C:\\Users\\kazum\\OneDrive\\Doc, Pre\\Benzarti\\HTML\\Zillow")


# saving graphs as html

raw_interactive_zori <- ggplotly(raw_zori)
htmlwidgets::saveWidget(raw_interactive_zori, "Zillow_rent_raw_zori.html", selfcontained = TRUE)


# standardizing ti-county data to start at 1
View(zori_filtered)
zori_one <- zori_filtered %>%
  mutate(standard = ifelse(RegionName == "Ventura County", ZORI/1785.719
                           , ifelse(RegionName == "Santa Barbara County", ZORI/1832.147
                                    , ZORI/1680.855
                           )))

one_zori <- ggplot(zori_one, aes(x = Date, y = standard, color = RegionName)) +
  geom_line(size = 1) +
  labs(title = "Zillow Observed Rent Index (ZORI) Over Time",
       x = "Year",
       y = "Ratio",
       subtitle = "Standardized at 1.31.2015",
       color = "County") +
  theme_minimal()
one_zori

one_interactive_zori <- ggplotly(one_zori)
htmlwidgets::saveWidget(one_interactive_zori, "Zillow_rent_standardzori.html", selfcontained = TRUE)

