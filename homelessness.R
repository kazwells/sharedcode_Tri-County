# libraries

library(readxl)
library(dplyr)  
library(purrr)  
library(ggplot2)
library(plotly)


# define file path

file_path <- "C:/Users/kazum/Downloads/homelessness.xlsx"


# get all sheet names

sheet_names <- excel_sheets(file_path)


# read all sheets and combine them

combined_data <- map_df(sheet_names[sheet_names %in% as.character(2007:2023)], 
                        ~ read_excel(file_path, sheet = .x) %>% mutate(Year = .x)) %>%
  select(c(`CoC Name`, `CoC Number`, `Overall Homeless`, `Year`)) %>%
  rename(CoC_Name = `CoC Name`, CoC_Number = `CoC Number`, Overall_Homeless = `Overall Homeless`)

combined_data_1 <- combined_data %>%
  filter(`CoC_Number` %in% c("CA-603", "CA-611", "CA-614"))


# adding in 2024 data

`add_on` <- data.frame(`CoC_Name` = c("Santa Maria/Santa Barbara County CoC",
                                      "San Luis Obispo County CoC",
                                      "Oxnard, San Buenaventura/Ventura County CoC"),
                       `CoC_Number` = c("CA-603", "CA-614", "CA-611"),
                       `Overall_Homeless` = c(2119, 1175, 2358),
                       Year = c('2024', '2024', '2024'))


# binding 2024 data to main dataset

combined_data_1 <- combined_data_1 %>%
  bind_rows(add_on)


# creating varaible with tri-counties all starting at 1

data_100 <- combined_data_1 %>%
  mutate(Overall_Homeless = ifelse(CoC_Number == "CA-603", Overall_Homeless/4253, 
                                   ifelse(CoC_Number== "CA-614", Overall_Homeless/2408,
                                          Overall_Homeless/1961)) )


# graphing gross homeless count

homeless <- ggplot(combined_data_1, aes(x = Year, y = `Overall_Homeless`, group = `CoC_Number`, 
                            color = `CoC_Number`)) +
  geom_line() +
  scale_color_manual(values = c("CA-603" = "navy", "CA-614" = "maroon", "CA-611" = "gold"),
                     labels = c("CA-603" = "Santa Barbara", "CA-614" = "San Luis Obispo", "CA-611" = "Ventura")) +
  
  labs(
    title = "Homelessness Point-in-Time Count",
    x = "Year",
    y = "Number of Homeless",
    color = "Area"
  ) +
  scale_x_discrete(breaks = seq(2000, 2025, by = 4)) +  # Show every 2 years
  theme_minimal()


# setting working directory

setwd("C:\\Users\\kazum\\OneDrive\\Doc, Pre\\Benzarti\\HTML\\Zillow")


# making graphics interactive and downloading as a .html files

homeless_interactive <- ggplotly(homeless)
htmlwidgets::saveWidget(homeless_interactive, "homeless_raw.html", selfcontained = TRUE)



homeless_one <- ggplot(data_100, aes(x = Year, y = `Overall_Homeless`, group = `CoC_Number`, 
                            color = `CoC_Number`)) +
  geom_line() +
  scale_color_manual(values = c("CA-603" = "navy", "CA-614" = "maroon", "CA-611" = "gold"),
                     labels = c("CA-603" = "Santa Barbara", "CA-614" = "San Luis Obispo", "CA-611" = "Ventura")) +
  
  labs(
    title = "Homelessness Point-in-Time Count",
    x = "Year",
    y = "Number of Homeless",
    color = "Area"
  ) +
  scale_x_discrete(breaks = seq(2000, 2025, by = 4)) +  # Show every 2 years
  
  theme_minimal()

homeless_o <- ggplotly(homeless_one)
htmlwidgets::saveWidget(homeless_o, "homeless_one.html", selfcontained = TRUE)





