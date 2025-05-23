#libraries
library(magick)
library(tesseract)
library(tidyr)
library(knitr)
library(kableExtra)

#reading screenshot of the reservoir pdf from SB County Flood Control
img_path <- "~/Documents/reservoir.png"
img <- image_read(img_path)

#preprocessing to improve accuracy
img <- image_resize(img, "1800x")         # upscale
img <- image_convert(img, colorspace = 'gray')
img <- image_enhance(img)

#OCR
text <- ocr(img)

#raw text
lines <- unlist(strsplit(text, "\n"))
lines <- trimws(lines)
lines <- lines[lines != ""]  

#where is reservoir mentioned?
res_rows <- grep("Reservoir", lines, value = TRUE)
data_lines <- lines[which(lines %in% res_rows) + 1] 

#cleaning into data frame
reservoirs <- gsub(" Reservoir", "", res_rows)
data_split <- strsplit(data_lines, "\\s{2,}") 


reservoir_data <- do.call(rbind, lapply(seq_along(reservoirs), function(i) {
  c(Reservoir = reservoirs[i], data_split[[i]])
}))

reservoir_df <- as.data.frame(reservoir_data, stringsAsFactors = FALSE)

#keeping what I want
reservoir_df <- reservoir_df[5:8,1, drop=F]

#separate into individual variables
reservoir_df <- reservoir_df %>%
  separate(Reservoir, into = c("Name", "Spillway Elev. (ft)", "Current Elev. (ft)", "Max Storage (ac-ft)", 
                          "Current Storage (ac-ft)", "Current Capacity (ac-ft)", 
                          "Monthly Storage Change (ac-ft)", "Annual Storage Change (ac-ft)"),
           sep = " ")

#dropping if Twitchell does not have a reading
reservoir_df <- reservoir_df[reservoir_df$`Spillway Elev.` != "Twitchell",]
View(reservoir_df)

#storing as html table
table_reservoir <- kable(reservoir_df, format = "html", table.attr = "class='table table-bordered'") %>%
  kable_styling(full_width = FALSE)
table_reservoir

save_kable(table_reservoir, file = "~/Documents/reservoir_table.html")
