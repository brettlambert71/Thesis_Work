library(tidyverse)
locality_data <- read_csv("Locality_Info_Only.csv")
locality_data <- locality_data |>
  select("Population number", "Putative Species", "Latitude", "Longitude")
View(locality_data)
write_csv(locality_data, "Filtered_Localities.csv")

loc <- read_csv("Filtered_Localities.csv")
View(loc)
