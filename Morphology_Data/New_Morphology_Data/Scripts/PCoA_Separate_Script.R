# Load packages for cleaning and analyses ---------------------------------
library(tidyverse)
library(ggplot2)
library(ggfortify)
library(devtools)
library(ggbiplot)
library(zoo)
library(fMultivar)
library(cluster)
library(ape)
library(vegan)
# Read in data and clean --------------------------------------------------
raw <- read_csv("Morph_Data/Final_RAW_Copy.csv") 
str(raw)
#Remove correlated or ambiguous traits
raw_2 <- raw %>% 
  select(-c(Abaxial_Leaf_Pubescence, Style_Tube_Shape, Calyx_Ad_Pub))

#calculate amount of missing data
na_freq <- raw_2 %>% 
  summarise(across(everything(), ~ sum(is.na(.x))))
na_perc <- na_freq/nrow(raw_2)
print(na_perc)


# Interpolation of missing data. Max missing is (blank) for (trait); the rest are (blank) or less. -------------------------------------------------------

# Create function to replace NA with mode
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Create function to interpolate based on col type
interp_by_type <- function(column) {
  if (is.numeric(column)) {
    column[is.na(column)] <- mean(column, na.rm = TRUE)
  } else if (is.character(column)) {
    column[is.na(column)] <- get_mode(column)
  }
  return(column)
}
str(raw_2)

#Apply function to all cols in df
full_interp_data <- data.frame(lapply(raw_2, interp_by_type))

#Save ID and Species then remove for analysis
ID_Spec <- full_interp_data %>% 
  select(1:2)

full_interp_data <- full_interp_data %>% 
  select(!1:2)

#Change character cols to factors
full_interp_data <- full_interp_data %>% 
  mutate(across(where(is.character), as.factor))

#Look at each factor to ensure the levels make sense
quali_unique <- full_interp_data %>% 
  select(where(is.character)) %>% 
  map(unique)
print(quali_unique)

# PCoA on Mixed Sep DF ----------------------------------------------------
#Calculate Gower distance for all traits
gower_dist <- daisy(full_interp_data, metric = "gower")

print(gower_dist)

# Perform PCoA
pcoa_result <- cmdscale(as.dist(gower_dist), k = 2, eig = TRUE)

# Extract coordinates
pcoa_coords <- as.data.frame(pcoa_result$points)
colnames(pcoa_coords) <- c("PCoA1", "PCoA2")
pcoa_coords$SampleID <- raw_2$ID

library(glue)
perc_var <- 100* pcoa_result$eig / sum(pcoa_result$eig)

perc_var <-round(perc_var, digits = 2)

labs <- c(glue("PCo 1 ({perc_var[1]}%)"), glue("PCo 2 ({perc_var[2]}%)"))

#PC1 explains ~39.5% variation, PC2 ~18.2% and PC3 about 12.4%

# Visualize the PCoA of sepals separate
ggplot(pcoa_coords, aes(x = PCoA1, y = PCoA2, label = "", colour = raw_2$Putative_Species)) +
  geom_point(size = 3) +
  geom_text(nudge_y = 0.02) +
  theme_light() +
  labs(x = labs[1], y = labs[2])

ggsave("Separate_PCoA_Avg_PC1-2.jpeg", plot = last_plot())
