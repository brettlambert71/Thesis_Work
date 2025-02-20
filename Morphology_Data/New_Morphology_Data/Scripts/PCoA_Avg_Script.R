# PLEASE READ: Lines 15-104 are only for following with the RAW dataset (Final_RAW_copy.csv). This is meant to show how data cleaning was performed.
#If you just want the results, run lines 4-11 then 105-the end of script using the "Final_Avg_Interp_Dataframe_for_Supp_Materials.csv" file. 
# Load packages for cleaning and analyses ---------------------------------
library(tidyverse)
library(ggplot2)
library(ggfortify)
library(devtools)
library(ggbiplot)
library(zoo)
library(fMultivar)
library(cluster)

# Read in data and clean --------------------------------------------------
raw <- read_csv("Morph_Data/Final_Avg_Interp_Dataframe_for_Supp_Materials.csv") 
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



# Split data into qualitative and quantitative variables ------------------

quali <- full_interp_data %>% select_if(is.character)

quanti <- full_interp_data %>% select_if(is.numeric)

ID_Species <- quali[c(1,2)]

quali <- quali |>
  select(-c(ID, Putative_Species))

#Keep only sepal traits separate for now until averaging is done.
other_traits <- quanti %>% 
  select(c(19:27))
quanti_sepals <- quanti %>% 
  select(!c(19:27))

#Average the traits
averaged <- quanti_sepals %>% 
  mutate(Avg_Sep_Angle = rowMeans(select(., contains("Angle")), na.rm = TRUE),
         Avg_Sep_Length = rowMeans(select(., contains("Length_cm")), na.rm = TRUE), Avg_Sep_MP_Width = rowMeans(select(., contains("MP_Width")), na.rm = TRUE),
         Avg_Length_Tapering = rowMeans(select(., contains("Length_to")), na.rm = TRUE),
         Avg_In_Out_Ratio = rowMeans(select(., contains("Inner_Outer")), na.rm = TRUE),
         Avg_Sep_Width = mean(c(S1_Width_cm, S2_Width_cm, S3_Width_cm)),
         .keep = "none")


#Change S1,2,3 tip shape to Consensus of tip shape (Sepal Tip Shape)
quali_2 <- quali %>% 
  mutate(Sepal_Tip_Shape = pmap_chr(list(S1_Tip_Shape, S2_Tip_Shape, S3_Tip_Shape), ~ {
    factors <- c(...)
    most_common <- names(sort(table(factors), decreasing = TRUE))[1]
    most_common
  })) %>% 
  relocate(Sepal_Tip_Shape, .before = 1) %>% 
  select(-c(S1_Tip_Shape, S2_Tip_Shape, S3_Tip_Shape))

#Join the two quantitative dfs again
averaged_quanti <- as.data.frame(c(averaged, other_traits))

#Join both the avg quanti df and consensus quali df
full_df <- as.data.frame(c(averaged_quanti, quali_2))

#Change character cols to factors
full_df <- full_df %>% 
  mutate(across(where(is.character), as.factor))


#Merge final averaged, interp dataframe with ID and putative species for supplemental materials. Future users will need to remove the ID and species cols as done in the analysis here. Those columns are only meant for labeling, not analysis.
full_df_1 <- as.data.frame(c(ID_Species, full_df))

#Write to csv; commented out to prevent it being written on accident

#write.csv(full_df_1, "Final_Avg_Interp_Dataframe_for_Supp_Materials.csv", row.names = FALSE)


# PCoA on Mixed Avg DF ----------------------------------------------------
#Calculate Gower distance for all traits
gower_dist <- daisy(full_df, metric = "gower")

print(gower_dist)

# Perform PCoA
pcoa_result <- cmdscale(as.dist(gower_dist), k = 2, eig = TRUE)

# Extract coordinates
pcoa_coords <- as.data.frame(pcoa_result$points)
colnames(pcoa_coords) <- c("PCoA1", "PCoA2")
pcoa_coords$SampleID <- raw_2$ID

perc_var <- 100* pcoa_result$eig / sum(pcoa_result$eig)

perc_var <-round(perc_var, digits = 2)

labs <- c(glue("PCo 1 ({perc_var[1]}%)"), glue("PCo 2 ({perc_var[2]}%)"))

#PC1 explains ~39.5% variation, PC2 ~18.2% and PC3 about 12.4%

# Visualize the PCoA of sepals separate
library(glue)
ggplot(pcoa_coords, aes(x = PCoA1, y = PCoA2, label = "", colour = raw_2$Putative_Species)) +
  geom_point(size = 3) +
  geom_text(nudge_y = 0.02) +
  theme_light() +
  labs(x = labs[1], y = labs[2])

ggsave("Mixed_PCoA_Avg_PC1-2.jpeg", plot = last_plot())
