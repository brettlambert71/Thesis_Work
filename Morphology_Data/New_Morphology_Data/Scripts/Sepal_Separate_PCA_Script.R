# PLEASE READ: Lines 15-64 are only for following with the RAW dataset (Final_RAW_copy.csv). This is meant to show how data cleaning was performed.
#If you just want the results, run lines 4-13 then 65-the end of script using the "Final_Avg_Interp_Dataframe_for_Supp_Materials.csv" file.
# Load packages for cleaning and analyses ---------------------------------
library(tidyverse)
library(ggplot2)
library(ggfortify)
library(devtools)
library(ggbiplot)
library(zoo)
library(fMultivar)
library(cluster)
library(FactoMineR)
library(factoextra)
# Read in data and clean --------------------------------------------------
raw <- read_csv("Morph_Data/Final_RAW_Copy.csv") 

#Check data structure to make sure it is correct
str(raw)

#calculate amount of missing data
na_freq <- raw %>% 
  summarise(across(everything(), ~ sum(is.na(.x))))
na_perc <- na_freq/nrow(raw)
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

#Apply function to all cols in df
full_interp_data <- data.frame(lapply(raw, interp_by_type))
str(full_interp_data)


# Split data into qualitative and quantitative variables ------------------

quali <- full_interp_data %>% select_if(is.character)
  
quanti <- full_interp_data %>% select_if(is.numeric)

ID_Species <- quali[c(1,2)]

quali <- quali |>
  select(-c(ID, Putative_Species))
str(quanti)

#Some cols maybe characters and not numeric, so change type
#new_quanti <- quanti %>% mutate_if(is.character, as.numeric)

# PCA on quantitative variables -PCs 1-2 -------------------------------------------
pc <- prcomp(quanti,
             center = TRUE,
            scale. = TRUE)
attributes(pc)
print(pc)

#Plot PCA
g <- ggbiplot(pc,
              choices = c(1,2),
              obs.scale = 1,
              var.scale = 1,
              groups = ID_Species$Putative_Species,
              ellipse = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(g)
#commented out saving plot to prevent accidentally saving
#ggsave("Sepals_Separate_PCA_PC1-2.jpeg", plot = last_plot())


#PCA PCs 2-3
pc_2_3 <- prcomp(quanti,
             center = TRUE,
            scale. = TRUE)
attributes(pc)
print(pc)

#Plot PCA
g_2_3 <- ggbiplot(pc_2_3,
              choices = c(2,3),
              obs.scale = 1,
              var.scale = 1,
              groups = ID_Species$Putative_Species,
              ellipse = TRUE)
g_2_3 <- g_2_3 + scale_color_discrete(name = '')
g_2_3 <- g_2_3 + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(g_2_3)
#commented out saving plot to prevent accidentally saving
#ggsave("Sepals_Separate_PCA_PC2-3.jpeg", plot = last_plot())


#PCA PCs 1-3
pc_1_3 <- prcomp(quanti,
             center = TRUE,
            scale. = TRUE)
attributes(pc)
print(pc)

#Plot PCA
g_1_3 <- ggbiplot(pc_1_3,
              choices = c(1,3),
              obs.scale = 1,
              var.scale = 1,
              groups = ID_Species$Putative_Species,
              ellipse = TRUE)
g_1_3 <- g_1_3 + scale_color_discrete(name = '')
g_1_3 <- g_1_3 + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(g_1_3)
#commented out saving plot to prevent accidentally saving
#]ggsave("Sepals_Separate_PCA_PC1-3.jpeg", plot = last_plot())



# Scree Plot --------------------------------------------------------------
#Use factoextra to plot PCs
fviz_eig(pc, addlabels = TRUE, ylim = c(0, 50), xlim = c(1,5.9), title = NULL, ggtheme = theme_light())

#save if desired
#ggsave("Scree_Plot_Separate.jpeg", plot = last_plot())




















#Interpret:


# PCA on Reduced Number of Traits (if desired) -----------------------------------------
# names(quanti)
# floral_pca <- quanti |>
#   select(-c(#remove certain cols
#     ))
# 
# floral_pca_res <- prcomp(floral_pca,
#              center = TRUE,
#             scale. = TRUE)
# attributes(floral_pca_res)
# print(floral_pca_res)
# 
# #Plot PCA
# 
# flo_plot <- ggbiplot(floral_pca_res,
#               choices = c(1,2),
#               obs.scale = 1,
#               var.scale = 1,
#               groups = ID_Species$Putative_Species,
#               ellipse = TRUE)
# flo_plot <- flo_plot + scale_color_discrete(name = '')
# flo_plot <- flo_plot + theme(legend.direction = 'horizontal', legend.position = 'top')
# print(flo_plot)
#Interpret:

