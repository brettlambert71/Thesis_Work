# Load packages for cleaning and analyses ---------------------------------
library(FactoMineR)
library(factoextra)
library(tidyverse)
library(ggplot2)
library(ggfortify)
library(devtools)
library(ggbiplot)
library(zoo)
library(fMultivar)
library(cluster)

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




#Take other traits out, will be easier to work with I think.
other_traits <- quanti %>% 
  select(c(19:27))
quanti_sepals <- quanti %>% 
  select(!c(19:27))

averaged <- quanti_sepals %>% 
  mutate(Avg_Sep_Angle = rowMeans(select(., contains("Angle")), na.rm = TRUE),
Avg_Sep_Length = rowMeans(select(., contains("Length_cm")), na.rm = TRUE), Avg_Sep_MP_Width = rowMeans(select(., contains("MP_Width")), na.rm = TRUE),
Avg_Length_Tapering = rowMeans(select(., contains("Length_to")), na.rm = TRUE),
Avg_In_Out_Ratio = rowMeans(select(., contains("Inner_Outer")), na.rm = TRUE),
Avg_Sep_Width = mean(c(S1_Width_cm, S2_Width_cm, S3_Width_cm)),
.keep = "none")



averaged_full_df <- as.data.frame(c(averaged, other_traits))



# PCA on quantitative variables -------------------------------------------
avg_pca <- prcomp(averaged_full_df,
             center = TRUE,
            scale. = TRUE)
attributes(avg_pca)
print(avg_pca)

#Plot PCA
avg_pca_plot_1_2 <- ggbiplot(avg_pca,
              choices = c(1,2),
              obs.scale = 1,
              var.scale = 1,
              groups = ID_Species$Putative_Species,
              ellipse = TRUE)
avg_pca_plot_1_2 <- avg_pca_plot_1_2 + scale_color_discrete(name = '')
avg_pca_plot_1_2 <- avg_pca_plot_1_2 + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(avg_pca_plot_1_2)
#commented out saving plot to prevent accidentally saving
#ggsave("Sepals_Avg_PCA_PC1-2.jpeg", plot = last_plot())

#Plot PCA - PCs 2-3
avg_pca_plot_2_3 <- ggbiplot(avg_pca,
              choices = c(2,3),
              obs.scale = 1,
              var.scale = 1,
              groups = ID_Species$Putative_Species,
              ellipse = TRUE)
avg_pca_plot_2_3 <- avg_pca_plot_2_3 + scale_color_discrete(name = '')
avg_pca_plot_2_3 <- avg_pca_plot_2_3 + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(avg_pca_plot_2_3)
#commented out saving plot to prevent accidentally saving
#ggsave("Sepals_Avg_PCA_PC2-3.jpeg", plot = last_plot())

#Plot PCA - PCs 1-3
avg_pca_plot_1_3 <- ggbiplot(avg_pca,
              choices = c(1,3),
              obs.scale = 1,
              var.scale = 1,
              groups = ID_Species$Putative_Species,
              ellipse = TRUE)
avg_pca_plot_1_3 <- avg_pca_plot_1_3 + scale_color_discrete(name = '')
avg_pca_plot_1_3 <- avg_pca_plot_1_3 + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(avg_pca_plot_1_3)
#commented out saving plot to prevent accidentally saving
#ggsave("Sepals_Avg_PCA_PC1-3.jpeg", plot = last_plot())


# Scree Plot --------------------------------------------------------------
#Use factoextra to plot PCs
fviz_eig(avg_pca, addlabels = TRUE, ylim = c(0, 50), xlim = c(1,5.9), title = NULL, ggtheme = theme_light())

#save if desired
#ggsave("Scree_Plot_Averaged.jpeg", plot = last_plot())
