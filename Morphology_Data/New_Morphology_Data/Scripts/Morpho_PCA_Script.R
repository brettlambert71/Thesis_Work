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
raw <- read_csv("Morph_Data/Final_RAW_Copy.csv") 
#rename column 1 to work make it easier to work with


#drop extra row with no values and observations never measured
raw <- raw |>
  drop_na(c(ID, Putative_Species)) %>% 
  select(-Purple_Calyx_Pub_Length)

#calculate amount of missing data
na_freq <- raw %>% 
  summarise(across(everything(), ~ sum(is.na(.x))))
na_perc <- na_freq/nrow(raw)
print(na_perc)
#drop high missing data columns or uncertain measurements (petiole width/length, bract width/length, stamen width, calyx glad tric, anthesis position) and cols just for metadata purposes

# filtered <- raw |>
#   select(!c("Calyx abaxial glandular trichomes (Y/N)" ,Collection_Label, Locality_Notes, Anthesis_Position, Stamen_Width, Petiole_Width, Petiole_Length, Bract_Width, Bract_Length, Notes))

#remove entries 98, 99, 154:156, 164:167, 171, 172, 240 due to high missing data

#filtered <- slice(filtered, -c(98,99,154,155,156,164,165,166,167,171,172,240))

#Okay, that worked. Now do % missing for what's left
na_freq_filt <- filtered %>% 
  summarise(across(everything(), ~ sum(is.na(.x))))
na_perc_filt <- na_freq_filt/nrow(filtered)
print(na_perc_filt)


# Interpolation of missing data. Max missing is 17% for pedicle width; the rest are 8.5% or less. -------------------------------------------------------

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



# Split data into qualitative and quantitative variables ------------------

quali <- full_interp_data %>% select_if(is.character)
  
quanti <- full_interp_data %>% select_if(is.numeric)

ID_Species <- quali[c(1,2)]

quali <- quali |>
  select(-c(ID, Putative_Species))
str(quanti)
#Some cols are characters not numeric so let's fix that
new_quanti <- quanti %>% mutate_if(is.character, as.numeric)

#possibly useful functions here but not this time
#all.equal(quanti, new_quanti)
#identical(quanti[4], new_quanti[4])

p 
q
# PCA on quantitative variables -------------------------------------------
pc <- prcomp(new_quanti,
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

#There is widespread variance of morphology when considering all 9 traits. Might be interesting to look at only floral traits as well. Also curious about the qualitative traits after cleaning them up a bit.


# PCA on Reduced Number of Traits -----------------------------------------
names(quanti)
floral_pca <- quanti |>
  select(-c("Avg_Leaf_Width..cm.", "Avg_Leaf_Length..cm.", "Stamen_Length", "Tube_Length.", "Pedicle_Width..mm.", "Pedicle_Length"))

floral_pca_res <- prcomp(floral_pca,
             center = TRUE,
            scale. = TRUE)
attributes(floral_pca_res)
print(floral_pca_res)

#Plot PCA
x <- floral_pca |>
  arrange(desc(Avg_Sepal_Width..cm.))

flo_plot <- ggbiplot(floral_pca_res,
              choices = c(1,2),
              obs.scale = 1,
              var.scale = 1,
              groups = ID_Species$Putative_Species,
              ellipse = TRUE)
flo_plot <- flo_plot + scale_color_discrete(name = '')
flo_plot <- flo_plot + theme(legend.direction = 'horizontal', legend.position = 'top')
print(flo_plot)
#PCA on three primary floral traits defined as diagnostic among putative species. All clustering groups reveal some overlap; suggesting that there is either incorrect identification or that the morphological boundaries between putative species are unclear


# Different PCA variations with different traits --------------------------
# Add back in tube length
floral_pca_2 <- quanti |>
  select(-c("Avg_Leaf_Width..cm.", "Avg_Leaf_Length..cm.", "Stamen_Length", "Pedicle_Width..mm.", "Pedicle_Length"))

floral_pca_res_2 <- prcomp(floral_pca_2,
             center = TRUE,
            scale. = TRUE)
#Plot PCA
flo_plot_2 <- ggbiplot(floral_pca_res_2,
              choices = c(1,2),
              obs.scale = 1,
              var.scale = 1,
              groups = ID_Species$Putative_Species,
              ellipse = TRUE)
flo_plot_2 <- flo_plot_2 + scale_color_discrete(name = '')
flo_plot_2 <- flo_plot_2 + theme(legend.direction = 'horizontal', legend.position = 'top')
print(flo_plot_2)
#Tube length seems to narrow clustering a bit, tigthening the spread of groups. Could indicate a useful trait as Acumionatum seemed to typically have larger flowers in general, leading to larger tube as well.


#Add back in leaf traits
floral_leaf_pca <- quanti |>
  select(-c("Stamen_Length", "Pedicle_Width..mm.", "Pedicle_Length"))

floral_leaf_pca_res <- prcomp(floral_leaf_pca,
             center = TRUE,
            scale. = TRUE)
#Plot PCA
plot_3 <- ggbiplot(floral_leaf_pca_res,
              choices = c(1,2),
              obs.scale = 1,
              var.scale = 1,
              groups = ID_Species$Putative_Species,
              ellipse = TRUE)
plot_3 <- plot_3 + scale_color_discrete(name = '')
plot_3 <- plot_3 + theme(legend.direction = 'horizontal', legend.position = 'top')
print(plot_3)
print(g)
#This plot is very similar to whole-data PCA, so it is likely that the traits removed here contribute very little to the variance in the data

# Clean Qualitative Variables ---------------------------------------------
View(quali)
unique(quali$Sepal_Lobe_Orientation)
#Accidentally left in one entry not recoded properly and made typos in sepal orientation. Fix that here
quali <- quali |>
  mutate(Sepal_Lobe_Orientation = recode(Sepal_Lobe_Orientation, "Intermediate (Some reflexed, others not)" = "Intermediate", "intermediate" = "Intermediate", "Incure" = "Incurved", "slight Reflex" = "Slight Reflex", "Slight Refelex" = "Slight Reflex"))

quali <- quali |>
  mutate(Leaf_Shape = recode(Leaf_Shape, "Reniform-chordate" = "Reniform-Chordate", "Renifrom-chordate" = "Reniform-Chordate"))

#change double to character; needed for factor encoding
quali$Rudimentary_Petals..Y.N. <- as.character(quali$Rudimentary_Petals..Y.N.)
glimpse(quali)

#Preserve df copy before factor transformation in case things don't go as planned
quali_2 <- quali

#Convert columns to factors
quali[] <- lapply(quali, function(x) if(is.character(x)) as.factor(x) else x)
str(quali)
#11 levels for sepal orientation seems wrong; turns out I made typos
unique(quali$Sepal_Lobe_Orientation)
unique(quali$Leaf_Shape)
# Perform Qualitative Clustering Method -----------------------------------
#Likely, the first thing to do is create new binary columns of all possible trait combinations to allow a distance metric to be used. How do I do this?
str(quali)
quali_copy <- quali
x <- dummy(quali, p ="all")

#Now that dummies are created, we can use some sort of distance method to group based on similarity


#Heirarchical Clustering




#K-Means Clustering


#library(dummies) is useful for converting each factor level into binary for distance metrics

library(dummy)
# calculate distance
d_dist <- daisy(quali, metric = "gower", weights =c(1,2,3,4,5,6,7,8,9,10))
# hierarchical clustering
hc<-hclust(d_dist, method = "complete")
# dendrogram 
plot(hc, labels=F)
rect.hclust(hc, k=3, border="red")
# choose k, number of clusters 
cluster<-cutree(hc, k=3)
# add cluster to original data 
df<-cbind(df,as.factor(cluster))









# Interpret Results and Discuss -------------------------------------------


