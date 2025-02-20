library(phytools)
library(mapdata)
library(tidyverse)
library(phylotools)

#Read in locality data and fix formatting
collections <- read_csv("./Final_Locality_Info_Copy_Final.csv")

collections <- collections %>% 
  group_by(`Putative Species`) %>% 
  arrange(`Putative Species`) %>% 
  select(!c(1,3))
collections[c(15,44), ] <- collections[c(44,15), ]

collections_test <- collections %>% 
  ungroup() %>%  # Ensure row_number() applies globally
  filter(!row_number() %in% c(1,23,30,34,40,41,48,54,58,64,75,78))


#Need a tree with just putative species as tip labels

#Read in no BS tree
tree_no_bs <- read.nexus("Test_Tree_Folder/Tree_3.nexus")

#plot tree and reroot if necessary. If rerooting with an outgroup, must provide the edge number as an integer. For example, my outgroup is sample 14, so I count the edges (beginning at 1 at top of tree) until I get to my desired outgroup. May take some trial and error. Then save edge lengths

rr_tree_no_bs <- reroot(tree_no_bs, 47)
plotTree(rr_tree_no_bs)


#Renaming taxon labels: 
# Read in taxon rename file
# File should be two columns with headers where first column are old labels
# and second column are new labels
taxon_names <- read.csv("Test_Tree_Folder/Sample_Info_Copy.csv", h = T)


test_tax <- taxon_names %>%
  separate(New_Analysis_Names, into = c("prefix", "middle", "rest"), sep = "_", extra = "merge") %>%
  mutate(prefix = recode(prefix, "A" = "A_")) %>% 
  mutate("New_Label" = str_c(prefix, middle)) %>% 
  select(c(Sample_Numb_Names, New_Label)) %>% 
  filter(!row_number() %in% c(1,23,30,34,40,41,48,54,58,64,75,78))


# Change taxon labels + check
new_tree <- sub.taxa.label(rr_tree_no_bs, test_tax)

plotTree(new_tree)

#If you want to save this tree
#write.nexus(new_tree, "Cladogram_Only_Put_Species_No_Individuals")


# Plot_Tree_On_Map --------------------------------------------------------
plotTree(new_tree,ftype="i",fsize=0.4,lwd=1)

# Remove "Putative Species" column but keep it for row names
collections_test_2 <- as.matrix(collections_test[2:3])
rownames(collections_test_2) <- collections_test$`Putative Species`


#Compute branch lengths
tree_brln <- compute.brlen(new_tree)

#root tree
new_tree <- root(new_tree, outgroup = "A_Caud", resolve.root = TRUE)

#
reflexum <- (node <- getMRCA(new_tree, c("A_Refl", "A_Refl")))

canadense <- (node <- getMRCA(new_tree, c("A_Cana", "A_Cana")))

acuminatum <- (node <- getMRCA(new_tree, c("A_Acum", "A_Acum")))
#cascade <- (node <- getMRCA(pica.r, c("OSC441", "AG23_47")))
#siskiyou <- (node <- getMRCA(pica.r, c("OSC453", "AG22_09")))


#I think I have the wrong kind of tree. It should be a "putative species" tree of three species with the df mapping to which they belong


#Stuck because matrices aren't supposed to have duplicate names
obj<-phylo.to.map(new_tree,collections_test,plot=FALSE)
