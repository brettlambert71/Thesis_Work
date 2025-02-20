##Let's make a map with our observation coordinates

#install.packages("phytools")
install.packages("mapdata")
install.packages("RColorBrewer")

library(phytools)
packageVersion("phytools")
library(mapdata)
library(sf)
library(terra)
library(ggplot2)
library(RColorBrewer)

##########################################
##Here is what you need:
##A tree file, ideally rooted with branch lengths (if it isn't, see
##below for how to generate a root and branch lengths).
##A .csv file of occurrence lat and lon, in WGS84. This file should be
##formatted to have the first column be names that match the tips of your
##tree, the second column should be lat, the third should be long.
## It is okay to have more than one occurrence per tip (such as multiple
##obserations per species). In fact, this is desirable for a clean map.
###########################################

##read in the occurrence data:

#this code will work if your occurrences have duplicate "species" names
pica.data <- read.csv (file = "PICA_OCC_inset.csv", row.names=NULL, header=TRUE)

#if your occurrences are named by unique sample names you can use this
#pica.data <- read.csv (file = "PICA_OCC2.csv", row.names=1, header=TRUE)

##################################
##put the data into a matrix:

##this works fine if there aren't duplicate rownames
#occ <- as.matrix(pica.data)

## but if there are duplicate rownames do this instead
geog <- as.matrix(pica.data[2:3])
rownames(geog) <- pica.data$X

################################

##read in the tree
pica.tree <- read.nexus("qage5.tre")

#####if you want to prune taxa out
##prune the taxa out of the tree that we don't have occ data for
##first generate a list of names from the occ matrix
#keepem <- row.names(geog)
##use this function to drop the tips I don't want
#pica.tree <- drop.tip(pica.tree, setdiff(pica.tree$tip.label, keepem))
#################################
##root the tree
#pica.p <- root(pica.tree, outgroup = "hemitomes_congestum", resolve.root = TRUE

##calculate branch lengths
#pica.b <- compute.brlen(pica.p)
#################################

##visualize
plotTree(pica.tree,ftype="i",fsize=0.4,lwd=1)
#tiplabels(width = 0.3, length = 0.3)

#assign clades based on nodes
#albion <- (node <- getMRCA(pica.r, c("PUA931", "JEPS813")))
#cascade <- (node <- getMRCA(pica.r, c("OSC441", "AG23_47")))
#siskiyou <- (node <- getMRCA(pica.r, c("OSC453", "AG22_09")))
#coast <- (node <- getMRCA (pica.r, c("CAS135", "AG22_06")))

#clades <- (c(albion, cascade, siskiyou, coast))

##create the map object
pica.phymap<-phylo.to.map(pica.tree, geog, plot=FALSE)

##Set the colors for your map using the brewer color palette Dark2, which is
##ideal for qualitative variables
cols<-setNames(sample(brewer.pal(n=Ntip(pica.tree), name = "Dark2")),
               pica.tree$tip.label)

#create the plot
plot(pica.phymap, direction = "rightwards", colors = cols,
     fsize = 0.5,ftype = "i", cex.points=c(0,0.45))

### You can control aspects of your plot like colors by assigning specific
### shades from the palette to specific tip IDs, or changing the xlim and ylim
### to particular coordinate ranges (which will adjust how scrunched your tree is)
### and the font with fsize/ftype, and the size of the points with cex.points.
### like this:
#plot(pica.phymap, direction = "rightwards", colors = 
#       c(hypopitys.california="#66A61E", pityopus.coast="#E7298A",
#                  pityopus.albion="#D95F02", pityopus.cascade="#1B9E77",
#                  pityopus.siskiyou="#E6AB02"),
#     xlim=c(-125.5,-122),
#     fsize = 0.5,ftype = "i", cex.points=c(0,0.45))


#pink= "#E7298A"
#teal= "#1B9E77"
#orange= "#D95F02"
#lt.green= "#66A61E" 
#yellow "#E6AB02"


