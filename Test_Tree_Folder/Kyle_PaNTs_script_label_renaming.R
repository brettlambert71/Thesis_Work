# package management
library(phylotools)
library(phytools)
library(tidyverse)
# Read in trees. If you have bootstrap values, the tree edge lengths may not load in by default. A work around is to read the tree without BS values, then copy the edge lengths into the BS tree like so.

#Read in no BS tree
tree_no_bs <- read.nexus("Test_Tree_Folder/Tree_3.nexus")

#plot tree and reroot if necessary. If rerooting with an outgroup, must provide the edge number as an integer. For example, my outgroup is sample 14, so I count the edges (beginning at 1 at top of tree) until I get to my desired outgroup. May take some trial and error. Then save edge lengths

rr_tree_no_bs <- reroot(tree_no_bs, 47)
plotTree(rr_tree_no_bs)

#Copy edge lengths
edgelengths <- tree_no_bs$edge.length

#Read in BS tree
bs_tree <- read.nexus("Test_Tree_Folder/All_Alleles_MQ_30_Min_Dist_IQ_Tree_1000BS.nexus")

bs_tree$edge.length <- edgelengths

#Reroot BS tree and plot to confirm topology is correct
rr_bs_tree <- reroot(bs_tree, 47)
plotTree(rr_bs_tree)

#Renaming taxon labels: 
# Read in taxon rename file
# File should be two columns with headers where first column are old labels
# and second column are new labels
taxon_names <- read.csv("Test_Tree_Folder/Sample_Info_Copy.csv", h = T)

#Drop any unnecessary columns
taxon_names <- taxon_names %>% 
  select(-c(1,4))

# Change taxon labels + check
new_tree <- sub.taxa.label(rr_bs_tree, taxon_names)

plotTree(new_tree)

#Use nodelabels function as follows. Simply replace your object name where a tree is required. You may have to clear plot history between comparing tree outputs.
nodelabels(new_tree$node.label,node=2:new_tree$Nnode+Ntip(new_tree))

# Export tree
write.tree(new_tree, file = "Final_BS_Tree.newick")


# Optional: If tree is not in readable format -----------------------------
#Copy the tree in text format as a string
test.tree <- "(sample02:0.0255074331,(sample03:0.0188309845,(((((((((sample04:0.0214052484,((((((((sample19:0.0313283739,((sample67:0.0236009569,sample88:0.0168739536)68:0.0018311600,sample81:0.0238586741)54:0.0017513713)72:0.0024648886,(((sample46:0.0241623116,sample86:0.0183813631)100:0.0034365353,sample83:0.0236311269)99:0.0035195854,(((((sample56:0.0160463869,sample72:0.0139426653)99:0.0022021134,sample90:0.0230675701)48:0.0013164208,sample76:0.0181603753)41:0.0016654177,(sample62:0.0168186516,(sample66:0.0207698625,sample77:0.0211168947)88:0.0023486139)76:0.0024833445)66:0.0024499384,(((sample57:0.0185872388,sample74:0.0116433330)68:0.0017828183,sample84:0.0342341247)70:0.0031212941,((sample65:0.0342994524,sample85:0.0149052735)32:0.0021281814,(sample73:0.0197127363,sample82:0.0219828407)69:0.0036510978)30:0.0020747745)37:0.0020288442)56:0.0023037611)94:0.0030227172)64:0.0022074934,(sample47:0.0239053476,sample70:0.0311138524)72:0.0038745841)100:0.0035407682,((((sample51:0.0223469568,((sample59:0.0298648357,(sample61:0.0175855872,sample69:0.0261819345)89:0.0034848590)100:0.0033533815,sample60:0.0249702564)99:0.0034481158)100:0.0032505439,sample80:0.0301954350)94:0.0020168310,sample68:0.0222072178)99:0.0033318787,(sample79:0.0263452994,sample87:0.0255382964)96:0.0030538172)92:0.0027414745)90:0.0027151288,sample63:0.0266691226)99:0.0023573958,((sample49:0.0202484233,sample55:0.0275085706)68:0.0027959846,(sample50:0.0216871436,sample52:0.0286416450)55:0.0013762292)100:0.0068326017)100:0.0051872708,sample71:0.0255724324)66:0.0024555888,sample89:0.0330316722)100:0.0169692414)100:0.0067111709,sample13:0.0095236210)96:0.0067928097,sample37:0.0089607377)100:0.0065827964,sample39:0.0138653928)88:0.0035637745,sample21:0.0086616061)100:0.0039964819,sample14:0.0425336214)100:0.0048005746,(((((((sample07:0.0149969581,sample08:0.0147635000)100:0.0063074820,sample10:0.0124630371)66:0.0017165864,sample11:0.0167116894)95:0.0030933384,((sample16:0.0156925840,sample42:0.0164540009)100:0.0036029728,sample43:0.0196198118)100:0.0044428089)87:0.0018037271,sample22:0.0119383548)100:0.0022992220,sample20:0.0179325858)92:0.0012392564,sample17:0.0029255871)100:0.0013985253)100:0.0039895101,(((sample18:0.0152385013,sample32:0.0286428378)99:0.0034783763,(((sample24:0.0120295327,(sample28:0.0193193674,sample36:0.0223765665)98:0.0024158720)61:0.0016234083,sample33:0.0238961800)87:0.0021183620,((sample25:0.0154479895,sample31:0.0111799212)72:0.0016415499,(sample29:0.0156396248,sample44:0.0159258699)91:0.0024744422)85:0.0021108450)99:0.0029825394)96:0.0023340878,sample26:0.0085062174)99:0.0024275530)100:0.0043999677,((((((sample05:0.0188053094,sample35:0.0155396322)98:0.0043256608,(sample38:0.0152222919,sample45:0.0176983816)56:0.0019248980)52:0.0017668312,(sample06:0.0186857213,sample15:0.0155600116)70:0.0020481387)62:0.0017552010,sample27:0.0158593607)100:0.0081214179,sample12:0.0120234982)100:0.0027083389,sample09:0.0184637747)73:0.0020390411)67:0.0024484487)98:0.0028650333,sample53:0.0275073618);")

#Use read.tree and specify the input is text
test_2 <- read.tree(text = test.tree)

#In this case, my bootstrapped tree does have edge lengths
rr_bs_tree <- reroot(test_2, 47)
plotTree(rr_bs_tree)

#Renaming taxon labels: 
# Read in taxon rename file
# File should be two columns with headers where first column are old labels
# and second column are new labels
taxon_names <- read.csv("Test_Tree_Folder/Sample_Info_Copy.csv", h = T)

#Drop any unnecessary columns
taxon_names <- taxon_names %>% 
  select(-c(1,4))

# Change taxon labels + check
new_tree <- sub.taxa.label(rr_bs_tree, taxon_names)

plotTree(new_tree)

#Use nodelabels function as follows. Simply replace your object name where a tree is required. You may have to clear plot history between comparing tree outputs.
nodelabels(new_tree$node.label,node=2:new_tree$Nnode+Ntip(new_tree))

# Export tree
write.tree(new_tree, file = "IQ_Tree_1000BS.newick")



