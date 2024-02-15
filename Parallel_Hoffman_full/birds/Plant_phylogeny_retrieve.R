install.packages("BIEN")
library(BIEN)


#for plants


#this phylogeny has 18,641 tips and 18,640 nodes. 
plant_phylogeny<- BIEN_phylogeny_conservative()

write.tree(plant_phylogeny, file = "Plants/full_tree.tre")

#that's probably it for this script. 