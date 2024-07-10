library(ggtree)
library(sf)

#this is the tree for range 1. 

parent_tree<- read.tree("Plants/species_level_full_treeMISHLER.tre")
temp_tree<- read.tree("Plants/hex_trees/8629adc47ffffff.tre")

#want to check the range of this species: "Acalypha_californica" and this: "Tragia_ramosa"
#want to get the hex data and should just do this on eduroam. 
