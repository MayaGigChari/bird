#this is a script to explore number of represented genera/species in different phylogneies. 

library(ape)
library(BIEN)
library(ggtree)
library(phytools)
library(dplyr)
library(sf)

tree_trimming_path<-file.path(current_directory, "App_functions_bird.R")
source(tree_trimming_path)
new_angio_tree<- ape::read.tree("Plants/ot_2304.tre.txt")

new_angio_tree_2<- chronos(new_angio_tree[[2]])


ggtree(new_angio_tree_2, layout = "circular")+ 
  geom_tiplab(aligh = TRUE, size = 1)+ 
  theme_tree2()

plot(new_angio_tree[[2]], show.tip.label = TRUE) + ape::axisPhylo()

bien_phylogeny<- BIEN_phylogeny_conservative()

#I wonder if the bien phylogeny is actually time calibrated. 
plot(bien_phylogeny, show.tip.label = FALSE) +  ape::axisPhylo()


cali_genus_tree<- read.tree("Plants/cali_genus_tree_APR2024.tre")

p <- ggtree(cali_genus_tree, layout = "circular") +
  geom_tiplab(align = TRUE, size = 1) +  # Adjust tip label size
  theme_tree2()  # Optional: Apply a theme for better aesthetics

# Print t


#mishler tree

#need a list of all the taxa that end up overlappign with california from the azathoth scripts. 
full_seed_tree<- read.tree("Plants/v0.1/GBOTB.tre")

checkValidPhylo(mishler_tree)

ggtree(mishler_treetree, layout = "circular")+ 
  geom_tiplab(aligh = TRUE, size = 1)+ 
  theme_tree2()

native_plants<- st_read("Plants/California_species_shapes/plants_range_california_only_natives_POTW.shp")

#300 unique species in california 
native_plants<- unique(native_plants$species)

native_plants_correct<- data.frame(native_plants)
colnames(native_plants_correct)<- "name"

#pretty good number of these plants are still represented in the mishler tree..... maybe I should jsut go to species?!
#not very good to trim things to genus. doesn't work very well. 
#a total of 2038 out of about 4000 species are kept. 
native_plants_rep<- remove_taxa(native_plants_correct, full_seed_tree)

#there are only 643 species not represented! 
native_plants_notrep<- check_taxa(native_plants_correct, full_seed_tree)
native_plants_rep$name

#there are 1992 plants represented by the michler tree, out of a total of 4000 with range data in california. Honestly
#this shoudl work!

native_plants_rep_tree_bien_phylogeny<- keep.tip(full_seed_tree, native_plants_rep$name)


native_plants_rep_tree_bien_phylogeny$tip.label
#the question is: should I rerun this with the 


#now I have made a tree with the plants of california, representing 1992 taxa somehow ???
native_plants_rep_tree<- keep.tip(mishler_tree, native_plants_rep$name)


#this is so much mf more complete!!!! Is this time calibrated? God I hope so wow. 

#doesn't it make sense to use this species-level phylogeny for plants?

#will export this phylogeny and try everything again with this. MAKE SURE TO RENAME EVERYTHING!. 
plot(native_plants_rep_tree_bien_phylogeny,no.margin=TRUE,edge.width=2,cex=0.7, show.tip.label = FALSE, type = "fan")
nodelabels(text=1:native_plants_rep_tree_bien_phylogeny$Nnode,node=1:native_plants_rep_tree_bien_phylogeny$Nnode+Ntip(native_plants_rep_tree_bien_phylogeny))

#IN ANY CASE: this seems so much better than bien.
ggtree(native_plants_rep_tree_bien_phylogeny) + 
  geom_tiplab(aligh = TRUE, size = 1) + 
  theme_tree2()

write.tree(native_plants_rep_tree_bien_phylogeny, file = "Plants/species_level_full_treeMISHLER.tre")

native_plants_rep_tree<- tre
native_plants_rep<-
  native_plants_correct %>%
  filter(name %in% mishler_tree$tip.label)

native_plants_correct$name

mishler_tree$tip.label

remove_taxa
