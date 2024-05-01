#this explores the different mammal phylogenies and picks the one that represents the most taxa

library(ape)
library(dplyr)
shape_cali_taxa<- st_read("Mammals/full_california_mammal_ranges.shp")

cali_species_mammals<- unique(shape_cali_taxa$sci_name)

cali_species_mammals<- gsub(" ", "_", cali_species)

cali_species_mammals<- data.frame(cali_species)

colnames(cali_species_mammals)<- "name"

write.csv(cali_species_mammals, file = "Mammals/cali_species_mammals_list.csv", row.names = TRUE)

#there are 172 unique mammals that have ranges that overlap with california according to the IUCN range list. 


mammal_tree_complete<- read.nexus("Mammals/MamPhy_fullPosterior_BDvr_DNAonly_4098sp_topoFree_NDexp_MCC_v2_target.tre")

remove_last_two_words <- function(input_string) {
  # Split the string into words
  words <- strsplit(input_string, "_")[[1]]
  
  # Remove the last two words
  result <- paste(words[1:(length(words)-2)], collapse = "_")
  
  return(result)
}

correct_tip_labels<- lapply(mammal_tree_complete$tip.label, remove_last_two_words)

mammal_tree_complete$tip.label<- unlist(correct_tip_labels)

write.tree(mammal_tree_complete, file = "Mammals/Corrected_Mammal_tree.tre")
