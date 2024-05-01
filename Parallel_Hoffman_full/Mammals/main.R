#main for Mammals
#when this file is executed, the cophen matrix will be generated for a squamate tree for the geographic area 
#of california. This can be altered at the specificed location if there are different
#larger trees of interest (ie: some other geographical area )
#in this, we generate trees for the molecular and the compelete phylogenies. will use the molecular from here on out for now. 
#this does not actually require any geospatial information. 


#TODO: generate 5 trees. 

#first step needs to be to prune the california tree to the list of california taxa 

#install necessary packages 
packages_to_install <- c("picante", "ape", "dplyr", "readr", "phytools", "BIEN", "phytools")
# Loop through the packages
for (pkg in packages_to_install) {
  # Check if the package is not already installed
  if (!requireNamespace(pkg, quietly = TRUE)) {
    # Install the package only if not already installed
    install.packages(pkg)
  }
}



#install.packages("BIEN")
#library(BIEN)

lapply(packages_to_install, library, character.only = TRUE)
#specify the clade that you are interested in

clade = "Mammals"


geog_area = "cali"

current_directory <- getwd()

coph_path <- file.path(current_directory, "Cophen_function.R")
tree_trimming_path<-file.path(current_directory, "App_functions_bird.R")
sample_tree_creator_path<- file.path(current_directory, "sample_tree_creator.R")


source(coph_path)
source(tree_trimming_path)
source(sample_tree_creator_path)


#complete_phylogeny for california that is based on molecular data but only has genus-level information 
complete_phylogeny_mammal<- read.tree("Mammals/Corrected_Mammal_tree.tre")



sf_data_taxa_list_cali_mammals<- read.csv("Mammals/cali_species_mammals_list.csv")
sf_data_taxa_list_cali_mammals$X<- NULL

unmatched_species<- check_taxa(
  sf_data_taxa_list_cali_mammals, complete_phylogeny_mammal)

#about 2000 represented taxa. need to double check this anyways. 
matched_species_mammals<- remove_taxa(sf_data_taxa_list_cali_mammals, complete_phylogeny_mammal)

#leaves us with a total of 149 tipes 
cali_tree_species_mammals<- sample_tree_generator(matched_species_mammals,complete_phylogeny_mammal)

write.tree(cali_tree_species_mammals, file = file.path(clade, "cali_species_tree.tre"))
#have already developed the species level tree and loaded above. 


#generate the cophenetic matrix for the genus tree
#save as cali_genus_cophen_matrix

#generate a corresponding cophenetic matrix. 
call_cophen(cali_tree_species_mammals, clade = "Mammals", geog_area = "cali_species") 



#for ecoregions with SPECIES

#only really need to use the phylogeny that actually has representative taxa. 
dir_list_ecoregions <- list.dirs("Mammals/ecoregion_data",recursive = FALSE)  
full_tree<- cali_tree_species_mammals

for(directory in dir_list_ecoregions)
{
  ecoregion_species<- read.csv(paste(directory, "/checklist.csv", sep = ""))
  ecoregion_species$X<- NULL
  ecoregion_species$x<- gsub(" ", "_", ecoregion_species$x)
  colnames(ecoregion_species)<- "name"
  unmatched_ecoregion_species<- check_taxa(ecoregion_species, full_tree)
  matched_ecoregion_species<-remove_taxa(ecoregion_species, full_tree)
  ecoregion_tree<- sample_tree_generator(matched_ecoregion_species, full_tree)
  
  #manually use cophen matrix because this format isn't working. 
  ecoregion_matrix<- cophen(ecoregion_tree)
  cophen_ecoregion<- as.matrix(ecoregion_matrix)
  
  
  #save all the files. 
  saveRDS(cophen_ecoregion, file = paste(directory, "/cophen_matrix_species", sep = ""))
  write.csv(unmatched_ecoregion_species, file = paste(directory, "/species_absent_from_tree.csv", sep = ""))
  write.csv(matched_ecoregion_species, file = paste(directory, "/species_present_in_tree.csv", sep = ""))
  write.tree(ecoregion_tree, file = paste(directory, "/trimmed_species_tree.tre", sep = ""))
}



#this is all archaic. 
#edited out call_cophen for sake of tree initialization statistics. 
#for(i in 1: 99)
#{
# temp<- read.tree(paste("Plants/Instance_", i, "/interpolated_phylogeny.tre", sep = ""))
# unmatched_species_list<- check_taxa(cali_plants, temp)
# matched_species_list<-remove_taxa(cali_plants, temp)
# temp_cali_tree<- sample_tree_generator(matched_species_list, temp)
# write.tree(temp_cali_tree, file = paste("Plants/Instance_", i, "/cali_tree_interpolated.tre", sep = ""))
# call_cophen_InterpTrees(temp_cali_tree, clade = "Plants", geog_area = "cali", instance = i) 
# print(i)
#}


#this is to get any interesting statistics but is not useful for tree building 
#full_tree_int<- read.tree("Plants/interpolated_full_tree.tre")
#unmatched_species_int<- check_taxa(cali_plants, full_tree_int)
#matched_species_plants_int<-remove_taxa(cali_plants, full_tree_int)
#max_species<- length(matched_species_plants_int[,1])






#old code 
#cali_tree_int<- sample_tree_generator(matched_species_plants_int, full_tree_int)
#write.tree(cali_tree_int, file = file.path(clade, "cali_tree_interpolated.tre"))
#find the max number of species of california plants.
#maketrees(cali_tree_int, 5, max_species, 5, clade = clade)

#throws an error once we reach the max but that's fine. 

#maybe should try to trim based on genus? 

#path to phylogeny.
#path_to_cali_phylogeny = file.path( clade, "cali_tree_interpolated.tre")
#phylo_tree<- read.tree(path_to_cali_phylogeny)



#the last two parameters are for naming, for the purpose of the call_cophen function
#call_cophen(phylo_tree, clade = "Plants", geog_area = "cali") 


#the output should be saved to the clade file. basically should just have to execute main. 
