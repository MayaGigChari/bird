#main for birds 
#when this file is executed, the cophen matrix will be generated for a bird tree for the geographic area 
#of california. This can be altered at the specificed location if there are different
#larger trees of interest (ie: some other geographcial area )

#04/14/2024: reran this with the true updated species range data. 


#first step needs to be to prune the california tree to the list of california taxa 

#TODO: edit this script so that there is the ability to generate null models for California and all the ecoregions. 

#install necessary packages 
packages_to_install <- c("picante", "ape", "dplyr", "readr")

# Loop through the packages
for (pkg in packages_to_install) {
  # Check if the package is not already installed
  if (!requireNamespace(pkg, quietly = TRUE)) {
    # Install the package only if not already installed
    install.packages(pkg)
  }
}

lapply(packages_to_install, library, character.only = TRUE)
#specify the clade that you are interested in

clade = "birds"
geog_area = "cali"

#load all the stuff from the cophen_function file and from the tree_trimming path 
current_directory <- getwd()

coph_path <- file.path(current_directory, "Cophen_function.R")
tree_trimming_path<-file.path(current_directory, "App_functions_bird.R")
sample_tree_creator_path<- file.path(current_directory, "sample_tree_creator.R")

source(coph_path)
source(tree_trimming_path)
source(sample_tree_creator_path)




#find the filepath to cali_tree
#edit the path to phylogeny line if you're not interested in the cali phylogeny. 

#step one is to make the phylogeny. 
#path to species checklist
path_to_cali_taxa_list_range = file.path(clade, "most_updated_bird_list_april2024.csv")
filepath_ecoregions = file.path(clade, "ecoregion_data_2")
path_to_full_tree = file.path(clade, "full_tree.tre") #complete bird tree. this hasn't changed. 
#this is just for california birds. Takes the list and makes it pretty. should move this out of main. 

cali_taxa_full<- st_read("birds/birds_consolidated_geometries_0507.shp")
cali_birds<-cali_taxa_full$sci_name
cali_species<- gsub(" ", "_", cali_birds)
cali_species<-data.frame(cali_species)
colnames(cali_species)<- "name"


#load the largest tree and prune to only california, then save the california tree
#todo: make this more transferable to geographic area.
full_tree<- read.tree(path_to_full_tree)
unmatched_species_birds<- check_taxa(cali_species, full_tree )

#only 50 unmatched species of birds
matched_species_birds<-remove_taxa(cali_species, full_tree)
cali_tree<- sample_tree_generator(matched_species_birds, full_tree)

#save all the birds present in the phylogrnu 
write.tree(cali_tree, file = file.path(clade, "cali_tree_from_range_data_0507.tre"))
write.csv(unmatched_species_birds, file = "birds/range_birds_absent_phylogeny_0507.csv")
write.csv(matched_species_birds, file = "birds/range_birds_present_phylogeny_0507.csv")

#there are about 50 taxa that are not represented.

#run the sample_tree_creator to generate the sample trees

#maketrees(cali_tree, 5, 600, 5, clade = clade)


#path to phylogeny.
path_to_cali_phylogeny = file.path( clade, "cali_tree_from_range_data.tre")
phylo_tree<- read.tree(path_to_cali_phylogeny)

#we already have a cali_range_cophen_matrix and stuff. 


#the last two parameters are for naming, for the purpose of the call_cophen function
call_cophen(phylo_tree, clade = "birds", geog_area = "cali_0507") #generate range matrix for birds



#now need to do this for all ecoregions. 


#below: generates phylogenetic trees and cophenetic matrices for each ecoregion. 

#need to make null models for each of these as well. 
dir_list_ecoregions <- list.dirs("birds/ecoregion_data_2",recursive = FALSE)  
full_tree<- read.tree(path_to_full_tree)

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
  saveRDS(cophen_ecoregion, file = paste(directory, "/cophen_matrix_0507", sep = ""))
  write.csv(unmatched_ecoregion_species, file = paste(directory, "/species_absent_from_tree_0507.csv", sep = ""))
  write.csv(matched_ecoregion_species, file = paste(directory, "/species_present_in_tree_0507.csv", sep = ""))
  write.tree(ecoregion_tree, file = paste(directory, "/trimmed_tree_0507.tre", sep = ""))
}

#now need to populate the hexagons as was done in the other thing and then ligma  balls. 

