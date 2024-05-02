#main for squamates 
#need to download the squamate range data from iucn.
#when this file is executed, the cophen matrix will be generated for a squamate tree for the geographic area 
#of california. This can be altered at the specificed location if there are different
#larger trees of interest (ie: some other geographical area )

#first step needs to be to prune the california tree to the list of california taxa 

#install necessary packages 
packages_to_install <- c("picante", "ape", "dplyr", "readr", p="phytools")
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

clade = "squamate" 
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
full_range<- st_read("squamate/squamate_complete_species_lists_by_ecoregion.shp")
taxa_list<- unique(full_range$binomial)

path_to_full_tree = file.path("squamate/squamate.tre")


#format the taxa list using App_functions_bird script
cali_squamates<-taxa_list
cali_squamates<- gsub(" ", "_", cali_squamates)
cali_squamates<- data.frame(cali_squamates)
colnames(cali_squamates)<- "name"

#load the largest tree and prune to only california, then save the california tree
#todo: make this more transferable to geographic area.

#there are a total of 93 matched species of squamates!
full_tree<- read.tree(path_to_full_tree)
unmatched_species<- check_taxa(cali_squamates, full_tree )
matched_species_squamates<-remove_taxa(cali_squamates, full_tree)
cali_tree<- sample_tree_generator(matched_species_squamates, full_tree)
write.tree(cali_tree, file = file.path(clade, "cali_tree.tre"))

max_species <- max_species(cali_squamates)
#run the sample_tree_creator to generate the sample trees
maketrees(cali_tree, 5, max_species, 5, clade = clade)


#path to phylogeny.
path_to_cali_phylogeny = file.path( clade, "cali_tree.tre")
phylo_tree<- read.tree(path_to_cali_phylogeny)



#the last two parameters are for naming, for the purpose of the call_cophen function
call_cophen(phylo_tree, clade = "squamate", geog_area = "cali") 


dir_list_ecoregions <- list.dirs("squamate/ecoregion_data",recursive = FALSE)  
full_tree<- phylo_tree

for(directory in dir_list_ecoregions)
{
  ecoregion_species<- read.csv(paste(directory, "/checklist.csv", sep = ""))
  ecoregion_species$X<- NULL
  ecoregion_species$x<- gsub(" ", "_", ecoregion_species$x)
  colnames(ecoregion_species)<- "name"
  print(ecoregion_species)
  
  unmatched_ecoregion_species<- check_taxa(ecoregion_species, full_tree)
  matched_ecoregion_species<-remove_taxa(ecoregion_species, full_tree)
  ecoregion_tree<- sample_tree_generator(matched_ecoregion_species, full_tree)
  print(ecoregion_tree)
  
  #manually use cophen matrix because this format isn't working. 
  ecoregion_matrix<- cophen(ecoregion_tree)
  cophen_ecoregion<- as.matrix(ecoregion_matrix)
  
  
  #save all the files. 
  saveRDS(cophen_ecoregion, file = paste(directory, "/cophen_matrix_species", sep = ""))
  write.csv(unmatched_ecoregion_species, file = paste(directory, "/species_absent_from_tree.csv", sep = ""))
  write.csv(matched_ecoregion_species, file = paste(directory, "/species_present_in_tree.csv", sep = ""))
  write.tree(ecoregion_tree, file = paste(directory, "/trimmed_species_tree.tre", sep = ""))
}
#the output should be saved to the clade file. basically should just have to execute main. 
