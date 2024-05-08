#main for Plants
#when this file is executed, the cophen matrix will be generated for a squamate tree for the geographic area 
#of california. This can be altered at the specificed location if there are different
#larger trees of interest (ie: some other geographical area )
#in this, we generate trees for the molecular and the compelete phylogenies. will use the molecular from here on out for now. 
#this does not actually require any geospatial information. 


#TODO: generate 5 trees. 

#first step needs to be to prune the california tree to the list of california taxa 

#install necessary packages 
packages_to_install <- c("picante", "ape", "dplyr", "readr", "phytools", "BIEN", "phytools", "ggtree")
# Loop through the packages
for (pkg in packages_to_install) {
  # Check if the package is not already installed
  if (!requireNamespace(pkg, quietly = TRUE)) {
    # Install the package only if not already installed
    install.packages(pkg)
  }
}

#for these, need to use just the ecoregion ones. which I probably generated prior. 



#install.packages("BIEN")
#library(BIEN)

lapply(packages_to_install, library, character.only = TRUE)
#specify the clade that you are interested in

clade = "Plants"


geog_area = "cali"

current_directory <- getwd()

coph_path <- file.path(current_directory, "Cophen_function.R")
tree_trimming_path<-file.path(current_directory, "App_functions_bird.R")
sample_tree_creator_path<- file.path(current_directory, "sample_tree_creator.R")


source(coph_path)
source(tree_trimming_path)
source(sample_tree_creator_path)


#complete_phylogeny for california that is based on molecular data but only has genus-level information 
complete_phylogeny<- read.tree("Plants/species_level_full_treeMISHLER.tre")

plot<-ggtree(complete_phylogeny, layout = "fan") #this is the complete phylogeny. 
ggsave("Plants/images/full_plant_phylogeny.png", plot)


cali_species_tree<- complete_phylogeny
#don't need to run this when the complete phylogeny is genus leve. 
#prune the parent tree to the genus tree
genus_complete_phylogeny<- genus_tree_generator(complete_phylogeny)
write.tree(genus_complete_phylogeny, file = paste(clade, "/full_tree_genus.tre", sep = ""))


sf_data_taxa_list_cali_natives<- st_read("Plants/total_combined_range_shapes.shp")
#this has all the possible taxa. 


#there are only 850 genera represented in the genus list! ridiculous. crazy. out of like 7,00 species. 
#out of the 850 taxa that are represented there are about 190 that are unmatched in the tree! 
cali_plants<- data.frame(sf_data_taxa_list_cali_natives$sci_name)
colnames(cali_plants)<- "name"
cali_plants$name<- gsub("\\.", "_", cali_plants$name)
cali_plants_genera<- genus_only(cali_plants)
cali_plants_genera$colnames<- NULL


#do not need to do this always. 
path_to_taxa_list = file.path(paste(clade, "/total_unique_noncultivated_genera_obs_and_exp_cali.csv", sep = ""))
path_to_full_tree = file.path(clade, "full_tree.tre")
path_to_full_genus_tree<- file.path(clade, "full_tree_genus.tre") 
#already to genus only.

#format the taxa list
cali_plants<-read.csv(path_to_taxa_list)
cali_plants$X <- NULL 
colnames(cali_plants)<- "name"



#load the largest tree and prune to only california, then save the california tree
#todo: make this more transferable to geographic area.

#for the full phylogenetic tree
#full_tree<- read.tree(path_to_full_tree)

#todo: what is going on here????
#move forward with genera
full_tree_genus<-read.tree(path_to_full_genus_tree)

unmatched_species<- check_taxa(cali_plants, complete_phylogeny)

unmatched_genera<- check_taxa(cali_plants_genera, genus_complete_phylogeny)

#there are 352 unmatched genera in the molecular phylogeny and 
#about 997 unmatched genera in the molecular phylogney


#about 2000 represented taxa. need to double check this anyways. 
#249 plants that actually have a matching 
matched_genera<- remove_taxa(cali_plants_genera, genus_complete_phylogeny)

cali_tree_genus<- sample_tree_generator(matched_genera, genus_complete_phylogeny)

plot<- ggtree(cali_tree_genus, layout = "fan")
ggsave("Plants/images/cali_genus_tree0505.png", plot)

write.tree(cali_tree_genus, file = file.path(clade, "cali_genus_tree_0505.tre"))
#have already developed the species level tree and loaded above


#there are 352 unmatched genera in the molecular phylogeny and 
#about 997 unmatched genera in the molecular phylogney


#about 2000 represented taxa. need to double check this anyways. 
#249 plants that actually have a matching 
#there are 2025 species represented in this tree that also have range data. 
matched_species<- remove_taxa(cali_plants, complete_phylogeny)

cali_tree_species<- sample_tree_generator(matched_species, complete_phylogeny)

plot<- ggtree(cali_tree_species, layout = "fan")
ggsave("Plants/images/cali_species_tree0505.png")

write.tree(cali_tree_species, file = file.path(clade, "cali_species_tree_0505.tre"))


#generate the cophenetic matrix for the genus tree
#save as cali_genus_cophen_matrix

#generate a corresponding cophenetic matrix. 
call_cophen(cali_tree_genus, clade = "Plants", geog_area = "cali_genus_Mishler0505") 
call_cophen(cali_tree_species, clade = "Plants", geog_area = "cali_species_Mishler0505") 




#for genera
dir_list_ecoregions <- list.dirs("Plants/ecoregion_data_2",recursive = FALSE)  
full_tree<- cali_tree_genus

for(directory in dir_list_ecoregions)
{
  ecoregion_species<- read.csv(paste(directory, "/checklist.csv", sep = ""))
  ecoregion_species$X<- NULL
  ecoregion_species$x<- gsub("\\.", "_", ecoregion_species$x)
  colnames(ecoregion_species)<- "names"
  ecoregion_genus<- genus_only(ecoregion_species)
  ecoregion_genus$colnames<- NULL
  colnames(ecoregion_genus)<- "name"
  
  unmatched_ecoregion_genus<- check_taxa(ecoregion_genus, full_tree)
  matched_ecoregion_genus<-remove_taxa(ecoregion_genus, full_tree)
  ecoregion_tree<- sample_tree_generator(matched_ecoregion_genus, full_tree)
  
  #manually use cophen matrix because this format isn't working. 
  ecoregion_matrix<- cophen(ecoregion_tree)
  cophen_ecoregion<- as.matrix(ecoregion_matrix)
  
  
  #save all the files. 
  saveRDS(cophen_ecoregion, file = paste(directory, "/cophen_matrix", sep = ""))
  write.csv(unmatched_ecoregion_genus, file = paste(directory, "/genera_absent_from_tree.csv", sep = ""))
  write.csv(matched_ecoregion_genus, file = paste(directory, "/genera_present_in_tree.csv", sep = ""))
  write.tree(ecoregion_tree, file = paste(directory, "/trimmed_tree.tre", sep = ""))
}


#for ecoregions with SPECIES (using the Mishler tree right now. )

dir_list_ecoregions <- list.dirs("Plants/ecoregion_data_2",recursive = FALSE)  
full_tree<- cali_tree_species

for(directory in dir_list_ecoregions)
{
  ecoregion_species<- read.csv(paste(directory, "/checklist.csv", sep = ""))
  ecoregion_species$X<- NULL
  ecoregion_species$x<- gsub("\\.", "_", ecoregion_species$x)
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

##Now need to regenerate all the null models with tree data as of 05/05
#note for hoffman: cophen _matrix is for genus and cophen_matrix_species is for species. 