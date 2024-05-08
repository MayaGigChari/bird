#main for amphibians
#redo amphibians and try genus. 

#install necessary packages 
packages_to_install <- c("picante", "ape", "dplyr", "readr", "phytools", "sf")
# Loop through the packages
for (pkg in packages_to_install) {
  # Check if the package is not already installed
  if (!requireNamespace(pkg, quietly = TRUE)) {
    # Install the package only if not already installed
    install.packages(pkg)
  }
}

#amphibians probably doesn't work because it's not even time calibrated! need to check mammals and squqmates for this before proceeding. 

lapply(packages_to_install, library, character.only = TRUE)
#specify the clade that you are interested in

clade = "amphibians" 
geog_area = "cali"

#load all the stuff from the cophen_function file and from the tree_trimming path 


coph_path <- file.path("Cophen_function.R")
tree_trimming_path<-file.path("App_functions_bird.R")
sample_tree_creator_path<- file.path("sample_tree_creator.R")


source(coph_path)
source(tree_trimming_path)
source(sample_tree_creator_path)




#find the filepath to cali_tree
#edit the path to phylogeny line if you're not interested in the cali phylogeny. 

#step one is to make the phylogeny. 
#path to species checklist
full_range<- st_read("amphibians/amphibians_complete_species_lists_by_ecoregion.shp")
taxa_list<- unique(full_range$sci_name)

path_to_full_tree = file.path("amphibians/amph_shl_new_Consensus_7238.tre")


#format the taxa list using App_functions_bird script
cali_amphibians<-taxa_list
cali_amphibians<- gsub(" ", "_", cali_amphibians)
cali_amphibians<- data.frame(cali_amphibians)
colnames(cali_amphibians)<- "name"

#load the largest tree and prune to only california, then save the california tree
#todo: make this more transferable to geographic area.

#there are a total of 93 matched species of amphibians!
#8 unmatched species. 32 matched species.
full_tree<- read.tree(path_to_full_tree)
unmatched_species<- check_taxa(cali_amphibians, full_tree )
matched_species_amphibians<-remove_taxa(cali_amphibians, full_tree)
cali_tree<- sample_tree_generator(matched_species_amphibians, full_tree)

p<- ggtree(cali_tree, layout = "circular")
ggsave("amphibians/images/cali_tree_species.png")
write.tree(cali_tree, file = file.path(clade, "cali_tree.tre"))




#do this for genera
#there are 17 representative genera of California amphibians. 
cali_genera<- genus_only(cali_amphibians)
cali_genus_tree_amphibians<- genus_tree_generator(full_tree)
unmatched_genera<- check_taxa(cali_genera,cali_genus_tree_amphibians )
matched_species_amphibians<-remove_taxa(cali_genera, cali_genus_tree_amphibians)
cali_tree_genus_amphibians<- sample_tree_generator(matched_species_amphibians, cali_genus_tree_amphibians)
write.tree(cali_tree_genus_amphibians, file = file.path(clade, "cali_genus_tree.tre"))

#this is just the whole genus tree. 

p<- ggtree(cali_tree_genus_amphibians, layout = "circular")
ggsave("amphibians/images/cali_tree_genus.png")

#path to phylogeny.
path_to_cali_phylogeny = file.path( clade, "cali_tree.tre")
phylo_tree<- read.tree(path_to_cali_phylogeny)



#the last two parameters are for naming, for the purpose of the call_cophen function
call_cophen(phylo_tree, clade = "amphibians", geog_area = "cali") 


dir_list_ecoregions <- list.dirs("amphibians/ecoregion_data",recursive = FALSE)  
full_tree<- full_tree

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
#there are very few species in each of these trees. Maybe I should just compare to california. 
