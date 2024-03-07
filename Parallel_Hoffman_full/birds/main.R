#main for birds 
#when this file is executed, the cophen matrix will be generated for a bird tree for the geographic area 
#of california. This can be altered at the specificed location if there are different
#larger trees of interest (ie: some other geographcial area )

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
path_to_cali_taxa_list_range = file.path(clade, "birds_seen_in_cali_with_overlap_range.csv")
filepath_ecoregions = file.path(clade, "ecoregion_data")
path_to_full_tree = file.path(clade, "full_tree.tre") #complete bird tree
#this is just for california birds. Takes the list and makes it pretty. should move this out of main. 
cali_birds<-read_csv(file = path_to_cali_taxa_list_range)
cali_species<-cali_birds$x
cali_species<- gsub(" ", "_", cali_species)
cali_species<-data.frame(cali_species)
colnames(cali_species)<- "names"
#there are 383 california species that are endemic. 
#list is now called cali_species

#load the largest tree and prune to only california, then save the california tree
#todo: make this more transferable to geographic area.
full_tree<- read.tree(path_to_full_tree)
unmatched_species_birds<- check_taxa(cali_species, full_tree )
matched_species_birds<-remove_taxa(cali_species, full_tree)
cali_tree<- sample_tree_generator(matched_species_birds, full_tree)
write.tree(cali_tree, file = file.path(clade, "cali_tree_from_range_data.tre"))
write.csv(unmatched_species_birds, file = "birds/range_birds_absent_phylogeny.csv")
write.csv(matched_species_birds, file = "birds/range_birds_present_phylogeny.csv")

#there are about 50 taxa that are not represented.

#run the sample_tree_creator to generate the sample trees

#maketrees(cali_tree, 5, 600, 5, clade = clade)


#path to phylogeny.
path_to_cali_phylogeny = file.path( clade, "cali_tree_from_range_data.tre")
phylo_tree<- read.tree(path_to_cali_phylogeny)



#the last two parameters are for naming, for the purpose of the call_cophen function
call_cophen(phylo_tree, clade = "birds", geog_area = "cali_range") #generate range matrix for birds

#the output should be saved to the clade file. basically should just have to execute main. 

#now need to basically do the above for ecoregions. 

