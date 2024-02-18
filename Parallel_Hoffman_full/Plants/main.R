#main for squamates 
#when this file is executed, the cophen matrix will be generated for a squamate tree for the geographic area 
#of california. This can be altered at the specificed location if there are different
#larger trees of interest (ie: some other geographical area )

#first step needs to be to prune the california tree to the list of california taxa 

#install necessary packages 
packages_to_install <- c("picante", "ape", "dplyr", "readr", "phytools", "BIEN")
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

clade = "Plants"
geog_area = "cali"

interpolated_phylogeny<-BIEN_phylogeny_complete()

write.tree(interpolated_phylogeny, file = "Plants/interpolated_full_tree.tre")

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
path_to_taxa_list = file.path(clade, paste(geog_area, "_species_list.csv", sep = ""))
path_to_full_tree = file.path(clade, "full_tree.tre")


#format the taxa list
cali_plants<-read.csv(path_to_taxa_list)
cali_plants$X <- NULL 
colnames(cali_plants)<- "name"


#load the largest tree and prune to only california, then save the california tree
#todo: make this more transferable to geographic area.

#for the full phylogenetic tree
full_tree<- read.tree(path_to_full_tree)
unmatched_species<- check_taxa(cali_plants, full_tree)
#about 8000 unmatched species. 

#only about 3415 species that actually match. fewer than 1/3 are represented. 
matched_species_plants<-remove_taxa(cali_plants, full_tree)
cali_tree<- sample_tree_generator(matched_species_plants, full_tree)
write.tree(cali_tree, file = file.path(clade, "cali_tree.tre"))


#find the max number of species of california plants. 
max_species <- max_species(cali_plants)


#for the interpolated complete tree
full_tree_int<- read.tree("Plants/interpolated_full_tree.tre")
unmatched_species_int<- check_taxa(cali_plants, full_tree_int)
#about 8000 unmatched species. 

#only about 3415 species that actually match. fewer than 1/3 are represented. 
matched_species_plants_int<-remove_taxa(cali_plants, full_tree_int)
cali_tree_int<- sample_tree_generator(matched_species_plants_int, full_tree_int)
write.tree(cali_tree_int, file = file.path(clade, "cali_tree_interpolated.tre"))


#find the max number of species of california plants.


#make sample trees for up to max species number of plants
#max number of species represented on california tree
max_species<- length(matched_species_plants_int[,1])
#run the sample_tree_creator to generate the sample trees
maketrees(cali_tree_int, 5, max_species, 5, clade = clade)

#throws an error once we reach the max but that's fine. 

#maybe should try to trim based on genus? 

#path to phylogeny.
path_to_cali_phylogeny = file.path( clade, "cali_tree_interpolated.tre")
phylo_tree<- read.tree(path_to_cali_phylogeny)



#the last two parameters are for naming, for the purpose of the call_cophen function
call_cophen(phylo_tree, clade = "Plants", geog_area = "cali") 


#the output should be saved to the clade file. basically should just have to execute main. 
