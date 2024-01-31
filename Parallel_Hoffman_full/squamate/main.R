#main for birds 
#when this file is executed, the cophen matrix will be generated for a bird tree for the geographic area 
#of california. This can be altered at the specificed location if there are different
#larger trees of interest (ie: some other geographcial area )

#first step needs to be to prune the california tree to the list of california taxa 


#Bethany TODO: write a function that gets the max number of species present in a list, and then round down to the nearest tenth
#do this in app_functions_birds

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
path_to_taxa_list = file.path(clade, paste(geog_area, "_species_list.txt", sep = ""))
path_to_full_tree = file.path(clade, "full_tree.txt")


#format the taxa list using App_functions_bird script
cali_squamates<-read(path_to_taxa_list)


#load the largest tree and prune to only california, then save the california tree
#todo: make this more transferable to geographic area.

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

#the output should be saved to the clade file. basically should just have to execute main. 
