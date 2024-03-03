#main for Plants
#when this file is executed, the cophen matrix will be generated for a squamate tree for the geographic area 
#of california. This can be altered at the specificed location if there are different
#larger trees of interest (ie: some other geographical area )
#in this, we generate trees for the molecular and the compelete phylogenies. will use the molecular from here on out for now. 

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
complete_phylogeny<- read.tree("Plants/full_tree.tre")
genus_complete_phylogeny<- genus_tree_generator(complete_phylogeny)
write.tree(genus_complete_phylogeny, file = paste(clade, "/full_tree_genus.tre", sep = ""))

#use the genus_complete phylogney

#produces 5 interpolated phylogenies 
#interpolated_phylogeny<-BIEN_phylogeny_complete(n_phylogenies = 100)

# for (i in 6:100) {
#   # Create the folder if it doesn't exist
#   folder_path <- paste("Plants/Instance_", i, sep = "")
#   if (!file.exists(folder_path)) {
#     dir.create(folder_path, recursive = TRUE)
#   }
#   
#   # Write the tree file
#   write.tree(interpolated_phylogeny[i], file = paste(folder_path, "/interpolated_phylogeny.tre", sep = ""))
# }
# 
# 

#write.tree(interpolated_phylogeny, file = "Plants/interpolated_full_tree.tre")


#find the filepath to cali_tree
#edit the path to phylogeny line if you're not interested in the cali phylogeny. 

#step one is to make the phylogeny. 
#path to species checklist

path_to_taxa_list = file.path(clade, paste(geog_area, "_species_list.csv", sep = ""))
path_to_full_tree = file.path(clade, "full_tree.tre")
path_to_full_genus_tree<- file.path(clade, "full_tree_genus.tre")

#format the taxa list
cali_plants<-read.csv(path_to_taxa_list)
cali_plants$X <- NULL 
colnames(cali_plants)<- "name"

#for the genus only taxa list
cali_plants_genera<- genus_only(cali_plants)

#load the largest tree and prune to only california, then save the california tree
#todo: make this more transferable to geographic area.

#for the full phylogenetic tree
#full_tree<- read.tree(path_to_full_tree)

#todo: what is going on here????
full_tree_genus<-read.tree(path_to_full_genus_tree)

unmatched_genera<- check_taxa(uniquecali_plants_genera, full_tree_genus)

#about 885 unmatched genera in the molecular phylogney


#about 2000 represented taxa
matched_genera<- remove_taxa(cali_plants_genera, full_tree_genus)

cali_tree_genus<- sample_tree_generator(matched_genera, full_tree_genus)

write.tree(cali_tree_genus, file = file.path(clade, "cali_genus_tree.tre"))


#generate the cophenetic matrix for the genus tree
#save as cali_genus_cophen_matrix
call_cophen(cali_tree_genus, clade = "Plants", geog_area = "cali_genus") 

#only about 3415 species that actually match. fewer than 1/3 are represented. 
#matched_species_plants<-remove_taxa(cali_plants, full_tree)
#cali_tree<- sample_tree_generator(matched_species_plants, full_tree)
#write.tree(cali_tree, file = file.path(clade, "cali_tree.tre"))


#find the max number of species of california plants. 
max_species <- max_species(cali_plants)


#for the interpolated complete tree
#this needs to read in all the trees and apply them to a list first. 

#unmatched_species_list<- list()
#matched_species_list<- list()

#should be the same matched/unmatched species each time. 


#edited out call_cophen for sake of tree initialization statistics. 
for(i in 1: 99)
{
 temp<- read.tree(paste("Plants/Instance_", i, "/interpolated_phylogeny.tre", sep = ""))
 unmatched_species_list<- check_taxa(cali_plants, temp)
 matched_species_list<-remove_taxa(cali_plants, temp)
 temp_cali_tree<- sample_tree_generator(matched_species_list, temp)
 write.tree(temp_cali_tree, file = paste("Plants/Instance_", i, "/cali_tree_interpolated.tre", sep = ""))
 call_cophen_InterpTrees(temp_cali_tree, clade = "Plants", geog_area = "cali", instance = i) 
 print(i)
}


#this is to get any interesting statistics but is not useful for tree building 
full_tree_int<- read.tree("Plants/interpolated_full_tree.tre")
unmatched_species_int<- check_taxa(cali_plants, full_tree_int)
matched_species_plants_int<-remove_taxa(cali_plants, full_tree_int)
max_species<- length(matched_species_plants_int[,1])






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
