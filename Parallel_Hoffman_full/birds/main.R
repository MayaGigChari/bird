#main for birds 
#when this file is executed, the cophen matrix will be generated for a bird tree for the geographic area 
#of california. This can be altered at the specificed location if there are different
#larger trees of interest (ie: some other geographcial area )

#get the current working directory 
if (!requireNamespace("picante", quietly = TRUE)) {
  # Install 'picante' only if not already installed
  install.packages("picante")
}
#specify the clade that you are interested in

clade = "birds"

#load all the stuff from the cophen_function file 
current_directory <- getwd()
coph_path <- file.path(current_directory, "Cophen_function.R")
source(coph_path)



#find the filepath to cali_tree
#edit the path to phylogeny line if you're not interested in the cali phylogeny. 
path_to_cali_phylogeny = file.path( clade, "cali_tree.tre")
phylo_tree<- read.tree(path_to_cali_phylogeny)


#the last two parameters are for naming, for the purpose of the call_cophen function
call_cophen(phylo_tree, clade = "birds", geog_area = "cali") 

#the output should be saved to the clade file. basically should just have to execute main. 




