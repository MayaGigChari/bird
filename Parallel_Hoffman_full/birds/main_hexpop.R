#TODO: resolve whatever is the issue with the cophenetic matrix
#TODO: get the california species list of all birds again! Might not be a good representative list after all.
#RERUN WITH A BETTER LIST OF CALI BIRDS!
#remove all single-taxon trees. 

packages_to_install <- c("picante", "ape",  "dplyr", "phytools")

# Load necessary packages
for (pkg in packages_to_install) {
  # Check if the package is not already installed
  if (!requireNamespace(pkg, quietly = TRUE)) {
    # Install the package only if not already installed
    install.packages(pkg)
  }
  # Load the package
  library(pkg, character.only = TRUE)
}

clade = "birds"

#issue with the cophenetic matrix. 
cophen<-readRDS("birds/cali_cophen_matrix")
parent_tree<- read.tree("birds/full_tree.tre")
cali_tree<- read.tree("birds/cali_tree.tre")
data<- readRDS("birds/occurrence_birds_polygons.rds") #hexagonal species data. 



current_directory <- getwd()

tree_trimming_path<-file.path(current_directory, "App_functions_bird.R")


source(tree_trimming_path)

#need to load the dataframe 


poly_labels<- names(data) #this gives us all the codes. 

?lapply

lapply(data, sample_tree_generator)
#instead of for loop should use lapply
#step 1: need to make a bunch of trees

missing_species<- list()
proportion_missing<- list()
species_in_tree<- list()
empty_hexes<- list()
list_trees<- list()
pd_values<- list()
mpd_values<- list()
mntd_values<- list()

#can generate the trees and write them all to a file.  
missing_species <- vector("list", length = length(poly_labels))
species_in_tree <- vector("list", length = length(poly_labels))
proportion_missing <- numeric(length(poly_labels))
pd_values <- numeric(length(poly_labels))
mpd_values <- numeric(length(poly_labels))
mntd_values <- numeric(length(poly_labels))
empty_hexes <- character(0)

# Loop through poly_labels

for (i in 1:length(poly_labels)) 
{
  temp <- data.frame(data[i])
  colnames(temp) <- "name"
  missing_species[i] <- check_taxa(temp,cali_tree)
  temp<- remove_taxa(temp, cali_tree)
  species_in_tree[i]<- temp
  proportion_missing[i]<- length(missing_species[i])/length(temp[,1])
  if(length(temp[,1]) == 0)
  {
    empty_hexes<- c(empty_hexes, poly_labels[i])
    write.tree(temp_tree, file = file.path(current_directory, "birds", "hex_trees", paste(poly_labels[i], ".tre", sep  = '')))
    pd_values[i]<- NA
    mpd_values[i]<- NA
    mntd_values[i]<- NA
  }
  else
  {
    temp_tree<- sample_tree_generator(temp, cali_tree)
    write.tree(temp_tree, file = file.path(current_directory, "birds", "hex_trees", paste(poly_labels[i], ".tre", sep  = '')))
    pd_values[i]<- pd_app_picante(temp_tree, parent_tree)$PD
    mpd_values[i]<- mpd_app_picante(temp_tree, cophen)
    mntd_values[i]<- mntd_app_picante(temp_tree, cophen)
  }
  print(i)
  #continue here: 
}
print(missing_species)


#got up to 790

poly_data<- data.frame(poly_labels[1:700])
colnames(poly_data)<- "id"
poly_data$pd<- pd_values[1:700]
poly_data$mpd<- mpd_values[1:700]
poly_data$mntd<-mntd_values[1:700]

named_list <- as.list(poly_data[-ncol(poly_data)])

# Convert named list to JSON
json_data <- toJSON(named_list, auto_unbox = TRUE)


json_data <- toJSON(poly_data, pretty = TRUE)
write_json(json_data, "birds/initial_poly_bird_data.json")

