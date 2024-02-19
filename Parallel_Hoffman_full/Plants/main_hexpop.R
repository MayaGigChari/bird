#TODO: resolve whatever is the issue with the cophenetic matrix
#TODO: get the california species list of all birds again! Might not be a good representative list after all.
#RERUN WITH A BETTER LIST OF CALI BIRDS!
#remove all single-taxon trees. 
#TODO: Check bird data so that the pd is calculated on the california tree not the whole parent tree!!! ****
#accidentally overwrote birds hex trees so need to rectify that. Now birds hex_trees is plants. Will move to plants. 

packages_to_install <- c("picante", "ape",  "dplyr", "phytools", "jsonlite", "tools")

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

clade = "Plants"


#issue with the cophenetic matrix. 
cophen<-readRDS("Plants/cali_cophen_matrix")
parent_tree<- read.tree("Plants/interpolated_full_tree.tre")
cali_tree<- read.tree("Plants/cali_tree_interpolated.tre")
data<- readRDS("Plants/occurrence_plants_polygonds.rds") #hexagonal species data. 

#this should have been done in a previous script. 
#names(data)<- h3_indexes


current_directory <- getwd()

tree_trimming_path<-file.path(current_directory, "App_functions_bird.R")x


source(tree_trimming_path)

#need to load the dataframe 


poly_labels<- names(data) #this gives us all the codes. 

?lapply

#not sure if I need to do this? but maybe I will? 
#lapply(data, sample_tree_generator)
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
#missing_species <- vector("list", length = length(poly_labels))
#species_in_tree <- vector("list", length = length(poly_labels))

proportion_missing <- numeric(length(poly_labels))
pd_values <- numeric(length(poly_labels))
mpd_values <- numeric(length(poly_labels))
mntd_values <- numeric(length(poly_labels))
empty_hexes <- character(0)

# Loop through poly_labels

for (i in 1:length(names(data)))
{
  temp <- data.frame(data[i])
  colnames(temp) <- "name"
  temp$name <- sub(" ", "_", temp$name)
  missing_species[i] <- check_taxa(temp,cali_tree)
  temp<- remove_taxa(temp, cali_tree)
  species_in_tree[i]<- temp
  proportion_missing[i]<- length(missing_species[i])/length(temp[,1])
  if(length(temp[,1]) <= 1)
  {
    empty_hexes<- c(empty_hexes, poly_labels[i])
    pd_values[i]<- NA
    mpd_values[i]<- NA
    mntd_values[i]<- NA
  }
  else
  {
    temp_tree<- sample_tree_generator(temp, cali_tree)
    write.tree(temp_tree, file = file.path(current_directory, "Plants", "hex_trees", paste(poly_labels[i], ".tre", sep  = '')))
    pd_values[i]<- pd_app_picante(temp_tree, cali_tree)$PD #why is this the parent tree and not the cali tree? 
    mpd_values[i]<- mpd_app_picante(temp_tree, cophen)
    mntd_values[i]<- mntd_app_picante(temp_tree, cophen)
  }
  print(i)
  #continue here: 
}


#node level data. 
print(proportion_missing)

#avg_proportion_missing<- sum(proportion_missing)/length(names(data))

tree_sizes<- species_in_tree
length<- lapply(tree_sizes, length)


lengths<- unlist(length)
poly_data<- data.frame(poly_labels)
colnames(poly_data)<- "id"
poly_data$pd<- pd_values
poly_data$mpd<- mpd_values
poly_data$mntd<-mntd_values
poly_data$retrieved_taxa<-lengths
poly_data$proportion_missing<- proportion_missing
#poly_data$present_taxa<- species_in_tree
#poly_data$absent_taxa<- missing_species


#summary statistics.
max_tree<- max(poly_data$tree_sizes) #this is the maximum tree size. 880 
min_tree<- min(poly_data$tree_sizes)
avg_tree<- mean(poly_data$tree_sizes)

#want to output a metadata file 


write.csv(poly_data, "Plants/Whole_pixel_data.csv")



json_data <- toJSON(poly_data, pretty = FALSE)
write(json_data, "Plants/initial_poly_plants_data.json")



#this is code to convert the file of trees to a json file. 
# Path to the folder containing .tre files
folder_path <- "Plants/hex_trees"

# List all .tre files in the folder
tre_files <- list.files(folder_path, pattern = "\\.tre$", full.names = TRUE)
tre_objects <- list()

# Loop through each .tre file, read its content, and add it to the list
#this constructs the json file 
for (file in tre_files) {
  # Extract code from the file name (assuming the code is before '.tre')
  code <- sub("\\.tre$", "", basename(file))
  # Read the contents of the .tre file
  tree_content <- readLines(file)
  # Store code as the key and tree content as the value
  tre_objects[[code]] <- tree_content
}

json_data <- toJSON(tre_objects, pretty = FALSE)
write_json(json_data, "Plants/bird_trees.json")


#now we have. 
#to give MIA the birds data 
#work with species_in_tree dataframe. 


#now have all the data needed to give to Mia. 
names(species_in_tree)<- poly_labels
json_data_species_names <- toJSON(species_in_tree, pretty = FALSE)
write_json(json_data_species_names, "Plants/plant_hex_species.json")

