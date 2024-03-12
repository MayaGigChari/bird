#TODO: resolve whatever is the issue with the cophenetic matrix
#TODO: get the california species list of all birds again! Might not be a good representative list after all.
#RERUN WITH A BETTER LIST OF CALI BIRDS!
#remove all single-taxon trees. 

#need to streamline this and make a function out of it. 

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

for (i in 791:length(poly_labels)) 
{
  temp <- data.frame(data[i])
  colnames(temp) <- "name"
  missing_species[i] <- check_taxa(temp,cali_tree)
  temp<- remove_taxa(temp, cali_tree)
  species_in_tree[i]<- temp
  proportion_missing[i]<- length(missing_species[i])/length(temp[,1])
  if(length(temp[,1]) <= 1)
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


tree_sizes<- species_in_tree
length<- lapply(tree_sizes, length)

#got up to 790

lengths<- unlist(length)
poly_data<- data.frame(poly_labels)
colnames(poly_data)<- "id"
poly_data$pd<- pd_values
poly_data$mpd<- mpd_values
poly_data$mntd<-mntd_values
poly_data$tree_sizes<-lengths


write.csv(poly_data, "birds/Whole_pixel_data.csv")



json_data <- toJSON(poly_data, pretty = FALSE)
write(json_data, "birds/initial_poly_bird_data.json")



#this is code to convert the file of trees to a json file. 
# Path to the folder containing .tre files
folder_path <- "birds/hex_trees"

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
write_json(json_data, "birds/bird_trees.json")

#now we have. 

#to give MIA the birds data 
#work with species_in_tree dataframe. 

names(species_in_tree)<- poly_labels
json_data_species_names <- toJSON(species_in_tree, pretty = FALSE)
write_json(json_data_species_names, "birds/bird_hex_species.json")



### below: will do essentially the same thing as above but for only the bird range data. 
###hexpop for bird range data from birds of the world only!
#here: now i will write a function to streamline this. 

#need to make hex trees folders. 
#ex: filepath would be parallel_hoffman_full/birds or parallel_hoffman_full/ecoregion_data/1 or something. 

#should only need to call this function once. 
popHexStats<- function(polygon_data, parent_tree, output_filepath, cophen_filepath)
{
  cophen_forfunc<- readRDS(cophen_filepath)
  print("cophen_made")
  hex_trees_filepath<- paste(output_filepath, "/hex_trees", sep = "")
  print(hex_trees_filepath)
  if (!file.exists(hex_trees_filepath)) 
  {
    dir.create(hex_trees_filepath)
    print("new directory created")
  }
  missing_species<- list()
  proportion_missing<- list()
  species_in_tree<- list()
  empty_hexes<- list()
  list_trees<- list()
  pd_values<- list()
  mpd_values<- list()
  mntd_values<- list()
  
  print("lists_made")
  poly_labels<- names(polygon_data)
  for (i in 1:length(poly_labels)) 
  {
    temp <- data.frame(polygon_data[i])
    colnames(temp) <- "name"
    temp$name<- gsub(" ", "_", temp$name)
    missing_species[i] <- check_taxa(temp,parent_tree)
    temp<- remove_taxa(temp, parent_tree)
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
      temp_tree<- sample_tree_generator(temp, parent_tree)
      write.tree(temp_tree, file = file.path(hex_trees_filepath, paste(poly_labels[i], ".tre", sep  = '')))
      pd_values[i]<- pd_app_picante(temp_tree, parent_tree)$PD
      mpd_values[i]<- mpd_app_picante(temp_tree, cophen_forfunc)
      mntd_values[i]<- mntd_app_picante(temp_tree, cophen_forfunc)
    }
    print(i)
    #continue here: 
  }
  list_return <- list(
    missing_species = missing_species,
    proportion_missing = proportion_missing,
    species_in_tree = species_in_tree,
    empty_hexes = empty_hexes,
    pd_values = pd_values,
    mpd_values = mpd_values,
    mntd_values = mntd_values
  )
  return(list_return)
}
data<- readRDS("birds/occurrence_birds_polygons_fromRangeData")
parent_tree<- read.tree("birds/cali_tree_from_range_data.tre")

hex_tree_stats_birds<- popHexStats(data, parent_tree, "birds", cophen_filepath = "birds/cali_range_cophen_matrix")

saveRDS(hex_tree_stats_birds, file = "birds/raw_hex_stats_from_ranges")

missing_species_rds<- hex_tree_stats_birds$missing_species
names(missing_species_rds)<- names(data)

present_species_rds<- hex_tree_stats_birds$species_in_tree
names(present_species_rds)<- names(data)

proportion_missing_rds<-hex_tree_stats_birds$proportion_missing
names(proportion_missing_rds)<- names(data)

pd_rds<- hex_tree_stats_birds$pd_values
names(pds_rds)<- names(data)

mpd_rds<- hex_tree_stats_birds$mpd_values
names(mds_rds)<- names(data)

mntd_rds<- hex_tree_stats_birds$mntd_values
names(mntd_rds)<- names(data)

combined_list<- Map(list,missing_species_rds, present_species_rds)

json_data <- jsonlite::toJSON(combined_list)

# Write the JSON data to a file
writeLines(json_data, "output.json")


#can always just do the hex data stuff afterwards. 

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

=

