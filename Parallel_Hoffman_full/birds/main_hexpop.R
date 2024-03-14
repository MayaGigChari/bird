#TODO: resolve whatever is the issue with the cophenetic matrix
#TODO: get the california species list of all birds again! Might not be a good representative list after all.
#RERUN WITH A BETTER LIST OF CALI BIRDS!
#remove all single-taxon trees. 

#need to streamline this and make a function out of it. 

packages_to_install <- c("picante", "ape",  "dplyr", "phytools", "jsonlite", "tools", "purr")

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
Model_gen_path <- file.path(current_directory, "Model_generation.R")

source(Model_gen_path)
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

#this function popHexStats only works for hexagonal data. 

popAreaStats<- 
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


hex_tree_stats_birds<- readRDS("birds/raw_hex_stats_from_ranges")
missing_species_rds<- hex_tree_stats_birds$missing_species
names(missing_species_rds)<- names(data)

#this is separating all the stuff. 

#need to use like key value pairs 
present_species_rds<- hex_tree_stats_birds$species_in_tree
names(present_species_rds)<- names(data)

proportion_missing_rds<-hex_tree_stats_birds$proportion_missing
names(proportion_missing_rds)<- names(data)

pd_rds<- hex_tree_stats_birds$pd_values
names(pd_rds)<- names(data)

mpd_rds<- hex_tree_stats_birds$mpd_values
names(mpd_rds)<- names(data)

mntd_rds<- hex_tree_stats_birds$mntd_values
names(mntd_rds)<- names(data)





#colorize the hexagons: 


#load and clean the data
#this might work better not inverted. 
quartile_info_pd<-data.frame(read.csv("birds/ranges_quantile_pd_output_bootstrap.csv"))
quartile_info_mpd<-data.frame(read.csv("birds/ranges_quantile_mpd_output_bootstrap.csv"))
quartile_info_mntd<-data.frame(read.csv("birds/ranges_quantile_mntd_output_bootstrap.csv"))

pd_cleaned[,]

pd_cleaned<- data_clean_quartiles(quartile_info_pd, "pd")
mpd_cleaned<- data_clean_quartiles(quartile_info_mpd, "mpd")
mntd_cleaned<- data_clean_quartiles(quartile_info_mntd, "mntd")


#create a function for determining the list of stuff
#this isn't going to work for now because it's not interpolated! based on raw ass data! Need to actually interpolate fitted 
#surfaces for all the intervals if I want this to work. 

sizes<- unlist(lapply(hex_tree_stats_birds$species_in_tree, length))

sizes[1120]
#basically rounds up when the tree size is not an exact multiple of 5, and checks based on that. 
getQuantileValues <- function(sample_size, metric_quantile_df) {
  while(sample_size %%5 != 0)
  {
    #rounds down. this I believe is better for significance. 
    sample_size<- sample_size - 1
  }
  return(metric_quantile_df %>%
           filter(tree_size == sample_size))
}

result <- lapply(sizes, function(size) getQuantileValues(size, metric_quantile_df = pd_cleaned))



names<- names(unlist(result[1]))


#need to make this into a function but will do it this way for now. 
quartile_info_pd<- list()
quartile_info_mpd<-list()
quartile_info_mntd<- list()


#basically all thsee values are wack. might be something wrong with the hex trees generated
#since there are so many steps here. 
#this is ultimately not working at all. 
#I'm going to bed. 


for(i in 1:length(result))
{
  
  list_comp<- unlist(result[i])[1:15]
  if(unlist(hex_tree_stats_birds$mpd_values)[i] < min(list_comp))
  {
    quartile_info_mpd[i]<- 0 
  }
  else if(unlist(hex_tree_stats_birds$mpd_values)[i] > max(list_comp))
  {
    print("true")
    quartile_info_mpd[i]<- 100 
  }
  else
  {
    max_value<- max(list_comp[list_comp < unlist(hex_tree_stats_birds$mpd_values)[i]])
    max_name <- names(list_comp)[which.max(list_comp == max_value)]
    quartile_info_mpd[i] <- max_name
  }
}
names(quartile_info_mpd)<- names(data)



#now I'm going to save everything as a json

#maybe here I should load the outputs of the surface data and assign the pds, mpds and mntds color values. 

#here quartile is the largest value that the pd value is smaller than. 
combined_list<- Map(list,missing_species_rds = missing_species_rds, present_species_rds = present_species_rds,pd = pd_rds, mpd = mpd_rds, mntd = mntd_rds, quartile = quartile_info )

json_data <- jsonlite::toJSON(combined_list)
# Write the JSON data to a file

#this I will give to Mia to look at. 
writeLines(json_data, "birds/range_hex_data_statistics.json")


#below is the code to populate the larger ecoregions with information. this doesn't have to do with the quartile stuff. 

pd_values_ecoregions<- list()
mpd_values_ecoregions<- list()
mntd_values_ecoregions<- list()
missing_species_ecoregions<- list()
present_species_ecoregions<- list()

dir_list_ecoregions <- list.dirs("birds/ecoregion_data",recursive = FALSE)  
ecoregion_codes<- c(1,13,14,4,5,6,7,78,8,80,81,85,9)
cali_tree_range_data_full<-read.tree("birds/cali_tree_from_range_data.tre")
cophe_cali_range_data<- readRDS("birds/cali_range_cophen_matrix")
for(i in 1:length(dir_list_ecoregions))
{
  tree_eco_temp<- read.tree(paste(dir_list_ecoregions[i], "/trimmed_tree.tre", sep = "" ))
  missing_species_ecoregions[i]<- list(read.csv(paste(dir_list_ecoregions[i], "/species_absent_from_tree.csv", sep = "" ))$names)
  present_species_ecoregions[i]<- list(read.csv(paste(dir_list_ecoregions[i], "/species_present_in_tree.csv", sep = "" ))$names)
  pd_values_ecoregions[i]<- pd_app_picante(tree_eco_temp, cali_tree_range_data_full)$PD
  mpd_values_ecoregions[i]<- mpd_app_picante(tree_eco_temp, cophe_cali_range_data)
  mntd_values_ecoregions[i]<- mntd_app_picante(tree_eco_temp, cophe_cali_range_data)
}

names(pd_values_ecoregions)<- ecoregion_codes
names(mpd_values_ecoregions)<- ecoregion_codes
names(mntd_values_ecoregions)<- ecoregion_codes
names(missing_species_ecoregions)<- ecoregion_codes
names(present_species_ecoregions)<- ecoregion_codes

#here quartile is the largest value that the pd value is smaller than. 
combined_list_ecoregions<- Map(list,missing_species = missing_species_ecoregions, present_species = present_species_ecoregions,pd = pd_values_ecoregions, mpd = mpd_values_ecoregions, mntd = mntd_values_ecoregions)

json_data_ecoregions <- jsonlite::toJSON(combined_list_ecoregions)
# Write the JSON data to a file

#this I will give to Mia to look at. 
writeLines(json_data_ecoregions, "birds/range_ecoregions_data_statistics.json")
