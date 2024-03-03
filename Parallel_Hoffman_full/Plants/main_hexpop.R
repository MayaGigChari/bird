#todo for plants: need to figure out how to save a hexcode file so easily retrievable 
#need to re-do the species lists for observations and actual ranges. 

packages_to_install <- c("picante", "ape",  "dplyr", "phytools", "jsonlite", "tools", "stringr")

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

#reading in the cophenetic matrix 
cophen<-readRDS("Plants/cali_genus_cophen_matrix")
#parent_tree<- read.tree("Plants/interpolated_full_tree.tre") probably don't really need this.
cali_tree<- read.tree("Plants/cali_genus_tree.tre") #load the genus tree
data<- readRDS("Plants/occurrence_plants_polygonds.rds") #hexagonal species data. 


#associate the data with the indexes
h3_indexes<- data.frame(read.csv("Cali_geometry/h3_indexes.csv"))
h3_indexes$X<- NULL #not sure why this always happens. 
dat_names<- h3_indexes$h3_indexes
names(data)<- dat_names


#now we have assigned data to each 

#the first word will always be the genus. 

#already have the occurrence data


#this should have been done in a previous script. 
#names(data)<- h3_indexes


current_directory <- getwd()

tree_trimming_path<-file.path(current_directory, "App_functions_bird.R")



#need to load the dataframe 


poly_labels<- names(data) #this gives us all the codes. 

?lapply

#not sure if I need to do this? but maybe I will? 
#lapply(data, sample_tree_generator)
#instead of for loop should use lapply
#step 1: need to make a bunch of trees


#need to fill empty hexes and list_trees properly. 
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

#proportion_missing <- numeric(length(poly_labels))
#pd_values <- numeric(length(poly_labels))
#mpd_values <- numeric(length(poly_labels))
#mntd_values <- numeric(length(poly_labels))
#empty_hexes <- character(0)

# Loop through poly_labels. comment out the temp$mname word line if you want to use species. also need to uncomment the commented line. 

for (i in 1:length(names(data)))
{
  temp <- data.frame(data[i])
  colnames(temp) <- "name"
  #temp$name <- sub(" ", "_", temp$name)
  temp$name <- word(temp$name, 1)
  temp<- unique(temp)
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


#need to populate proportion missing
for (i in 1:length(names(data)))
{
  proportion_missing[i]<- length(missing_species[[i]])/(length(missing_species[[i]]) + length(species_in_tree[[i]]))
}
#node level data. 
print(proportion_missing)

#avg_proportion_missing<- sum(proportion_missing)/length(names(data))

tree_sizes<- species_in_tree
length<- lapply(tree_sizes, length)


lengths<- unlist(length)
poly_data<- data.frame(unlist(poly_labels))
colnames(poly_data)<- "id"
poly_data$pd<- unlist(pd_values)
poly_data$mpd<- unlist(mpd_values)
poly_data$mntd<-unlist(mntd_values)
poly_data$retrieved_taxa<-unlist(lengths)
poly_data$proportion_missing<- unlist(proportion_missing)
poly_data<- data.frame(poly_data)
#can potentially do a lot with these statistics

#just make a hist and manually save
hist(proportion_missing, main = "Histogram of Proportion Missing Genera in Plant Hexes")
#poly_data$absent_taxa<- missing_species


#summary statistics. only works when there are no NA's 
max_tree<- max(poly_data$tree_sizes) #this is the maximum tree size. 880 
min_tree<- min(poly_data$tree_sizes)
avg_tree<- mean(poly_data$tree_sizes)

#want to output a metadata file 


write.csv(poly_data, "Plants/Whole_pixel_data_genus.csv")

json_data <- toJSON(poly_data)
write(json_data, "Plants/initial_poly_plants_data_genus.json")



#this is code to convert the file of trees to a json file. 
# Path to the folder containing .tre files

#this is genus level. 
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

json_data <- toJSON(tre_objects)
write_json(json_data, "Plants/bird_trees_genus.json")


#now we have. 
#to give MIA the birds data 
#work with species_in_tree dataframe. 


#now have all the data needed to give to Mia. 
names(species_in_tree)<- poly_labels
json_data_species_names <- toJSON(species_in_tree, pretty = FALSE)
write_json(json_data_species_names, "Plants/plant_hex_species.json")

