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
cophen_plants<-readRDS("Plants/cali_genus_cophen_matrix")
#parent_tree<- read.tree("Plants/interpolated_full_tree.tre") probably don't really need this.
#made the genus tree in main. 
cali_tree_plants<- read.tree("Plants/cali_genus_tree.tre") #load the genus tree
data_plants<- readRDS("Plants/hexagon_true_genera_plant_data") #hexagonal species data. 


#associate the data with the indexes
#this stays the same for observational and range data. 
h3_indexes<- data.frame(h3_indexes)
h3_indexes$X<- NULL #not sure why this always happens. 
dat_names<- h3_indexes$h3_indexes
names(data)<- dat_names


#now we have assigned data to each 

#the first word will always be the genus. 

#already have the occurrence data


#this should have been done in a previous script. 
#names(data)<- h3_indexes


#need to load the dataframe 


poly_labels<- names(data) #this gives us all the codes. 

#run this function with the most recent polygon data 

popHexStats<- function(polygon_data, parent_tree, output_filepath, cophen_filepath)
{
  #some of these actually have zero intersecting species. 
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
    if(length(unlist(polygon_data[i]))<=1)
    {
      print("t")
      empty_hexes<- c(empty_hexes, poly_labels[i])
      pd_values[i]<- NA
      mpd_values[i]<- NA
      mntd_values[i]<- NA
    }
    else
    {
      temp <- data.frame(polygon_data[i])
      colnames(temp) <- "name"
      temp$name<- gsub(" ", "_", temp$name)
      missing_species[i] <- check_taxa(temp,parent_tree)
      temp<- remove_taxa(temp, parent_tree)
      species_in_tree[i]<- temp
      print(species_in_tree[i])
      proportion_missing[i]<- length(missing_species[i])/length(temp[,1])
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
hex_tree_stats_birds<- popHexStats(data_plants, cali_tree_plants, "Plants", cophen_filepath = "Plants/cali_genus_cophen_matrix")
hex_tree_stats_plants<- hex_tree_stats_birds

pop_hex_stats_plants_df<- data_frame("h3_index" = names(data), "pd_values" = unlist(hex_tree_stats_plants$pd_values), "mpd_values" = unlist(hex_tree_stats_plants$mpd_values), "mntd_values" = unlist(hex_tree_stats_plants$mntd_values), 
                                    "tree_size" = unlist(lapply(hex_tree_stats_plants$species_in_tree, length)), "missing_taxa_tree_size" = unlist(lapply(hex_tree_stats_plants$missing_species, length)) )

pop_hex_stats_plants_df$proportion_missing<- pop_hex_stats_plants_df$missing_taxa_tree_size/(pop_hex_stats_plants_df$tree_size + pop_hex_stats_plants_df$missing_taxa_tree_size)



#this is just an aside to make a figure of mssing taxa. 

#need to make 

library(ggplot2)
missing_hist<- ggplot(pop_hex_stats_plants_df, aes(x = proportion_missing)) +
  geom_histogram(bins = 50) +
  ggtitle("Missing Taxa Proportions: Hexagon Data California")

ggsave(filename = "Plants/images/Missing_taxa_proportions_hexagon_california.png", 
       plot = missing_hist, 
       dpi = 300, 
       width = 8, 
       height = 6, 
       units = "in")

#this just saves the file hex_tree_stats_birds without any geographic information. 
saveRDS(hex_tree_stats_birds, file = "birds/raw_hex_stats_from_ranges")

#there are 200 hexagons with missing data. 

