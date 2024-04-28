#TODO: resolve whatever is the issue with the cophenetic matrix
#TODO: get the california species list of all birds again! Might not be a good representative list after all.
#RERUN WITH A BETTER LIST OF CALI BIRDS!
#remove all single-taxon trees. 


#upodate 04/16/2024: come back to this hexpop generation from a new perspective: forget about quartile shit. 
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
cali_tree<- read.tree("birds/cali_tree_from_range_data.tre")
data<- readRDS("birds/occurrence_birds_polygons_fromRangeData") #hexagonal species data. 

<<<<<<< HEAD


current_directory <- getwd()

tree_trimming_path<-file.path(current_directory, "App_functions_bird.R")
Model_gen_path <- file.path(current_directory, "Model_generation.R")

source(Model_gen_path)
source(tree_trimming_path)

=======
#reading in the cophenetic matrix 
cophen_plants<-readRDS("Plants/cali_genus_cophen_matrix")
#parent_tree<- read.tree("Plants/interpolated_full_tree.tre") probably don't really need this.
#made the genus tree in main. 
cali_tree_plants<- read.tree("Plants/cali_genus_tree_APR2024.tre") #load the genus tree
data_plants<- readRDS("Plants/hexagon_true_genera_plant_data") #hexagonal species data. 

#total of 1966 genera. interesting. 
cali_tree_plants
plot(cali_tree_plants, type = "fan", show.tip.label = FALSE)

cali_tree_plants #660 total plants represented in the phylogeny. 
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


>>>>>>> 5e4406aced4ad24e478144b470f9f69ad6c4ef08
#need to load the dataframe 


poly_labels<- names(data) #this gives us all the codes. 

#run this function with the most recent polygon data 

<<<<<<< HEAD
<<<<<<< HEAD
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
  if(length(temp[,1]) <= 1)
  {
    empty_hexes<- c(empty_hexes, poly_labels[i])
    write.tree(temp_tree, file = file.path(current_directory, "birds", "hex_trees", paste(poly_labels[i], ".tre", sep  = '')))
    pd_values[i]<- NA
    mpd_values[i]<- NA
    mntd_values[i]<- NA
=======
=======
#there should be some kind of difference with pd no? maybe not. 
>>>>>>> 5c21e0a7d2ee6bec8c2b1bf8702d76a1ebdc35f5
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
>>>>>>> 5e4406aced4ad24e478144b470f9f69ad6c4ef08
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
<<<<<<< HEAD
    temp_tree<- sample_tree_generator(temp, cali_tree)
    write.tree(temp_tree, file = file.path(current_directory, "birds", "hex_trees", paste(poly_labels[i], ".tre", sep  = '')))
    pd_values[i]<- pd_app_picante(temp_tree, parent_tree)$PD
    mpd_values[i]<- mpd_app_picante(temp_tree, cophen)
    mntd_values[i]<- mntd_app_picante(temp_tree, cophen)
=======
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
>>>>>>> 5e4406aced4ad24e478144b470f9f69ad6c4ef08
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

<<<<<<< HEAD
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
=======
pop_hex_stats_plants_df$proportion_missing<- pop_hex_stats_plants_df$missing_taxa_tree_size/(pop_hex_stats_plants_df$tree_size + pop_hex_stats_plants_df$missing_taxa_tree_size)



#this is just an aside to make a figure of mssing taxa. 

#need to make 

library(ggplot2)
missing_hist<- ggplot(pop_hex_stats_plants_df, aes(x = proportion_missing)) +
  geom_histogram(bins = 50) +
  ggtitle("Missing Taxa Proportions: Hexagon Data California")
>>>>>>> 5e4406aced4ad24e478144b470f9f69ad6c4ef08

ggsave(filename = "Plants/images/Missing_taxa_proportions_hexagon_california.png", 
       plot = missing_hist, 
       dpi = 300, 
       width = 8, 
       height = 6, 
       units = "in")

#this just saves the file hex_tree_stats_birds without any geographic information. 
saveRDS(pop_hex_stats_plants_df, file = "birds/raw_hex_stats_from_ranges")

<<<<<<< HEAD
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

hex_tree_stats_birds$proportion_missing
names(hex_tree_stats_birds)
#try to save all this as an sf file. 

pop_hex_stats_birds_df<- data_frame("h3_index" = names(data), "pd_values" = unlist(hex_tree_stats_birds$pd_values), "mpd_values" = unlist(hex_tree_stats_birds$mpd_values), "mntd_values" = unlist(hex_tree_stats_birds$mntd_values), 
                                    "tree_size" = unlist(lapply(hex_tree_stats_birds$species_in_tree, length)), "missing_taxa_tree_size" = unlist(lapply(hex_tree_stats_birds$missing_species, length)) )

pop_hex_stats_birds_df$proportion_missing<- pop_hex_stats_birds_df$missing_taxa_tree_size/(pop_hex_stats_birds_df$tree_size + pop_hex_stats_birds_df$missing_taxa_tree_size)

#this is just an aside to make a figure of mssing taxa. 

#need to make 
missing_hist<- ggplot(pop_hex_stats_birds_df, aes(x = proportion_missing)) +
  geom_histogram(bins = 50) +
  ggtitle("Missing Taxa Proportions: Hexagon Data California")
  
ggsave(filename = "birds/images/Missing_taxa_proportions_hexagon_california.png", 
       plot = missing_hist, 
       dpi = 300, 
       width = 8, 
       height = 6, 
       units = "in")

#this just saves the file hex_tree_stats_birds without any geographic information. 
saveRDS(hex_tree_stats_birds, file = "birds/raw_hex_stats_from_ranges")



#want to merge this back to the polygon sf object 
#use the "polygons" file generated in california-partition. 
#this is called(hex_data_as_sf in the Cali_Geometry file. )

cali_hexes_as_sf<- st_read("Cali_Geometry/hex_data_as_sf.shp")

polygon_data_full<- left_join(data.frame(cali_hexes_as_sf), pop_hex_stats_birds_df, by = "h3_index")

polygon_data_full<- st_as_sf(polygon_data_full)

polygon_data_full<- ecoregion_id(polygon_data_full, full = TRUE) #get the overlapping ecoregions of each reserve. 

#ec_js_pd<- ecoregion_json_filename shortened. 
polygon_data_full$ec_js_pd<- unlist(lapply(polygon_data_full$US_L3CODE, makejsonstring, metric = "pd"))
polygon_data_full$ec_js_mpd<- unlist(lapply(polygon_data_full$US_L3CODE, makejsonstring, metric = "mpd"))
polygon_data_full$ec_js_mntd<- unlist(lapply(polygon_data_full$US_L3CODE, makejsonstring, metric = "mntd"))



#need to determine for each polygon in polygon_data_full if the pd is significant or not, and make a table. similar to what was done in the reserves. 
#need to make a function that generates a null model somehow concatenated from multiple ecoregions (idk how to do this. need to figure this out.)

#for now I will compare each polygon to california and its null model ecoregion, multiple ecoregions will be left out. 



####THIS WHOLE CHUNK OF CODE IS GENERATING SIGNIFICANCE VALUES. 
polygon_data_CI_ranges_pd_cali<- lapply(polygon_data_full$tree_size, cI_generator, params_json_file = "birds/bird_ranges_wholeCali_pd_model_params.json")
CI_cali_significance_polygons_pd<- Map(check_significance_other_metrics, polygon_data_full$pd_values, upper_lower_keyvals = polygon_data_CI_ranges_pd_cali)

png("birds/images/CI_cali_significance_hexes_pd_hist.png", width = 800, height = 600, units = "px", res = 100)
histogram(unlist(CI_cali_significance_polygons_pd), main = "CI_cali_significance_hexes_pd_hist", xlab = "PD", ylab = "Frequency")
dev.off()

polygon_data_CI_ranges_mpd_cali<- lapply(polygon_data_full$tree_size, cI_generator, params_json_file = "birds/bird_ranges_wholeCali_mpd_model_params.json")
CI_cali_significance_polygons_mpd<- Map(check_significance_other_metrics, polygon_data_full$mpd_values, upper_lower_keyvals = polygon_data_CI_ranges_mpd_cali)


png("birds/images/CI_cali_significance_hexes_mpd_hist.png", width = 800, height = 600, units = "px", res = 100)
histogram(unlist(CI_cali_significance_polygons_mpd)) #about 95% of all polygons in california have significantly low pd. 
dev.off()

polygon_data_CI_ranges_mntd_cali<- lapply(polygon_data_full$tree_size, cI_generator, params_json_file = "birds/bird_ranges_wholeCali_mntd_model_params.json")
CI_cali_significance_polygons_mntd<- Map(check_significance_other_metrics, polygon_data_full$mntd_values, upper_lower_keyvals = polygon_data_CI_ranges_mntd_cali)

png("birds/images/CI_cali_significance_hexes_mntd_hist.png", width = 800, height = 600, units = "px", res = 100)
histogram(unlist(CI_cali_significance_polygons_mntd)) #about 95% of all polygons in california have significantly low pd. 
dev.off()

#for some reason the mean nearest taxon distance is pretty much always insignificant 


#need to generate the json file names for each ecoregion. 
#double check that ecoregions are in the correct order. might be wrong null models. 
#for ecoregions: 
polygon_data_CI_ranges_pd_ecoregions <- Map(cI_generator, polygon_data_full$tree_size, params_json_file = polygon_data_full$ec_js_pd)
CI_ecoregions_significance_polygons_pd<- Map(check_significance_other_metrics, polygon_data_full$pd_values, upper_lower_keyvals = polygon_data_CI_ranges_pd_ecoregions)

png("birds/images/CI_cali_significance_hexes_pd_hist_ecoregions.png", width = 800, height = 600, units = "px", res = 100)
histogram(unlist(CI_ecoregions_significance_polygons_pd), main = "CI_cali_significance_hexes_pd_hist", xlab = "PD", ylab = "Frequency")
dev.off()


polygon_data_CI_ranges_mpd_ecoregions <- Map(cI_generator, polygon_data_full$tree_size, params_json_file = polygon_data_full$ec_js_mpd)
CI_ecoregions_significance_polygons_mpd<- Map(check_significance_other_metrics, polygon_data_full$mpd_values, upper_lower_keyvals = polygon_data_CI_ranges_mpd_ecoregions)

png("birds/images/CI_cali_significance_hexes_mpd_hist_ecoregions.png", width = 800, height = 600, units = "px", res = 100)
histogram(unlist(CI_ecoregions_significance_polygons_mpd), main = "CI_cali_significance_hexes_mpd_hist", xlab = "mpd", ylab = "Frequency")
dev.off()


polygon_data_CI_ranges_mntd_ecoregions <- Map(cI_generator, polygon_data_full$tree_size, params_json_file = polygon_data_full$ec_js_mntd)
CI_ecoregions_significance_polygons_mntd<- Map(check_significance_other_metrics, polygon_data_full$mpd_values, upper_lower_keyvals = polygon_data_CI_ranges_mntd_ecoregions)

png("birds/images/CI_cali_significance_hexes_mntd_hist_ecoregions.png", width = 800, height = 600, units = "px", res = 100)
histogram(unlist(CI_ecoregions_significance_polygons_mntd), main = "CI_cali_significance_hexes_mntd_hist", xlab = "mntd", ylab = "Frequency")
dev.off()


###ADD ALL THESE SIGNIFICANCE VALUES TO THE DATA STRUCTURE: 

#for some reason there's a dimensionality issue. 
#need to figure out how to deal with polygons on multuiple ecoregions stat!!
polygon_data_full$pdSigCal<- unlist(CI_cali_significance_polygons_pd)
polygon_data_full$mpdSigCal<- unlist(CI_cali_significance_polygons_mpd)
polygon_data_full$mntdSigCal<- unlist(CI_cali_significance_polygons_mntd)


#need to change NULL to NA to make this datastructure work. 
#make NA's. line = 
#CI_ecoregions_significance_polygons_mntd[sapply(CI_ecoregions_significance_polygons_mntd, is.null)] <- NA


polygon_data_full$pdSigEco<- unlist(CI_ecoregions_significance_polygons_pd)
polygon_data_full$mpdSigEco<- unlist(CI_ecoregions_significance_polygons_mpd)
polygon_data_full$mntdSigEco<- unlist(CI_ecoregions_significance_polygons_mntd)
#the output of this is 11792 units long. 
#this is now the updated list that has doubles for reserves that cover at least one ecoregion 


#1763 polygons have at least 2 ecoregions 
num_polygons_multiple_regions<- polygon_data_full%>%
  group_by(h3_index)%>%
  count() 

polygons_multiple_ecoregions<- num_polygons_multiple_regions%>%
  filter(n > 1)
  
#this is double the number of polygons multiple ecoregions. 
#produces 1 for each ecoregion. 
polygons_multiple_ecoregions_full<- st_as_sf(left_join(data.frame(polygons_multiple_ecoregions), data.frame(polygon_data_full), by = "h3_index"))

histogram(polygons_multiple_ecoregions_full$pd_cali_significance)


#just plot multiple ecoregion histogram 
multiple_ecoregions<- ggplot(num_polygons_multiple_regions, aes(x = n))+ 
  geom_histogram(bins = 50)+
  ggtitle("Hexagons with multiple ecoregions (n = number of ecoregions covered)")


ggsave(filename = "birds/images/Hexagons_with_multiple_ecoregions_histogram.png", 
       plot = multiple_ecoregions, 
       dpi = 300, 
       width = 8, 
       height = 6, 
       units = "in")


hist(polygons_multiple_ecoregions_full$pd_cali_significance)
#now plot frequency of significance within california of just these multiple ecoregion points. 

#seems like the hexagons that are at the boundaries of ecoregions have pretty similar behavior to hexagons within the ecoregions. 
#need to compare against a larger null model, though
#this might look very different for birds. extremely similar patters for hexes that lie on more than one ecoregion. 
multiple_ecoregions_hist<- ggplot(polygons_multiple_ecoregions_full, aes(x = mntd_cali_significance))+ 
  geom_histogram(bins = 50)+
  ggtitle("distribution of mntd significance for hexagons in multiple ecoregions")

ggsave(filename = "birds/images/distribution_mntd_significance_hexagons_multiple-ecoregions.png",
       plot = multiple_ecoregions_hist, 
       dpi = 300, 
       width = 8, 
       height = 6, 
       units = "in")


#for some reason this isn't working 

polygon_data_full<- st_as_sf(polygon_data_full)
data.frame(sapply(polygon_data_full,class))

#truncated some fields but this is now good to go. #also change extension to .geojson 
st_write(polygon_data_full, "birds/hex_data_sf/polygon_data.shp", delete_dsn = TRUE)

#need to write as well a json of polygons and the corresponding species lists. 


polygon_json_species<- data_frame("h3_index" = names(data), "species_in_tree" = hex_tree_stats_birds$species_in_tree, "species_absent_from_tree" = hex_tree_stats_birds$missing_species)

json_data_polygon_species <- jsonlite::toJSON(polygon_json_species)

writeLines(json_data_polygon_species, "birds/hexagon_species_in_tree.json")


##################
# AT THIS POINT I HAVE GENERATED AN SF DATA OBJECT OF ALL HEXES, PD/MPD/MNTD'S, TREE STATS AND SIGNIFICANCE, IN ADDITION TO A JSON FILE WITH THE SPECIES PRESENCE/ABSENCE VECTORS
##################



?st_write
################
##PART 3
#great. now this is written as a shape file and we can get all the ecoregion information. 
st_read("birds/hex_data_sf/polygon_data.shp")

pd_rds[1]
sizes[1]
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




###THIS WILL HAPPEN AFTER THE NULL MODELS ARE RERUN: TODO AFTER TRADER JOES 
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

sizes[2]
pd_cleaned[2]

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



result[669]
pd_rds[669]

#basically all thsee values are wack. might be something wrong with the hex trees generated
#since there are so many steps here. 
#this is ultimately not working at all. 
#I'm going to bed. 

hex_tree_stats_birds$pd_values[1]
for(i in 1:length(result))
{
  
  list_comp<- unlist(result[i])[1:15]
  if(unlist(hex_tree_stats_birds$pd_values)[i] < min(list_comp))
  {
    quartile_info_pd[i]<- 0 
  }
  else if(unlist(hex_tree_stats_birds$pd_values)[i] > max(list_comp))
  {
    print("true")
    quartile_info_pd[i]<- 100 
  }
  else
  {
    max_value<- max(list_comp[list_comp < unlist(hex_tree_stats_birds$pd_values)[i]])
    max_name <- names(list_comp)[which.max(list_comp == max_value)]
    quartile_info_pd[i] <- max_name
  }
}
names(quartile_info_pd)<- names(data)



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
=======
#there are 200 hexagons with missing data. 

<<<<<<< HEAD
>>>>>>> 5e4406aced4ad24e478144b470f9f69ad6c4ef08
=======

######################
#step 2: assign significance values to hexagon data
######################




#want to merge this back to the polygon sf object 
#use the "polygons" file generated in california-partition. 
#this is called(hex_data_as_sf in the Cali_Geometry file. )

cali_hexes_as_sf<- polygons

polygon_data_full_plants<- left_join(data.frame(cali_hexes_as_sf), pop_hex_stats_plants_df, by = "h3_index")

polygon_data_full_plants_with_eco<- left_join(polygon_data_full_plants, data.frame(hexes_with_ecoregions))


#ec_js_pd<- ecoregion_json_filename shortened. 
#could remove the NA values and merge them back in later (hexagons with no ecoregion)
polygon_data_full_plants_with_eco$ec_js_pd<- unlist(lapply(polygon_data_full_plants_with_eco$US_L3CODE, makejsonstring, clade = "Plants",  metric = "pd"))
polygon_data_full_plants_with_eco$ec_js_mpd<- unlist(lapply(polygon_data_full_plants_with_eco$US_L3CODE, makejsonstring, clade = "Plants", metric = "mpd"))
polygon_data_full_plants_with_eco$ec_js_mntd<- unlist(lapply(polygon_data_full_plants_with_eco$US_L3CODE, makejsonstring,clade = "Plants", metric = "mntd"))



#need to determine for each polygon in polygon_data_full if the pd is significant or not, and make a table. similar to what was done in the reserves. 
#need to make a function that generates a null model somehow concatenated from multiple ecoregions (idk how to do this. need to figure this out.)

#for now I will compare each polygon to california and its null model ecoregion, multiple ecoregions will be left out. 



####THIS WHOLE CHUNK OF CODE IS GENERATING SIGNIFICANCE VALUES. 

#unsure why everything is normal. we shall see.
#there are zero plant hexagons that have negative values for pd. wtf. 
polygon_data_CI_ranges_pd_cali_plants<- lapply(polygon_data_full_plants_with_eco$tree_size, cI_generator, params_json_file = "Plants/pd_model_params.json")
CI_cali_significance_polygons_pd_plants<- Map(check_significance_other_metrics, polygon_data_full_plants_with_eco$pd_values, upper_lower_keyvals = polygon_data_CI_ranges_pd_cali_plants)

png("Plants/images/CI_cali_significance_hexes_pd_hist.png", width = 800, height = 600, units = "px", res = 100)
histogram(unlist(CI_cali_significance_polygons_pd_plants), main = "CI_cali_significance_hexes_pd_hist", xlab = "PD", ylab = "Frequency")
dev.off()

polygon_data_CI_ranges_mpd_cali_plants<- lapply(polygon_data_full_plants_with_eco$tree_size, cI_generator, params_json_file = "Plants/mpd_model_params.json")
CI_cali_significance_polygons_mpd_plants<- Map(check_significance_other_metrics, polygon_data_full_plants_with_eco$mpd_values, upper_lower_keyvals = polygon_data_CI_ranges_mpd_cali_plants)


png("birds/images/CI_cali_significance_hexes_mpd_hist.png", width = 800, height = 600, units = "px", res = 100)
histogram(unlist(CI_cali_significance_polygons_mpd_plants), main = "CI_cali_significance_hexes_mpd_hist", xlab = "PD", ylab = "Frequency")
dev.off()

polygon_data_CI_ranges_mntd_cali_plants<- lapply(polygon_data_full_plants_with_eco$tree_size, cI_generator, params_json_file = "Plants/mntd_model_params.json")
CI_cali_significance_polygons_mntd_plants<- Map(check_significance_other_metrics, polygon_data_full_plants_with_eco$mntd_values, upper_lower_keyvals = polygon_data_CI_ranges_mntd_cali_plants)


png("birds/images/CI_cali_significance_hexes_mntd_hist.png", width = 800, height = 600, units = "px", res = 100)
histogram(unlist(CI_cali_significance_polygons_mntdd_plants), main = "CI_cali_significance_hexes_mntdd_hist", xlab = "PD", ylab = "Frequency")
dev.off()

#for some reason the mean nearest taxon distance is pretty much always insignificant 


#need to generate the json file names for each ecoregion. 
#double check that ecoregions are in the correct order. might be wrong null models. 
#for ecoregions: 
polygon_data_CI_ranges_pd_ecoregions_plants <- Map(cI_generator, polygon_data_full_plants_with_eco$tree_size, params_json_file = polygon_data_full_plants_with_eco$ec_js_pd)
CI_ecoregions_significance_polygons_pd_plants<- Map(check_significance_other_metrics, polygon_data_full_plants_with_eco$pd_values, upper_lower_keyvals = polygon_data_CI_ranges_pd_ecoregions_plants)

png("birds/images/CI_cali_significance_hexes_pd_hist_ecoregions.png", width = 800, height = 600, units = "px", res = 100)
histogram(unlist(CI_ecoregions_significance_polygons_pd_plants), main = "CI_cali_significance_hexes_pd_hist", xlab = "PD", ylab = "Frequency")
dev.off()


polygon_data_CI_ranges_mpd_ecoregions_plants <- Map(cI_generator, polygon_data_full_plants_with_eco$tree_size, params_json_file = polygon_data_full_plants_with_eco$ec_js_mpd)
CI_ecoregions_significance_polygons_mpd_plants<- Map(check_significance_other_metrics, polygon_data_full_plants_with_eco$mpd_values, upper_lower_keyvals = polygon_data_CI_ranges_mpd_ecoregions_plants)

#check them against their own ecoregions. 
png("birds/images/CI_cali_significance_hexes_mpd_hist_ecoregions.png", width = 800, height = 600, units = "px", res = 100)
histogram(unlist(CI_ecoregions_significance_polygons_mpd_plants), main = "CI_cali_significance_hexes_mpd_hist", xlab = "mpd", ylab = "Frequency")
dev.off()


polygon_data_CI_ranges_mntd_ecoregions_plants <- Map(cI_generator, polygon_data_full_plants_with_eco$tree_size, params_json_file = polygon_data_full_plants_with_eco$ec_js_mntd)
CI_ecoregions_significance_polygons_mntd_plants<- Map(check_significance_other_metrics, polygon_data_full_plants_with_eco$mntd_values, upper_lower_keyvals = polygon_data_CI_ranges_mntd_ecoregions_plants)

png("birds/images/CI_cali_significance_hexes_mntd_hist_ecoregions.png", width = 800, height = 600, units = "px", res = 100)
histogram(unlist(CI_ecoregions_significance_polygons_mntd_plants), main = "CI_cali_significance_hexes_mntd_hist", xlab = "mntd", ylab = "Frequency")
dev.off()


###ADD ALL THESE SIGNIFICANCE VALUES TO THE DATA STRUCTURE: 

#for some reason there's a dimensionality issue. 
#need to figure out how to deal with polygons on multuiple ecoregions stat!!
#for some reason unlist gets rid of null. don't want that. 
polygon_data_full_plants_with_eco$pdSigCal<- unlist(CI_cali_significance_polygons_pd_plants)
polygon_data_full_plants_with_eco$mpdSigCal<- unlist(CI_cali_significance_polygons_mpd_plants)
polygon_data_full_plants_with_eco$mntdSigCal<- unlist(CI_cali_significance_polygons_mntd_plants)


#need to change NULL to NA to make this datastructure work. 
#make NA's. line = 
#CI_cali_significance_polygons_pd_plants[sapply(CI_cali_significance_polygons_pd_plants, is.null)] <- NA


polygon_data_full_plants_with_eco$pdSigEco<- unlist(CI_ecoregions_significance_polygons_pd_plants)
polygon_data_full_plants_with_eco$mpdSigEco<- unlist(CI_ecoregions_significance_polygons_mpd_plants)
polygon_data_full_plants_with_eco$mntdSigEco<- unlist(CI_ecoregions_significance_polygons_mntd_plants)
#the output of this is 11792 units long. 
#this is now the updated list that has doubles for reserves that cover at least one ecoregion 


#1763 polygons have at least 2 ecoregions 
num_polygons_multiple_regions<- polygon_data_full_plants_with_eco%>%
  group_by(h3_index)%>%
  count() 

polygons_multiple_ecoregions<- num_polygons_multiple_regions%>%
  filter(n > 1)

#this is double the number of polygons multiple ecoregions. 
#produces 1 for each ecoregion. 
polygons_multiple_ecoregions_full<- st_as_sf(left_join(data.frame(polygons_multiple_ecoregions), data.frame(polygon_data_full), by = "h3_index"))

histogram(polygons_multiple_ecoregions_full$pd_cali_significance)


#just plot multiple ecoregion histogram 
multiple_ecoregions<- ggplot(num_polygons_multiple_regions, aes(x = n))+ 
  geom_histogram(bins = 50)+
  ggtitle("Hexagons with multiple ecoregions (n = number of ecoregions covered)")


ggsave(filename = "birds/images/Hexagons_with_multiple_ecoregions_histogram.png", 
       plot = multiple_ecoregions, 
       dpi = 300, 
       width = 8, 
       height = 6, 
       units = "in")


hist(polygons_multiple_ecoregions_full$pd_cali_significance)
#now plot frequency of significance within california of just these multiple ecoregion points. 

#seems like the hexagons that are at the boundaries of ecoregions have pretty similar behavior to hexagons within the ecoregions. 
#need to compare against a larger null model, though
#this might look very different for birds. extremely similar patters for hexes that lie on more than one ecoregion. 
multiple_ecoregions_hist<- ggplot(polygons_multiple_ecoregions_full, aes(x = mntd_cali_significance))+ 
  geom_histogram(bins = 50)+
  ggtitle("distribution of mntd significance for hexagons in multiple ecoregions")

ggsave(filename = "birds/images/distribution_mntd_significance_hexagons_multiple-ecoregions.png",
       plot = multiple_ecoregions_hist, 
       dpi = 300, 
       width = 8, 
       height = 6, 
       units = "in")


#for some reason this isn't working 

polygon_data_full_plants_with_eco<- st_as_sf(polygon_data_full_plants_with_eco)



#want to make a plot 
# Assuming you have already loaded the required libraries, such as sf and ggplot2

library(tidyr)
polygon_full_plants_with_eco_nona<- drop_na(polygon_data_full_plants_with_eco)
#another attempt to plot 
# Convert NA values to a specific label

# Convert pdSigCal to factor

# 3. Color the hexagons based on pdSigCal values

plot_area <- ggplot() +
  theme_void() +  # Remove default axes and background
  coord_equal()    # Ensure equal aspect ratio


plot_area <- plot_area +
  geom_sf(data = polygon_full_plants_with_eco_nona, aes(fill = factor(mntdSigEco)))
# Customize the legend and color scale



plot_area_fin <- plot_area +
  scale_fill_manual(values = c("-1" = "blue", "0" = "white", "1" = "red"),
                    name = "mntdSigEco",
                    labels = c("-1", "0", "1"),
                    guide = "legend")

ggsave("Plants/ecoregion_mntd_distribution.png", plot_area_fin, width = 10, height = 10, dpi = 300)

dev.off()


unique(polygon_data_full_plants_with_eco$pdSigCal)
# 2. Plot the hexagons
plot_area <- plot_area +
  geom_sf(data = polygon_data_full_plants_with_eco, aes(fill = pdSigCal))

# Customize the legend and color scale
plot_area <- plot_area +
  scale_fill_manual(values = c("-1" = "blue", "0" = "white", "1" = "red"),
                    name = "pdSigCal",
                    labels = c("-1" = "-1", "0" = "0", "1" = "1"))

# Display the plot
plot_area

#truncated some fields but this is now good to go. #also change extension to .geojson 
st_write(polygon_data_full_plants_with_eco, "Plants/hex_data_sf/polygon_data.shp", delete_dsn = TRUE)

#need to write as well a json of polygons and the corresponding species lists. 


polygon_json_species<- data_frame("h3_index" = names(data), "species_in_tree" = hex_tree_stats_birds$species_in_tree, "species_absent_from_tree" = hex_tree_stats_birds$missing_species)

json_data_polygon_species <- jsonlite::toJSON(polygon_json_species)

writeLines(json_data_polygon_species, "birds/hexagon_species_in_tree.json")

plot(genus_complete_phylogeny, type = "fan", show.tip.label = FALSE)

plottype = "fan", show.tip.label = FALSE)



>>>>>>> 5c21e0a7d2ee6bec8c2b1bf8702d76a1ebdc35f5
