
#population hexagons for mammals
packages_to_install <- c("picante", "ape",  "dplyr", "phytools", "jsonlite", "tools", "stringr", "sf", "h3")

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

clade = "Mammals"

tree_trimming_path<-file.path(current_directory, "App_functions_bird.R")

source(tree_trimming_path)


#issue with the cophenetic matrix. 

#reading in the cophenetic matrix 
#can toggle this between species data and genus data. 
cophen_mammals<-readRDS("Mammals/cali_species_cophen_matrix")
#parent_tree<- read.tree("Plants/interpolated_full_tree.tre") probably don't really need this.
#made the genus tree in main. 
cali_tree_mammals<- read.tree("Mammals/cali_species_tree.tre") #load the genus tree
cali_species_ranges<- st_read("Mammals/mammal_complete_species_lists_by_ecoregion.shp")

#step 1 here: need to form hexagon joined data with the species_in_california data 
#nede to join with the hexes with ecoregions data. 

california <- sf::read_sf("Cali_Geometry/ca-state-boundary/CA_State_TIGER2016.shp") %>%
  sf::st_transform(4326)
h3_indexes <- polyfill(california[1, ], res = 6)
h3_indexes[1]
polygons <- h3_to_geo_boundary_sf(h3_indexes)
polygons[1,]

#get geometry for ecoregions 
ecoregions<- st_read("Cali_geometry/ca_eco_l3", packlage = "sf")
ecoregions<- st_transform(ecoregions, "WGS84")
ecoregions_L3codes<- ecoregions$US_L3CODE

hexes_with_ecoregions<- st_join(polygons, ecoregions) %>%
  dplyr::select(US_L3CODE, h3_index)

st_write(hexes_with_ecoregions, "Cali_Geometry/hexes_with_ecoregions_joined.shp")

#now I can just read in this ecoregions. 


#join the hexes with ecoregions to the species range data
#don't actually need to join with the ecoregion-labeled polygon data because that just creates redundancy in the dataset. 

hex_species_ranges<- st_join(polygons, cali_species_ranges)



st_write(hex_species_ranges, "Mammals/full_hex_intersect_ranges_joined.shp", append = FALSE)


genHexRaw_prejoined<- function(hex_range_joined_data, indexes) #the indexes will be h3_indexes usually 
{
  #want to select just the species and the index. But can do this later. 
  #should always have an h3_index field. This is necessary for the filter statement 
  #joining the species that could exist in that area with the polygons in that area! I think this might work... 
  hex_species<- list()
  #need to iterate through hexes. not i's 
  for(i in 1:length(indexes))
  {
    poly_temp_db<- hex_range_joined_data %>%
      filter(h3_index == indexes[i])
    poly_species<- unique(poly_temp_db$sci_name)
    hex_species[i] <- list(poly_species)
    print(indexes[i])
  }
  print("hexRaw done")
  return(hex_species)
}

Mammal_hex_species_list<- genHexRaw_prejoined(hex_species_ranges, h3_indexes)

names(Mammal_hex_species_list)<- h3_indexes



poly_labels<- names(Mammal_hex_species_list) #this gives us all the codes. 

#run this function with the most recent polygon data 

#there should be some kind of difference with pd no? maybe not.
#maybe I should have populated the hexes with some other parent/cophenetic tree??
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

#the hex_tree_stats_birds is the raw output
hex_tree_stats_mammals<- popHexStats(Mammal_hex_species_list, cali_tree_mammals, "Mammals", cophen_filepath = "Mammals/cali_species_cophen_matrix")

hex_tree_stats_plants_copy_mammals<- hex_tree_stats_mammals

pop_hex_stats_mammals_df<- data_frame("h3_index" = names(Mammal_hex_species_list), "pd_values" = unlist(hex_tree_stats_mammals$pd_values), "mpd_values" = unlist(hex_tree_stats_mammals$mpd_values), "mntd_values" = unlist(hex_tree_stats_mammals$mntd_values), 
                                     "tree_size" = unlist(lapply(hex_tree_stats_mammals$species_in_tree, length)), "missing_taxa_tree_size" = unlist(lapply(hex_tree_stats_mammals$missing_species, length)) )

#seems that there are generally about 40% of taxa missing. this might fuck everything up but who knows. 
pop_hex_stats_mammals_df$proportion_missing<- pop_hex_stats_mammals_df$missing_taxa_tree_size/(pop_hex_stats_mammals_df$tree_size + pop_hex_stats_mammals_df$missing_taxa_tree_size)


histogram(pop_hex_stats_mammals_df$proportion_missing)
#this is just an aside to make a figure of mssing taxa. 

#need to make 

library(ggplot2)
missing_hist<- ggplot(pop_hex_stats_mammals_df, aes(x = proportion_missing)) +
  geom_histogram(bins = 50) +
  ggtitle("Missing Taxa Proportions: Hexagon Data California")

ggsave(filename = "Mammals/Missing_taxa_proportions_hexagon_california_species.png", 
       plot = missing_hist, 
       dpi = 300, 
       width = 8, 
       height = 6, 
       units = "in")

#this just saves the file hex_tree_stats_birds without any geographic information. 
saveRDS(pop_hex_stats_mammals_df, file = "Mammals/raw_hex_stats_from_ranges")

######################
#step 2: assign significance values to hexagon data
######################

#want to merge this back to the polygon sf object 
#use the "polygons" file generated in california-partition. 
#this is called(hex_data_as_sf in the Cali_Geometry file. )

cali_hexes_as_sf<- polygons

polygon_data_full_mammals<- left_join(data.frame(cali_hexes_as_sf), pop_hex_stats_mammals_df, by = "h3_index")

polygon_data_full_mammals_with_eco<- left_join(polygon_data_full_mammals, data.frame(hexes_with_ecoregions))


#ec_js_pd<- ecoregion_json_filename shortened. 
#could remove the NA values and merge them back in later (hexagons with no ecoregion)

#was this possibly using birds? no way at all. 
polygon_data_full_mammals_with_eco$ec_js_pd<- unlist(lapply(polygon_data_full_mammals_with_eco$US_L3CODE, makejsonstring, clade = "Mammals",  metric = "pd"))
polygon_data_full_mammals_with_eco$ec_js_mpd<- unlist(lapply(polygon_data_full_mammals_with_eco$US_L3CODE, makejsonstring, clade = "Mammals", metric = "mpd"))
polygon_data_full_mammals_with_eco$ec_js_mntd<- unlist(lapply(polygon_data_full_mammals_with_eco$US_L3CODE, makejsonstring,clade = "Mammals", metric = "mntd"))



#need to determine for each polygon in polygon_data_full if the pd is significant or not, and make a table. similar to what was done in the reserves. 
#need to make a function that generates a null model somehow concatenated from multiple ecoregions (idk how to do this. need to figure this out.)

#for now I will compare each polygon to california and its null model ecoregion, multiple ecoregions will be left out. 



####THIS WHOLE CHUNK OF CODE IS GENERATING SIGNIFICANCE VALUES. 

#unsure why everything is normal. we shall see.
#there are zero plant hexagons that have negative values for pd. wtf. 

#perhaps there is an issue with the CI generation?


polygon_data_CI_ranges_pd_cali_mammals<- lapply(polygon_data_full_mammals_with_eco$tree_size, cI_generator, params_json_file = "Mammals/pd_model_params.json")
CI_cali_significance_polygons_pd_mammals<- Map(check_significance_other_metrics, polygon_data_full_mammals_with_eco$pd_values, upper_lower_keyvals = polygon_data_CI_ranges_pd_cali_mammals)
#everything for mammals is significantly negative? 

png("Mammals/images/CI_cali_significance_hexes_pd_hist.png", width = 800, height = 600, units = "px", res = 100)
histogram(unlist(CI_cali_significance_polygons_pd_mammals), main = "CI_cali_significance_hexes_pd_hist", xlab = "PD", ylab = "Frequency")
dev.off()

polygon_data_CI_ranges_mpd_cali_mammals<- lapply(polygon_data_full_mammals_with_eco$tree_size, cI_generator, params_json_file = "Mammals/mpd_model_params.json")
CI_cali_significance_polygons_mpd_mammals<- Map(check_significance_other_metrics, polygon_data_full_mammals_with_eco$mpd_values, upper_lower_keyvals = polygon_data_CI_ranges_mpd_cali_mammals)


png("Mammals/images/CI_cali_significance_hexes_mpd_hist.png", width = 800, height = 600, units = "px", res = 100)
histogram(unlist(CI_cali_significance_polygons_mpd_mammals), main = "CI_cali_significance_hexes_mpd_hist", xlab = "PD", ylab = "Frequency")
dev.off()

polygon_data_CI_ranges_mntd_cali_mammals<- lapply(polygon_data_full_mammals_with_eco$tree_size, cI_generator, params_json_file = "Mammals/mntd_model_params.json")
CI_cali_significance_polygons_mntd_mammals<- Map(check_significance_other_metrics, polygon_data_full_mammals_with_eco$mntd_values, upper_lower_keyvals = polygon_data_CI_ranges_mntd_cali_mammals)


png("Mammals/images/CI_cali_significance_hexes_mntd_hist.png", width = 800, height = 600, units = "px", res = 100)
histogram(unlist(CI_cali_significance_polygons_mntd_mammals), main = "CI_cali_significance_hexes_mntdd_hist", xlab = "PD", ylab = "Frequency")
dev.off()

#for some reason the mean nearest taxon distance is pretty much always insignificant 


#need to generate the json file names for each ecoregion. 
#double check that ecoregions are in the correct order. might be wrong null models. 
#for ecoregions: 
polygon_data_CI_ranges_pd_ecoregions_mammals <- Map(cI_generator, polygon_data_full_mammals_with_eco$tree_size, params_json_file = polygon_data_full_mammals_with_eco$ec_js_pd)
CI_ecoregions_significance_polygons_pd_mammals<- Map(check_significance_other_metrics, polygon_data_full_mammals_with_eco$pd_values, upper_lower_keyvals = polygon_data_CI_ranges_pd_ecoregions_mammals)



#everything somehow becomes significantly negative relative to the ecoregion? not sure how this makes any sense. 
png("Mammals/images/CI_cali_significance_hexes_pd_hist_ecoregions.png", width = 800, height = 600, units = "px", res = 100)
histogram(unlist(CI_ecoregions_significance_polygons_pd_mammals), main = "CI_cali_significance_hexes_pd_hist", xlab = "PD", ylab = "Frequency")
dev.off()


polygon_data_CI_ranges_mpd_ecoregions_mammals <- Map(cI_generator, polygon_data_full_mammals_with_eco$tree_size, params_json_file = polygon_data_full_mammals_with_eco$ec_js_mpd)
CI_ecoregions_significance_polygons_mpd_mammals<- Map(check_significance_other_metrics, polygon_data_full_mammals_with_eco$mpd_values, upper_lower_keyvals = polygon_data_CI_ranges_mpd_ecoregions_mammals)

#check them against their own ecoregions. 
png("Mammals/images/CI_cali_significance_hexes_mpd_hist_ecoregions.png", width = 800, height = 600, units = "px", res = 100)
histogram(unlist(CI_ecoregions_significance_polygons_mpd_mammals), main = "CI_cali_significance_hexes_mpd_hist", xlab = "mpd", ylab = "Frequency")
dev.off()


polygon_data_CI_ranges_mntd_ecoregions_mammals <- Map(cI_generator, polygon_data_full_mammals_with_eco$tree_size, params_json_file = polygon_data_full_mammals_with_eco$ec_js_mntd)
CI_ecoregions_significance_polygons_mntd_mammals<- Map(check_significance_other_metrics, polygon_data_full_mammals_with_eco$mntd_values, upper_lower_keyvals = polygon_data_CI_ranges_mntd_ecoregions_mammals)

png("Mammals/images/CI_cali_significance_hexes_mntd_hist_ecoregions.png", width = 800, height = 600, units = "px", res = 100)
histogram(unlist(CI_ecoregions_significance_polygons_mntd_mammals), main = "CI_cali_significance_hexes_mntd_hist", xlab = "mntd", ylab = "Frequency")
dev.off()


###ADD ALL THESE SIGNIFICANCE VALUES TO THE DATA STRUCTURE: 

#for some reason there's a dimensionality issue. 
#need to figure out how to deal with polygons on multuiple ecoregions stat!!
#for some reason unlist gets rid of null. don't want that. 
polygon_data_full_mammals_with_eco$pdSigCal<- unlist(CI_cali_significance_polygons_pd_mammals)
polygon_data_full_mammals_with_eco$mpdSigCal<- unlist(CI_cali_significance_polygons_mpd_mammals)
polygon_data_full_mammals_with_eco$mntdSigCal<- unlist(CI_cali_significance_polygons_mntd_mammals)


#need to change NULL to NA to make this datastructure work. 
#make NA's. line = 
CI_cali_significance_polygons_pd_mammals[sapply(CI_cali_significance_polygons_pd_mammals, is.null)] <- NA
CI_cali_significance_polygons_mpd_mammals[sapply(CI_cali_significance_polygons_mpd_mammals, is.null)] <- NA
CI_cali_significance_polygons_mntd_mammals[sapply(CI_cali_significance_polygons_mntd_mammals, is.null)] <- NA

CI_ecoregions_significance_polygons_pd_mammals[sapply(CI_ecoregions_significance_polygons_pd_mammals, is.null)] <- NA
CI_ecoregions_significance_polygons_mpd_mammals[sapply(CI_ecoregions_significance_polygons_mpd_mammals, is.null)] <- NA
CI_ecoregions_significance_polygons_mntd_mammals[sapply(CI_ecoregions_significance_polygons_mntd_mammals, is.null)] <- NA


polygon_data_full_mammals_with_eco$pdSigEco<- unlist(CI_ecoregions_significance_polygons_pd_mammals)
polygon_data_full_mammals_with_eco$mpdSigEco<- unlist(CI_ecoregions_significance_polygons_mpd_mammals)
polygon_data_full_mammals_with_eco$mntdSigEco<- unlist(CI_ecoregions_significance_polygons_mntd_mammals)
#the output of this is 11792 units long. 
#this is now the updated list that has doubles for reserves that cover at least one ecoregion 

#this might be ab issue, never really actually saved any of this stuff. 




#want to make a plot 
# Assuming you have already loaded the required libraries, such as sf and ggplot2

library(tidyr)
polygon_full_mammals_with_eco_nona<- st_as_sf(drop_na(polygon_data_full_mammals_with_eco))
#another attempt to plot 
# Convert NA values to a specific label

# Convert pdSigCal to factor

# 3. Color the hexagons based on pdSigCal values
# need to make a function that generates all these figures instead of manually doing it all the time. 
library(ggplot2)
plot_area <- ggplot() +
  theme_void() +  # Remove default axes and background
  coord_equal()    # Ensure equal aspect ratio


plot_area <- plot_area +
  geom_sf(data = polygon_full_mammals_with_eco_nona, aes(fill = factor(mntdSigEco)))
# Customize the legend and color scale


#we see significant clustering. 
plot_area_fin <- plot_area +
  scale_fill_manual(values = c("-1" = "blue", "0" = "white", "1"= "red"),
                    name = "Significance",
                    labels = c("-1","0", "1"),
                    guide = "legend")

ggsave("Mammals/Ecoregion_species_level_mntd_distribution.png", plot_area_fin, width = 10, height = 10, dpi = 300)

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
#has hexagon data with NA's. 
st_write(polygon_data_full_plants_with_eco, "Plants/species_level_polygon_data.shp", delete_dsn = TRUE)


st_read("Plants/species_level_polygon_data.shp")
#need to write as well a json of polygons and the corresponding species lists. 


polygon_json_species<- data_frame("h3_index" = names(data), "species_in_tree" = hex_tree_stats_birds$species_in_tree, "species_absent_from_tree" = hex_tree_stats_birds$missing_species)

json_data_polygon_species <- jsonlite::toJSON(polygon_json_species)

writeLines(json_data_polygon_species, "birds/hexagon_species_in_tree.json")

plot(genus_complete_phylogeny, type = "fan", show.tip.label = TRUE, hex = 0.5)


#below I will try to draw the phylogeny 
library(dplyr)
visualize_sample(unlist(data_plants[1]), cali_tree_plants)


library(ggtree)
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("ggtree")
# Create a ggtree object
ggtree_obj <- ggtree(cali_tree_plants)

# Filter the tree to retain only the tip labels you want to show
ggtree_obj_filtered <- ggtree_obj %>% 
  geom_tiplab(align = TRUE)

# Plot the filtered tree with labeled tip labels
print(ggtree_obj_filtered)
