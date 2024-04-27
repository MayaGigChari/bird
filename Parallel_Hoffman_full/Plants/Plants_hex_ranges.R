##This file is located in PLANTS
#This file takes the hexagonal information from the h3 library and the california shape file
#from Cali_goemetry, and parses california into indexes
#this is a script for getting species lists for all the hexagons within california and within respective ecoregions! fucking hell. 
#in this script we basically just want to get a list of all hexagons and associated species lists of plants. 


#for each hexagon: 
#join the hexagon with the ecoregion shape file (located somewhere or another in some file)
#count the unique species. This should actually work nicely for hexagons that overlap multiple ecoregions as well. 

#the geometry asociated with indexes are then used to creates species list and trees for birds based on pre-defined range
#data from range_gen. 
library(h3)
library(sf)
library(dplyr)
library(rlist)

<<<<<<< HEAD
current_directory<- getwd()
tree_trimming_path<-file.path(current_directory, "App_functions_bird.R")
=======
#load dependent scripts
tree_trimming_path<-file.path(current_directory, "App_functions_bird.R")


>>>>>>> 5e4406aced4ad24e478144b470f9f69ad6c4ef08
source(tree_trimming_path)


#generate geometry for california 
california <- sf::read_sf("Cali_Geometry/ca-state-boundary/CA_State_TIGER2016.shp") %>%
  sf::st_transform(4326)
h3_indexes <- polyfill(california[1, ], res = 6)
polygons <- h3_to_geo_boundary_sf(h3_indexes)


#get geometry for ecoregions 
ecoregions<- st_read("Cali_geometry/ca_eco_l3", packlage = "sf")
ecoregions<- st_transform(ecoregions, "WGS84")
ecoregions_L3codes<- ecoregions$US_L3CODE

<<<<<<< HEAD
#now want to intersect all polygons with all the data for everything (all species ranges within ecoregions?)



=======
plot(ecoregions[5,])
#perhaps the first step is to get the ecoregion id for each geometry. Not sure if this is actually a useful first step thoug. 
>>>>>>> 5e4406aced4ad24e478144b470f9f69ad6c4ef08

#some polygons don't have ecoregions. 
#geometry is still polygons. 
hexes_with_ecoregions<- st_join(polygons, ecoregions) %>%
  select(US_L3CODE, h3_index)

#load the complete plant data (filtered based on ecoregions that should actually be included.)
#these plant ranges filtered by ecoregion shoudl do the job...
Plant_ranges_touse<- st_read("Plants/plant_complete_species_lists_by_ecoregion.shp")

#these are the ranges of all native plants. say we join on these 
Plant_ranges_allnatives<- st_read("Plants/California_species_shapes")

st_write(hexes_and_all_natives, "Plants/Hexagons_total_range_data.shp", append = FALSE)

#this gives us all the hexagon information. 
#this join started at 5:27 pm. 
hexes_and_all_natives<- st_join(polygons, Plant_ranges_allnatives)

hexes_and_all_natives<- st_read("Plants/Hexagons_total_range_data.dbf")

#if we do this: for each hexagon, we can generate a list of all overlapping species and also generate a list of all species that can 
#possibly be in the ecoregion, and remove the edges/anomolies. 

#try to do a super fast join: just the first hexagon and the first ecoregion 

#going to do a massive join on hexes_with_ecoregions and plant_ranges_touse. 

#hex_overlapping_range_complete_data<- st_join(hexes_with_ecoregions, Plant_ranges_touse) #this should work but maybe it doesn't

Plant_ranges_eco1<- st_read("Plants/ecoregion_data/1/ecoregion.shp")



<<<<<<< HEAD
#load data for intersection: make a function to do this. 
=======

#shouldn't these ranges be truncated to their particular ecoregions?? yes, they are. 
>>>>>>> 5e4406aced4ad24e478144b470f9f69ad6c4ef08

#this will probably take a ridiculously long time. 
#started at 12:29 pm 

#need to figure this out. 
temp_trial<- (hexes_with_ecoregions %>% filter(US_L3CODE == 1))[25,]
p<- ggplot()+ 
  geom_sf(data = temp_trial)+
  geom_sf(data = ecoregions[1,])+
  geom_sf(data = temp_trial[25,], aes(fill = "red"))


test<- st_join(temp_trial, Plant_ranges_eco1)


library(ggplot2)
p<- ggplot() +
  geom_sf(data = temp_trial[1], aes(fill = US_L3CODE)) +
  geom_sf(data = ecoregions[1,])


#somehow these are all..points
#this also needs to be edited. TODO next.
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
    poly_species<- unique(poly_temp_db$species)
    hex_species[i] <- list(poly_species)
    print(indexes[i])
  }
  print("hexRaw done")
  return(hex_species)
}


#for california birds. 
#hopefully this is now right. 

#this does NOT have hexagons as names yet. must assume a common order. 
cali_plants_unfiltered_hex_list<- genHexRaw_prejoined(hexes_and_all_natives, h3_indexes)
#this generates a list of plants that are in each hex but does not account for range edge issue. 


#need to get the ecoregion that each hex is in, compare that ecoregion potential species list to the hexagon list and then remove the species that just don't belong.
#large_ecoregion_matrix is the matrix that has all the plants and all their ecoregions. 
#this data is stored in ecoregions_plant_data_intersection_cond2
#indexes should be a list of h3_indexes

#try analysis with this and see what it does 
remove_nonRepSpec<- function(unfiltered_hex_list, large_ecoregion_matrix, indexes)
{
  true_species_list<- list()
  artifact_species_list<- list()
  for(i in 1: length(indexes))
  {
    tuple_temp<- hexes_with_ecoregions %>%
      filter(h3_index == indexes[i])
    #get the ecoregion of that index. 
    eco_assign<- as.integer((data.frame(tuple_temp))$US_L3CODE)
    #print(eco_assign)
    #get the unique species associated with that ecoregion 
    eco_temp<- large_ecoregion_matrix %>%
      filter(US_L3CODE == eco_assign)
    #print(eco_temp)
    eco_true_species<- unique(eco_temp$species)

    true_species_list_temp<- intersect(unlist(unfiltered_hex_list[i]), unlist(eco_true_species))
    #check difference between expected species and true species.  
    #list the number of species that are removed.
    artifact_species_list_temp<- setdiff(unlist(unfiltered_hex_list[i]), true_species_list_temp )
    #print(artifact_species_list_temp)
    
    #this artifact_species_list might not be correct. 
    true_species_list[i]<- list(true_species_list_temp)
    artifact_species_list[i]<- list(artifact_species_list_temp)
    #print(artifact_species_list[i])
    print(i)

  }
  list_return<- list(true_species = true_species_list, artifact_species = artifact_species_list)
  return(list_return)
}


true_species_lists<- remove_nonRepSpec(cali_plants_unfiltered_hex_list, Plant_ranges_touse, h3_indexes)

#we also have unfiltered_hex_list to work with. 
true_species_hexagons_plants<- true_species_lists$true_species
names(true_species_hexagons_plants)<- h3_indexes

helper_genus<- function(species_list)
{
  genus<- sapply(strsplit(species_list,"_"), `[`, 1)
  return(unique(genus))
}

true_genera_hexagons_plants<- lapply(true_species_hexagons_plants, helper_genus)

saveRDS(true_species_hexagons_plants, file = "Plants/hexagon_true_species_plant_data")

saveRDS(true_genera_hexagons_plants, file = "Plants/hexagon_true_genera_plant_data")
#save as an rds file to use in the next step. 

#now I need to determine the significance, in a similar way to  what was done in birds

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



