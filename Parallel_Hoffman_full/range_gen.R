
#need to generalize this. right now just for plant.s 
#install.packages("sf")
#devtools::install_github("crazycapivara/h3-r")
#right now this is just a range generator for plants. 


library(sf)
library(tibble)
library(geojsonsf)
library(h3)
library(dplyr)
library(stringr)
library(jsonlite)
# Read the GeoJSON file
#this is for the azathoth rstudio remote server


#for range data




# Validate GeoJSON file
geojson_file <- "bien_ranges.geojson"
bird_ranges <- st_read(geojson_file)

#this originally evaluated to false 
validity <- st_is_valid(bird_ranges)

#need to make all the bird ranges valid before running more things.
bird_ranges <- st_make_valid(bird_ranges)


plot(st_geometry(mono_litto))

#these are all valid now. 


st_crs(bird_ranges) <- st_crs(4326) 

#bounding box for california and polygon type in sf package
#california_bbox <- st_bbox(c(xmin = -124.41060660766607, xmax = -114.13445790587905, ymin = 32.5342307609976, ymax = 42.00965914828148))
#california_bbox_polygon <- st_as_sfc(st_bbox(california_bbox), crs = st_crs(bird_ranges))

#read california shape file 
california <- sf::read_sf("Cali_Geometry/ca-state-boundary/CA_State_TIGER2016.shp") %>%
  sf::st_transform(4326)

# filters geojson for only those that intersect with bounding box or califrornia shape file
#this is misleading, it's actually plants I just mislabeled everything as birds. 
birds_in_california <- st_intersection(bird_ranges, california) 


#use this to populate hex data later
#plants_range_california has all these species
plants_range_california<- birds_in_calfornia

#lupinus_onustus doesn't exist in the range data, for example but it is still a california species! also "Boschniakia strobilacea"
#however, something like Allium_rhizomatum is expected but not seen, but doesn't even have a range in california!
plants_range_california%>%
  filter(species == "Monanthochloe_littoralis")


#writing to a st file or something
st_write(birds_in_california, "Plants/california_plants.shp")

#this doesn't work. need to figure it out. 
sf_geojson(birds_in_california, "Plants/california_plants.geojson")

write.csv(birds_in_california$species, "Plants/california_plant_species_list.csv ")


#written the csv. can now pull this csv onto local.

#now need to get all the plants that intersect with each hexcode 


#get the whole plant observation dataset
#this is a huge file so it might not actua.ly be possible to read it in 

#plants in cali is the occurrence dataset of plants seen in california. 
#these are ALL THE SEEN PLANTS!
plants_in_cali<- st_read("Plants/bien_occurrence_california.geojson")


#these might be mostly empty. 
plants_in_cali$geometry

#drop empty geometries. 
geog_plant_observations = plants_in_cali[!st_is_empty(plants_in_cali),,drop=FALSE]

#not sure 
geog_plant_observations$is_cultivated_observation

geog_plant_observations%>%
  filter(scrubbed_species_binomial == "Allium tribracteatum")

#index california in the same way. 
#each index should be associated with some kind of geometry 
h3_indexes <- polyfill(california[1, ], res = 6)
polygons <- h3_to_geo_boundary_sf(h3_indexes)


#popPixel is redundant in California_partition 
#need to get 

#list for each hexagon
taxon_ids <- list()
# Iterate through each polygon


#don't need to do a groupby. 
#now going to run this with the full data. 

#I think this is done with the full data. 

#do a join on st_within, whatever that means. 
#redo this with more coarse select statements. 
#maybe the joined observational data should not be trimmed until later.

#need to do this join for the 
joined_data_observations<-st_join (geog_plant_observations, polygons, join = st_within)%>% 
  filter(is.na(h3_index) == FALSE)


#there are 44927 specemins that are cultivated 
num_observed_cultivars<- sum(joined_data_observations$is_cultivated_observation, na.rm=TRUE)

#there are 29326 taxon that are cultivated 
num_probable_cultivars<- sum(joined_data_observations$is_cultivated_taxon, na.rm = TRUE)

#need to remove all the observations of taxa that are cultivated and that are generally cultivated. 

#finish this join and determine whether I will keep or exclude cultivar.s 

#this is a subset for ONLY NATIVE PLANTS pretty much 
joined_data_subset<- joined_data_observations%>%
  filter(is_introduced == 0, is_cultivated_in_region == 0, is_cultivated_observation != 1)

#this is a subset for native and introduced plants but no cultivars
joined_data_subset_with_introduced<- joined_data_observations%>%
  filter(is_cultivated_in_region == 0, is_cultivated_observation != 1, is_cultivated_taxon != 1)

#checking particular species 



hex_species_plants<- list()
for(i in 1: length(h3_indexes))
{
  poly_temp_db<- joined_data_observations %>%
    filter(h3_index == h3_indexes[i])
  poly_species<- unique(poly_temp_db$scrubbed_taxon_name_no_author)
  hex_species_plants[i] <- list(poly_species)
}

length(hex_species_plants)
names(hex_species_plants)<- h3_indexes

saveRDS(hex_species_plants, file = "Plants/occurrence_plants_polygons.rds")
#also has taxonomic status data!
#hex_species_plants

#need to do the same thing as above but for range_plants_polygons.rds
#joined data is for ranges, joined_data observations is for observations
joined_data<-st_join (birds_in_california, polygons, join = st_intersects)%>% 
  select(h3_index,species)

species_data<- unique(joined_data$species)
write.csv(species_data, "Plants/full_species_range_list.csv")
genus_data<- unique(word(species_data, 1, sep = "_"))
write.csv(genus_data, "Plants/full_genus_range_list.csv")

#do this in a better way such that the species plant_range list is generated based on genus data? 
hex_species_plants_range<- list()
for(i in 1: length(h3_indexes))
{
  poly_temp_db<- joined_data %>%
    filter(h3_index == h3_indexes[i])
  poly_species<- unique(poly_temp_db$species)
  hex_species_plants[i] <- list(poly_species)
  print(i)
}


#range 
hex_genus_plants_range<- list()
for(i in 1:length(h3_indexes))
{
  list_temp<- data.frame(hex_species_plants[i])
  colnames(list_temp)<- "name"
  list_temp$name<- word(list_temp$name, 1, sep = "_")
  hex_genus_plants_range[i]<- list(unique(list_temp$name))
}

saveRDS(hex_genus_plants_range, "Plants/hex_genus_plants_range")

#want to make a third one of these with the combined list of observations and ranges. 



#now I want to compare what we should see in california vs. what we actually have observationally seen.
#maybe should join gid of the observed and range species. 


#keep introduced species but not cultivars. 
expected_cali_plants<- plants_range_california$species
expected_cali_plants_unique_species<- unique(expected_cali_plants)
expected_cali_plants_unique_genus<- unique(word(expected_cali_plants, 1, sep = "_"))
observed_cali_plants<-gsub(" ", "_", joined_data_subset_with_introduced$scrubbed_species_binomial)
observed_cali_plants_unique_species<- unique(observed_cali_plants)
observed_cali_plants_unique_genus<- unique(word(observed_cali_plants_unique_species, 1, sep = "_"))

num_exp_unique_species<- length(expected_cali_plants_unique_species)
num_exp_unique_genus<- length(expected_cali_plants_unique_genus)
num_obs_unique_species<- length(observed_cali_plants_unique_species)
num_obs_unique_genus<- length(observed_cali_plants_unique_genus)
#we observe 19112 species in california and we only expect about 11000. 

#maybe should use join operations. 

common_species_obs_exp<- intersect(expected_cali_plants_unique_species, observed_cali_plants_unique_species)
species_only_exp<- setdiff(expected_cali_plants_unique_species, observed_cali_plants_unique_species)
species_only_obs<- setdiff(observed_cali_plants_unique_species, expected_cali_plants_unique_species)


#this is weird. for example, lupins_onustus is only in the observed but should be in the expected too. 

num_common_species<- length(common_species_obs_exp)
num_species_only_exp<- length(species_only_exp)
num_species_only_obs<- length(species_only_obs)

common_genera_obs_exp<- intersect(expected_cali_plants_unique_genus, observed_cali_plants_unique_genus)
#there are 1176 genera that are observed and expected 
genera_only_exp<- setdiff(expected_cali_plants_unique_genus, observed_cali_plants_unique_genus)
genera_only_obs<- setdiff(observed_cali_plants_unique_genus, expected_cali_plants_unique_genus)



total_unique_noncult_species<-c(common_species_obs_exp, species_only_exp, species_only_obs)
total_unique_noncult_genera<- c(common_genera_obs_exp, genera_only_exp, genera_only_obs)

#these should be what we use to construct the null model! 
write.csv(total_unique_noncult_species, file = "Plants/total_unique_noncultivated_species_obs_and_exp_cali.csv")
write.csv(total_unique_noncult_genera, file = "Plants/total_unique_noncultivated_genera_obs_and_exp_cali.csv")
#total of 1963 genera both observed and expected. 

num_common_genera<- length(common_genera_obs_exp)
num_genera_only_exp<- length(genera_only_exp)
num_genera_only_obs<- length(genera_only_obs)


#this might not add up properly. 
write.csv(common_species_obs_exp, file = "Plants/specis_common_to_observations_and_ranges_cali.csv")
write.csv(species_only_exp, file = "Plants/expected_species_not_observed_cali.csv")
write.csv(species_only_obs, file = "Plants/observed_species_not_expected_cali.csv")


write.csv(common_genera_obs_exp, file = "Plants/genera_common_to_observations_and_ranges_cali.csv")
write.csv(genera_only_exp, file = "Plants/expected_genera_not_observed_cali.csv")
write.csv(genera_only_obs, file = "Plants/observed_genera_not_expected_cali.csv")

#maybe we should rethink the null surfaces? I don't know. Maybe I should remove non-north american endemic species. 

#saving summary data as a json 
list_data_attributes <- list(
  num_observed_cultivars = num_observed_cultivars,
  num_probable_cultivars = num_probable_cultivars,
  num_common_species = num_common_species,
  num_species_only_exp = num_species_only_exp,
  num_species_only_obs = num_species_only_obs,
  num_common_genera = num_common_genera,
  num_genera_only_exp = num_genera_only_exp,
  num_genera_only_obs = num_genera_only_obs
)

# Step 2: Convert the list to JSON
json_data <- toJSON(list_data_attributes, pretty = FALSE)

# Save JSON to a file
write(json_data, file = "Plants/Observational_data_statistics.json")
#there are lots of plants that are observed in california that are not expected in california, basically. 


#best conceptual ground: build null 

#there are lichen representation  here! 
