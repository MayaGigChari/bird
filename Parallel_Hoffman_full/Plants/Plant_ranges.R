#this is a file analogous to bird_ranges

#step 1: already generated in the range_gen script: plants that have ranges that interesect California? 

#what about non-native plants? what did we end up deciding? 


#here I will try to get a list of all california natives (like in birds)

#this part can be done on the local. 
install.packages("rvest")
install.packages("httr")
library(httr)
library(rvest)
library(dplyr)
library(sf)

# URL of the webpage
url <- "https://ucjeps.berkeley.edu/cgi-bin/get_JM_name_data"

# Make an HTTP POST request to the webpage without specifying the NAME parameter
response <- POST(url)

# Extract the HTML content of the response
html_content <- content(response, as = "text")

# Parse the HTML content
page <- read_html(html_content)

# Extract the data you need using CSS selectors or XPath
# For example, if the data is in a table, you can use:
data <- page %>% html_table()

data_useful<- data[[4]]

#there are 7,392 species that are technically california natives. 
data_useful_only_natives<- data_useful%>%
  filter(nativity == "NATIVE")

#write this to a csv or an rds file; might still be too big. 

species_data_useful<- data_useful_only_natives$name_minus_authors

write.csv(species_data_useful, file = "Plants/species_native_to_Cali.csv")

# If the data is in a different format or structure, you may need to use different functions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

# to extract it. You can use functions like html_nodes() to select specific HTML elements and
# html_text() to extract the text content.

# Once you have extracted the data, you can further process it or save it to a file.
# For example, to save it to a CSV file:


#########Everything above this was done on the local. now going to properly prune the big data. 
species_of_interest<- read.csv("Plants/species_native_to_Cali.csv")
species_of_interest$X<- NULL
colnames(species_of_interest)<- "species"
species_of_interest<- data.frame(species_of_interest)
species_of_interest$species <- gsub(" ", "_", species_of_interest$species)
#the next step with this is to trim the regional data to only birds that are supposed to be in california 


plants_range_california_only<- st_read("Plants/california_plants.shp")


#there are 12574 

#this join statement might have been used incorrectly in other scripts

#there are a total of 3,918 species of plants in this file that are California natives and also are represented with range data overlapping california. 
#is it possible that I should have been using inner joins all along? 
plants_range_california_only_natives<- st_as_sf(inner_join(species_of_interest, plants_range_california_only))
      
st_write(plants_range_california_only_natives, "Plants/California_species_shapes/plants_range_california_only_natives_POTW.shp")


#sanity check to make sure that we have the correct number of tuples. 
plants_names_both<- which(plants_range_california$species %in% species_of_interest$species)

#always do an inner join. never do a left join in this project ever. 


############
#Now: We have a list of birds that overlap with california but also that exist in the california species checklist of native plants. 
#it looks like a bunch of subspecies were removed.
#SWITCH BACK TO LOCAL FOR ECOREGIONS STUFF!
############

#from California

#these are all the california species (only about 4,000 of them)
california_species_ranges_only_natives<- st_read("Plants/California_species_shapes/plants_range_california_only_natives_POTW.shp")

california_species_ranges_only_natives$range_area<- st_area(california_species_ranges_only_natives)
ecoregions<- st_read("Cali_Geometry/ca_eco_l3/ca_eco_l3.shp")

ecoregions<- st_transform(ecoregions, "WGS84")

#THIS IS AN ARBITRARY THRESHOLD: if < 1% of an ecoregion is covered by the range, then we will remove that range. 
ecoregions$area <- st_area(ecoregions) #Take care of units
ecoregions$min_thresh<- 0.02*ecoregions$area #can always amend this minimum threshold by just accessing the area. 

#small trial to make sure this works ok. 
ecoregions_plant_data_intersection_test_thresh<- st_intersection(ecoregions, california_species_ranges_only_natives%>% filter(species == "Lathyrus_palustris"))
ecoregions_plant_data_intersection_test_thresh$eco_area<- st_area(ecoregions_plant_data_intersection_test_thresh)
ecoregions_plant_data_intersection_test_thresh<- ecoregions_plant_data_intersection_test_thresh%>%
  filter(eco_area >= min_thresh | eco_area >= 0.10*range_area)

#TODO: preprocess the ecoregion data so that the range is not included if the ecoregion area in a certain range is below a threshold. 
#require that at least 5% of the ecoregion be covered in order for it to be included in the range. 

#for real. this will take forever to run. 
#running this again to force myself to have a break. 
#saving ecoregions_plant_data_intersection just in case. 
#this should assign species with particular ranges to each ecoregion. HOWEVER, it might only preserve the ecoregion gometry not the actual species range geometry!
ecoregions_plant_data_intersection_cond2<- st_intersection(ecoregions, california_species_ranges_only_natives)

plot(ecoregions_plant_data_intersection_cond2[200,])
#get the areas of all the intersections 
#started at 7:59 pm 
ecoregions_plant_data_intersection_cond2$eco_area<- st_area(ecoregions_plant_data_intersection_cond2)

#filter the data so that the overlapped area is greater than the minimum threshold. 
#the geometry of this is: for each tuple species i, ecoregion j, the geometry is the overlap of species 
# i only in ecoregion j. 
#note: each species is uniuqe and only has one geometry. 
ecoregions_plant_data_intersection_cond2<- ecoregions_plant_data_intersection_cond2%>%
  filter(eco_area>= min_thresh | eco_area >= 0.10*range_area)

st_write(ecoregions_plant_data_intersection_cond2, "Plants/plant_complete_species_lists_by_ecoregion.shp")
#this is now done. the join has been formalized. I wish I didn't formalize it but I did. 

#now we have ecoregion plant data that filters out overlaps that cover less than 5% of an ecoregion. 
#this join takes all the ecoregions 
#ecoregions_plant_data_including_small_areas<- inner_join(data.frame(ecoregions), california_species_ranges_only_natives)

#the exact shape of the ecoregions shouldn't matter, all that matters is their shape that intersects california. 

#now I want to join back the 

#this is a huge file. join of all the ecoregions. will write this to the local. Save didn't work

#don't save. takes too long. 
#saveRDS(ecoregions_plant_data, "Plants/California_species_shapes/plants_range_california_only_natives_POTW_joined_ecoregions")
#perhaps this join is not correct? 
#this is the entire ecoregion bird data and it IS correct. Birds for each ecoregion. 

#strange because these species are particularly NOT in many ranges. 

#this is a sanity check. 


test<- ecoregions_plant_data_intersection_cond2 %>%
  filter(species == "Lathyrus_palustris") 
library(ggplot2)

# Plot the first plot using ggplot2 and color by US_L3CODE

#the species Cymopterus_panamintensis has been fully removed! 

#may have to filter on if the threshold of the range is below a certain threshold of the whole species range? 
#as an artifact of this probram, some species with poorly-defined small ranges are removed. 

#if we filter out the ecoregion that a hexagon is in, we should just get the part of the range of species n that is within that ecoregion. 
p<- ggplot() +
  geom_sf(data = test[1][6,], aes(fill = US_L3CODE)) +
  geom_sf(data = california_species_ranges_only_natives %>%
            filter(species == "Lathyrus_palustris"), 
          color = "red", alpha = 0.1)

ggsave("Plants/Cymopterus_panamintensis_range_coverage_before_trim_removed_after.png", plot = p,width = 10, height = 8,  dpi = 300)
#made a directory called ecoregion_data to store shape files and lists. 
ecoregion_codes<- ecoregions$US_L3CODE

#here I'm going to manually go through for hexagon 1 and do everything manually to the fuck double check this shit. 

#step 1: get the 200th index. or nth as it is labeled. 
nth_index<- polygon_data_full_plants_with_eco$h3_index[200]

polygon_n<-hexes_with_ecoregions%>%
  filter(h3_index == nth_index)

#just get the ranges in this particular ecoregion. 
trimmed_poss_species<-ecoregions_plant_data_intersection_cond2%>% 
  filter(US_L3CODE == polygon_n$US_L3CODE)

#started running this at 6:37 pm
nth_species<- st_intersection(polygon_n, trimmed_poss_species)

unique(nth_species$species)

species_observed<- polygon_data_full_plants_with_eco%>%
  filter(h3_index == polygon_n$h3_index)
#this is the same: there are 1635 observed species that overlap with that particular index of which 923 are represented on this tree. 
#this produces the exact same pd value as expected. 

#now want to check to make sure the pd value is actually correct. 
#read in the parent tree: 
parent_tree<- read.tree("Plants/species_level_full_treeMISHLER.tre")
temp <- data.frame(unique(ecoregions_plant_data_intersection_cond2$species))
colnames(temp) <- "name"
temp$name<- gsub(" ", "_", temp$name)
check_taxa(temp, )
temp<- remove_taxa(temp, parent_tree)

#is it possible that it's not being compared to the correct estimations for tree its tree size? 
#it's just SLIGHTLY above! 
#maybe I need to remove all taxa that literally don't exist in an ecoregion at all. 
cI_generator(923, params_json_file = "Plants/pd_model_params_PLANTS_SPECIES_0418.json")

#the temp tree generated has 2037 tips and 2036 internal nodes. somehow the parent tree has 2038 but this really shouldn't make a fuckign difference!!!
temp_tree<- sample_tree_generator(temp, parent_tree)
pd_app_picante(temp_tree, parent_tree)

#there are a total of 3917 species. I think this is the exact same as there was before. 
unique(ecoregions_plant_data_intersection_cond2$species)

remove_taxa()

#going to take a look at this tree. honestly have no idea how to take a look at this tree though. 

plot(temp_tree,type = "fan", show.tip.label = FALSE)
plot(parent_tree, type = "fan", show.tip.label = FALSE)

#honestly I don't know, I need to go to sleep. 

#do not need to rerun this. already have all the ecoregion species stuff. 
#ecoregion bird data is already merged! This is basically good to go for the entire ecoregion. but also need to do the other thing with the ecoregions (hex data stuff)
#these shape files are good for the entire overlapping species list for a region, need to be able to populate hex data as well.

#edited this to only select some fields, but actually only edited it for one ecoregion. 
for(code in ecoregion_codes) {
  folder_path <- file.path("Plants/ecoregion_data", code)
  
  # Check if folder exists, if not, create it
  if (!file.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
    cat("Created folder:", folder_path, "\n")
  }
  
  #also selects the geometry. 
  temp <- ecoregions_plant_data_intersection_cond2 %>%
    filter(US_L3CODE == code) %>%
    select(species, US_L3CODE, US_L3NAME, L1_KEY)
  
  filepath_shape <- file.path("Plants/ecoregion_data", code, "ecoregion.shp")
  filepath_checklist <- file.path("Plants/ecoregion_data", code, "checklist.csv")
  
  # Check if shapefile already exists
  if(file.exists(filepath_shape)) {
    print(paste("Shapefile", filepath_shape, "already exists. Deleting..."))
    file.remove(filepath_shape)
  }
  
  sf::st_write(temp, filepath_shape)
  print(unique(temp$species))
  write.csv(unique(temp$species), file = filepath_checklist)
}
#all the ecoregion species lists have now been written to their respective files. 
#honestly you never know, this might be wrong. 

#maybe I should just do everything to do with the geometry on this server even though it sucks. 
