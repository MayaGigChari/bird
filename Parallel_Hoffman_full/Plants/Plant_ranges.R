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

ecoregions<- st_read("Cali_geometry/ca_eco_l3", packlage = "sf")

ecoregions<- st_transform(ecoregions, "WGS84")

#THIS IS AN ARBITRARY THRESHOLD: if < 1% of an ecoregion is covered by the range, then we will remove that range. 
ecoregions$area <- st_area(ecoregions) #Take care of units
ecoregions$min_thresh<- 0.01*ecoregions$area


#TODO: preprocess the ecoregion data so that the range is not included if the ecoregion area in a certain range is below a threshold. 
ecoregions_plant_data_intersection<- st_intersection(ecoregions, california_species_ranges_only_natives)

ecoregions_plant_data<- st_join(ecoregions, california_species_ranges_only_natives)

#this is a huge file. join of all the ecoregions. will write this to the local. Save didn't work

#don't save. takes too long. 
#saveRDS(ecoregions_plant_data, "Plants/California_species_shapes/plants_range_california_only_natives_POTW_joined_ecoregions")
#perhaps this join is not correct? 
#this is the entire ecoregion bird data and it IS correct. Birds for each ecoregion. 

#strange because these species are particularly NOT in many ranges. 
test<- ecoregions_plant_data %>%
  filter(species == "Atriplex_serenana") 
library(ggplot2)

# Plot the first plot using ggplot2 and color by US_L3CODE
ggplot() +
  geom_sf(data = ecoregions[1], aes(fill = US_L3CODE)) +
  geom_sf(data = california_species_ranges_only_natives %>%
            filter(species == "Atriplex_serenana"), 
          color = "red", alpha = 0.6)
#made a directory called ecoregion_data to store shape files and lists. 
ecoregion_codes<- ecoregions$US_L3CODE

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
  
  temp <- ecoregions_plant_data %>%
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
