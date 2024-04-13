#this is a script for loading and parsing the bird ranges. 

#this script output is bird ranges that overlap with california in various ways, and also birds that overlap with other ecoregions in various ways. 

#this does not populate hexagonal data. 
install.packages("sf")
library(remotes)
library(stringr)
library(dplyr)

#need to do this to be able to change multisurface to multipolygon data. get an error, however. 

#here I will use the bird checklist from https://californiabirds.org/checklist.asp. make sure to cite this. 
install_github("r-spatial/sf")

#read bird checklist 

#note: in order to make st object valid, I needed to run this sf_use_s2(FALSE)

birds_expected_cali<- read("birds/Bird_list.txt")
# Get the parenthesis and what is inside
birds_expected_cali <- str_extract_all(birds_expected_cali, "\\([^()]+\\)")[[1]]
# Remove parenthesis
birds_expected_cali <- substring(birds_expected_cali, 2, nchar(birds_expected_cali)-1)

bird_ranges_species<- bird_ranges$sci_name



birds_expected_cali[1]%in% bird_ranges_species

which(birds_expected_cali %in% bird_ranges_species)

#bird range data takes a long time to load. 
bird_ranges<- st_read("birds/BotW_2023_1/BOTW.gdb")

bird_ranges_cali<- bird_ranges%>% 
  filter(sci_name %in%  birds_expected_cali)


bird_ranges_geom_types<- st_geometry_type(bird_ranges_cali)

multi_surface_indices <- which(bird_ranges_geom_types == "MULTISURFACE")


#get all the bird data 

#there are about 170 multi_surface indices
#for ease, I will remove these and note the species. 

#uncomment when I can cast these. 
#for (index in multi_surface_indices) {
#  bird_ranges[index, ] <- st_cast(bird_ranges[index, ], "MULTIPOLYGON")
#}

#want to cast these but for now will just remove them. only like 6 species. 

bird_ranges_cut<- bird_ranges_cali[-multi_surface_indices,] #removoe everything that has a multisurface geomertry. 


# Convert MultiSurface to MultiPolygon for features with MultiSurface geometries
#ask jonathan about casting multisurface to multipolygon 

#need to make 

bool<- st_is_valid(bird_ranges_cut)

#simplify first and then use makevalid
#bird_ranges_cut<-st_simplify(bird_ranges_cut)

which(bool == FALSE)
bird_ranges_cut<- st_make_valid(bird_ranges_cut)





california <- sf::read_sf("Cali_Geometry/ca-state-boundary/CA_State_TIGER2016.shp") %>%
  sf::st_transform(4326)


#NOT sure if i have to run this step anymore. now only have to run birds for each ecoregion 

#only about 300 birds that are native to california actually have ranges that intersect wiht california??

#this is probably right for california. 
birds_california<- st_intersection(bird_ranges_cut, california)

birds_california %>%
  filter(sci_name == "Anser albifrons")


seen<- unique(birds_california$sci_name)
write.csv(seen, file = "birds/birds_seen_in_cali_with_overlap_range.csv")

exp<- unique(birds_california$sci_name)

list_exp_not_seen<-which(!exp %in% seen)

# Specify the correct file path
file_path <- "birds/bird_ranges_all_birds_seen_california_checklist.shp"

# Write the sf object to the shapefile
#this did not work. 
st_write(bird_ranges_cut, file_path)
#everything native to california also intersects with california! great. 
#this join might actually not hv


#use north american checklist instead to filter birds 

north_american_birds<- read.csv("birds/NACC_list_species.csv")
north_american_bird_species<- north_american_birds$species

bird_ranges_noram<- bird_ranges%>% 
  filter(sci_name %in%  north_american_bird_species)

bird_ranges_geom_types_noram<- st_geometry_type(bird_ranges_noram)

multi_surface_indices_noram <- which(bird_ranges_geom_types_noram == "MULTISURFACE")

bird_ranges_cut_noram<- bird_ranges_noram[-multi_surface_indices_noram,] #removoe everything that has a multisurface geomertry. 

bool_noram<- st_is_valid(bird_ranges_cut_noram)

list_invalid<- which(bool_noram == FALSE)

for(num in list_invalid)
{
  bird_ranges_cut_noram[num,]<- st_make_valid(bird_ranges_cut_noram[num,])
  print(num)
}

bool_noram_2<- st_is_valid(bird_ranges_cut_noram)

which(bool_noram_2== FALSE) #now this is all. 

#only about 300 birds that are native to california actually have ranges that intersect wiht california??
birds_california_noram<- st_intersection(bird_ranges_cut_noram, california)

unique(birds_california_noram$sci_name)

file_path_range <- "birds/bird_ranges_overlapping_california_complete_checklist.shp"

# Write the sf object to the shapefile
#this did not work. 
st_write(birds_california, file_path_range)

st_temp<- st_read(file_path_range)


#need to make shape files for all overlapping birds for the california provinces. 
ecoregions<- st_read("Cali_geometry/ca_eco_l3", packlage = "sf")

ecoregions<- st_transform(ecoregions, "NAD83")

filename_mia = "Cali_geometry/ecoregions_for_mia.shp"
st_write(ecoregions, filename_mia)
birds_california_transform<- st_transform(birds_california, 4269)

#perhaps this join is not correct? 
birds_california
ecoregion_bird_data<- st_join(birds_california_transform, ecoregions) #no intersection??

ecoregion_bird_data %>%
  filter(US_L3CODE == 78)
#made a directory called ecoregion_data to store shape files and lists. 
ecoregion_codes<- ecoregions$US_L3CODE

#do not need to rerun this. already have all the ecoregion species stuff. 
#ecoregion bird data is already merged! This is basically good to go for the entire ecoregion. but also need to do the other thing with the ecoregions (hex data stuff)
#these shape files are good for the entire overlapping species list for a region, need to be able to populate hex data as well.

#edited this to only select some fields, but actually only edited it for one ecoregion. 
for(code in ecoregion_codes)
{
  dir.create(file.path("birds/ecoregion_data", code))
  temp<- ecoregion_bird_data %>%
    filter(US_L3CODE == code)%>%
    select(sci_name, US_L3CODE, US_L3NAME, L1_KEY, Shape)
  filepath_shape<- paste("birds/ecoregion_data/", code, "/ecoregion.shp", sep = "")
  sf::st_write(temp, filepath_shape)
  filepath_checklist<- paste("birds/ecoregion_data/", code, "/checklist.csv", sep = "")
  print(unique(temp$sci_name))
  write.csv(unique(temp$sci_name), file = filepath_checklist)
}
#now that we have species lists for all the ecoregions we can assemble their null models. and the null model for all of california. 
#now this seems like it should work. 

a <- st_read("birds/ecoregion_data/1/ecoregion.shp")
#save birds without overlapping ranges that have still been seen


unexpected_birds<- which(! birds_expected_cali %in% birds_california$sci_name)


write.csv(birds_expected_cali[unexpected_birds], file = "birds/birds_no_overlap_range_cali.csv")
