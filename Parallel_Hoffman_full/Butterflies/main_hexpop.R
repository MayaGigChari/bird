#populating hexagons with butterfly data 

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

clade = "butterflies"

tree_trimming_path<-file.path(current_directory, "App_functions_bird.R")

source(tree_trimming_path)


#issue with the cophenetic matrix. 

#reading in the cophenetic matrix 
#can toggle this between species data and genus data. 
cophen_butterflies<-readRDS("butterflies/cali_genus_cophen_matrix")
#parent_tree<- read.tree("butterflies/interpolated_full_tree.tre") probably don't really need this.
#made the genus tree in main. 
cali_tree_butterflies<- read.tree("butterflies/cali_genera_tree.tre") #load the genus tree



#populate hexagons. 



cali_species_ranges_butterflies<- st_read("butterflies/full_california_butterflies_ranges.shp")
california <- sf::read_sf("Cali_Geometry/ca-state-boundary/CA_State_TIGER2016.shp") %>%
  sf::st_transform(4326)
h3_indexes <- polyfill(california[1, ], res = 6)
h3_indexes[1]
polygons <- h3_to_geo_boundary_sf(h3_indexes)


#get geometry for ecoregions 
ecoregions<- st_read("Cali_geometry/ca_eco_l3", packlage = "sf")
ecoregions<- st_transform(ecoregions, "WGS84")
ecoregions_L3codes<- ecoregions$US_L3CODE

#join the hexagons with the california species ranges to ge tthe complete dataset 
hex_species_range_butterflies<- st_join(polygons, cali_species_ranges_butterflies)

hex_species_range_butterflies$binomial

st_write(hex_species_range_butterflies, "butterflies/full_hex_intersect_ranges_joined.shp", append = FALSE)


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
    poly_species<- unique(poly_temp_db$binomial)
    hex_species[i] <- list(poly_species)
    print(indexes[i])
  }
  print("hexRaw done")
  return(hex_species)
}

#don't have to rerun genHexRawa gain. already did this, need to narrow down to species. 
butterflies_hex_species_list<- genHexRaw_prejoined(hex_species_range_butterflies, h3_indexes)
names(butterflies_hex_species_list)<- h3_indexes

saveRDS(butterflies_hex_species_list, file = "butterflies/hexagon_true_species_butterfly_data")

extract_genus <- function(string_list) {
  first_words <- lapply(string_list, function(string) {
    words <- strsplit(string, "_")[[1]]
    return(words[1])
  })
  return(unlist(first_words))
}

butterflies_hex_genera_list<- lapply(butterflies_hex_species_list, extract_genus)

unique_butterflies_hex_genera_list<- lapply(butterflies_hex_genera_list, unique)
names(unique_butterflies_hex_genera_list)<- h3_indexes

#total of 1966 genera. interesting. 
cali_tree_butterflies
plot(cali_tree_butterflies, type = "fan", show.tip.label = FALSE)

#total of 2038 tips represented in MISHLER phylogeny
cali_tree_butterflies #660 total butterflies represented in the phylogeny. 
#associate the data with the indexes
#this stays the same for observational and range data. 
h3_indexes<- data.frame(h3_indexes)
h3_indexes$X<- NULL #not sure why this always happens. 
dat_names<- h3_indexes$h3_indexes


#now we have assigned data to each 

#the first word will always be the genus. 

#already have the occurrence data


#this should have been done in a previous script. 
#names(data)<- h3_indexes


#need to load the dataframe 


poly_labels<- names(unique_butterflies_hex_genera_list) #this gives us all the codes. 

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
hex_tree_stats_butterflies_genera- popHexStats(unique_butterflies_hex_genera_list, cali_tree_butterflies, "butterflies", cophen_filepath = "butterflies/cali_genus_cophen_matrix")

hex_tree_stats_butterflies_genera_copy<- hex_tree_stats_butterflies_genera

pop_hex_stats_butterflies_df_genus<- data_frame("h3_index" = names(data), "pd_values" = unlist(hex_tree_stats_butterflies_genera$pd_values), "mpd_values" = unlist(hex_tree_stats_butterflies_genera$mpd_values), "mntd_values" = unlist(hex_tree_stats_butterflies_genera$mntd_values), 
                                     "tree_size" = unlist(lapply(hex_tree_stats_butterflies_genera$species_in_tree, length)), "missing_taxa_tree_size" = unlist(lapply(hex_tree_stats_butterflies_genera$missing_species, length)) )

#seems that there are generally about 40% of taxa missing. this might fuck everything up but who knows. 
pop_hex_stats_butterflies_df_genus$proportion_missing<- pop_hex_stats_butterflies_df_genus$missing_taxa_tree_size/(pop_hex_stats_butterflies_df_genus$tree_size + pop_hex_stats_butterflies_df_genus$missing_taxa_tree_size)


#there is up to 80% of butterflies missing. crazy. 
histogram(pop_hex_stats_butterflies_df_genus$proportion_missing)
#this is just an aside to make a figure of mssing taxa. 

#need to make 

library(ggplot2)
missing_hist<- ggplot(pop_hex_stats_butterflies_df_genus, aes(x = proportion_missing)) +
  geom_histogram(bins = 50) +
  ggtitle("Missing Taxa Proportions: Hexagon Data California")

ggsave(filename = "butterflies/Missing_taxa_proportions_hexagon_california_genera.png", 
       plot = missing_hist, 
       dpi = 300, 
       width = 8, 
       height = 6, 
       units = "in")

#this just saves the file hex_tree_stats_birds without any geographic information. 
saveRDS(pop_hex_stats_butterflies_df_genus, file = "butterflies/raw_hex_stats_from_ranges_genera_butterflies")


pop_hex_stats_butterflies_df_genus<- readRDS("butterflies/raw_hex_stats_from_ranges_genera_butterflies")

######################
#step 2: assign significance values to hexagon data
######################




#want to merge this back to the polygon sf object 
#use the "polygons" file generated in california-partition. 
#this is called(hex_data_as_sf in the Cali_Geometry file. )

cali_hexes_as_sf<- polygons

polygon_data_full_butterflies_genus<- left_join(data.frame(cali_hexes_as_sf), pop_hex_stats_butterflies_df_genus, by = "h3_index")

polygon_data_full_butterflies_with_eco_genus<- left_join(polygon_data_full_butterflies_genus, data.frame(hexes_with_ecoregions))


#ec_js_pd<- ecoregion_json_filename shortened. 
#could remove the NA values and merge them back in later (hexagons with no ecoregion)

#was this possibly using birds? no way at all. 
polygon_data_full_butterflies_with_eco_genus$ec_js_pd<- unlist(lapply(polygon_data_full_butterflies_with_eco_genus$US_L3CODE, makejsonstringGenus, clade = "butterflies",  metric = "pd"))
polygon_data_full_butterflies_with_eco_genus$ec_js_mpd<- unlist(lapply(polygon_data_full_butterflies_with_eco_genus$US_L3CODE, makejsonstringGenus, clade = "butterflies", metric = "mpd"))
polygon_data_full_butterflies_with_eco_genus$ec_js_mntd<- unlist(lapply(polygon_data_full_butterflies_with_eco_genus$US_L3CODE, makejsonstringGenus,clade = "butterflies", metric = "mntd"))



#need to determine for each polygon in polygon_data_full if the pd is significant or not, and make a table. similar to what was done in the reserves. 
#need to make a function that generates a null model somehow concatenated from multiple ecoregions (idk how to do this. need to figure this out.)

#for now I will compare each polygon to california and its null model ecoregion, multiple ecoregions will be left out. 



####THIS WHOLE CHUNK OF CODE IS GENERATING SIGNIFICANCE VALUES. 

#unsure why everything is normal. we shall see.
#there are zero plant hexagons that have negative values for pd. wtf. 

#perhaps there is an issue with the CI generation?

#pretty much all these freaking butterflies are overdispersed. 
polygon_data_CI_ranges_pd_cali_butterflies_genus<- lapply(polygon_data_full_butterflies_with_eco_genus$tree_size, cI_generator, params_json_file = "butterflies/pd_model_params.json")
CI_cali_significance_polygons_pd_butterflies_genus<- Map(check_significance_other_metrics, polygon_data_full_butterflies_with_eco_genus$pd_values, upper_lower_keyvals = polygon_data_CI_ranges_pd_cali_butterflies_genus)


#HOW THE FUCK IS EVERYTHING SIGNIFICANTLY THE FUCK HIGH!?????

png("butterflies/images/CI_cali_significance_hexes_pd_hist_genus.png", width = 800, height = 600, units = "px", res = 100)
histogram(unlist(CI_cali_significance_polygons_pd_butterflies_genus), main = "CI_cali_significance_hexes_pd_hist", xlab = "PD", ylab = "Frequency")
dev.off()

polygon_data_CI_ranges_mpd_cali_butterflies_genus<- lapply(polygon_data_full_butterflies_with_eco_genus$tree_size, cI_generator, params_json_file = "butterflies/mpd_model_params.json")
CI_cali_significance_polygons_mpd_butterflies_genus<- Map(check_significance_other_metrics, polygon_data_full_butterflies_with_eco_genus$mpd_values, upper_lower_keyvals = polygon_data_CI_ranges_mpd_cali_butterflies_genus)


png("butterflies/images/CI_cali_significance_hexes_mpd_hist_genus.png", width = 800, height = 600, units = "px", res = 100)
histogram(unlist(CI_cali_significance_polygons_mpd_butterflies_genus), main = "CI_cali_significance_hexes_mpd_hist", xlab = "PD", ylab = "Frequency")
dev.off()

polygon_data_CI_ranges_mntd_cali_butterflies_genus<- lapply(polygon_data_full_butterflies_with_eco_genus$tree_size, cI_generator, params_json_file = "butterflies/mntd_model_params.json")
CI_cali_significance_polygons_mntd_butterflies_genus<- Map(check_significance_other_metrics, polygon_data_full_butterflies_with_eco_genus$mntd_values, upper_lower_keyvals = polygon_data_CI_ranges_mntd_cali_butterflies_genus)


png("butterflies/images/CI_cali_significance_hexes_mntd_hist_genus.png", width = 800, height = 600, units = "px", res = 100)
histogram(unlist(CI_cali_significance_polygons_mntd_butterflies_genus), main = "CI_cali_significance_hexes_mntdd_hist", xlab = "PD", ylab = "Frequency")
dev.off()

#for some reason the mean nearest taxon distance is pretty much always insignificant 


#need to generate the json file names for each ecoregion. 
#double check that ecoregions are in the correct order. might be wrong null models. 
#for ecoregions: 
polygon_data_CI_ranges_pd_ecoregions_butterflies_genus <- Map(cI_generator, polygon_data_full_butterflies_with_eco_genus$tree_size, params_json_file = polygon_data_full_butterflies_with_eco_genus$ec_js_pd)
CI_ecoregions_significance_polygons_pd_butterflies_genus<- Map(check_significance_other_metrics, polygon_data_full_butterflies_with_eco_genus$pd_values, upper_lower_keyvals = polygon_data_CI_ranges_pd_ecoregions_butterflies_genus)

png("birds/images/CI_cali_significance_hexes_pd_hist_ecoregions_genus.png", width = 800, height = 600, units = "px", res = 100)
histogram(unlist(CI_ecoregions_significance_polygons_pd_butterflies_genus), main = "CI_cali_significance_hexes_pd_hist", xlab = "PD", ylab = "Frequency")
dev.off()


polygon_data_CI_ranges_mpd_ecoregions_butterflies_genus <- Map(cI_generator, polygon_data_full_butterflies_with_eco_genus$tree_size, params_json_file = polygon_data_full_butterflies_with_eco_genus$ec_js_mpd)
CI_ecoregions_significance_polygons_mpd_butterflies_genus<- Map(check_significance_other_metrics, polygon_data_full_butterflies_with_eco_genus$mpd_values, upper_lower_keyvals = polygon_data_CI_ranges_mpd_ecoregions_butterflies_genus)

#check them against their own ecoregions. 
png("butterflies/images/CI_cali_significance_hexes_mpd_hist_ecoregions_genus.png", width = 800, height = 600, units = "px", res = 100)
histogram(unlist(CI_ecoregions_significance_polygons_mpd_butterflies_genus), main = "CI_cali_significance_hexes_mpd_hist", xlab = "mpd", ylab = "Frequency")
dev.off()


polygon_data_CI_ranges_mntd_ecoregions_butterflies_genus <- Map(cI_generator, polygon_data_full_butterflies_with_eco_genus$tree_size, params_json_file = polygon_data_full_butterflies_with_eco_genus$ec_js_mntd)
CI_ecoregions_significance_polygons_mntd_butterflies_genus<- Map(check_significance_other_metrics, polygon_data_full_butterflies_with_eco_genus$mntd_values, upper_lower_keyvals = polygon_data_CI_ranges_mntd_ecoregions_butterflies_genus)

png("butterflies/images/CI_cali_significance_hexes_mntd_hist_ecoregions.png", width = 800, height = 600, units = "px", res = 100)
histogram(unlist(CI_ecoregions_significance_polygons_mntd_butterflies_genus), main = "CI_cali_significance_hexes_mntd_hist", xlab = "mntd", ylab = "Frequency")
dev.off()


###ADD ALL THESE SIGNIFICANCE VALUES TO THE DATA STRUCTURE: 

#for some reason there's a dimensionality issue. 
#need to figure out how to deal with polygons on multuiple ecoregions stat!!
#for some reason unlist gets rid of null. don't want that. 

CI_cali_significance_polygons_pd_butterflies_genus[sapply(CI_cali_significance_polygons_pd_butterflies_genus, is.null)] <- NA
CI_cali_significance_polygons_mpd_butterflies_genus[sapply(CI_cali_significance_polygons_mpd_butterflies_genus, is.null)] <- NA
CI_cali_significance_polygons_mntd_butterflies_genus[sapply(CI_cali_significance_polygons_mntd_butterflies_genus, is.null)] <- NA

CI_ecoregions_significance_polygons_pd_butterflies_genus[sapply(CI_ecoregions_significance_polygons_pd_butterflies_genus, is.null)] <- NA
CI_ecoregions_significance_polygons_mpd_butterflies_genus[sapply(CI_ecoregions_significance_polygons_mpd_butterflies_genus, is.null)] <- NA
CI_ecoregions_significance_polygons_mntd_butterflies_genus[sapply(CI_ecoregions_significance_polygons_mntd_butterflies_genus, is.null)] <- NA


polygon_data_full_butterflies_with_eco_genus$pdSigCal<- unlist(CI_cali_significance_polygons_pd_butterflies_genus)
polygon_data_full_butterflies_with_eco_genus$mpdSigCal<- unlist(CI_cali_significance_polygons_mpd_butterflies_genus)
polygon_data_full_butterflies_with_eco_genus$mntdSigCal<- unlist(CI_cali_significance_polygons_mntd_butterflies_genus)


#need to change NULL to NA to make this datastructure work. 
#make NA's. line = 

polygon_data_full_butterflies_with_eco_genus$pdSigEco<- unlist(CI_ecoregions_significance_polygons_pd_butterflies_genus)
polygon_data_full_butterflies_with_eco_genus$mpdSigEco<- unlist(CI_ecoregions_significance_polygons_mpd_butterflies_genus)
polygon_data_full_butterflies_with_eco_genus$mntdSigEco<- unlist(CI_ecoregions_significance_polygons_mntd_butterflies_genus)


#might have messed things up with names. 
st_write(st_as_sf(polygon_data_full_butterflies_with_eco_genus), "butterflies/polygon_data_full_butterflies_with_eco_genus.shp")

###NOTE:: already saved butterfly data, use original save!
st_write(st_as_sf(polygon_data_full_butterflies_with_eco_genus),"butterflies/final_output.shp")
saveRDS(polygon_data_full_butterflies_with_eco_genus, "butterflies/final_output_dataframe")
#already wrote this! 

###DONE for butterflies. 

#the output of this is 11792 units long. 
#this is now the updated list that has doubles for reserves that cover at least one ecoregion 


#1763 polygons have at least 2 ecoregions 
num_polygons_multiple_regions<- polygon_data_full_butterflies_with_eco_genus%>%
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


ggsave(filename = "butterflies/images/Hexagons_with_multiple_ecoregions_histogram.png", 
       plot = multiple_ecoregions, 
       dpi = 300, 
       width = 8, 
       height = 6, 
       units = "in")


hist(polygons_multiple_ecoregions_full$pd_cali_significance)
#now plot frequency of significance within california of just these multiple ecoregion points. 

#seems like the hexagons that are at the boundaries of ecoregions have pretty similar behavior to hexagons within the ecoregions. 
#need to compare against a larger null model, though
#this might look very different for butterflies. extremely similar patters for hexes that lie on more than one ecoregion. 
multiple_ecoregions_hist<- ggplot(polygons_multiple_ecoregions_full, aes(x = mntd_cali_significance))+ 
  geom_histogram(bins = 50)+
  ggtitle("distribution of mntd significance for hexagons in multiple ecoregions")

ggsave(filename = "butterflies/images/distribution_mntd_significance_hexagons_multiple-ecoregions.png",
       plot = multiple_ecoregions_hist, 
       dpi = 300, 
       width = 8, 
       height = 6, 
       units = "in")


#for some reason this isn't working 

polygon_data_full_butterflies_with_eco_genus<- st_as_sf(polygon_data_full_butterflies_with_eco_genus)



#want to make a plot 
# Assuming you have already loaded the required libraries, such as sf and ggplot2

library(tidyr)
polygon_full_butterflies_with_eco_nona_genus<- st_as_sf(drop_na(polygon_data_full_butterflies_with_eco_genus))
#another attempt to plot 
# Convert NA values to a specific label

# Convert pdSigCal to factor

# 3. Color the hexagons based on pdSigCal values
library(ggplot2)
plot_area <- ggplot() +
  theme_void() +  # Remove default axes and background
  coord_equal()    # Ensure equal aspect ratio


plot_area <- plot_area +
  geom_sf(data = polygon_full_butterflies_with_eco_nona_genus, aes(fill = factor(pdSigEco)))
# Customize the legend and color scale



plot_area_fin <- plot_area +
  scale_fill_manual(values = c("-1" = "blue", "0" = "white", "1" = "red"),
                    name = "Significance",
                    labels = c("-1", "0", "1"),
                    guide = "legend")

ggsave("butterflies/Ecoregion_genus_level_pd_distribution_check.png", plot_area_fin, width = 10, height = 10, dpi = 300)

dev.off()

##need to also do this for the genus model and compare, since a very low percentage of the butterfly species are actually represented. 

#now have all the maps and distributions but everything is wack. literally everything is wack as hell. 
