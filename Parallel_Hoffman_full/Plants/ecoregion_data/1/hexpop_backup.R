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


#need to load the dataframe 


poly_labels<- names(data) #this gives us all the codes. 

#run this function with the most recent polygon data 

#there should be some kind of difference with pd no? maybe not. 
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
saveRDS(pop_hex_stats_plants_df, file = "Plants/raw_hex_stats_from_ranges")

#there are 200 hexagons with missing data. 


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

plot(genus_complete_phylogeny, type = "fan", show.tip.label = TRUE, hex = 0.5)

plottype = "fan", show.tip.label = FALSE)



