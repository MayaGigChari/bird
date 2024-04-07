
#need to read in all the shape files for the uc reserves and also the cali bird range data. 
#likely need to run this on azathoth because that is where the california data is. 

library(sf)
library(picante)

#read the bird ranges 


#need to load dependent functions

current_directory <- getwd()

tree_trimming_path<-file.path(current_directory, "App_functions_bird.R")

source(tree_trimming_path)

#background data: need the parent tree (california birds), the cophenetic matrix

parent_tree<- read.tree("birds/cali_tree_from_range_data.tre")
cophen_matrix<- readRDS("birds/cali_range_cophen_matrix")

bird_occurrences_cali<- read_sf("birds/bird_ranges_overlapping_california_complete_checklist.shp")


sf_use_s2(FALSE)
bird_occurrences_cali<-st_make_valid(bird_occurrences_cali)
st_is_valid(bird_occurrences_cali)

uc_reserve_shps<- readRDS("Cali_Geometry/uc_reserve_shapefiles_sf_objects")

reserve_names<- names(uc_reserve_shps)

#extract the true reserve names from the saved names. 
extracted_names <- lapply(reserve_names, function(x) {
  base_name <- basename(x) # Extract the base name (last part after the /)
  name <- gsub("\\.shp$", "", base_name) # Remove the .shp extension
  return(name)
})


reserve_species_lists<- list()
for(i in 1: length(reserve_names))
{
  temp_sf_ranges<- st_intersection(bird_occurrences_cali, uc_reserve_shps[[i]])
  species_reserve<- unlist(unique(temp_sf_ranges$sci_nam))
  reserve_species_lists[[i]]<- species_reserve
}

#now have list of reserve species in each reserve. 


#Next step: calculate the pd, mpd and mntd for each reserve. 

#there are missing taxa but there shouldn't be. this is odd. need to probably redo the california taxa. for now 

#just kidding, these taxa missing might be because the california tree doesn't actually have all bird taxa. 

#should corroborate missing taxa with true missing taxa

#short check but looks like everything is fine. 
#taxa_missing_large_tree<- read.csv("birds/range_birds_absent_phylogeny.csv")
#which(taxa_missing[[1]]$names %in% taxa_missing_large_tree$names)

reserve_names_df<- lapply(reserve_species_lists, read_df)
taxa_missing<- lapply(reserve_names_df, check_taxa, master_phylogeny= parent_tree )
present_taxa<- lapply(reserve_names_df, remove_taxa, master_phylogeny = parent_tree)
reserve_trees<- lapply(present_taxa, sample_tree_generator, master_phylogeny = parent_tree)


#calculate pd, mpd and mntd for all the reserves 
pd_vals<- lapply(reserve_trees, pd_app_picante, master_phylogeny = parent_tree)

mpd_vals<- lapply(reserve_trees, mpd_app_picante, coph_mat = cophen_matrix)

mntd_vals <- lapply(reserve_trees, mntd_app_picante, coph_mat = cophen_matrix)
#generated phylogneies for the reserve trees. 

#need to determine which uc reserve to compare this to. To do this, we need to have a function that determins the upper and lower bounds for a given x-value
#this I will do tomorrow maybe? 


