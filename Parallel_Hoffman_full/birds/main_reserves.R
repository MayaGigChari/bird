
#need to read in all the shape files for the uc reserves and also the cali bird range data. 
#likely need to run this on azathoth because that is where the california data is. 

library(sf)
library(picante)

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("phyloseq")

library(phyloseq)

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

reserve_tree_sizes<- lapply(reserve_trees, ntaxa) #get the tree sizes. 


#calculate pd, mpd and mntd for all the reserves 
pd_vals<- lapply(reserve_trees, pd_app_picante, master_phylogeny = parent_tree)

pd_vals_only<- lapply(pd_vals, helper_get_pd)

mpd_vals<- lapply(reserve_trees, mpd_app_picante, coph_mat = cophen_matrix)

mntd_vals <- lapply(reserve_trees, mntd_app_picante, coph_mat = cophen_matrix)
#generated phylogneies for the reserve trees. 

#need to generate confidence intervals 

#for California 

CI_cali_range_pd<- lapply(reserve_tree_sizes, cI_generator, params_json_file = "birds/bird_ranges_wholeCali_pd_model_params.json")
CI_cali_significance_pd<- Map(check_significance_pd, pd_vals, upper_lower_keyvals = CI_cali_range_pd)

CI_cali_range_mpd<- lapply(reserve_tree_sizes, cI_generator, params_json_file = "birds/bird_ranges_wholeCali_mpd_model_params.json")
CI_cali_significance_mpd<- Map(check_significance_other_metrics, mpd_vals, upper_lower_keyvals = CI_cali_range_mpd)

CI_cali_range_mntd<- lapply(reserve_tree_sizes, cI_generator, params_json_file = "birds/bird_ranges_wholeCali_mntd_model_params.json")
CI_cali_significance_mntd<- Map(check_significance_other_metrics, mntd_vals, upper_lower_keyvals = CI_cali_range_mntd)


#need to determine which uc reserve to compare this to. To do this, we need to have a function that determins the upper and lower bounds for a given x-value
#this I will do tomorrow maybe? 


#figure out which ecoregion the reserves are in. some are in multiple ecoregions which are interesting. 
reserve_ecoregions<- lapply(uc_reserve_shps, ecoregion_id)
reserve_ecoregions<- lapply(reserve_ecoregions, unique)


#generate CI ranges for corresponding ecoregions within california. This will be for each UC reserve. 

#these names are the same for the ecoregion number thing.
#this will be the same order, so will do order. but also should work with names. 


#need to skip ana nuevo because it is not a part of the UC reserves. 
#there is one reserve that is on the corner of three ecoregions. 
filename_list_ecoregions_pd<- list()
filename_list_ecoregions_mpd<- list()
filename_list_ecoregions_mntd<- list()
#this will be a list that corresponds to the appropriate null models. 
for(i in 1:length(reserve_names))
{
  null_reserve<- (reserve_ecoregions)[[i]]
  if(identical(null_reserve, integer(0)) == FALSE)
  {
    file_name<- paste("birds/ecoregion_data/", null_reserve, sep = "")
    filename_list_ecoregions_pd[[i]]<- paste(file_name, "/pd_model_params.json", sep = "")
    filename_list_ecoregions_mpd[[i]]<- paste(file_name, "/mpd_model_params.json", sep = "")
    filename_list_ecoregions_mntd[[i]]<- paste(file_name, "/mntd_model_params.json", sep = "")
  }
  else 
  {
    filename_list_ecoregions_pd[[i]]<- NULL
    filename_list_ecoregions_mpd[[i]]<- NULL
    filename_list_ecoregions_mntd[[i]]<- NULL
  }
  
}

CI_ecoregions_ranges_pd <- Map(cI_generator, reserve_tree_sizes, params_json_file = filename_list_ecoregions_pd)
CI_ecoregions_significance_pd<- Map(check_significance, pd_vals, upper_lower_keyvals = CI_ecoregions_ranges_pd)


CI_ecoregions_ranges_mpd <- Map(cI_generator, reserve_tree_sizes, params_json_file = filename_list_ecoregions_mpd)
CI_ecoregions_significance_mpd<- Map(check_significance_other_metrics, mpd_vals, upper_lower_keyvals = CI_ecoregions_ranges_mpd)


CI_ecoregions_ranges_mntd <- Map(cI_generator, reserve_tree_sizes, params_json_file = filename_list_ecoregions_mntd)
CI_ecoregions_significance_mntd<- Map(check_significance_other_metrics, mntd_vals, upper_lower_keyvals = CI_ecoregions_ranges_mntd)

#now need to conglomerate everything into a table to portray on the map. 
#emerson, ano nuevo and steele burned are either not in an ecoregion or are in more than one. 
ecoregions_nonames<- reserve_ecoregions
ecoregions_nonames$`Cali_Geometry/uc_reserves_geog/Steele_Burnand_A-B_DRC/Steele_Burnand_A-B_DSP_Boundary.shp`<- 0 
ecoregions_nonames$`Cali_Geometry/uc_reserves_geog/Ano_Nuevo_Island/Ano_Nuevo_Island.shp`<- 0 
ecoregions_nonames$`Cali_Geometry/uc_reserves_geog/Emerson_Oaks/Emerson_Oaks_Boundary.shp`<- 0
names(ecoregions_nonames)<- NULL
ecoregions_nonames<- unlist(ecoregions_nonames)
taxa_missing_nonames<- lapply(taxa_missing,helper_get_taxa_only)
taxa_present_nonames<- lapply(present_taxa,helper_get_taxa_only )

tree_strings <- list()

# Convert each phylo object to Newick format and store in the list
for (tree in reserve_trees) {
  tree_string <- write.tree(tree)
  tree_strings <- c(tree_strings, tree_string)
}


json_data <- toJSON(tre_objects, pretty = FALSE)
write_json(json_data, "birds/bird_trees.json")


summary_data<- data_frame("reserve" =  unlist(extracted_names), "ecoregion_id" = ecoregions_nonames, "true_pd"= pd_vals_only,"pd_significance_california"= CI_cali_significance_pd, "pd_significance_ecoregions"= CI_ecoregions_significance_pd, 
                          "true_mpd"= mpd_vals,"mpd_significance_california"= CI_cali_significance_mpd, "mpd_significance_ecoregions"= CI_ecoregions_significance_mpd,
                          "true_mntd"= mntd_vals,"mntd_significance_california"= CI_cali_significance_pd, "mntd_significance_ecoregions"= CI_ecoregions_significance_mntd, 
                          "taxa_missing_from_tree" = taxa_missing_nonames, "taxa_present_in_tree"= taxa_present_nonames, "phylogeny" = tree_strings)

json_data_summary_reserves <- jsonlite::toJSON(summary_data, pretty = FALSE)
write(json_data_summary_reserves, "birds/json_data_summary_reserves.json")


#this below doesn't work. 
#summary_json<- list("reserve" =  unlist(extracted_names), "ecoregion_id" = ecoregions_nonames, "true_pd"= pd_vals,"pd_significance_california"<- CI_cali_significance_pd, "pd_significance_ecoregions"= CI_ecoregions_significance_pd, 
#                     "true_mpd"= mpd_vals,"mpd_significance_california"= CI_cali_significance_mpd, "mpd_significance_ecoregions"= CI_ecoregions_significance_mpd,
#                     "true_mntd"= mntd_vals,"mntd_significance_california"= CI_cali_significance_pd, "mntd_significance_ecoregions"= CI_ecoregions_significance_mntd, 
#                      "taxa_missing_from_tree" = taxa_missing, "taxa_present_in_tree"<- present_taxa)



#json_data_summary_reserves <- toJSON(summary_json)

#write(json_data_summary_reserves, "birds/json_data_summary_reserves.json")
