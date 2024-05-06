#main for Butterflies


#when this file is executed, the cophen matrix will be generated for a squamate tree for the geographic area 
#of california. This can be altered at the specificed location if there are different
#larger trees of interest (ie: some other geographical area )
#in this, we generate trees for the molecular and the compelete phylogenies. will use the molecular from here on out for now. 
#this does not actually require any geospatial information. 


#TODO: generate 5 trees. 

#first step needs to be to prune the california tree to the list of california taxa 

#install necessary packages 
packages_to_install <- c("picante", "ape", "dplyr", "readr", "phytools",  "phytools", "ggtree")
# Loop through the packages
for (pkg in packages_to_install) {
  # Check if the package is not already installed
  if (!requireNamespace(pkg, quietly = TRUE)) {
    # Install the package only if not already installed
    install.packages(pkg)
  }
}



#install.packages("BIEN")
#library(BIEN)

lapply(packages_to_install, library, character.only = TRUE)
#specify the clade that you are interested in

clade = "butterflies"


geog_area = "cali"

current_directory <- getwd()

coph_path <- file.path(current_directory, "Cophen_function.R")
tree_trimming_path<-file.path(current_directory, "App_functions_bird.R")
sample_tree_creator_path<- file.path(current_directory, "sample_tree_creator.R")


source(coph_path)
source(tree_trimming_path)
source(sample_tree_creator_path)


#complete_phylogeny for california that is based on molecular data but only has genus-level information 

#use the dated amino acid tree. 

#this bayes tree is the one that actually makes sense for me to use. 
complete_phylogeny_butterfly<- read.nexus("butterflies/Data.S24.RevBayes_Papilionoidea_BDS_rates_MAP2.tre")

#this can be plotted again later. 
plot(complete_phylogeny_butterfly, type = "fan", show.tip.label = FALSE)

labels_butterfly<- complete_phylogeny_butterfly$tip.label

labels_butterfly[454]

labels_butterfly[933]

extract_specific_words <- function(input_string) {
  if (endsWith(input_string, "X_ME")) {  # Check if the string ends with "X_ME"
    words <- strsplit(input_string, "_")[[1]]  # Split the string by underscores
    
    # Extract the fourth and third-to-last words
    extracted_words <- c(words[length(words) - 3], words[length(words) - 2])
    
    # Join the extracted words with underscores
    result <- paste(extracted_words, collapse = "_")
  } else {
    words <- strsplit(input_string, "_")[[1]]  # Split the string by underscores
    
    # Extract the last two words
    extracted_words <- tail(words, 2)
    
    # Join the extracted words with underscores
    result <- paste(extracted_words, collapse = "_")
  }
  
  return(result)
}


check_string_in_list <- function(input_string) {
  if(is.na(input_string))
  {
    return(0)
  }
  string_list<- labels_butterfly
  for (str in string_list) {
    if (grepl(input_string, str)) 
    {
      print('yes')
      return(1)
    }
  }
  
  return(0)
}



#code to prune to a genus? maybe check unique genera first 
#there are 71 total species represented in the tree that are also overlapping with California. 
a<- lapply(sf_data_taxa_list_cali_butterflies$name, check_string_in_list)
sum(unlist(a))


#now check genera. 


  

#complete phylogeny butterfly is now done. 


#load the shape file containing all species represented

butterfly_shape<- st_read("butterflies/full_california_butterflies_ranges.shp")

sf_data_taxa_list_cali_butterflies<- data.frame(unique(butterfly_shape$binomial))
colnames(sf_data_taxa_list_cali_butterflies)<- "name"

sf_data_genera_list_cali_butterflies<-genus_only(sf_data_taxa_list_cali_butterflies)
sf_data_genera_list_cali_butterflies$colnames<- NULL
colnames(sf_data_genera_list_cali_butterflies)<- "name"


#these are the unique genera
unique(sf_data_genera_list_cali_butterflies$genus_list)

b<- lapply(unique(sf_data_genera_list_cali_butterflies$genus_list), check_string_in_list)

#there are 87 unique genera represented. should probably use genera over species. 
sum(unlist(b))


labels_butterfly_2<- lapply(labels_butterfly, extract_specific_words)
complete_phylogeny_butterfly$tip.label<- unlist(labels_butterfly_2)

#now that this is labeled with the species we can make the genus tree


genus_butterfly_tree<- genus_tree_generator(complete_phylogeny_butterfly)


#how the fuck are there 176 unmatched species 


#need to make the choice to use the butterfly genus or species tree. 
unmatched_species_butterflies<- check_taxa(sf_data_taxa_list_cali_butterflies, complete_phylogeny_butterfly)

matched_species_butterflies<- remove_taxa(sf_data_taxa_list_cali_butterflies, complete_phylogeny_butterfly)

cali_tree_species_butterflies<- sample_tree_generator(matched_species_butterflies,complete_phylogeny_butterfly)

ggtree(cali_tree_species_butterflies, layout = "circular")+
  geom_tiplab(size = 3) # Change the size value as needed

# Save the plot

write.tree(cali_tree_species_butterflies, file = file.path(clade, "cali_species_tree.tre"))

#there are 12 unmatched genera 
unmatched_genera<- check_taxa(sf_data_genera_list_cali_butterflies, genus_butterfly_tree)

#about 2000 represented taxa. need to double check this anyways. 
matched_genera_butterflies<- remove_taxa(sf_data_genera_list_cali_butterflies, genus_butterfly_tree)

#leaves us with a total of 149 tipes 
cali_tree_genera_butterflies<- sample_tree_generator(matched_genera_butterflies,genus_butterfly_tree)

write.tree(cali_tree_genera_butterflies, file = file.path(clade, "cali_genera_tree.tre"))

ggtree(cali_tree_genera_butterflies, layout = "circular")+
  geom_tiplab(size = 3) # Change the size value as needed

#have already developed the species level tree and loaded above. 

#USING GENUS-LEVEL might lose some of the range granularity but using species level might lead to inflation of tips somehow. 
#maybe I should just run both for butterflies and see whether there is a hughe significant difference. 

#generate the cophenetic matrix for the genus tree
#save as cali_genus_cophen_matrix

#generate a corresponding cophenetic matrix. 

#made one for species and genus. 
call_cophen(cali_tree_genera_butterflies, clade = "butterflies", geog_area = "cali_genus") 



#for ecoregions with SPECIES

#only really need to use the phylogeny that actually has representative taxa. 
dir_list_ecoregions <- list.dirs("butterflies/ecoregion_data",recursive = FALSE)  
full_tree<- cali_tree_species_butterflies

for(directory in dir_list_ecoregions)
{
  ecoregion_species<- read.csv(paste(directory, "/checklist.csv", sep = ""))
  ecoregion_species$X<- NULL
  ecoregion_species$x<- gsub(" ", "_", ecoregion_species$x)
  colnames(ecoregion_species)<- "name"
  unmatched_ecoregion_species<- check_taxa(ecoregion_species, full_tree)
  matched_ecoregion_species<-remove_taxa(ecoregion_species, full_tree)
  ecoregion_tree<- sample_tree_generator(matched_ecoregion_species, full_tree)
  
  #manually use cophen matrix because this format isn't working. 
  ecoregion_matrix<- cophen(ecoregion_tree)
  cophen_ecoregion<- as.matrix(ecoregion_matrix)
  
  
  #save all the files. 
  saveRDS(cophen_ecoregion, file = paste(directory, "/cophen_matrix_species", sep = ""))
  write.csv(unmatched_ecoregion_species, file = paste(directory, "/species_absent_from_tree.csv", sep = ""))
  write.csv(matched_ecoregion_species, file = paste(directory, "/species_present_in_tree.csv", sep = ""))
  write.tree(ecoregion_tree, file = paste(directory, "/trimmed_species_tree.tre", sep = ""))
}

##for ecoregion with genus

#for ecoregions with GENUS
full_tree_genus<- cali_tree_genera_butterflies

for(directory in dir_list_ecoregions)
{
  ecoregion_species<- read.csv(paste(directory, "/checklist.csv", sep = ""))
  ecoregion_species$X<- NULL
  ecoregion_species$x<- gsub(" ", "_", ecoregion_species$x)
  colnames(ecoregion_species)<- "names"
  ecoregion_genus<- genus_only(ecoregion_species)
  ecoregion_genus$colnames<- NULL
  colnames(ecoregion_genus)<- "name"
  
  unmatched_ecoregion_genus<- check_taxa(ecoregion_genus, full_tree_genus)
  matched_ecoregion_genus<-remove_taxa(ecoregion_genus, full_tree_genus)
  ecoregion_tree_genus<- sample_tree_generator(matched_ecoregion_genus, full_tree_genus)
  
  #manually use cophen matrix because this format isn't working. 
  ecoregion_matrix_genus<- cophen(ecoregion_tree_genus)
  cophen_ecoregion_genus<- as.matrix(ecoregion_matrix_genus)
  
  
  #save all the files. 
  saveRDS(cophen_ecoregion_genus, file = paste(directory, "/cophen_matrix_genus", sep = ""))
  write.csv(unmatched_ecoregion_genus, file = paste(directory, "/genera_absent_from_tree.csv", sep = ""))
  write.csv(matched_ecoregion_genus, file = paste(directory, "/genera_present_in_tree.csv", sep = ""))
  write.tree(ecoregion_tree_genus, file = paste(directory, "/trimmed_tree_genus.tre", sep = ""))
}

#now we have butterfly everything for genus AND species 
