packages_to_install <- c("picante", "ape", "dplyr", "readr", "phytools", "BIEN", "phytools", "RPANDA")
# Loop through the packages
for (pkg in packages_to_install) {
  # Check if the package is not already installed
  if (!requireNamespace(pkg, quietly = TRUE)) {
    # Install the package only if not already installed
    install.packages(pkg)
  }
}

#plot the eriogonum genus, Grimmia genus and Brachythecium genus


if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("ggtree")

library(ggtree)
library(gridExtra)


lapply(packages_to_install, library, character.only = TRUE)

tree_trimming_path<-file.path(getwd(), "App_functions_bird.R")
source(tree_trimming_path)


?ggtree

eriogonum_trees <- list()
Grimmia_trees<- list()
Brachythecium_trees<- list()
full_trees<- list()

# Loop over the instances to read the trees and create plots
for (i in 1:100) {
  # Read the tree
  init <- read.tree(paste0("Plants/Instance_", i, "/cali_tree_interpolated.tre"))
  
  # Subset the tree for eriogonum species
  #should be the same species for all trees 
  eriogonum_species <- grep("Eriogonum", init$tip.label, value = TRUE)
  Grimmia_species<- grep("Grimmia", init$tip.label, value = TRUE)
  Brachythecium_species<- grep("Brachythecium", init$tip.label, value = TRUE)
  eriogonum_trees[[i]] <- keep.tip(init, eriogonum_species)
  Grimmia_trees[[i]]<- keep.tip(init, Grimmia_species)
  Brachythecium_trees[[i]]<- keep.tip(init, Brachythecium_species)
  full_trees[[i]]<- init
}

# Create plots for each tree. might be too big. 
plots_eriogonum <- lapply(eriogonum_trees, function(tree) ggtree(tree))
plots_Grimmia<- lapply(Grimmia_trees, function(tree) ggtree(tree))
plots_Brachy<- lapply(Brachythecium_trees, function(tree) ggtree(tree))

# Arrange plots in a 5-paneled plot
combined_plot_eriogonum <- grid.arrange(grobs = plots_eriogonum, nrow = 10, ncol = 10)
combined_plots_Grimmia<- grid.arrange(grobs = plots_Grimmia, nrow = 10, ncol = 10)
combined_plots_Brachy<- grid.arrange(grobs = plots_Brachy, nrow = 10, ncol = 10)

# Display the combined plot
print(combined_plots_Grimmia)

# Calculate some statistics for the 5 trees
# First need to make a community matrix in the correct form 
pds_eriogonum <- lapply(eriogonum_trees, function(tree) pd_app_picante(tree, tree)$PD)
histogram(unlist(pds_eriogonum), breaks = 50)
pds_grimmia<-lapply(Grimmia_trees, function(tree) pd_app_picante(tree, tree)$PD)
histogram(unlist(pds_grimmia), breaks = 50)
pds_brachy<-lapply(Brachythecium_trees, function(tree) pd_app_picante(tree, tree)$PD)
histogram(unlist(pds_brachy), breaks = 50)

pds_full<- lapply(full_trees, function(tree) pd_app_picante(tree, tree)$PD)
histogram(unlist(pds_full), breaks = 50)


#do some statistics on the spectral decomposition of each tree. 

#compute an average tree

#apply the 
spectral_decomposition_data_eriogonum<-lapply(eriogonum_trees, spectR)

dom_eigenvalues<- list()
assymetry<- list()
peakedness<- list()

for(i in 1:99)
{
  dom_eigenvalues[i]<- spectral_decomposition_data_eriogonum[[i]]$principal_eigenvalue
  assymetry[i]<-spectral_decomposition_data_eriogonum[[i]]$asymmetry
  peakedness[i]<-spectral_decomposition_data_eriogonum[[i]]$peakedness
}

histogram(unlist(dom_eigenvalues))
histogram(unlist(assymetry))
histogram(unlist(peakedness))


spectral_decomposition_data_grimmia<-lapply(Grimmia_trees, spectR)

dom_eigenvalues<- list()
assymetry<- list()
peakedness<- list()

for(i in 1:99)
{
  dom_eigenvalues[i]<- spectral_decomposition_data_grimmia[[i]]$principal_eigenvalue
  assymetry[i]<-spectral_decomposition_data_grimmia[[i]]$asymmetry
  peakedness[i]<-spectral_decomposition_data_grimmia[[i]]$peakedness
}

histogram(unlist(dom_eigenvalues))
histogram(unlist(assymetry))
histogram(unlist(peakedness))


#use the molecular phylogeny


spectral_decomposition_data_eriogonum[[1]]$

#this takes extremely long. 
#average_cali_tree<- averageTree(full_trees)

#