
#this script generates trees of several sizes to be used in further analysis 
packages_to_install <- c("phylocomr", "ape")

# Loop through the packages
for (pkg in packages_to_install) {
  # Check if the package is not already installed
  if (!requireNamespace(pkg, quietly = TRUE)) {
    # Install the package only if not already installed
    install.packages(pkg)
  }
}

lapply(packages_to_install, library, character.only = TRUE)


#tree_sizes = list(75,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000) #this has been edited. Can accomodate any sizes 

#tree_sizes = c(3:50)

#made some alterations for the birds. 
maketrees<- function(parent_name, min_size, max_size, increment, clade)
{
  tree_sizes = seq(min_size, max_size, increment)
  if (file.exists(file.path(clade, "sample_trees"))) {
    cat("Removing existing folder...\n")
    unlink(file.path(clade, "sample_trees"), recursive = TRUE)
  }
  
  # Create the folder
  dir.create(file.path(clade, "sample_trees"))
  
  for(i in tree_sizes)
  {
    species_names_2<-parent_name$tip.label
    random_sample_2<-sample(species_names_2,i)
    random_sample_2l<- c(random_sample_2)
    sample_tree_2<-keep.tip(parent_name, random_sample_2l) #maybe try to prit this tree somehow later. 
    filename = paste("sample_tree_cluster", i, sep = "")
    path = file.path(clade, "sample_trees", filename)
    write.tree(sample_tree_2, file = path)
}
}
