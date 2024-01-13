
#this script generates trees of several sizes to be used in further analysis 
library(phylocomr)
library(ape)

#read parent tree (can amend to other types of trees)
#full_fish_phylo<-read.tree(file = "full_tree_for_cluster.tre")


#this is wrong. need to be sampled from CALIFORNIA!!!! not from the whole freaking ass bird freakingass tree!!

cali_bird_phylo<-read.tree(file = "cali_bird_tree.tre")

#tree_sizes = list(75,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000) #this has been edited. Can accomodate any sizes 

#tree_sizes = c(3:50)

#made some alterations for the birds. 
maketrees<- function(parent_name)
{
  tree_sizes = seq(5, 600, by=5)
  for(i in tree_sizes)
  {
    species_names_2<-parent_name$tip.label
    random_sample_2<-sample(species_names_2,i)
    random_sample_2l<- c(random_sample_2)
    sample_tree_2<-keep.tip(parent_name, random_sample_2l) #maybe try to prit this tree somehow later. 
    filename = paste("sample_tree_cluster", i, sep = "")
    write.tree(sample_tree_2, file = filename)
}
}
maketrees(cali_bird_phylo)
