
#this script generates trees of several sizes to be used in further analysis 
library(phylocomr)
library(ape)

#read parent tree
full_fish_phylo<-read.tree(file = "full_tree_for_cluster.tre")

tree_sizes = list(10,50,100,200,1000)

for(i in tree_sizes)
{
  species_names_2<-full_fish_phylo$tip.label
  random_sample_2<-sample(species_names_2,i)
  random_sample_2l<- c(random_sample_2)
  sample_tree_2<-keep.tip(full_fish_phylo, random_sample_2l) #maybe try to print this tree somehow later. 
  filename = paste("sample_tree_cluster", i, sep = "")
  write.tree(sample_tree_2, file = filename)
}