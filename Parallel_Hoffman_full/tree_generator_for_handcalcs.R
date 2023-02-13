library(ape)
library(phylocomr)

randomizations = c(1:1000);
full_tree<-read.tree(file = "full_tree_for_cluster.tre") #this file should be contained in the same working directory.


for(n in randomizations) #1000 times
{
  #generate a random tree
  print(n)
  i = 5 #sample size.
  species_names_2<-full_tree$tip.label
  random_sample_2<-sample(species_names_2,i)
  random_sample_2l<- c(random_sample_2)
  sample_tree_2<-keep.tip(full_tree, random_sample_2l) #maybe try to prit this tree somehow later. 
  df_touse<-data.frame(sample = "species", occurrence = 1, names = sample_tree_2$tip.label) #create a dataframe compatibile with phcomstruct function
  res_sample<- ph_comstruct(sample = df_touse, phylo = full_tree_cluster, randomizations = 1) #the real bulk of the program. Not sure why 2 randomizations but will keep this.  \
  if(res_sample['mpd.rnd']>255)
  {
    print("true")
    write.tree(sample_tree_2, file = paste("/Users/mayachari/Desktop/bird_local/large_mpd_tree_", n, ".tre",sep = ""))
    write.csv(res_sample, file = paste("/Users/mayachari/Desktop/bird_local/large_mpd_tree_", n, ".csv",  sep = ""))
  }
}

sample_tree<-read.tree("/Users/mayachari/Desktop/bird_local/large_mpd_tree_4.tre")

library(ggtree)
png(file="/Users/mayachari/Desktop/bird_local/sample_tree.png",
    width=600, height=350)
plot(sample_tree)
edgelabels(sample_tree$edge.length, col="blue", font=2)

visualize_sample<-function(sample, master_phylogeny)
{
  library(ggtree)
  nodes<- helper_whereOnTree(master_phylogeny, sample)
  print(nodes)
  png(file="/Users/mayachari/Desktop/bird_local/sample_tree.png",
      width=600, height=350)
  ggtree(master_phylogeny, layout = "circular")+
    geom_tiplab( geom = "text",aes(subset=(node %in% nodes)), size = 1.4, colour = "red", check.overlap = "TRUE", family = "serif")
}

helper_whereOnTree<-function(parent_tree, sample_tree) #function for taking a tree and a sample and returns nodes in tree that exist in sample
{
  is_tip<-parent_tree$edge[,2]<=length(parent_tree$tip.label) #label each node with true/false depending on if it is a tip. 
  ordered_tips_func<-parent_tree$edge[is_tip,2] #get the nodes that return true. 
  ordered_tips_names_func =parent_tree$tip.label[ordered_tips_func] #extracts only the tip names in the order of the ordered tips? 
  names(ordered_tips_names_func)= ordered_tips_names_func #these two lines of code asssign names to each tip.
  nodes_in_sample_return = which(names(ordered_tips_names_func)%in%sample_tree$tip.label)
  return(nodes_in_sample_return)
}
visualize_sample(sample_tree, full_tree)

#notes: meeting
#use markdown and version control. 
#don't use loops in R. r wants to do array operations
  #llaply and faply apply functions in R. write this again in apply. 
#need run randomziations, store them and then have function that deals with stored trees, grabs all trees 
#store tree objects and separate. ape has a datastruct that can store multiple trees
  #name trees by mpd
#create df and put each tree into dataframe 
#make a dummy collection of trees 
  #cols: run number, newick file for tree, PD calculation/pd/mpd. 
  #faster to generate newick of trees and then do operations on tree list. 
#prepare a markdown document for next 

#is this how I should be doing mpd calculations?? confused. 
#don't know how to pull out the tree generated. 