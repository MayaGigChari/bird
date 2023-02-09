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
#is this how I should be doing mpd calculations?? confused. 
#don't know how to pull out the tree generated. 