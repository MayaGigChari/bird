#functions needed

#1. read in list of species
#2. count number of species 
#3. null models (pd, mpd, mntd)
#4. visualizations of species on larger tree
#5. pd, mpd, mntd calculations for sample 
#6. visualizations for the pd differences between expected and sample

#main?

#example input: "baby ghost, parent ghost" 

#species_list<-function(list) #will take a comma delineated list of species and turn it into a dataframe with _ instead of spaces. 
#this takes a while. will try to run it on the cluster.
#useless
#null_commdata<- function(phylogeny, sample_size, num_samples) #this function outputs the actual dataframe that is useful. 
#{
#  species_names_null<- phylogeny$tip.label #get the species names of the phylogeny
#  species_names_null_df<-data.frame(species_names_null)
#  print(typeof(species_names_null_df))
#  for(i in (1:num_samples))
#  {
#    random_sample<-sample(species_names_null, sample_size) #get a random sample from the species names
#    #should I add a part to generate trees for each random sample?
#    random_sample<-c(random_sample)
#    species_names_null_df[paste('sample',i)] = NA
#    for( j in 1: length(species_names))
#    {
#      if(species_names_null_df[j,1]%in% random_sample)
#        species_names_null_df[j,i+1] = 1
#      else
#        species_names_null_df[j,i+1] = 0
#    } #assign 1 and or 0 to each sample. 
#  }
#  rownames(species_names_null_df) = species_names_null_df$species_names_null
#  species_names_null_df$species_names_null = NULL
#  return(species_names_null_df)
#}


#commdata_sample<- function(phylogeny, sample)
#{
#  species_names_exp<- phylogeny$tip.label #get the species names of the phylogeny
#  species_names_exp_df<- data.frame(species_names_exp)
#  species_names_exp_df["collected_sample"] = NA
#  for(i in 1:length(species_names_exp))
#  {
#    if(species_names_exp_df[i,1]%in% sample)
#      species_names_exp_df[i,2] = 1
#    else
#      species_names_exp_df[i,2] = 0
#  }
#  rownames(species_names_exp_df) = species_names_exp_df$species_names_exp
#  species_names_exp_df$species_names_exp = NULL
#  return(species_names_exp_df)
#}



#function to readin the data (must be in the form of a list of species) with different species on each line
#we
read<-function(txt_file)
{
  species_list<-read.table(file = txt_file, sep = '\n')
  colnames(species_list)[1]<- "name" #set the first column to be species 
  #species_list$name<-paste0(""",species_list$name,""")
  species_list$name <- sub(" ", "_", species_list$name)
  return(species_list)
}

#function that takes an r dataframe and cleans it 
read_df<- function(r_dataframe)
{
  r_dataframe<- gsub(" ", "_", r_dataframe)
  r_dataframe<-data.frame(r_dataframe)
  colnames(r_dataframe)<- "names"
  r_dataframe = na.omit(r_dataframe)
  return(unique(r_dataframe))
}



#function to check the species against the master tree and report missing taxa
check_taxa<-function(species_list, master_phylogeny)
{
  unmatched_species<-species_list%>%
    filter(!(species_list$name%in%master_phylogeny$tip.label))
  print("the following species on your list are not members of the larger phylogeny")
  print(unmatched_species$name)
  return(unmatched_species)
}


#function to remove taxa from list that aren't on the master tree

remove_taxa<- function(species_list, master_phylogeny)
{
  matched_species<-species_list%>% 
    filter((species_list$name%in%master_phylogeny$tip.label))
  return(matched_species)
}

#species_list_pruned<- remove_taxa(test,full_fish_phylo)

#function to produce a tree with all the taxa the user has observed

sample_tree_generator<-function(sample, master_phylogeny)
{
  library(phytools)
  library(ape)
  list_species<- c(sample$name)
  sample_tree<-keep.tip(master_phylogeny, list_species) #maybe try to print this tree somehow later. 
  return(sample_tree)
}

#tree_test<-sample_tree_generator(species_list_pruned,full_fish_phylo)

#function to get the max number of species and round down to the nearest ten

max_species <- function(species_list) {
  max_species <- max(nrow(species_list), 10)
  rounded_species <- floor(max_species / 10) * 10
  return(rounded_species)
}




#functon to produce a figure of all the taxa you have seen laid out on a tree. uses the helper function below called helper_whereontree

visualize_sample<-function(sample, master_phylogeny)
{
  library(ggtree)
  nodes<- helper_whereOnTree(master_phylogeny, sample)
  print(nodes)
  png(file="~/Desktop/FishPICCC.png",
      width=600, height=350)
  ggtree(master_phylogeny, layout = "circular")+
    geom_tiplab( geom = "text",aes(subset=(node %in% nodes)), size = 1.4, colour = "red", check.overlap = "TRUE", family = "serif")
}
#use the nodes defined above to make a visual. 

helper_whereOnTree<-function(parent_tree, sample_tree) #function for taking a tree and a sample and returns nodes in tree that exist in sample
{
  is_tip<-parent_tree$edge[,2]<=length(parent_tree$tip.label) #label each node with true/false depending on if it is a tip. 
  ordered_tips_func<-parent_tree$edge[is_tip,2] #get the nodes that return true. 
  ordered_tips_names_func =parent_tree$tip.label[ordered_tips_func] #extracts only the tip names in the order of the ordered tips? 
  names(ordered_tips_names_func)= ordered_tips_names_func #these two lines of code asssign names to each tip.
  nodes_in_sample_return = which(names(ordered_tips_names_func)%in%sample_tree$tip.label)
  return(nodes_in_sample_return)
}









# 
# 
# #the following functions are for diversity index analyses. They are now archaic (remove)
# #TODO: update with what you have done with paralellization on hoffman. 
# divcalc_app<- function(sample_tree, master_phylogeny)
# {
#   library(phylocomr)
#   df_touse<-data.frame(sample = "species", occurrence = 1, names = sample_tree$tip.label)
#   res_sample<- ph_comstruct(sample = df_touse, phylo = master_phylogeny, randomizations = 10)
#   return(res_sample)
# }
# 
# divcalc_app_parallel<- function(sample_tree, master_phylogeny) 
# {
#   library(phylocomr)
#   df_touse<-data.frame(sample = "species", occurrence = 1, names = sample_tree$tip.label)
#   res_sample<- ph_comstruct(sample = df_touse, phylo = master_phylogeny, randomizations = 1)
#   return(res_sample)  
# }
# 
# stats<-divcalc_app(tree_test, full_fish_phylo)
# 
# pd_app<- function (sample_tree,master_phylogeny)
# {
#   library(phylocomr)
#   df_touse<-data.frame(sample = "species", occurrence = 1, names = sample_tree$tip.label)
#   return(ph_pd(df_touse, master_phylogeny))
# }
# 



