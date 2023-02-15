library(ape)
library(phylocomr)
install.packages("phylobase")
install.packages("phangorn")
library("phangorn")
library(phylobase)


full_tree<-read.tree(file = "full_tree_for_cluster.tre") #this file should be contained in the same working directory.


###NOTES: to  add to markdown for students in Alfaro lab. for individual tree visualization and pdcalc. 
#first we read the tree. we can refer to the 
sample_tree<-read.tree("/Users/mayachari/Desktop/bird_local/large_mpd_tree_4.tre")



library(ggtree)

#here's a way we can visualize the tree simply with tips labeled as speciies
ggtree(sample_tree) + geom_tiplab(as_ylab=TRUE, color='firebrick') 

#here we can add a root edge as well to make the tree cleaner
ggtree(sample_tree) + geom_tiplab(as_ylab=TRUE, color='firebrick') + geom_rootedge(50)#here's a way we can visualize the tree simply with tips labeled as speciies

#visualize your tree with sample_tree. how many internal nodes are in your tree? using this information, how many 
str(sample_tree)

#try plotting with the edge lengths using ape
plot(sample_tree, main = "Sample tree with edge lengths", root.edge = TRUE)
edgelabels(round(sample_tree$edge.length, 3), cex = 0.7)
edgelabels(cex = 0.7)

#we also might want to look at the edge numbers... this might be useful in our mpd calculations. 
plot(sample_tree, main = "Sample tree with edge lengths", root.edge = TRUE)
edgelabels( cex = 0.7)

#finally, let's look at the internral nodes. it's messy-- try cleaning it up! check out the documentation. 
plot(sample_tree, main = "Sample tree with edge lengths", root.edge = TRUE)
nodelabels( cex = 0.7)
tiplabels(cex = 0.7)


#find mrca of 2 nodes

pdcalc<- function(tree)
{
  return(sum(edge.length))
}

#now let's try a hand calculation of MPD. Refer to the attached document to see the formula for calculation 
#of mpd. There are varying levels of complexity/difficulty you can use to approach this. I'm going a simple route,
#using built in functionality in APE

mpdcalc<- function(tree)
{
  nodes<- tree$Nnode + 1 #there will always be n-1 internal nodes, where n is the number of tips
}
  


#for each set of tips: pseudocode!!
## IF the children (tips) share a parent, then add the two branch lengths of the children
## IF the children do not share a parent, move back one parent and check if the children share the parent
## IF the parent one node behind is shared, find the edges included in the shortest path back to the mrca (most recent common ancestor)
## store these edges and add them together! bing. 

#below is a hand_calc function for calculating PD! This is just the way I decided to do it... it might 
#be worth it to look into the documentation so you can do it too. 

edge_table_builder<- function(tree)
{
  edge_table <- cbind(tree$edge, tree$edge.length)
  rownames(edge_table) <-  1:nrow(edge_table)
  names<- as.numeric(rownames(edge_table))
  rownames(edge_table)<- NULL
  edge_table<-cbind(edge_table,names)
  colnames(edge_table) <- c("parent", "child", "edge_length", "edge_number")
  return(edge_table)
}
compute_mpd<- function(tree)
{
  edge_table<-edge_table_builder(tree)
  sum_mpd = 0
  tips<- tree$Nnode + 1 #number of tips is the number of nodes + 1. 
  tip_combos<- data.frame(combn(tips, 2)) #get all possible combinations of tip values. 
  tip_combos_t<-t(tip_combos)
  for(i in 1:length(tip_combos))
  {
    path <- nodepath(sample_tree, from = tip_combos_t[i,1], to = tip_combos_t[i,2])
    list_temp_parent<- c()
    list_temp_child <- c()
    for(i in 1:length(path)-1)
    {
      list_temp_child[i] = path[i]
      list_temp_parent[i] = path[i+1]
    }
    
    edge_temp<-data.frame(c(list_temp_parent,list_temp_child), c(list_temp_child,list_temp_parent)) #this is very hacky. 
    colnames(edge_temp)<-c("parent", "child")
    
    df_temp <- merge(x=edge_table,y=edge_temp, 
                by=c("parent","child"))
    sum_mpd = sum_mpd + sum(df_temp$edge_length)
  }
  result = (2/(tips*tips))*sum_mpd #some contention here: do we do tips*tips+1 or tips*tips? 
  #result = (2/(tips*tips+1))*sum_mpd is technically algorithmically correct.
  return(result)
}

compute_mpd(sample_tree)

sample_tree_2<-read.tree("/Users/mayachari/Desktop/bird_local/large_mpd_tree_7.tre")
compute_mpd(sample_tree_2)


#yay! now we have the edge numbers we need to add up. will now make this iterative. 



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