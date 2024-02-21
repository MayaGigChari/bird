#this is a script that will calculate the minimum and maximum diversity metrics possible
#for each step size of a tree. 


#maybe to also calculate general statistics comparing california to 
#a bunch of trees of the same size from the parent tree for taxa. 

#need to try to understand how the cophenetic matrix is actually made. 

library(picante)

#load all the trees and set variables. 

clade = "Plants"
min_size = 2
increment = 1

cali_tree<- read.tree(paste(clade, "/cali_tree_interpolated.tre", sep = ""))

bird_cophen<- cophenetic.phylo(cali_tree)

cali_cophen_matrix<- readRDS(paste(clade, "/cali_cophen_matrix", sep = ""))
max_size<- length(cali_tree$tip.label) #can change this to the number of tips in california. 


hc <- hclust(as.dist(cali_cophen_matrix), method = "average")
num_clusters <- 10 # Adjust this based on your preference or using a method like elbow method
clusters <- cutree(hc, k = num_clusters)

#this isn't helpful
#this is something I need to do

#only need to run these calculations one time. but need to somehow find the smallest and largest values in a matrix. 

#bascially need to take all combinations of 2 nodes and find the closest possible node. 

