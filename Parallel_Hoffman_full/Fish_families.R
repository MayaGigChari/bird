library(fishtree)
library(picante)
library(dplyr)
?fishtree

tax_info<-subset(fishtree_taxonomy(), rank == "family")
tax_info_updated<-subset(tax_info, name!="Incertae sedis in Carangaria" & name!="Incertae sedis in Eupercaria" & name!= "Incertae sedis in Ovalentaria")
full_tree<-read.tree(file = "full_tree_for_cluster.tre")


#here we get everything that has more than 5 species. 
list_fams <- list()
names_fams<- list()
for(i in 1:length(tax_info_updated$name))
{
  fam_temp<-tax_info_updated$name[i]
  tax_temp<- fishtree_taxonomy(rank = fam_temp)
  spec_list<-tax_temp[[fam_temp]]$species 
  if(length(spec_list) >= 5)
  {
      print("true")
      list_fams[[length(list_fams)+1]] <- spec_list
      names_fams[[length(names_fams)+1]] <- fam_temp
  }
}
#now we write all these files to csv or whatever. 
for(i in 1: length(list_fams))
{
  write.table(unlist(list_fams[i]), file = paste("/Users/mayachari/Desktop/bird/Parallel_Hoffman_full/fish_families/", names_fams[i], ".txt" ,sep = ""), row.names = FALSE, col.names = FALSE)
}
# now these fish families are ready to be sent through the pipeline! 

