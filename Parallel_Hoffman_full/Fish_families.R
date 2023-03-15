library(fishtree)
?fishtree
tax_info<-subset(fishtree_taxonomy(), rank == "family")
tax_info_updated<-subset(tax_info, name!="Incertae sedis in Carangaria" & name!="Incertae sedis in Eupercaria" & name!= "Incertae sedis in Ovalentaria")


for(i in 1:length(tax_info_updated$name))
{
  fam_temp<-tax_info_updated$name[i]
  tax_temp<- fishtree_taxonomy(rank = fam_temp)
  print(tax_temp$fam_temp$species)
  #print((length(tax_temp$tax_info_updated$name[i]$species)))
  #if((length(tax_temp$tax_info_updated$name[i]$species))<5)
  #{
  #  print("yes")
  #}
}

tree_vector<-c(NA,length(tax_info_updated$name))

for(i in 1:length(tax_info_updated$name))
{
  tree_vector[i]<- fishtree_complete_phylogeny(rank = tax_info_updated$name[i])
}

tree_vector[1]
make_fam_list<- function(family_list)
{
  tax<- fishtree_taxonomy(rank = family_list )
  family_list<-tax
}

fams_larger_5<-function(family_list)
{
  family_list<-(length(family_list$species))
}


temp<-make_fam_list(tax_info_updated$name)
temp[2]$species
temp2<-fams_larger_5(temp)

family_list<-(length(tax$family_list$species))
list<-c(species_touse)
tax<- fishtree_taxonomy(rank = species_touse)
tax$list$species
species_touse ="Stromateidae"
print(unlist(species_touse[1]))
length(tax$"Stromateidae"$species)
length(tax$species_touse$species)

tax$"Stromateidae"$species #returns what I want. 

tax[[species_touse]]$species #returns null 


tax$species_touse
?fishtree_taxonomy

