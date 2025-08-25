### ECOLOGICAL GROUPS ###

eco.groups.neotoma = function(sitenum)
{
  taxon.name <- pollen_all[[sitenum]]$taxon.list$taxon.name
  ecological.group <- pollen_all[[sitenum]]$taxon.list$ecological.group
  df <- data.frame(taxon.name, ecological.group)
  eco.groups.neotoma = df
}


#make groupdf
groupdf = NULL
for(name in names(All_Age_Depth_Curves))
{
  sitenumdf = eco.groups.neotoma(name)
  if(name=="12"){
    groupdf = sitenumdf
  }
  else{
    groupdf = dplyr::bind_rows(groupdf, sitenumdf)
  }
}

groupdf <- unique(groupdf)
