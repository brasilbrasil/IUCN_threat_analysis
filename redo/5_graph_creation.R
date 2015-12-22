rm(list = ls()) #remove all past worksheet variables

##run this code after merging all IUCN data
wd="D:/Dropbox/current work/IUCN_threat_analysis_redo/"
#wd="C:/Users/Kaipo Dye/Dropbox/PICCC/IUCN_threats_analysis_outputs/"
setwd(wd)
all_data_combined = read.csv(paste0("results/all_data_combined_onlySppWThreatInfo",".csv"), header = T, row.names = NULL, check.names = FALSE)

library(reshape2)
library(ggplot2)
#keep only species that have at least 1 non-climatic threat (or else test will not
#be of augmentation)
#all_data_combined=all_data_combined[all_data_combined$n_nonCC_threat!=0,]  

##function to create ggplot bar graphs based on 2 independent factors, 1 dependent
##factor and specified dataset to use
create_bar_graph=function(ind_factors, dep_factor, dataset, string){
  mean_vals=aggregate(dataset[,dep_factor], list(dataset[,ind_factors[2]], dataset[,ind_factors[1]]), mean)

  names(mean_vals)=c(ind_factors[2], ind_factors[1], paste0("mean of ", dep_factor))
  dataset_wide=dcast(dataset, get(ind_factors[1]) + get(ind_factors[2]) ~ ., value.var=dep_factor, fun.aggregate = mean, na.rm = TRUE)
  names(dataset_wide)=c(ind_factors, dep_factor)
  a=ggplot(dataset_wide, aes(x=get(ind_factors[1]), y=get(dep_factor), fill=get(ind_factors[2]))) +   
    geom_bar(aes(fill = get(ind_factors[2])), position = "dodge", stat="identity")
  a=a+xlab(ind_factors[1]) + ylab(dep_factor) + theme(legend.title=element_blank())
  a=a + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  a  
  ggsave(a, file=paste0("results/barplots/barplot_for_", string,"_of_",dep_factor,"_by_", ind_factors[1], "_and_", ind_factors[2], ".tiff"), width=6, height=4, compression = "lzw")
}


dep_factor="n_nonCC_threat"
##all species
dataset=all_data_combined
string="allSpp"
ind_factors=c("Red.List.status", "CC_threat")  ##Red.List.status/Phylum/status(geog)/Year.assessed/Kingdom
create_bar_graph(ind_factors, dep_factor, dataset, string)
ind_factors=c("Phylum", "CC_threat")  ##Red.List.status/Phylum/status(geog)/Year.assessed/Kingdom
create_bar_graph(ind_factors, dep_factor, dataset, string)
ind_factors=c("Kingdom", "CC_threat")  ##Red.List.status/Phylum/status(geog)/Year.assessed/Kingdom
create_bar_graph(ind_factors, dep_factor, dataset, string)
ind_factors=c("Year.assessed", "CC_threat")  ##Red.List.status/Phylum/status(geog)/Year.assessed/Kingdom
create_bar_graph(ind_factors, dep_factor, dataset, string)  

##all animals
dataset=all_data_combined[all_data_combined$Kingdom=="ANIMALIA",]
string="Animalia"
ind_factors=c("Red.List.status", "CC_threat")  ##Red.List.status/Phylum/status(geog)/Year.assessed/Kingdom
create_bar_graph(ind_factors, dep_factor, dataset, string)
ind_factors=c("Phylum", "CC_threat")  ##Red.List.status/Phylum/status(geog)/Year.assessed/Kingdom
create_bar_graph(ind_factors, dep_factor, dataset, string)
ind_factors=c("Year.assessed", "CC_threat")  ##Red.List.status/Phylum/status(geog)/Year.assessed/Kingdom
create_bar_graph(ind_factors, dep_factor, dataset, string)  

##all plants
dataset=all_data_combined[all_data_combined$Kingdom=="PLANTAE",]
string="Plantae"
ind_factors=c("Red.List.status", "CC_threat")  ##Red.List.status/Phylum/status(geog)/Year.assessed/Kingdom
create_bar_graph(ind_factors, dep_factor, dataset, string)
ind_factors=c("Phylum", "CC_threat")  ##Red.List.status/Phylum/status(geog)/Year.assessed/Kingdom
create_bar_graph(ind_factors, dep_factor, dataset, string)
ind_factors=c("Year.assessed", "CC_threat")  ##Red.List.status/Phylum/status(geog)/Year.assessed/Kingdom
create_bar_graph(ind_factors, dep_factor, dataset, string)  

###########################################################
###CALCULATE MAGNITUDE OF CLIMATE-BASED THREAT AUGMENTATION
create_threat_augmentatio_table=function(ind_factor, dataset, string){
  mean_vals=aggregate(dataset[,"n_nonCC_threat"], list(dataset[,"CC_threat"], dataset[,ind_factor]), mean)
  sd_vals=aggregate(dataset[,"n_nonCC_threat"], list(dataset[,"CC_threat"], dataset[,ind_factor]), sd)
  count_vals=aggregate(dataset[,"n_nonCC_threat"], list(dataset[,"CC_threat"], dataset[,ind_factor]), length)
  all_vals=cbind(mean_vals,sd=sd_vals[,3],count=count_vals[,3])
  names(all_vals)[3]="mean"
  
  library(reshape2)
  mean_vals1=dcast(all_vals, Group.2 ~ Group.1, value.var="mean")
  names(mean_vals1)[2:3]=paste0("mean_", names(mean_vals1)[2:3])
  sd_vals1=dcast(all_vals, Group.2 ~ Group.1, value.var="sd")
  names(sd_vals1)[2:3]=paste0("sd_", names(sd_vals1)[2:3])
  count_vals1=dcast(all_vals, Group.2 ~ Group.1, value.var="count")
  names(count_vals1)[2:3]=paste0("count_", names(count_vals1)[2:3])
  all_vals1=cbind(mean_vals1, sd_vals1[,2:3], count_vals1[,2:3])
  all_vals1=cbind(all_vals1, CCaug=all_vals1[,2]/all_vals1[,3])
  names(all_vals1)[1]=ind_factor
  #jnk=apply(mean_vals1,1,function(x) all(!is.na(x))) #which rows have no missing vals?
  filename=paste0("results/climateThreatAugmentationTable_for_", string,"_grouped_by_", ind_factor, ".csv")
  write.table(all_vals1, file = filename, sep=",", row.names = FALSE)  
}
dataset=all_data_combined
string="allSpp"
create_threat_augmentatio_table("Red.List.status", dataset, string)
create_threat_augmentatio_table("Phylum", dataset, string)
create_threat_augmentatio_table("Kingdom", dataset, string)


##################################################
###CREATE HISTOGRAMS TO INSPECT DATA ACROSS GROUPS
multiHistFx=function(histDf, xcol, fillcol, txtID, x_lim=NULL){
  if (is.null(x_lim)){
   x_lim=max(histDf[,xcol]) 
  }
  mean_vals=aggregate(histDf[,xcol], list(histDf[,fillcol]), mean)
  #mean_vals=data.frame(mean_vals, cols=letters[1:(dim(mean_vals)[1])])
  a=ggplot(histDf, aes(get(xcol), ..count.., fill = get(fillcol))) + xlim(0, x_lim) + 
    geom_histogram(binwidth=1, alpha=1, position="dodge")+xlab("Number of non-climatic threats")+ylab("Count")+
    geom_vline(data = mean_vals,aes(xintercept=x, colour=Group.1), show.legend = FALSE)
  b=ggplot(histDf, aes(get(xcol), ..density.., fill = get(fillcol))) + xlim(0, x_lim) + 
    geom_histogram(binwidth=1, alpha=1, position="dodge")+xlab("Number of non-climatic threats")+ylab("Density of counts")+  
    geom_vline(data = mean_vals,aes(xintercept=x, colour=Group.1), show.legend = FALSE)
  a=a+theme(legend.title=element_blank())
  b=b+theme(legend.title=element_blank())
  ggsave(a, file=paste0("results/hists_and_counts/hist_for_", txtID,"_of_",xcol,"_counts_by_", fillcol, ".tiff"), width=6, height=4, compression = "lzw")
  ggsave(b, file=paste0("results/hists_and_counts/hist_for_", txtID,"_of_",xcol,"_density_by_", fillcol, ".tiff"), width=6, height=4, compression = "lzw")  
}

###histograms across all species
xcol="n_nonCC_threat"
fillcol="Red.List.status"
txtID="allSpp"
histDf=all_data_combined
multiHistFx(histDf, xcol, fillcol, txtID, x_lim=30)

xcol="n_nonCC_threat"
fillcol="Kingdom"
txtID="allSpp"
histDf=all_data_combined
multiHistFx(histDf, xcol, fillcol, txtID, x_lim=30)

#MS figure 1
xcol="n_nonCC_threat"
fillcol="CC_threat"
txtID="allSpp"
histDf2=all_data_combined
levels(histDf2$CC_threat) <- c('climate vulnerable', 'not climate vulnerable')
multiHistFx(histDf2, xcol, fillcol, txtID, x_lim=30)

###histograms animals only
txtID="allAnimals"
histDf=all_data_combined[all_data_combined$Kingdom=="ANIMALIA",]
multiHistFx(histDf, xcol, fillcol, txtID)

txtID="allCRAnimals"
histDf=all_data_combined[all_data_combined$Kingdom=="ANIMALIA",]
histDf=histDf[histDf$Red.List.status=="CR",]
multiHistFx(histDf, xcol, fillcol, txtID)

txtID="allENAnimals"
histDf=all_data_combined[all_data_combined$Kingdom=="ANIMALIA",]
histDf=histDf[histDf$Red.List.status=="EN",]
multiHistFx(histDf, xcol, fillcol, txtID)

txtID="allLCAnimals"
histDf=all_data_combined[all_data_combined$Kingdom=="ANIMALIA",]
histDf=histDf[histDf$Red.List.status=="LC",]
multiHistFx(histDf, xcol, fillcol, txtID)

###histograms plants only
txtID="allPlants"
histDf=all_data_combined[all_data_combined$Kingdom=="PLANTAE",]
multiHistFx(histDf, xcol, fillcol, txtID)

txtID="allCRPlants"
histDf=all_data_combined[all_data_combined$Kingdom=="PLANTAE",]
histDf=histDf[histDf$Red.List.status=="CR",]
multiHistFx(histDf, xcol, fillcol, txtID)

txtID="allENPlants"
histDf=all_data_combined[all_data_combined$Kingdom=="PLANTAE",]
histDf=histDf[histDf$Red.List.status=="EN",]
multiHistFx(histDf, xcol, fillcol, txtID)

txtID="allLCPlants"
histDf=all_data_combined[all_data_combined$Kingdom=="PLANTAE",]
histDf=histDf[histDf$Red.List.status=="LC",]
multiHistFx(histDf, xcol, fillcol, txtID)


