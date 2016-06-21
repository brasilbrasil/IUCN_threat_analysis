rm(list = ls()) #remove all past worksheet variables

##run this code after merging all IUCN data
#wd="D:/Dropbox/current work/IUCN_threats_analysis_outputs/"
wd="D:/Dropbox/current work/IUCN_threat_publication/IUCN_threat_analysis_redo/"
setwd(wd)
megamat_filename=paste0("results/all_data_combined_onlySppWThreatInfo",".csv")
remove_zero_nonccthreats=F

library(reshape2)
library(ggplot2)

#threats=c(1:12)
threats=c(1:9, 11)
threats=as.character(threats)

create_threat_augmentatio_table=function(ind_factor, dataset, string){
  mean_vals=aggregate(dataset[,"other_threat_count"], list(dataset[,"threat"], dataset[,ind_factor]), mean)
  sd_vals=aggregate(dataset[,"other_threat_count"], list(dataset[,"threat"], dataset[,ind_factor]), sd)
  count_vals=aggregate(dataset[,"other_threat_count"], list(dataset[,"threat"], dataset[,ind_factor]), length)
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
  all_vals1=cbind(all_vals1, Threat_Aug=all_vals1[,3]/all_vals1[,2])
  names(all_vals1)[1]=ind_factor
  #jnk=apply(mean_vals1,1,function(x) all(!is.na(x))) #which rows have no missing vals?
  filename=paste0("results/threat_augmentation_comparison/ThreatAugmentationTable_for_threat_",threat, "_and_", string,"_grouped_by_", ind_factor, ".csv")
  write.table(all_vals1, file = filename, sep=",", row.names = FALSE)  
}

threat="11"
for (threat in threats){
  all_data_combined = read.csv(megamat_filename, header = T, row.names = NULL, check.names = FALSE)
  jnk=all_data_combined[,threat]>0
  other_threats=threats[!(threats %in% threat)]
  other_threat_count=apply(all_data_combined[,other_threats], 1, sum)
  all_data_combined=cbind(all_data_combined,threat=jnk, other_threat_count, all="all")
  if (remove_zero_nonccthreats) all_data_combined=all_data_combined[all_data_combined$other_threat_count!=0,]  
  
  #all_data_combined
  
  ###########################################################
  ###CALCULATE MAGNITUDE OF CLIMATE-BASED THREAT AUGMENTATION
  dataset=all_data_combined
  string="allSpp"
  ind_factor="all"
  create_threat_augmentatio_table("Red.List.status", dataset, string)
  #create_threat_augmentatio_table("Phylum", dataset, string)
  create_threat_augmentatio_table("Kingdom", dataset, string)  
  create_threat_augmentatio_table("all", dataset, string)  
}

#merge results
ind_factor="all"
threat="1"
i=1
threat_names=c("Residential & commercial development", "Agriculture & aquaculture", "Energy production & mining",
               "Transportation & service corridors", "Biological resource use", "Human intrusions & disturbance",
               "Natural system modifications", "Invasive & other problematic species, genes & diseases",
               "Pollution", "Geological events", "Climate change & severe weather", "Other threats")
# threat_names=c("Residential & commercial development", "Agriculture & aquaculture", "Energy production & mining",
#                "Transportation & service corridors", "Biological resource use", "Human intrusions & disturbance",
#                "Natural system modifications", "Invasive & other problematic species, genes & diseases",
#                "Pollution", "Climate change & severe weather")

for (threat in threats){
  filename=paste0("results/threat_augmentation_comparison/ThreatAugmentationTable_for_threat_",threat, "_and_", string,"_grouped_by_", ind_factor, ".csv")
  jnk = read.csv(filename, header = T, row.names = NULL, check.names = FALSE)
  jnk = cbind(threat=threat_names[as.numeric(threat)],jnk)
  if (i==1){
    combined=jnk
  }else{
    combined=rbind(combined, jnk)
  }
  i=i+1
}

filename=paste0("results/threat_augmentation_comparison/Augmentation_across_all_threats_for_", string, ".csv")
write.table(combined, file = filename, sep=",", row.names = FALSE)  


