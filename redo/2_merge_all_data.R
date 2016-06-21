rm(list = ls()) #remove all past worksheet variables
#This code can run after extracting threat info for each group
#wd="D:/Dropbox/current work/IUCN_threats_analysis_outputs/"
#wd="C:/Users/Kaipo Dye/Dropbox/PICCC/IUCN_threats_analysis_outputs/"
wd="D:/Dropbox/current work/IUCN_threat_publication/IUCN_threat_analysis_redo/"
setwd(wd)
library(stringi)
geographical=F
remove_LRlc=T

if (geographical){
  species_types=c("antartica", "carribean", "east_asia", "europe", "meso_amer", "nort_amer", "nort_asia", "oceania", "s_east_asia", "sout_amer", "sub_sahara", "w_cent_asia")
  species_types_short=c("ANT", "CAR", "EAS", "EU", "MA", "NA", "NAS", "OC", "SEA", "SA", "SUB", "CAS")
}else{
#   species_types=c("critically endangered", "endangered", "least concern")
#   species_types_short=c("CR", "EN", "LC")  
  species_types=c("all_species")
  species_types_short=c("all")  
}


for (i in 1:length(species_types)){
  species_type=species_types[i]
  if (i==1){
    all_spp_data=read.csv(paste0("data/",species_type,".csv"))#
    all_spp_data=cbind(all_spp_data, status=species_types_short[i])
  }else{
    jnk=read.csv(paste0("data/",species_type,".csv")) #
    jnk=cbind(jnk, status=species_types_short[i])
    all_spp_data=rbind(all_spp_data, jnk)    
  }
}

#all_threats
for (i in 1:length(species_types)){
  species_type=species_types[i]
  if (i==1){
    all_threat_data=read.csv(paste0("data/threat_matrix_",species_type,".csv"), header = T, row.names = NULL, check.names = FALSE)#
    #all_threat_data=cbind(all_threat_data, status=species_types_short[i])
  }else{
    jnk=read.csv(paste0("data/threat_matrix_",species_type,".csv"), header = T, row.names = NULL, check.names = FALSE)#
    #jnk=cbind(jnk, status=species_types_short[i])
    all_threat_data=rbind(all_threat_data, jnk)    
  }
}

#threats=as.character(c(1:12))
threats=as.character(c(1:9, 11))
jnk=c(names(all_threat_data)[1],threats)
all_threat_data_summary=all_threat_data[,jnk]
n_threats=rowSums(all_threat_data[,threats])
all_threat_data_summary=cbind(all_threat_data_summary,n_threats)

all_data_combined=cbind(all_spp_data, all_threat_data_summary)
all_data_combined=cbind(all_data_combined, CC_threat=all_data_combined[,"11"]>0)
all_data_combined=cbind(all_data_combined, n_nonCC_threat=all_data_combined$n_threats-all_data_combined[,"11"])
all_data_combined=cbind(all_data_combined, n_threat_cat=all_data_combined$n_threats)
all_data_combined[all_data_combined$n_threats>1,"n_threat_cat"]="Multiple"
all_data_combined[all_data_combined$n_threats==1,"n_threat_cat"]="Single"
all_data_combined[all_data_combined$n_threats==0,"n_threat_cat"]="None"
all_data_combined[all_data_combined$CC_threat==T,"CC_threat"]="Climate_vulnerable"
all_data_combined[all_data_combined$CC_threat==F,"CC_threat"]="Not_climate_vulnerable"

#remove species duplicates
jnk=duplicated(all_data_combined$Species.ID)
dup_vals=unique(all_data_combined$Species.ID[jnk])
all_data_combined=all_data_combined[!jnk,]

#remove species in LR/lc status as they have not been evaluated in long time (since 2000)
if (remove_LRlc){
  all_data_combined=all_data_combined[all_data_combined$Red.List.status!="LR/lc",]  
}else{
  all_data_combined[all_data_combined$Red.List.status=="LR/lc","Red.List.status"]="LC"    
}


#remove protists as they are few and not sure how assessed regarding cc
all_data_combined=all_data_combined[all_data_combined$Kingdom!="PROTISTA",]  

#remove FUNGI as they are few and not sure how assessed regarding cc
all_data_combined=all_data_combined[all_data_combined$Kingdom!="FUNGI",]  

#remove CHROMISTA kingdom (way too few):
all_data_combined=all_data_combined[all_data_combined$Kingdom!="CHROMISTA",]  

##all coral removed
all_data_combined=all_data_combined[all_data_combined$Phylum!="CNIDARIA",]

##keep only relevant IUCN categories
IUCN_cats=c("LC", "NT", "VU", "EN", "CR")
all_data_combined=all_data_combined[all_data_combined$Red.List.status %in% IUCN_cats,]


##remove Red.List.criteria and other columns (too messy, has led to errors reading large table)
to_remove=c('Red.List.criteria', 'Authority', 'Red.List.criteria.version', 'Infraspecific.rank', 'Infraspecific.name', 'Infraspecific.authority', 'Stock.subpopulation', 'Synonyms', 'Common.names..Eng.', 'Common.names..Fre.', 'Common.names..Spa.', 'Petitioned')
to_remove=names(all_data_combined) %in% to_remove
all_data_combined=all_data_combined[,!to_remove]

##remove species without threat info
all_data_combined=all_data_combined[all_data_combined$n_threats>0,]

##all species with multi regions with geo column recoded to "multi" region
if (geographical){
  jnk2=all_data_combined$Species.ID %in% dup_vals
  levels(all_data_combined[,"status"])=c(levels(all_data_combined[,"status"]),"multi")
  all_data_combined[jnk2,"status"]="multi"  
  write.table(all_data_combined, file = paste0("resultsCheck/all_data_combined_onlySppWThreatInfo_wGeog",".csv"), sep=",", row.names = FALSE)  
}else{
  write.table(all_data_combined, file = paste0("results/all_data_combined_onlySppWThreatInfo",".csv"), sep=",", row.names = FALSE)  
}
