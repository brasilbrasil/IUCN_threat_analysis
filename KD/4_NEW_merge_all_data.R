#This code can run after extracting threat info for each group
#wd="D:/Dropbox/current work/IUCN_threats_analysis_outputs/"
wd="C:/Users/Kaipo Dye/Dropbox/PICCC/IUCN_threats_analysis_outputs/"
setwd(wd)
library(stringi)
species_types=c("antartica", "carribean", "east_asia", "europe", "meso_amer", "nort_amer", "nort_asia", "oceania", "s_east_asia", "sout_amer", "sub_sahara", "w_cent_asia")
species_types_short=c("ANT", "CAR", "EAS", "EU", "MA", "NA", "NAS", "OC", "SEA", "SA", "SUB", "CAS")

for (i in 1:length(species_types)){
  species_type=species_types[i]
  if (i==1){
    all_spp_data=read.csv(paste0("dataGEO/",species_type,".csv"))#
    all_spp_data=cbind(all_spp_data, status=species_types_short[i])
  }else{
    jnk=read.csv(paste0("dataGEO/",species_type,".csv")) #
    jnk=cbind(jnk, status=species_types_short[i])
    all_spp_data=rbind(all_spp_data, jnk)    
  }
}

#all_threats
for (i in 1:length(species_types)){
  species_type=species_types[i]
  if (i==1){
    all_threat_data=read.csv(paste0("threat_matrix_",species_type,".csv"), header = T, row.names = NULL, check.names = FALSE)#
    #all_threat_data=cbind(all_threat_data, geography=species_types_short[i])
  }else{
    jnk=read.csv(paste0("threat_matrix_",species_type,".csv"), header = T, row.names = NULL, check.names = FALSE)#
    #jnk=cbind(jnk, geography=species_types_short[i])
    all_threat_data=rbind(all_threat_data, jnk)    
  }
}

threats=as.character(c(1:12))
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
all_data_combined[all_data_combined$CC_threat==T,"CC_threat"]="Climate_threatened"
all_data_combined[all_data_combined$CC_threat==F,"CC_threat"]="Not_climate_threatened"

##all geo dupes removed
jnk=duplicated(all_data_combined$Species.ID)
dup_vals=unique(all_data_combined$Species.ID[jnk])
all_data_combined=all_data_combined[!jnk,]
jnk2=all_data_combined$Species.ID %in% dup_vals
levels(all_data_combined[,"status"])=c(levels(all_data_combined[,"status"]),"multi")
all_data_combined[jnk2,"status"]="multi"

##all coral removed
all_data_combined=all_data_combined[all_data_combined$Phylum!="CNIDARIA",]

all_data_combined_wThreat=all_data_combined[all_data_combined$n_threats>0,]

write.table(all_data_combined, file = paste0("resultsNEW/all_data_combined",".csv"), sep=",", row.names = FALSE)
write.table(all_data_combined_wThreat, file = paste0("resultsNEW/all_data_combined_onlySppWThreatInfo",".csv"), sep=",", row.names = FALSE)

