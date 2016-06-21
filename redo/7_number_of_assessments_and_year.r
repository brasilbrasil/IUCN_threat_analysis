###MUST SAVE TABLE AND IMAGE OUTPUTS!

wd="D:/Dropbox/current work/IUCN_threat_publication/IUCN_threat_analysis_redo/"
#wd="C:/Users/Kaipo Dye/Dropbox/PICCC/IUCN_threats_analysis_outputs/"
setwd(wd)
library(stringi)
# species_types=c("critically endangered", "endangered", "least concern")
# species_types_short=c("CR", "EN", "LC")
# 
# for (i in 1:length(species_types)){
#   species_type=species_types[i]
#   if (i==1){
#     all_spp_data=read.csv(paste0("data/",species_type,".csv"))#
#     all_spp_data=cbind(all_spp_data, status=species_types_short[i])
#   }else{
#     jnk=read.csv(paste0("data/",species_type,".csv")) #
#     jnk=cbind(jnk, status=species_types_short[i])
#     all_spp_data=rbind(all_spp_data, jnk)    
#   }
# }
# 
# #all_threats
# for (i in 1:length(species_types)){
#   species_type=species_types[i]
#   if (i==1){
#     all_threat_data=read.csv(paste0("threat_matrix_",species_type,".csv"), header = T, row.names = NULL, check.names = FALSE)#
#     #all_threat_data=cbind(all_threat_data, status=species_types_short[i])
#   }else{
#     jnk=read.csv(paste0("threat_matrix_",species_type,".csv"), header = T, row.names = NULL, check.names = FALSE)#
#     #jnk=cbind(jnk, status=species_types_short[i])
#     all_threat_data=rbind(all_threat_data, jnk)    
#   }
# }

all_spp_data=read.csv(paste0("data/all_species.csv"))#
all_threat_data=read.csv(paste0("data/threat_matrix_all_species.csv"), header = T, row.names = NULL, check.names = FALSE)#

threats=as.character(c(1:12))
jnk=c(names(all_threat_data)[1],threats)
all_threat_data_summary=all_threat_data[,jnk]
n_threats=rowSums(all_threat_data[,threats])
all_threat_data_summary=cbind(all_threat_data_summary,n_threats)
#

#total threats per year
plot(as.numeric(all_spp_data$Year.assessed), all_threat_data_summary$n_threats)

#CC threats per year (hist)
#number of threats assessed have been going up, but not necessarily a climate bias
hist(all_spp_data[all_threat_data_summary[,"n_threats"]>0,"Year.assessed"])
hist(all_spp_data[all_threat_data_summary[,"11"]>0,"Year.assessed"])
plot(all_threat_data_summary[,"n_threats"], all_threat_data_summary[,"11"])
all_threat_freq=as.data.frame(table(all_spp_data[all_threat_data_summary[,"n_threats"]>0,"Year.assessed"]))
CC_threat_freq=as.data.frame(table(all_spp_data[all_threat_data_summary[,"11"]>0,"Year.assessed"]))
names(all_threat_freq)=c("Year", "All.Species.count")
names(CC_threat_freq)=c("Year", "CC.Species.count")
all_threat_freq$Year=as.numeric(as.character(all_threat_freq$Year))
CC_threat_freq$Year=as.numeric(as.character(CC_threat_freq$Year))
minYr=min(c(all_threat_freq$Year,CC_threat_freq$Year))
maxYr=max(c(all_threat_freq$Year,CC_threat_freq$Year))
all_yr_freq=data.frame(Year=c(minYr:maxYr))
#c(minYr:maxYr) %in% all_threat_freq$Year
all_yr_freq2=merge(all_yr_freq, all_threat_freq, by="Year", all.x =T)
all_yr_freq2=merge(all_yr_freq2, CC_threat_freq, by="Year", all.x =T)
all_yr_freq2[is.na(all_yr_freq2)]=0

library(plyr)
library(reshape2)
# melt the data frame for plotting
data.m <- melt(all_yr_freq2, id.vars='Year')

library(ggplot2)
# plot everything
#MS appendix 5
data.m2=data.m
levels(data.m2$variable) <- c('All species', 'Climate vulnerable species')
a=ggplot(data.m2, aes(Year, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity")+ylab("Number of IUCN species assessments")+
  theme(legend.title=element_blank())
a
ggsave(a, file=paste0("results/graphs/n_species_assessed_by_year.tiff"), width=6, height=4, compression = "lzw")

plot(all_yr_freq2$All.Species.count, all_yr_freq2$CC.Species.count)

#CC threats per year plot
plot(as.numeric(all_spp_data$Year.assessed), all_threat_data_summary[,"11"])

#two way anova
aovData=cbind(all_spp_data, all_threat_data_summary)
aovData=cbind(aovData, CC_threat=aovData[,"11"]>0)
aovData_wThreat=aovData[aovData$n_threats>0,]
int <- aov(aovData_wThreat$n_threats ~ aovData_wThreat$CC_threat*aovData_wThreat$Red.List.status*aovData_wThreat$Year.assessed)
#int <- aov(aovData_wThreat$n_threats ~ aovData_wThreat$CC_threat*aovData_wThreat$status)
summary(int)
plot(int)

glmFit=glm(aovData_wThreat$n_threats ~ aovData_wThreat$CC_threat*aovData_wThreat$status)
summary(glmFit)
plot(glmFit)

