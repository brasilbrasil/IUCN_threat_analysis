##run this code after merging all IUCN data
#wd="D:/Dropbox/current work/IUCN_threats_analysis_outputs/"
wd="C:/Users/Kaipo Dye/Dropbox/PICCC/IUCN_threats_analysis_outputs/"

setwd(wd)
all_data_combined = read.csv(paste0("resultsNEW/all_data_combined",".csv"), header = T, row.names = NULL, check.names = FALSE)
all_data_combined_wThreat = read.csv(paste0("resultsNEW/all_data_combined_onlySppWThreatInfo",".csv"), header = T, row.names = NULL, check.names = FALSE)
library(reshape2)
library(ggplot2)

##function to create ggplot bar graphs based on 2 independent factors, 1 dependent
##factor and specified dataset to use
create_bar_graph=function(ind_factors, dep_factor, dataset){
  dataset_wide=dcast(dataset, get(ind_factors[1]) + get(ind_factors[2]) ~ ., value.var=dep_factor, fun.aggregate = mean, na.rm = TRUE)
  names(dataset_wide)=c(ind_factors, dep_factor)
  a=ggplot(dataset_wide, aes(x=get(ind_factors[1]), y=get(dep_factor), fill=get(ind_factors[2]))) +   
    geom_bar(aes(fill = get(ind_factors[2])), position = "dodge", stat="identity")
  a=a+xlab(ind_factors[1]) + ylab(dep_factor) + theme(legend.title=element_blank())
  a=a + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  a  
  ggsave(a, file=paste0("resultsNEW/barplot_of_",dep_factor,"_by_", ind_factors[1], "_and_", ind_factors[2], ".tiff"), width=6, height=4)
}


ind_factors=c("Year.assessed", "CC_threat") ##Red.List.status/Phylum/status(geog)/Year.assessed/Kingdom
dep_factor="n_nonCC_threat"
dataset=all_data_combined_wThreat
create_bar_graph(ind_factors, dep_factor, dataset)

# #test showing erroneous ggplot graph (DO NOT USE!)
# a=ggplot(all_data_combined, aes(x=CC_threat, y=n_nonCC_threat, fill=status)) +   
#   geom_bar(aes(fill = status), position = "dodge", stat="identity")
# a
names(all_data_combined_wThreat)
names(all_data_combined)


#CC threats per year (hist)
#number of threats assessed have been going up, but not necessarily a climate bias
##hist(all_data_combined_wThreat[all_data_combined_wThreat[,"n_threats"]>0,"n_threats"])
##hist(all_data_combined_wThreat[all_data_combined_wThreat[,"11"]>0,"n_threats"])
##plot(all_data_combined_wThreat[,"n_threats"], all_data_combined_wThreat[,"11"])
##all_threat_freq=as.data.frame(table(all_spp_data[all_data_combined_wThreat[,"n_threats"]>0,"Year.assessed"]))
##CC_threat_freq=as.data.frame(table(all_spp_data[all_data_combined_wThreat[,"11"]>0,"Year.assessed"]))
##names(all_threat_freq)=c("Threats", "All.Species.count")
##names(CC_threat_freq)=c("Threats", "CC.Species.count")

##all_threat_freq$Year=as.numeric(as.character(all_threat_freq$Year))
##CC_threat_freq$Year=as.numeric(as.character(CC_threat_freq$Year))
##minYr=min(c(all_threat_freq$Year,CC_threat_freq$Year))
##maxYr=max(c(all_threat_freq$Year,CC_threat_freq$Year))
##all_yr_freq=data.frame(Year=c(minYr:maxYr))
##c(minYr:maxYr) %in% all_threat_freq$Year
##all_yr_freq2=merge(all_yr_freq, all_threat_freq, by="Year", all.x =T)
##all_yr_freq2=merge(all_yr_freq2, CC_threat_freq, by="Year", all.x =T)
##all_yr_freq2[is.na(all_yr_freq2)]=0

library(plyr)
library(reshape2)
# melt the data frame for plotting
data.m <- melt(all_yr_freq2, id.vars='Threats')

library(ggplot2)
# plot everything
ggplot(data.m, aes(Threats, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity")

plot(all_yr_freq2$All.Species.count, all_yr_freq2$CC.Species.count)

#CC threats per year plot
plot(as.numeric(all_spp_data$Year.assessed), all_threat_data_summary[,"11"])


