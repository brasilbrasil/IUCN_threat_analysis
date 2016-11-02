#wd="D:/Dropbox/current work/IUCN_threats_analysis_outputs/"
#wd="C:/Users/Kaipo Dye/Dropbox/IUCN_test_analysis_results20160621/"
wd="D:/Dropbox/current work/IUCN_threat_publication/IUCN_test_analysis_results20160621/"
setwd(wd)
IUCN_cats=c("LC", "NT", "VU", "EN", "CR")
kingdoms=c("", "ANIMALIA", "PLANTAE")
proj_names=c("all", "CC", "nCC")
groups=c("none", "single", "multiple")

species_type=IUCN_cats[1]
proj_name=proj_names[1]
group=groups[1]
for (species_type in IUCN_cats){
  for (kingdom in kingdoms){
    for (proj_name in proj_names){  
      name = paste0("results/hists_and_counts/", kingdom, proj_name, "_threat_count_distr_summary_",species_type,"_species.csv") #names jpeg file to be created
      table=read.csv(name, header = T, row.names = NULL, check.names = FALSE) #
      table=cbind(kingdom, proj_name, species_type, table)
      for (group in groups){
        jnk=table[1,group]/sum(table[1,groups])
        table=cbind(table, jnk)
        names(table)[dim(table)[2]]=paste0(group,"_prop")
      }    
      if ((species_type==IUCN_cats[1]) & (proj_name==proj_names[1]) & (kingdom==kingdoms[1])){
        all_tables=table
      }else{
        all_tables=rbind(all_tables, table)
      }
    }    
  }
}  
#http://www.cyclismo.org/tutorial/R/confidence.html
error=qt(0.975,df=(all_tables$non_zero_n-1)*all_tables$sd.n.threats.nonZero/sqrt(all_tables$non_zero_n-1))
all_tables=cbind(all_tables, upper=all_tables$mean.n.threats.nonZero+error)
all_tables=cbind(all_tables, lower=all_tables$mean.n.threats.nonZero-error)
write.table(all_tables, file = paste0("results/all_threat_count_tables",".csv"), sep=",", row.names = FALSE)

for (species_type in IUCN_cats){
  for (kingdom in kingdoms){
    name = paste0("results/ttests/", "ttest_n_nCC_trheats_by_CC_nCC_threatened_",kingdom, species_type,"_species.csv") #names jpeg file to be created
    table=read.csv(name, header = T, row.names = NULL, check.names = FALSE) #
    table=cbind(kingdom, species_type, table)
    if ((species_type==IUCN_cats[1]) & (kingdom==kingdoms[1])){
      all_ttest_tables=table
    }else{
      all_ttest_tables=rbind(all_ttest_tables, table)
    }
  }
}
write.table(all_ttest_tables, file = paste0("results/all_t.test_comparisons",".csv"), sep=",", row.names = FALSE)

all_tables=all_tables[all_tables$proj_name!="all",]
kingdom=kingdoms[1]
for (kingdom in kingdoms){
  all_tables_kingdom=all_tables[all_tables$kingdom==kingdom,]
  
  library(ggplot2)
  # plot proportion of multiple threat species
  #   a=ggplot(all_tables_kingdom, aes(x=species_type, y=multiple_prop, fill=proj_name)) +   
  #     geom_bar(aes(fill = proj_name), position = "dodge", stat="identity")
  #   a
  #   ggsave(a, file=paste0("results/graphs/", kingdom,"proportion_of_species_under_multiple_threats.tiff"), width=6, height=4)
  #   
  #   
  #   # plot proportion of single threat species
  #   a=ggplot(all_tables_kingdom, aes(x=proj_name, y=single_prop, fill=species_type)) +   
  #     geom_bar(aes(fill = species_type), position = "dodge", stat="identity")
  #   a
  #   ggsave(a, file=paste0("results/graphs/", kingdom,"proportion_of_species_under_single_threat.tiff"), width=6, height=4)
  
  # plot number of mean number of threats for species with threat info
  a=ggplot(all_tables_kingdom, aes(x=species_type, y=mean.n.threats.nonZero, fill=proj_name)) + 
    geom_bar(position=position_dodge(), stat="identity")+ ylab("Mean number of non-zero threats")
  a
  ggsave(a, file=paste0("results/graphs/mean.n.threats.nonZero", kingdom,".tiff"), width=6, height=4, compression = "lzw")
  
  a=ggplot(all_tables_kingdom, aes(x=species_type, y=mean.n.threats, fill=proj_name)) + 
    geom_bar(position=position_dodge(), stat="identity")+ ylab("Mean number of threats")
  a
  ggsave(a, file=paste0("results/graphs/mean.n.threats", kingdom,".tiff"), width=6, height=4, compression = "lzw")
  
  #   #with error
  #   a=ggplot(all_tables_kingdom, aes(x=species_type, y=mean.n.threats.nonZero, fill=proj_name)) + 
  #     geom_bar(position=position_dodge(), stat="identity") +
  #     geom_errorbar(aes(ymin=lower, ymax=upper),
  #                   width=.2,                    # Width of the error bars
  #                   position=position_dodge(.9))
  #   a
  #   ggsave(a, file=paste0("results/graphs/", kingdom,"mean.n.threats.nonZero.with.error.tiff"), width=6, height=4)
  
}

#calculate n of species in each IUCN/ sp group
all_counts=read.csv("results/all_data_combined_onlySppWThreatInfo.csv")
all_counts_few_cols=all_counts[, c("Kingdom", "Red.List.status", "X11")]
all_counts_few_cols$count_n=1
aggregate(count_n ~ Kingdom + Red.List.status, data = all_counts_few_cols, FUN = sum)


difference_table_for_graphs=all_ttest_tables[all_ttest_tables$kingdom!="",]
a=ggplot(difference_table_for_graphs, aes(x=species_type, y=(threat_increase*100-100), fill=kingdom)) +   
  geom_bar(aes(fill = kingdom), position = "dodge", stat="identity") + ylab("Percent difference in number of threats")
a
ggsave(a, file=paste0("results/graphs/", "threat_increase_all_species_by_kingdom_and_IUCN.tiff"), width=6, height=4, compression = "lzw")

a=ggplot(difference_table_for_graphs, aes(x=species_type, y=group.difference, fill=kingdom)) +   
  geom_bar(aes(fill = kingdom), position = "dodge", stat="identity")+ ylab("Difference in number of threats")
a
ggsave(a, file=paste0("results/graphs/", "threat_n_difference_all_species_by_kingdom_and_IUCN.tiff"), width=6, height=4, compression = "lzw")

a=ggplot(difference_table_for_graphs, aes(x=species_type, y=group.difference, fill=kingdom)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=group.difference.CI.low, ymax=group.difference.CI.high),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+ ylab("Difference in number of non-climatic threats") + xlab("IUCN ranking")
a
ggsave(a, file=paste0("results/graphs/", "threat_n_increase_all_species_95CI_by_kingdom_and_IUCN.tiff"), width=6, height=4, compression = "lzw")


# MS figure 2
difference_table_for_graphs=all_ttest_tables[all_ttest_tables$kingdom=="",]
a=ggplot(difference_table_for_graphs, aes(x=species_type, y=(threat_increase*100-100))) +   
  geom_bar(aes(), position = "dodge", stat="identity") + ylab("Percent increase in number of non-climatic threats") + xlab("IUCN Red List category")
a
ggsave(a, file=paste0("results/graphs/", "threat_increase_all_species_by_IUCN.tiff"), width=6, height=4, compression = "lzw")

# MS figure 2 with standard error bars
dodge <- position_dodge(width=0.9)
difference_table_for_graphs=all_ttest_tables[all_ttest_tables$kingdom=="",]
a=ggplot(difference_table_for_graphs, aes(x=species_type, y=(threat_increase*100-100))) +   
  geom_bar(aes(), position = dodge, stat="identity") + ylab("Percent increase in number of non-climatic threats") + xlab("IUCN Red List category")
a
limits=aes(ymin=((difference_table_for_graphs$group.difference.CI.low+difference_table_for_graphs$t.estimate.group2)*100/difference_table_for_graphs$t.estimate.group2)-100, ymax=((difference_table_for_graphs$group.difference.CI.high+difference_table_for_graphs$t.estimate.group2)*100/difference_table_for_graphs$t.estimate.group2)-100)
a=a+geom_errorbar(limits, width=.2, position=dodge)
a
ggsave(a, file=paste0("results/graphs/", "threat_increase_all_species_by_IUCN_95CI.tiff"), width=6, height=4, compression = "lzw")

a=ggplot(difference_table_for_graphs, aes(x=species_type, y=group.difference)) +   
  geom_bar(aes(), position = "dodge", stat="identity")+ ylab("Difference in number of threats")
a
ggsave(a, file=paste0("results/graphs/", "threat_n_difference_all_species_by_IUCN.tiff"), width=6, height=4, compression = "lzw")

a=ggplot(difference_table_for_graphs, aes(x=species_type, y=group.difference)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=group.difference.CI.low, ymax=group.difference.CI.high),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+ ylab("Difference in number of threats")
a
ggsave(a, file=paste0("results/graphs/", "threat_n_increase_all_species_95CI_by_IUCN.tiff"), width=6, height=4, compression = "lzw")


