wd="D:/Dropbox/current work/IUCN_threats_analysis_outputs/"
setwd(wd)
species_types=c("critically endangered", "endangered", "least concern")
proj_names=c("all", "CC", "nCC")
groups=c("none", "single", "multiple")

species_type=species_types[1]
proj_name=proj_names[1]
group=groups[1]
for (proj_name in proj_names){
  for (species_type in species_types){
    
    name = paste0("results/", proj_name, "_threat_count_distr_summary_",species_type,"_species.csv") #names jpeg file to be created
    table=read.csv(name, header = T, row.names = NULL, check.names = FALSE) #
    table=cbind(proj_name, species_type, table)
    for (group in groups){
      jnk=table[1,group]/sum(table[1,groups])
      table=cbind(table, jnk)
      names(table)[dim(table)[2]]=paste0(group,"_prop")
    }    
    if ((species_type==species_types[1]) & (proj_name==proj_names[1])){
      all_tables=table
    }else{
      all_tables=rbind(all_tables, table)
    }
  }
}  
all_tables=cbind(all_tables, upper=all_tables$mean.n.threats.nonZero+all_tables$sd.n.threats.nonZero)
all_tables=cbind(all_tables, lower=all_tables$mean.n.threats.nonZero-all_tables$sd.n.threats.nonZero)
write.table(all_tables, file = paste0("results/all_threat_count_tables",".csv"), sep=",", row.names = FALSE)



library(ggplot2)
# plot proportion of multiple threat species
a=ggplot(all_tables, aes(x=proj_name, y=multiple_prop, fill=species_type)) +   
  geom_bar(aes(fill = species_type), position = "dodge", stat="identity")
a
ggsave(a, file="results/proportion_of_species_under_multiple_threats.tiff", width=6, height=4)


# plot proportion of single threat species
a=ggplot(all_tables, aes(x=proj_name, y=single_prop, fill=species_type)) +   
  geom_bar(aes(fill = species_type), position = "dodge", stat="identity")
a
ggsave(a, file="results/proportion_of_species_under_single_threat.tiff", width=6, height=4)

# plot number of mean number of threats for species with threat info
a=ggplot(all_tables, aes(x=proj_name, y=mean.n.threats.nonZero, fill=species_type)) + 
  geom_bar(position=position_dodge(), stat="identity")
a
ggsave(a, file="results/mean.n.threats.nonZero.tiff", width=6, height=4)

#with error
a=ggplot(all_tables, aes(x=proj_name, y=mean.n.threats.nonZero, fill=species_type)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=lower, ymax=upper),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
a
ggsave(a, file="results/mean.n.threats.nonZero.with.error.tiff", width=6, height=4)

