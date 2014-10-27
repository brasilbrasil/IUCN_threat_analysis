wd="D:/Dropbox/current work/FB SDM/IUCN_threats/"
setwd(wd)
library(stringi)

species_types=c("critically endangered", "endangered", "least concern")
species_type="critically endangered" #least concern/ #critically endangered/endangered
for (species_type in species_types){
  threat_matrix = read.csv(paste0("threat_matrix_",species_type,".csv"), header = T, row.names = NULL, check.names = FALSE) #
  threats=as.character(c(1:12))
  jnk=c(names(threat_matrix)[1],threats)
  threat_matrix_summary=threat_matrix[,jnk]
  n_threats=rowSums(threat_matrix[,threats])
  threat_matrix_summary=cbind(threat_matrix_summary,n_threats)
  #most_threatened_sp=threat_matrix_summary[which(n_threats==max(n_threats)),names(threat_matrix)[1]]
  
  threat_summary_fx=function(threat_matrix_summary, proj_name){
    jpeg_name = paste0("results/", proj_name, "_histogram_of_number_of_threats_",species_type,"_species.jpg") #names jpeg file to be created
    jpeg(jpeg_name, #creates blank jpeg file in working directory
         width = 10, height = 10, units = "in", pointsize = 12, quality = 90, bg = "white", res = 300)
    hist_vals=hist(threat_matrix_summary[,"n_threats"],breaks=c((0-0.5):(max(n_threats)+1)), main=NULL, xlab="Number of identified threats", ylab=paste0("number of ",species_type, " species"))
    dev.off() #saves file to jpeg
    
    threat_count_distr=data.frame(n.threats=hist_vals$mids, count=hist_vals$counts)
    name = paste0("results/", proj_name, "_threat_count_distr_",species_type,"_species.csv") #names jpeg file to be created
    write.table(threat_count_distr, file = name, sep=",", row.names = FALSE)
    threat_count_distr_summary=data.frame(none=threat_count_distr[1,2], single=threat_count_distr[2,2],
                                          multiple=sum(threat_count_distr[3:(dim(threat_count_distr)[1]),2]),
                                          mean.n.threats=mean(as.matrix(threat_matrix_summary[,"n_threats"])),
                                          mean.n.threats.nonZero=mean(as.matrix(threat_matrix_summary[threat_matrix_summary[,"n_threats"]>0,"n_threats"])),
                                          sd.n.threats=sd(as.matrix(threat_matrix_summary[,"n_threats"])),
                                          sd.n.threats.nonZero=sd(as.matrix(threat_matrix_summary[threat_matrix_summary[,"n_threats"]>0,"n_threats"])))
    name = paste0("results/", proj_name, "_threat_count_distr_summary_",species_type,"_species.csv") #names jpeg file to be created
    write.table(threat_count_distr_summary, file = name, sep=",", row.names = FALSE)
    #meanNthreats=apply(threat_matrix_summary[,2:dim(threat_matrix_summary)[2]],2,mean)
    #meanNthreats_nonZero=apply(threat_matrix_summary[threat_matrix_summary[,"n_threats"]>0,2:dim(threat_matrix_summary)[2]],2,mean)  
  }
  
  threat_summary_fx(threat_matrix_summary, "all")
  
  #climate change
  clim_threat_matrix_summary=threat_matrix_summary[threat_matrix_summary[,"11"]>0,]
  threat_summary_fx(clim_threat_matrix_summary, "CC")
  
  #non climate change non zero
  nclim_threat_matrix_summary=threat_matrix_summary[threat_matrix_summary[,"11"]==0,]
  nclim_threat_matrix_summary=nclim_threat_matrix_summary[nclim_threat_matrix_summary[,"n_threats"]>0,]
  threat_summary_fx(nclim_threat_matrix_summary, "nCC")
  
}