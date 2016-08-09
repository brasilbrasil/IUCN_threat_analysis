#losely based on 
#http://thebiobucket.blogspot.com/2012/06/use-iucn-data-with-r-xpath.html
#wd="C:/Users/Kaipo Dye/Dropbox/PICCC/IUCN_threats_analysis_outputs/"
wd="D:/Dropbox/current work/IUCN_threat_publication/IUCN_test_analysis_results20160621/"
setwd(wd)
require(XML)
library(stringi)
library(stringr)

species_type="all_species" #least concern/ #critically endangered/endangered
spp_info = read.csv(paste0("data/",species_type,".csv")) #
Species_IDs=spp_info[,"Species.ID"]

code = Species_IDs[1]
for (code in Species_IDs){
  cat("doing ", code,  "\n")
  code_str=str_pad(code, 8, pad = "0")
  filenm=paste0("sp_threat_info/",species_type,"_", code_str,".csv")  
  sp_data=read.csv(filenm)
  if (code == Species_IDs[1]){
    spp_data=sp_data
  }else{
    spp_data=rbind(spp_data, sp_data)
  }
}
#remove Xs from col names
#spp_data = read.csv(paste0("threat_matrix_",species_type,".csv"), header = T, row.names = NULL, check.names = FALSE) #
colnames(spp_data)[2:ncol(spp_data)]=sapply(strsplit(colnames(spp_data)[2:ncol(spp_data)], "X"), "[", 2)
filenm=paste0("data/threat_matrix_",species_type,".csv")  
write.table(spp_data, file = filenm, sep=",", row.names = FALSE)    
