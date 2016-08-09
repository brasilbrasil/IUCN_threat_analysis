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

#load threat info
threat_codes_table = read.csv("IUCN_threats.csv")
threat_codes=threat_codes_table[,"Code"]
threat_codes=as.character(threat_codes)
#create matrix
all_threats = data.frame(matrix(0, 1, length(threat_codes)+1, dimnames=list(c(), c("sp_code", threat_codes))), stringsAsFactors=F)
names(all_threats)=c("sp_code", threat_codes)
code=Species_IDs[1]
code=2917
#code=19672351
iucn_threat_fetch_fx=function(code){
  code_str=str_pad(code, 8, pad = "0")
  filenm=paste0("sp_threat_info/",species_type,"_", code_str,".csv")
  if (!file.exists(filenm)){
    cat('\n',"doing species ", code)
    all_threats1=all_threats
    all_threats1[1,"sp_code"]=code
    
    j<-htmlParse(paste0("http://www.iucnredlist.org/details/classify/",code,"/0#threats")) 
    #http://www.iucnredlist.org/details/classify/42285/0#threats
    threats <-xpathSApply(j, '//div[@id="threats"]', xmlValue)
    if (length(threats)>0){
      #threats=str_replace_all(threats,"\n"," ")
      threats=stri_replace_all_charclass(threats, "\\p{WHITE_SPACE}", " ")
      threat = threat_codes[1]
      for (threat in threat_codes){
        threat_str=paste0(" ",threat,". ")
        if (grepl(threat_str, threats)){
          all_threats1[1,threat]=str_count(threats,threat_str)
        }      
      }
    }else{
      cat('\n',"no threat data")    
    }
    write.table(all_threats1, file = filenm, sep=",", row.names = FALSE)    
  }
}

require(snowfall)
cpucores=as.integer(Sys.getenv('NUMBER_OF_PROCESSORS'))
sfInit( parallel=T, cpus=4) # 
sfLibrary(stringr)
sfLibrary(stringi)
sfLibrary(XML)
sfExportAll() 
sfLapply(Species_IDs,fun=iucn_threat_fetch_fx) #extra function arguments go after the first two sfLapply arguments
sfRemoveAll()
sfStop()

