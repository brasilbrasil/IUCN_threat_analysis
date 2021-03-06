#losely based on 
#http://thebiobucket.blogspot.com/2012/06/use-iucn-data-with-r-xpath.html

wd="D:/Dropbox/current work/FB SDM/IUCN_threats/"
setwd(wd)
require(XML)
#require(maptools)
#require(jpeg)

spp_info = read.csv("endangered.csv")
Species_IDs=spp_info[,"Species.ID"]

#load threat info
threat_codes_table = read.csv("IUCN_threats.csv")
threat_codes=threat_codes_table[,"Code"]
threat_codes=as.character(threat_codes)
#create matrix
all_threats = data.frame(matrix(0, length(Species_IDs), length(threat_codes)+1, dimnames=list(c(), c("sp_code", threat_codes))), stringsAsFactors=F)
all_threats[,"sp_code"]=Species_IDs
names(all_threats)=c("sp_code", threat_codes)
code=Species_IDs[1]
code=19672351
for (code in Species_IDs){
  cat('\n',"doing species ", code)
  j<-htmlParse(paste0("http://www.iucnredlist.org/details/classify/",code,"/0#threats")) 
  #http://www.iucnredlist.org/details/classify/19892724/0#threats
  threats <-xpathSApply(j, '//div[@id="threats"]', xmlValue)
  if (length(threats)>0){
    threats=str_replace_all(threats,"\n"," ")
    threat = threat_codes[1]
    for (threat in threat_codes){
      threat_str=paste0(" ",threat,". ")
      if (grepl(threat_str, threats)){
        jnk=which(code==Species_IDs)
        all_threats[jnk,threat]=all_threats[jnk,threat]+1
      }      
    }
  }else{
    cat('\n',"no threat data")    
  }
}

write.table(all_threats, file = "threat_matrix.csv", sep=",", row.names = FALSE)

##junk below
# input = "panthera-uncia"
# h <- htmlParse(paste("http://api.iucnredlist.org/go/",
#                      input, sep = ""))
# 
# j<-htmlParse("http://www.iucnredlist.org/details/classify/32463/0#threats")
# pop <-xpathSApply(h, '//div[@id="population"]/text()[preceding-sibling::br]', xmlValue)
# #threats <-xpathSApply(j, '//div[@id="threats"]/text()[preceding-sibling::br]', xmlValue)
# threats <-xpathSApply(j, '//div[@id="threats"]/text()[children::br]', xmlValue)
# threats <-xpathSApply(j, '//div[@id="threats"]', xmlValue)
