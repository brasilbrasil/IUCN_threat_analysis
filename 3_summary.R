wd="D:/Dropbox/current work/IUCN_threat_publication/IUCN_test_analysis_results20160621/"
setwd(wd)
library(stringi)
remove_zero_nonccthreats=F

dir.create("results/hists_and_counts/", showWarnings = F, recursive = T)
dir.create("results/graphs/", showWarnings = F, recursive = T)
dir.create("results/ttests/", showWarnings = F, recursive = T)
dir.create("results/threat_augmentation_comparison/", showWarnings = F, recursive = T)

mega_matrix = read.csv(paste0("results/all_data_combined_onlySppWThreatInfo",".csv"), header = T, row.names = NULL, check.names = FALSE)
#jnk=mega_matrix[, c("Red.List.status", "CC_threat")]
#table(jnk)

threats=as.character(c(1:9, 11))
jnk=c(names(mega_matrix)[1],threats)
threat_matrix_summary=mega_matrix[,jnk]
n_threats=rowSums(mega_matrix[,threats])
n_ncc_threats=n_threats-mega_matrix[,"11"]
threat_matrix_summary=cbind(threat_matrix_summary,n_threats, n_ncc_threats)

threat_summary_fx=function(threat_matrix_summary, proj_name, species_type){
  jpeg_name = paste0("results/hists_and_counts/", proj_name, "_histogram_of_number_of_threats_",species_type,"_species.jpg") #names jpeg file to be created
  jpeg(jpeg_name, #creates blank jpeg file in working directory
       width = 10, height = 10, units = "in", pointsize = 12, quality = 90, bg = "white", res = 300)
  hist_vals=hist(threat_matrix_summary[,"n_ncc_threats"],breaks=c((0-0.5):(max(n_ncc_threats)+1)), main=NULL, xlab="Number of identified non-climatic threats", ylab=paste0("number of ",species_type, " species"))
  dev.off() #saves file to jpeg
  
  threat_count_distr=data.frame(n.threats=hist_vals$mids, count=hist_vals$counts)
  name = paste0("results/hists_and_counts/", proj_name, "_threat_count_distr_",species_type,"_species.csv") #names jpeg file to be created
  write.table(threat_count_distr, file = name, sep=",", row.names = FALSE)
  threat_count_distr_summary=data.frame(none=threat_count_distr[1,2], single=threat_count_distr[2,2],
                                        multiple=sum(threat_count_distr[3:(dim(threat_count_distr)[1]),2]),
                                        non_zero_n=sum(threat_count_distr[2:(dim(threat_count_distr)[1]),2]),
                                        mean.n.threats=mean(as.matrix(threat_matrix_summary[,"n_ncc_threats"])),
                                        mean.n.threats.nonZero=mean(as.matrix(threat_matrix_summary[threat_matrix_summary[,"n_ncc_threats"]>0,"n_ncc_threats"])),
                                        sd.n.threats=sd(as.matrix(threat_matrix_summary[,"n_ncc_threats"])),
                                        sd.n.threats.nonZero=sd(as.matrix(threat_matrix_summary[threat_matrix_summary[,"n_ncc_threats"]>0,"n_ncc_threats"])))
  name = paste0("results/hists_and_counts/", proj_name, "_threat_count_distr_summary_",species_type,"_species.csv") #names jpeg file to be created
  write.table(threat_count_distr_summary, file = name, sep=",", row.names = FALSE)
  #meanNthreats=apply(threat_matrix_summary[,2:dim(threat_matrix_summary)[2]],2,mean)
  #meanNthreats_nonZero=apply(threat_matrix_summary[threat_matrix_summary[,"n_ncc_threats"]>0,2:dim(threat_matrix_summary)[2]],2,mean)  
}

tmp_threat_matrix_summary=threat_matrix_summary
if (remove_zero_nonccthreats) tmp_threat_matrix_summary=tmp_threat_matrix_summary[tmp_threat_matrix_summary[,"n_ncc_threats"]>0,]
threat_summary_fx(threat_matrix_summary, "all", "all")

#climate change
clim_threat_matrix_summary=threat_matrix_summary#[threat_matrix_summary[,"11"]>0,]
if (remove_zero_nonccthreats) clim_threat_matrix_summary=clim_threat_matrix_summary[clim_threat_matrix_summary[,"n_ncc_threats"]>0,]
threat_summary_fx(clim_threat_matrix_summary, "CC", "all")

#non climate change non zero
nclim_threat_matrix_summary=threat_matrix_summary#[threat_matrix_summary[,"11"]==0,]
if (remove_zero_nonccthreats) nclim_threat_matrix_summary=nclim_threat_matrix_summary[nclim_threat_matrix_summary[,"n_ncc_threats"]>0,]
threat_summary_fx(nclim_threat_matrix_summary, "nCC", "all")

#test overall differences!
#test differences
ttest_matrix=mega_matrix #n_nonCC_threat
if (remove_zero_nonccthreats) ttest_matrix=ttest_matrix[threat_matrix_summary[,"n_ncc_threats"]>0,]
ttest_matrix=ttest_matrix[, c("n_nonCC_threat", "CC_threat")]
test_results=t.test(n_nonCC_threat~CC_threat, data=ttest_matrix)
#summary(test_results)
t.statistic=test_results$statistic
t.p.value=test_results$p.value
t.estimate.group1=test_results$estimate[1]
t.estimate.group2=test_results$estimate[2]
group.difference=t.estimate.group1-t.estimate.group2
group.difference.CI.low=test_results$conf.int[1]
group.difference.CI.high=test_results$conf.int[2]
threat_increase=t.estimate.group1/t.estimate.group2
df=data.frame(t.statistic, t.p.value, t.estimate.group1, t.estimate.group2, group.difference, 
              group.difference.CI.low, group.difference.CI.high, threat_increase)
name = paste0("results/ttests/", "ttest_n_nCC_trheats_by_CC_nCC_threatened_ALLSPECIES.csv") #names jpeg file to be created
write.table(df, file = name, sep=",", row.names = FALSE)

#some graphs
#figure 1
library(ggplot2)
mega_matrix2=mega_matrix
levels(mega_matrix2$CC_threat) <- c('climate vulnerable', 'not climate vulnerable')
levels(mega_matrix2$Kingdom) <- c('Animalia', 'Plantae')
a=ggplot(data=mega_matrix2, aes(x=Kingdom, y=n_nonCC_threat, fill=CC_threat)) + 
  geom_bar(position=position_dodge(), stat="summary", fun.y = "mean")+ ylab("Mean number of non-climatic threats") +
  theme(legend.title=element_blank()) + scale_fill_grey(start = 0, end = .5)
a
ggsave(a, file=paste0("results/graphs/mean.n.ncc.threats.by.kingdom_grey.tiff"), width=6, height=4, compression = "lzw")

#figure 1 (with error bars)
library(ggplot2)
mega_matrix2=mega_matrix
levels(mega_matrix2$CC_threat) <- c('climate vulnerable', 'not climate vulnerable')
levels(mega_matrix2$Kingdom) <- c('Animalia', 'Plantae')
#mega_matrix3=mega_matrix2[,c("n_nonCC_threat", "Kingdom", "CC_threat")]
#summary_df=aggregate(formula=n_nonCC_threat~Kingdom+CC_threat, FUN=mean, data=mega_matrix2)
summary_df=aggregate(formula=n_nonCC_threat~Kingdom+CC_threat, FUN=function(x) c(mn = mean(x), n = length(x), sd = sd(x), se=sd(x)/sqrt(length(x)), upper=mean(x)+sd(x)/sqrt(length(x)), lower=mean(x)-sd(x)/sqrt(length(x))), data=mega_matrix2) #http://stackoverflow.com/questions/12064202/using-aggregate-to-apply-several-functions-on-several-variables-in-one-call
#summary_df2=as.data.frame(summary_df)
summary_df=as.data.frame(as.list(summary_df))

#http://docs.ggplot2.org/0.9.3.1/geom_errorbar.html
dodge <- position_dodge(width=0.9)
a=ggplot(data=summary_df, aes(x=Kingdom, y=n_nonCC_threat.mn, fill=CC_threat)) + 
  geom_bar(position=dodge, stat="identity")+ ylab("Mean number of non-climatic threats") + #stat="summary", fun.y = "mean"
  theme(legend.title=element_blank()) + scale_fill_grey(start = 0.25, end = .5)
limits <- aes(ymax = summary_df$n_nonCC_threat.upper, ymin=summary_df$n_nonCC_threat.lower)
a=a + geom_errorbar(limits, position=dodge, width=0.5)
ggsave(a, file=paste0("results/graphs/mean.n.ncc.threats.by.kingdom_grey_with_SE.tiff"), width=6, height=4, compression = "lzw")

#COLOR VERSION
a=ggplot(data=summary_df, aes(x=Kingdom, y=n_nonCC_threat.mn, fill=CC_threat)) + 
  geom_bar(position=dodge, stat="identity")+ ylab("Mean number of non-climatic threats") + #stat="summary", fun.y = "mean"
  theme(legend.title=element_blank()) 
limits <- aes(ymax = summary_df$n_nonCC_threat.upper, ymin=summary_df$n_nonCC_threat.lower)
a=a + geom_errorbar(limits, position=dodge, width=0.5)
ggsave(a, file=paste0("results/graphs/mean.n.ncc.threats.by.kingdom_COLOR_with_SE.tiff"), width=6, height=4, compression = "lzw")


a=ggplot(data=mega_matrix2, aes(x=Kingdom, y=n_nonCC_threat, fill=CC_threat)) + 
  geom_bar(position=position_dodge(), stat="summary", fun.y = "mean")+ ylab("Mean number of non-climatic threats") +
  theme(legend.title=element_blank())
a
ggsave(a, file=paste0("results/graphs/mean.n.ncc.threats.by.kingdom.tiff"), width=6, height=4, compression = "lzw")

#proj_name, species_type
IUCN_cats=c("LC", "NT", "VU", "EN", "CR")
#species_types=IUCN_cats
proj_names=c("all", "CC", "nCC")
kingdoms=c("", "ANIMALIA", "PLANTAE")

for (IUCN_cat in IUCN_cats){
  for (kingdom in kingdoms){
    ttest_matrix=mega_matrix #n_nonCC_threat
    if (remove_zero_nonccthreats) ttest_matrix=ttest_matrix[threat_matrix_summary[,"n_ncc_threats"]>0,]
    if (kingdom!=""){
      ttest_matrix=ttest_matrix[ttest_matrix$Kingdom == kingdom,] #n_nonCC_threat
      tmp_threat_matrix_summary=threat_matrix_summary[mega_matrix$Kingdom == kingdom & mega_matrix$Red.List.status == IUCN_cat,]      
    }else{
      tmp_threat_matrix_summary=threat_matrix_summary[mega_matrix$Red.List.status == IUCN_cat,]            
    }
    if (remove_zero_nonccthreats) tmp_threat_matrix_summary=tmp_threat_matrix_summary[tmp_threat_matrix_summary[,"n_ncc_threats"]>0,]
    #test differences
    ttest_matrix=ttest_matrix[ttest_matrix$Red.List.status == IUCN_cat,] #n_nonCC_threat
    ttest_matrix=ttest_matrix[, c("n_nonCC_threat", "CC_threat")]
    test_results=t.test(n_nonCC_threat~CC_threat, data=ttest_matrix)
    #summary(test_results)
    t.statistic=test_results$statistic
    t.p.value=test_results$p.value
    t.estimate.group1=test_results$estimate[1]
    t.estimate.group2=test_results$estimate[2]
    group.difference=t.estimate.group1-t.estimate.group2
    group.difference.CI.low=test_results$conf.int[1]
    group.difference.CI.high=test_results$conf.int[2]
    threat_increase=t.estimate.group1/t.estimate.group2
    df=data.frame(t.statistic, t.p.value, t.estimate.group1, t.estimate.group2, group.difference, 
                  group.difference.CI.low, group.difference.CI.high, threat_increase)
    name = paste0("results/ttests/", "ttest_n_nCC_trheats_by_CC_nCC_threatened_",kingdom, IUCN_cat,"_species.csv") #names jpeg file to be created
    write.table(df, file = name, sep=",", row.names = FALSE)
    
    #all species
    #tmp_threat_matrix_summary=tmp_threat_matrix_summary[mega_matrix$Red.List.status == IUCN_cat,]
    #tmp_threat_matrix_summary=tmp_threat_matrix_summary#[tmp_threat_matrix_summary[,"n_ncc_threats"]>0,]
    threat_summary_fx(tmp_threat_matrix_summary, paste0(kingdom, "all"), IUCN_cat)
    
    #climate change
    clim_threat_matrix_summary=tmp_threat_matrix_summary[tmp_threat_matrix_summary[,"11"]>0,]
    #clim_threat_matrix_summary=clim_threat_matrix_summary#[clim_threat_matrix_summary[,"n_ncc_threats"]>0,]
    threat_summary_fx(clim_threat_matrix_summary, paste0(kingdom, "CC"), IUCN_cat)
    
    #non climate change non zero
    nclim_threat_matrix_summary=tmp_threat_matrix_summary[tmp_threat_matrix_summary[,"11"]==0,]
    #nclim_threat_matrix_summary=nclim_threat_matrix_summary#[nclim_threat_matrix_summary[,"n_ncc_threats"]>0,]
    threat_summary_fx(nclim_threat_matrix_summary, paste0(kingdom, "nCC"), IUCN_cat)    
  }
}


#most_threatened_sp=threat_matrix_summary[which(n_ncc_threats==max(n_ncc_threats)),names(threat_matrix)[1]]
