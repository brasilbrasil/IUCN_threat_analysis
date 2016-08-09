rm(list = ls()) #remove all past worksheet variables
library(reshape2)
library(ggplot2)
##run this code after merging all IUCN data
sink.reset <- function(){
  for(i in seq_len(sink.number())){
    sink(NULL)
  }
}

#wd="C:/Users/Kaipo Dye/Dropbox/PICCC/Kaipo vulnerability and multiple threats/IUCN_test_analysis_results20160621/"
#wd="D:/Dropbox/current work/contracting shared folders/Kaipo vulnerability and multiple threats/IUCN_test_analysis_results20160621/"
wd="D:/Dropbox/current work/IUCN_threat_publication/IUCN_test_analysis_results20160621/"
setwd(wd)
all_data_combined = read.csv(paste0("results/all_data_combined_onlySppWThreatInfo",".csv"), header = T, row.names = NULL, check.names = FALSE)

#all_data_combined_onlySppWThreatInfo have at least 1  threat (or else test will not be of augmentation)
#all_data_combined=all_data_combined[all_data_combined$n_nonCC_threat!=0,] <-- line removed(must have at least 1 threat) 


dependents="n_nonCC_threat"
data=all_data_combined

#0ne way anova 
independents=c("CC_threat")  ##n_nCC_threats ~ CC_threat/Red.List.status/Kingdom
int <- aov(as.formula(paste0(dependents, " ~ ", independents[1])), data=data)
aov_summary=summary(int)
aov_summary
plot(int)

con=file(paste0("results/","n_nonCC_threats_dep_vs_AOV_cc_threat.txt"), open="wt")
sink(con)
cat('these are the results for the 1-way anova test for independent variables ', independents, " and dependent ",dependents,"\n")
int
cat("\n", "\n", '1-way anova sumary below:', "\n")
aov_summary
cat('2-way anova test results done', "\n")
sink.reset()
close(con)

#two way anova (Kingdom/IUCN Status)

independents=c("CC_threat", "Red.List.status")
int2 <- aov(as.formula(paste0(dependents, " ~ ", independents[1],"*", independents[2])), data=data)
aov_summary2=summary(int2)
plot(int2)

con1=file(paste0("results/","n_nonCC_threats_dep_vs_AOV_cc_threat+IUCN_Status_interactive.txt"), open="wt")
sink(con1)
cat('these are the results for the 2-way anova test for independent variables ', independents, " and dependent ",dependents,"\n")
int2
cat("\n", "\n", '2-way anova sumary below:', "\n")
aov_summary2
cat('2-way anova test results done', "\n")
sink.reset()
close(con1)

independents=c("CC_threat", "Kingdom")
int3 <- aov(as.formula(paste0(dependents, " ~ ", independents[1],"*", independents[2])), data=data)
aov_summary3=summary(int3)
plot(int3)

con3=file(paste0("results/","n_nonCC_threats_dep_vs_AOV_cc_threat+Kingdom_interactive.txt"), open="wt")
sink(con3)
cat('these are the results for the 2-way anova test for independent variables ', independents, " and dependent ",dependents,"\n")
int3
cat("\n", "\n", '2-way anova sumary below:', "\n")
aov_summary3
cat('2-way anova test results done', "\n")
sink.reset()
close(con3)

#three way anova (with and without interaction)
independents=c("CC_threat", "Red.List.status", "Kingdom")
aov.with <- aov(as.formula(paste0(dependents, " ~ ", independents[1],"*", independents[2], "*", independents[3])), data=data)
#int <- aov(all_data_combined$n_threats ~ all_data_combined$CC_threat*all_data_combined$status)
summary(aov.with)
plot(aov.with)

#Investigate each interaction (cc_threat,Red.List.Status)
TukeyHSD(aov.with, conf.level=.99)

independents=c("CC_threat", "Red.List.status", "Kingdom")
aov.wout <- aov(as.formula(paste0(dependents, " ~ ", independents[1],"+", independents[2], "+", independents[3])), data=data)
#int <- aov(all_data_combined$n_threats ~ all_data_combined$CC_threat*all_data_combined$status)
summary(aov.wout)


#Chi-square test for interactivity legitimacy
anova(aov.with,aov.wout,test="Chi")
Chi=anova(aov.with,aov.wout,test="Chi")
Chi

Chi_summary=summary(Chi)
plot(Chi)

con5=file(paste0("results/","n_nonCC_threats_AOV_cc_threat_Chi.txt"), open="wt")
sink(con5)
cat('these are the results for the Chi-square interactivity test',"\n")
Chi
cat("\n", "\n", 'Chi-square sumary below:', "\n")
Chi_summary
cat('Chi-square test results done', "\n")
sink.reset()
close(con5)

#1 factor GLM models
#glm and poisson family
#poison since dependent variable is count data
independents=c("CC_threat") ##n_nCC_threats ~ CC_threat/Red.List.status/Kingdom
glmFit=glm(as.formula(paste0(dependents, " ~ ", independents[1])), data=data, family=poisson())
summary(glmFit)
plot(glmFit)


#2 factor GLM models
#glm and poisson family
#poison since dependent variable is count data
independents=c("CC_threat") ## n_nCC_threats ~ CC_threat*Red.List.status/Kingdom
glmFit=glm(as.formula(paste0(dependents, " ~ ", independents[1],"*", independents[2])), data=data, family=poisson())
summary(glmFit)
plot(glmFit)

#3 factor GLM models
#glm and poisson family (with and without interaction)
#poison since dependent variable is count data
independents=c("CC_threat", "Red.List.status", "Kingdom")
glmFitW=glm(as.formula(paste0(dependents, " ~ ", independents[1],"*", independents[2],"*", independents[3])), data=data, family=poisson())
summary(glmFitW)
plot(glmFitW)

#no interactions
glmFitWO=glm(as.formula(paste0(dependents, " ~ ", independents[1]," + ", independents[2]," + ", independents[3])), data=data, family=poisson())
summary(glmFitWO)
plot((-)glmFitWO)

#CHI-square Interactivity test (Independence test)
anova(glmFitW,glmFitWO,test="Chi")

#Multi-FACTORIAL ANOVA
aov.out=aov(n_nonCC_threat ~ CC_threat*Red.List.status*Kingdom,data=all_data_combined)
summary(aov.out)
TukeyHSD(aov.out, conf.level=.99)

library(boot)
glm.diag=glm.diag(glmFitW)
glm.diag.plots(glmFit,glm.diag)
