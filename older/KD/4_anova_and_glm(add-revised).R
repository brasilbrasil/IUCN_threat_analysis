rm(list = ls()) #remove all past worksheet variables
library(reshape2)
library(ggplot2)
##run this code after merging all IUCN data

#wd="D:/Dropbox/current work/IUCN_threats_analysis_outputs/"
wd="C:/Users/Kaipo Dye/Dropbox/PICCC/IUCN_threats_analysis_outputs/"
setwd(wd)
all_data_combined = read.csv(paste0("resultsCheck/all_data_combined_onlySppWThreatInfo_wGeog",".csv"), header = T, row.names = NULL, check.names = FALSE)

#keep only species that have at least 1 non-climatic threat (or else test will not
#be of augmentation)
all_data_combined=all_data_combined[all_data_combined$n_nonCC_threat!=0,]  

attach(all_data_combined)

##SIMPLE 1-WAY GLM (4X: n_threats ~ *CC_threat/status/geography/kingdom) *MOST Signif
model_CC_threat=glm(all_data_combined$n_nonCC_threat~all_data_combined$CC_threat,poisson)
summary(model_CC_threat)

model_IUCN_status=glm(all_data_combined$n_nonCC_threat~all_data_combined$Red.List.status,poisson)
summary(model_IUCN_status)

model_geog=glm(all_data_combined$n_nonCC_threat~all_data_combined$status,poisson)
summary(model_geog)

model_kingdom=glm(all_data_combined$n_nonCC_threat~all_data_combined$Kingdom,poisson)
summary(model_kingdom)

##ADDITIVE 2-WAY GLM (n_threats ~ CC_threat + IV (status/geography/kingdom))
model_add_status=glm(all_data_combined$n_nonCC_threat~all_data_combined$CC_threat+all_data_combined$Red.List.status,poisson)
summary(model_add_status)

model_add_geog=glm(all_data_combined$n_nonCC_threat~all_data_combined$CC_threat+all_data_combined$status,poisson)
summary(model_add_geog)

model_add_king=glm(all_data_combined$n_nonCC_threat~all_data_combined$CC_threat+all_data_combined$Kingdom,poisson)
summary(model_add_king)

model_add_year=glm(all_data_combined$n_nonCC_threat~all_data_combined$CC_threat+all_data_combined$Year.assessed,poisson)
summary(model_add_year)


##INTERACTIVE 2-WAY GLM (n_threats ~ CC_threat + IV (status/geography/kingdom))
model_INT_status=glm(all_data_combined$n_nonCC_threat~all_data_combined$CC_threat*all_data_combined$Red.List.status,poisson)
summary(model_INT_status)
model_INT_status1=update(model_INT_status,~.-all_data_combined$CC_threat:all_data_combined$Red.List.status)
## Remove Interaction Test (retaintion significant)
anova(model_INT_status,model_INT_status1,test="Chi")

model_INT_geog=glm(all_data_combined$n_nonCC_threat~all_data_combined$CC_threat*all_data_combined$status,poisson)
summary(model_INT_geog)
model_INT_geog1=update(model_INT_geog,~.-all_data_combined$CC_threat:all_data_combined$status)
anova(model_INT_geog,model_INT_geog1,test="Chi")

model_INT_king=glm(all_data_combined$n_nonCC_threat~all_data_combined$CC_threat*all_data_combined$Kingdom,poisson)
summary(model_INT_king)
model_INT_king1=update(model_INT_king,~.-all_data_combined$CC_threat:all_data_combined$Kingdom)
anova(model_INT_king,model_INT_king1,test="Chi")

model_INT_year=glm(all_data_combined$n_nonCC_threat~all_data_combined$CC_threat*all_data_combined$Year.assessed,poisson)
summary(model_INT_year)
model_INT_year1=update(model_INT_year,~.-all_data_combined$CC_threat:all_data_combined$Year.assessed)
anova(model_INT_year,model_INT_year1,test="Chi")

model_INT_phyl=glm(all_data_combined$n_nonCC_threat~all_data_combined$CC_threat*all_data_combined$Phylum,poisson)
summary(model_INT_phyl)
model_INT_phyl1=update(model_INT_phyl,~.-all_data_combined$CC_threat:all_data_combined$Phylum)
anova(model_INT_phyl,model_INT_phyl1,test="Chi")

## INTERACTIVE 3-way GLM (W/INTERactive removal test)
model_INT=glm(all_data_combined$n_nonCC_threat~all_data_combined$CC_threat*all_data_combined$Kingdom*all_data_combined$Red.List.status*all_data_combined$status,poisson)
summary(model_INT)
anova(model_INT, test ="Chisq")
anova(update(model_INT,~.-(all_data_combined$CC_threat:all_data_combined$Kingdom:all_data_combined$Red.List.status:all_data_combined$status+all_data_combined$Kingdom:all_data_combined$Red.List.status:all_data_combined$status+all_data_combined$Red.List.status:all_data_combined$status)),test="Chisq")

model_INTout=update(model_INT,~.-(all_data_combined$CC_threat:all_data_combined$Kingdom:all_data_combined$Red.List.status:all_data_combined$status+all_data_combined$Kingdom:all_data_combined$Red.List.status:all_data_combined$status+all_data_combined$Red.List.status:all_data_combined$status))
summary(model_INTout)
anova(model_INTout, test ="Chisq")

anova(model_INT,model_INTout,test="Chi")


## geog removed
model_INT1=glm(all_data_combined$n_nonCC_threat~all_data_combined$CC_threat*all_data_combined$Kingdom*all_data_combined$Red.List.status,poisson)
summary(model_INT1)

model_INT1out=glm(all_data_combined$n_nonCC_threat~all_data_combined$CC_threat*all_data_combined$Kingdom,poisson)
summary(model_INT1out)

anova(model_INT1,model_INT2,test="Chi")

##3-way Factorial ANOVA
aov.out=aov(n_nonCC_threat ~ CC_threat*Red.List.status*Kingdom*status,data=all_data_combined)
summary(aov.out)
##TukeyHSD(aov.out) <-- removed too many levels of interaction
summary.lm(aov.out)

#GEOGRAPHY REMOVED (GEOG retained Significant in model)
aov.out1=aov(n_nonCC_threat ~ CC_threat*Red.List.status*Kingdom,data=all_data_combined)
summary(aov.out1)
summary.lm(aov.out1)
TukeyHSD(aov.out1)

##2-way Factorial ANOVA n_threats~cc_threatened*2nd_IV (Status/Kingdom)
aov.status=aov(n_nonCC_threat ~ CC_threat*Red.List.status,data=all_data_combined)
summary(aov.status)
summary.lm(aov.status)
TukeyHSD(aov.status)

aov.kingdom=aov(n_nonCC_threat ~ CC_threat*Red.List.status,data=all_data_combined)
summary(aov.out1)
summary.lm(aov.out1)
TukeyHSD(aov.out1)

aov.cc_threat=aov(n_nonCC_threat ~ status*Kingdom,data=all_data_combined)
summary(aov.cc_threat)
summary.lm(aov.cc_threat)
TukeyHSD(aov.cc_threat)

#LogLinear Analysis:
library(MASS)

Log_model=loglm(n_nonCC_threat~CC_threat*Kingdom*status*Red.List.status,data=all_data_combined)
Log_model1=update(Log_model, ~.-(CC_threat:Kingdom:status:Red.List.status+Kingdom:status:Red.List.status+status:Red.List.status))
Log_model1
step(Log_model, direction="backward")
