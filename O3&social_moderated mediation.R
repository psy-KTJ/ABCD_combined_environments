###Unstandardized Coefficients
###Moderated Mediation Model
###List-wise Deletion
library(data.table)
library(dplyr)
library(ggplot2)
library(mice)
library(lme4)
library(lmerTest)
library(miceadds)
library(broom.mixed)

library(haven)
participant_structure_cognitive <- read_sav("D:/ME/BNU/ABCD/environment-culture-brain/participant_structure_total_brain.sav")
View(participant_structure_cognitive)


participant_structure_cognitive <- replace(participant_structure_cognitive, participant_structure_cognitive  ==  999.000, NA);participant_structure_cognitive
summary(participant_structure_cognitive)
names(participant_structure_cognitive)
str(participant_structure_cognitive)


#####neighbor as moderator###
####total cognition as DV
names(participant_structure_cognitive)
###summarize missingness for the variables listed
dat_nms = c("site_id0", "family_id0", "age0", "age2", 
            "race", "sex","s_type2",
            "hplh0","hplh2",  "hprh0","hprh2",
            "agrh0","agrh2",  "aglh0","aglh2","totalvolume2",
            "Cneighbor","Co3", 
            "totalcog2","totalcog0")

if (sum(as.numeric(dat_nms %in% names(participant_structure_cognitive))) != length(dat_nms))
  print("Error: missing core demographics. Add those first")
dat = data.table(participant_structure_cognitive[,dat_nms])
for (m in dat_nms) print(paste("number missing for ",m,": ",sum(is.na(participant_structure_cognitive[[m]]) | (participant_structure_cognitive[m] == "")),sep=""))

# Number of multiple imputed datasets & maximum number of iterations
n.imp = 20 
n.iter = 5

options(max.print = 20000)

#set the variable list
var.ls1 <- c("site_id0", "family_id0", "age0", "age2", 
             "race", "sex","s_type2",
             "hplh0","hplh2",  "hprh0","hprh2",
             "agrh0","agrh2",  "aglh0","aglh2","totalvolume2",
             "Cneighbor","Co3", 
             "totalcog2","totalcog0")
dat_mod1 <- dat[, var.ls1, with = FALSE ]

str(dat_mod1)
summary(dat_mod1)
names(dat_mod1)

newdata1<-dat_mod1[complete.cases(dat_mod1),]
newdata1<-na.omit(dat_mod1)
summary(newdata1)
sum(is.na(newdata1))
str(newdata1)




###Right Amygdala Volume as a Mediator
###Mediator ~ Independent Variable * Moderator
fit_m1 <-lm(agrh2 ~ Co3 * Cneighbor + agrh0 + 
              race + sex + age2 + s_type2 +totalvolume2,
            data = newdata1)
summary(fit_m1)


##Dependent Variable ~ Independent Variable * Moderator + Mediator

fit_y1 <-lm(totalcog2 ~ Co3 * Cneighbor + agrh2 + totalcog0 +
              race + sex + age2 , data = newdata1)
summary(fit_y1)

##Extract Mediation Effect Results
##Low Level of Moderator Variable
low.neighbor<-mean(newdata1$Cneighbor,na.rm = TRUE)-sd(newdata1$Cneighbor,na.rm = TRUE) 
low.neighbor #Check value of variable
library(mediation)
Mod.Med.low.neighbor1 <- mediate(fit_m1, fit_y1,    
                                 covariates = list(Cneighbor = low.neighbor), 
                                 sims = 4000, treat='Co3', mediator='agrh2',
                                 cluster = newdata1$site_id0)
summary(Mod.Med.low.neighbor1,options(digits=10))


##High Level of Moderator Variable
options(digits=10)
high.neighbor<- mean(newdata1$Cneighbor,na.rm = TRUE)+sd(newdata1$Cneighbor,na.rm = TRUE) 
high.neighbor #Check value of variable
library(mediation)
Mod.Med.high.neighbor1 <- mediate(fit_m1, fit_y1,    
                                  covariates = list(Cneighbor = high.neighbor),    
                                  sims = 4000, treat='Co3', mediator='agrh2',
                                  cluster = newdata1$site_id0)
summary(Mod.Med.high.neighbor1)

# tests whether the difference between indirect effects at each level of the moderator is significantly different from zero
Mod.Med.Testneighbor1 <- mediate(model.m = fit_m1, model.y = fit_y1, sims = 4000, 
                                 treat='Co3', mediator='agrh2',
                                 cluster = newdata1$site_id0)

test.modmed(Mod.Med.Testneighbor1, covariates.1 = list(Cneighbor = low.neighbor),   
            covariates.2 = list(Cneighbor = high.neighbor), sims = 4000) 
#Here we specify both levels of the moderator that we want to test


####Psychotic like Experiences###
names(participant_structure_cognitive)
###summarize missingness for the variables listed
dat_nms = c("site_id0", "family_id0", "age0", "age2", 
            "race", "sex","s_type2",
            "hplh0","hplh2",  "hprh0","hprh2",
            "agrh0","agrh2",  "aglh0","aglh2",
            "Cneighbor","Co3","totalvolume2",
            "ples2","ples0")

if (sum(as.numeric(dat_nms %in% names(participant_structure_cognitive))) != length(dat_nms))
  print("Error: missing core demographics. Add those first")
dat = data.table(participant_structure_cognitive[,dat_nms])
for (m in dat_nms) print(paste("number missing for ",m,": ",sum(is.na(participant_structure_cognitive[[m]]) | (participant_structure_cognitive[m] == "")),sep=""))

# Number of multiple imputed datasets & maximum number of iterations
n.imp = 20 
n.iter = 5

options(max.print = 20000)

#set the variable list
var.ls13 <- c("site_id0", "family_id0", "age0", "age2", 
              "race", "sex","s_type2",
              "hplh0","hplh2",  "hprh0","hprh2",
              "agrh0","agrh2",  "aglh0","aglh2","totalvolume2",
              "Cneighbor","Co3",
              "ples2","ples0")
dat_mod13 <- dat[, var.ls13, with = FALSE ]

str(dat_mod13)
summary(dat_mod13)
names(dat_mod13)

newdata13<-dat_mod13[complete.cases(dat_mod13),]
newdata13<-na.omit(dat_mod13)
summary(newdata13)
sum(is.na(newdata13))
str(newdata13)


###Right Amygdala Volume as a Mediator
###Mediator ~ Independent Variable * Moderator
fit_m20 <-lm(agrh2 ~ Co3 * Cneighbor + agrh0 + 
               race + sex + age2 + s_type2 +totalvolume2,
             data = newdata13)
summary(fit_m20)


##Dependent Variable ~ Independent Variable * Moderator + Mediator
fit_y20 <-lm(ples2 ~ Co3 * Cneighbor + agrh2 + ples0 +
               race + sex + age2  , data = newdata13)
summary(fit_y20)

##Extract Mediation Effect Results
##Low Level of Moderator Variable
low.neighbor<-mean(newdata13$Cneighbor)-sd(newdata13$Cneighbor) 
low.neighbor #Check value of variable
library(mediation)
Mod.Med.low.neighbor20 <- mediate(fit_m20, fit_y20,    
                                  covariates = list(Cneighbor = low.neighbor),  
                                  sims = 4000, treat='Co3', mediator='agrh2',
                                  cluster = newdata13$site_id0)
summary(Mod.Med.low.neighbor20)

##High Level of Moderator Variable
high.neighbor<-mean(newdata13$Cneighbor)+sd(newdata13$Cneighbor) 
high.neighbor #Check value of variable
library(mediation)
Mod.Med.high.neighbor20 <- mediate(fit_m20, fit_y20,    
                                   covariates = list(Cneighbor = high.neighbor),    
                                   sims = 4000, treat='Co3', mediator='agrh2',
                                   cluster = newdata13$site_id0)
summary(Mod.Med.high.neighbor20)


# tests whether the difference between indirect effects at each level of the moderator is significantly different from zero
Mod.Med.Testneighbor20 <- mediate(model.m = fit_m20, model.y = fit_y20, sims = 4000, 
                                  treat='Co3', mediator='agrh2',
                                  cluster = newdata13$site_id0)
summary(Mod.Med.Testneighbor20)
test.modmed(Mod.Med.Testneighbor20, covariates.1 = list(Cneighbor = low.neighbor),   
            covariates.2 = list(Cneighbor = high.neighbor), sims = 4000) 
#Here we specify both levels of the moderator that we want to test


## Unstandardized Correlation Coefficients
####list-wise deletion dataset
###School as a Moderator Variable
###Total Cognition Scores as DV
names(participant_structure_cognitive)
###summarize missingness for the variables listed
dat_nms = c("site_id0", "family_id0", "age0", "age2", 
            "race", "sex","s_type2",
            "hplh0","hplh2",  "hprh0","hprh2",
            "agrh0","agrh2",  "aglh0","aglh2","totalvolume2",
            "Cschool","Co3", 
            "totalcog2","totalcog0")

if (sum(as.numeric(dat_nms %in% names(participant_structure_cognitive))) != length(dat_nms))
  print("Error: missing core demographics. Add those first")
dat = data.table(participant_structure_cognitive[,dat_nms])
for (m in dat_nms) print(paste("number missing for ",m,": ",sum(is.na(participant_structure_cognitive[[m]]) | (participant_structure_cognitive[m] == "")),sep=""))

# Number of multiple imputed datasets & maximum number of iterations
n.imp = 20 
n.iter = 5

options(max.print = 20000)

#set the variable list
var.ls1 <- c("site_id0", "family_id0", "age0", "age2", 
             "race", "sex","s_type2",
             "hplh0","hplh2",  "hprh0","hprh2",
             "agrh0","agrh2",  "aglh0","aglh2","totalvolume2",
             "Cschool","Co3", 
             "totalcog2","totalcog0")
dat_mod1 <- dat[, var.ls1, with = FALSE ]

str(dat_mod1)
summary(dat_mod1)
names(dat_mod1)

newdata1<-dat_mod1[complete.cases(dat_mod1),]
newdata1<-na.omit(dat_mod1)
summary(newdata1)
sum(is.na(newdata1))
str(newdata1)



##left hippocampus volume as mediator
##Mediator ~ Independent Variable * Moderator##
fit_m1 <-lm(hplh2 ~ Co3 * Cschool + hplh0 + 
              race + sex + age2 + s_type2 +totalvolume2,
            data = newdata1)
summary(fit_m1)


###Dependent Variable ~ Independent Variable * Moderator + Mediator
fit_y1 <-lm(totalcog2 ~ Co3 * Cschool + hplh2 + totalcog0 +
              race + sex + age2 , data = newdata1)
summary(fit_y1)


##Extract Mediation Effect Results
##Low Level of Moderator Variable
low.school<-mean(newdata1$Cschool,na.rm = TRUE)-sd(newdata1$Cschool,na.rm = TRUE) 
low.school #Check value of variable
library(mediation)
Mod.Med.low.school1 <- mediate(fit_m1, fit_y1,    
                               covariates = list(Cschool = low.school), 
                               sims = 4000, treat='Co3', mediator='hplh2',
                               cluster = newdata1$site_id0)
summary(Mod.Med.low.school1)
library(interactions)
johnson_neyman(model = Mod.Med.low.school1 ,pred = Co3, modx = Cschool)


##high Level of Moderator Variable
high.school<- mean(newdata1$Cschool,na.rm = TRUE)+sd(newdata1$Cschool,na.rm = TRUE) 
high.school #Check value of variable
library(mediation)
Mod.Med.high.school1 <- mediate(fit_m1, fit_y1,    
                                covariates = list(Cschool = high.school),    
                                sims = 4000, treat='Co3', mediator='hplh2',
                                cluster = newdata1$site_id0)
summary(Mod.Med.high.school1)


# tests whether the difference between indirect effects at each level of the moderator is significantly different from zero
Mod.Med.Testschool1 <- mediate(model.m = fit_m1, model.y = fit_y1, sims = 4000, 
                               treat='Co3', mediator='hplh2',
                               cluster = newdata1$site_id0)

test.modmed(Mod.Med.Testschool1, covariates.1 = list(Cschool = low.school),   
            covariates.2 = list(Cschool = high.school), sims = 4000) 
#Here we specify both levels of the moderator that we want to test



####Psychotic-like Experiences as DV ###
names(participant_structure_cognitive)
###summarize missingness for the variables listed
dat_nms = c("site_id0", "family_id0", "age0", "age2", 
            "race", "sex","s_type2",
            "hplh0","hplh2",  "hprh0","hprh2",
            "agrh0","agrh2",  "aglh0","aglh2",
            "Cschool","Co3","totalvolume2",
            "ples2","ples0")

if (sum(as.numeric(dat_nms %in% names(participant_structure_cognitive))) != length(dat_nms))
  print("Error: missing core demographics. Add those first")
dat = data.table(participant_structure_cognitive[,dat_nms])
for (m in dat_nms) print(paste("number missing for ",m,": ",sum(is.na(participant_structure_cognitive[[m]]) | (participant_structure_cognitive[m] == "")),sep=""))

# Number of multiple imputed datasets & maximum number of iterations
n.imp = 20 
n.iter = 5


#set the variable list
var.ls13 <- c("site_id0", "family_id0", "age0", "age2", 
              "race", "sex","s_type2",
              "hplh0","hplh2",  "hprh0","hprh2",
              "agrh0","agrh2",  "aglh0","aglh2","totalvolume2",
              "Cschool","Co3",
              "ples2","ples0")
dat_mod13 <- dat[, var.ls13, with = FALSE ]

str(dat_mod13)
summary(dat_mod13)
names(dat_mod13)

newdata13<-dat_mod13[complete.cases(dat_mod13),]
newdata13<-na.omit(dat_mod13)
summary(newdata13)
sum(is.na(newdata13))
str(newdata13)



fit_m20 <-lm(hplh2 ~ Co3 * Cschool + hplh0 + 
               race + sex + age2 + s_type2 +totalvolume2,
             data = newdata13)
summary(fit_m20)



fit_y20 <-lm(ples2 ~ Co3 * Cschool + hplh2 + ples0 +
               race + sex + age2  , data = newdata13)
summary(fit_y20)


low.school<-mean(newdata13$Cschool)-sd(newdata13$Cschool) 
low.school #Check value of variable
library(mediation)
Mod.Med.low.school20 <- mediate(fit_m20, fit_y20,    
                                covariates = list(Cschool = low.school),  
                                sims = 4000, treat='Co3', mediator='hplh2',
                                cluster = newdata13$site_id0)
summary(Mod.Med.low.school20)


high.school<-mean(newdata13$Cschool)+sd(newdata13$Cschool) 
high.school #Check value of variable
library(mediation)
Mod.Med.high.school20 <- mediate(fit_m20, fit_y20,    
                                 covariates = list(Cschool = high.school),    
                                 sims = 4000, treat='Co3', mediator='hplh2',
                                 cluster = newdata13$site_id0)
summary(Mod.Med.high.school20)


# tests whether the difference between indirect effects at each level of the moderator is significantly different from zero
Mod.Med.Testschool20 <- mediate(model.m = fit_m20, model.y = fit_y20, sims = 4000, 
                                treat='Co3', mediator='hplh2',
                                cluster = newdata13$site_id0)
summary(Mod.Med.Testschool20)
test.modmed(Mod.Med.Testschool20, covariates.1 = list(Cschool = low.school),   
            covariates.2 = list(Cschool = high.school), sims = 4000) 
#Here we specify both levels of the moderator that we want to test



### Unstandardized Coefficients
### Moderated Mediation Model
### multiple imputed Dataset
library(data.table)
library(dplyr)
library(ggplot2)
library(mice)
library(lme4)
library(lmerTest)
library(miceadds)
library(broom.mixed)

library(haven)
participant_structure_cognitive <- read_sav("D:/ME/BNU/ABCD/environment-culture-brain/participant_structure_total_brain.sav")
View(participant_structure_cognitive)


participant_structure_cognitive <- replace(participant_structure_cognitive, participant_structure_cognitive  ==  999.000, NA);participant_structure_cognitive
summary(participant_structure_cognitive)
names(participant_structure_cognitive)
str(participant_structure_cognitive)

### Neighbor as a Moderator Variable
### Total Cognitive Score as the Dependent Variable
names(participant_structure_cognitive)
###summarize missingness for the variables listed
dat_nms = c("site_id0", "family_id0", "age0", "age2", 
            "race", "sex","s_type2",
            "hplh0","hplh2",  "hprh0","hprh2",
            "agrh0","agrh2",  "aglh0","aglh2","totalvolume2",
            "Cneighbor","Co3", "Co3Cneigh",
            "totalcog2","totalcog0")

if (sum(as.numeric(dat_nms %in% names(participant_structure_cognitive))) != length(dat_nms))
  print("Error: missing core demographics. Add those first")
dat = data.table(participant_structure_cognitive[,dat_nms])
for (m in dat_nms) print(paste("number missing for ",m,": ",sum(is.na(participant_structure_cognitive[[m]]) | (participant_structure_cognitive[m] == "")),sep=""))

# Number of multiple imputed datasets & maximum number of iterations
n.imp = 20 
n.iter = 5


#set the variable list
var.ls1 <- c("site_id0", "family_id0", "age0", "age2", 
             "race", "sex","s_type2",
             "hplh0","hplh2",  "hprh0","hprh2",
             "agrh0","agrh2",  "aglh0","aglh2","totalvolume2",
             "Cneighbor","Co3", "Co3Cneigh",
             "totalcog2","totalcog0")
dat_mod1 <- dat[, var.ls1, with = FALSE ]

str(dat_mod1)
summary(dat_mod1)
names(dat_mod1)

ini <- mice( dat_mod1, m = 1, maxit = 0 )
meth = ini$meth


#put variables in that you want to impute

meth["age0"] <- "pmm" 
meth["age2"] <- "pmm" 
meth["s_type2"] <- "pmm" 
meth["hplh0"] <- "pmm"
meth["hprh0"] <- "pmm"
meth["aglh0"] <- "pmm"
meth["agrh0"] <- "pmm"
meth["hplh2"] <- "pmm"
meth["hprh2"] <- "pmm"
meth["aglh2"] <- "pmm"
meth["agrh2"] <- "pmm"
meth["Co3"] <- "pmm"
meth["Cneighbor"] <- "pmm"
meth["Co3Cneigh"] <- "pmm"
meth["totalvolume2"] <- "pmm"
meth["totalcog2"] <- "pmm"
meth["totalcog0"] <- "pmm"


pred = ini$pred
pred


# Specifying parameters for the imputation
post <- mice( dat_mod1, meth = meth, pred = pred, seed = 111,
              m = 1, maxit = 0)$post


bpmtot.imp1 <- mice( dat_mod1, meth = meth, pred = pred, post = post,
                     seed = 1111,
                     m = n.imp, maxit = n.iter)

library(data.table)
library(miceadds)
bpmtot.complete1 <- complete(bpmtot.imp1, action = "long")  
bpmtot.df1 <- as.data.frame(bpmtot.complete1)  
names(bpmtot.df1)
str(bpmtot.df1)



fit_m1 <-lm(agrh2 ~ Co3 * Cneighbor + agrh0 + 
              race + sex + age2 + s_type2 +totalvolume2,
            data = bpmtot.df1)
summary(fit_m1)


fit_y1 <-lm(totalcog2 ~ Co3 * Cneighbor + agrh2 + totalcog0 +
              race + sex + age2 , data = bpmtot.df1)
summary(fit_y1)


low.neighbor<-mean(bpmtot.df1$Cneighbor,na.rm = TRUE)-sd(bpmtot.df1$Cneighbor,na.rm = TRUE) 
low.neighbor #Check value of variable
library(mediation)
Mod.Med.low.neighbor1 <- mediate(fit_m1, fit_y1,    
                                 covariates = list(Cneighbor = low.neighbor), 
                                 sims = 1000, treat='Co3', mediator='agrh2',
                                 cluster = bpmtot.df1$site_id0)
summary(Mod.Med.low.neighbor1)




high.neighbor<- mean(bpmtot.df1$Cneighbor,na.rm = TRUE)+sd(bpmtot.df1$Cneighbor,na.rm = TRUE) 
high.neighbor #Check value of variable
library(mediation)
Mod.Med.high.neighbor1 <- mediate(fit_m1, fit_y1,    
                                  covariates = list(Cneighbor = high.neighbor),    
                                  sims = 1000, treat='Co3', mediator='agrh2',
                                  cluster = bpmtot.df1$site_id0)
summary(Mod.Med.high.neighbor1)


# tests whether the difference between indirect effects at each level of the moderator is significantly different from zero
Mod.Med.Testneighbor1 <- mediate(model.m = fit_m1, model.y = fit_y1, sims = 1000, 
                                 treat='Co3', mediator='agrh2',
                                 cluster = bpmtot.df1$site_id0)

test.modmed(Mod.Med.Testneighbor1, covariates.1 = list(Cneighbor = low.neighbor),   
            covariates.2 = list(Cneighbor = high.neighbor), sims = 1000) 
#Here we specify both levels of the moderator that we want to test


####psychotic-like experiences as DV ###
names(participant_structure_cognitive)
###summarize missingness for the variables listed
dat_nms = c("site_id0", "family_id0", "age0", "age2", 
            "race", "sex","s_type2",
            "hplh0","hplh2",  "hprh0","hprh2",
            "agrh0","agrh2",  "aglh0","aglh2","totalvolume2",
            "Cneighbor","Co3", "Co3Cneigh",
            "ples2","ples0")

if (sum(as.numeric(dat_nms %in% names(participant_structure_cognitive))) != length(dat_nms))
  print("Error: missing core demographics. Add those first")
dat = data.table(participant_structure_cognitive[,dat_nms])
for (m in dat_nms) print(paste("number missing for ",m,": ",sum(is.na(participant_structure_cognitive[[m]]) | (participant_structure_cognitive[m] == "")),sep=""))

# Number of multiple imputed datasets & maximum number of iterations
n.imp = 20 
n.iter = 5


#set the variable list
var.ls13 <- c("site_id0", "family_id0", "age0", "age2", 
              "race", "sex","s_type2",
              "hplh0","hplh2",  "hprh0","hprh2",
              "agrh0","agrh2",  "aglh0","aglh2","totalvolume2",
              "Cneighbor","Co3", "Co3Cneigh",
              "ples2","ples0")
dat_mod13 <- dat[, var.ls13, with = FALSE ]

str(dat_mod13)
summary(dat_mod13)
names(dat_mod13)

ini <- mice( dat_mod13, m = 1, maxit = 0 )
meth = ini$meth


#put variables in that you want to impute

meth["age0"] <- "pmm" 
meth["age2"] <- "pmm" 
meth["s_type2"] <- "pmm" 
meth["hplh0"] <- "pmm"
meth["hprh0"] <- "pmm"
meth["aglh0"] <- "pmm"
meth["agrh0"] <- "pmm"
meth["hplh2"] <- "pmm"
meth["hprh2"] <- "pmm"
meth["aglh2"] <- "pmm"
meth["agrh2"] <- "pmm"
meth["Co3"] <- "pmm"
meth["Cneighbor"] <- "pmm"
meth["Co3Cneigh"] <- "pmm"
meth["totalvolume2"] <- "pmm"
meth["ples2"] <- "pmm"
meth["ples0"] <- "pmm"


pred = ini$pred
pred


# Specifying parameters for the imputation
post <- mice( dat_mod13, meth = meth, pred = pred, seed = 111,
              m = 1, maxit = 0)$post



bpmtot.imp13 <- mice( dat_mod13, meth = meth, pred = pred, post = post,
                      seed = 1111,
                      m = n.imp, maxit = n.iter)

library(data.table)
library(miceadds)
bpmtot.complete13 <- complete(bpmtot.imp13, action = "long")  
bpmtot.df13 <- as.data.frame(bpmtot.complete13) 
names(bpmtot.df13)
str(bpmtot.df13)



fit_m20 <-lm(agrh2 ~ Co3 * Cneighbor + agrh0 + 
               race + sex + age2 + s_type2 +totalvolume2,
             data = bpmtot.df13)
summary(fit_m20)


fit_y20 <-lm(ples2 ~ Co3 * Cneighbor + agrh2 + ples0 +
               race + sex + age2  , data = bpmtot.df13)
summary(fit_y20)


low.neighbor<-mean(bpmtot.df13$Cneighbor)-sd(bpmtot.df13$Cneighbor) 
low.neighbor #Check value of variable
library(mediation)
Mod.Med.low.neighbor20 <- mediate(fit_m20, fit_y20,    
                                  covariates = list(Cneighbor = low.neighbor),  
                                  sims = 1000, treat='Co3', mediator='agrh2',
                                  cluster = bpmtot.df13$site_id0)
summary(Mod.Med.low.neighbor20)



high.neighbor<-mean(bpmtot.df13$Cneighbor)+sd(bpmtot.df13$Cneighbor) 
high.neighbor #Check value of variable
library(mediation)
Mod.Med.high.neighbor20 <- mediate(fit_m20, fit_y20,    
                                   covariates = list(Cneighbor = high.neighbor),    
                                   sims = 1000, treat='Co3', mediator='agrh2',
                                   cluster = bpmtot.df13$site_id0)
summary(Mod.Med.high.neighbor20)


# tests whether the difference between indirect effects at each level of the moderator is significantly different from zero
Mod.Med.Testneighbor20 <- mediate(model.m = fit_m20, model.y = fit_y20, sims = 1000, 
                                  treat='Co3', mediator='agrh2',
                                  cluster = bpmtot.df13$site_id0)
summary(Mod.Med.Testneighbor20)
test.modmed(Mod.Med.Testneighbor20, covariates.1 = list(Cneighbor = low.neighbor),   
            covariates.2 = list(Cneighbor = high.neighbor), sims = 1000) 
#Here we specify both levels of the moderator that we want to test



## School as a Moderator Variable
###Total Cognitive Score as the Dependent Variable
names(participant_structure_cognitive)
###summarize missingness for the variables listed
dat_nms = c("site_id0", "family_id0", "age0", "age2", 
            "race", "sex","s_type2",
            "hplh0","hplh2",  "hprh0","hprh2",
            "agrh0","agrh2",  "aglh0","aglh2","totalvolume2",
            "Cschool","Co3",  "Co3Cschool" ,
            "totalcog2","totalcog0")

if (sum(as.numeric(dat_nms %in% names(participant_structure_cognitive))) != length(dat_nms))
  print("Error: missing core demographics. Add those first")
dat = data.table(participant_structure_cognitive[,dat_nms])
for (m in dat_nms) print(paste("number missing for ",m,": ",sum(is.na(participant_structure_cognitive[[m]]) | (participant_structure_cognitive[m] == "")),sep=""))

# Number of multiple imputed datasets & maximum number of iterations
n.imp = 20 
n.iter = 5


#set the variable list
var.ls1 <- c("site_id0", "family_id0", "age0", "age2", 
             "race", "sex","s_type2",
             "hplh0","hplh2",  "hprh0","hprh2",
             "agrh0","agrh2",  "aglh0","aglh2","totalvolume2",
             "Cschool","Co3", "Co3Cschool" ,
             "totalcog2","totalcog0")
dat_mod1 <- dat[, var.ls1, with = FALSE ]

str(dat_mod1)
summary(dat_mod1)
names(dat_mod1)

ini <- mice( dat_mod1, m = 1, maxit = 0 )
meth = ini$meth


#put variables in that you want to impute

meth["age0"] <- "pmm" 
meth["age2"] <- "pmm" 
meth["s_type2"] <- "pmm" 
meth["hplh0"] <- "pmm"
meth["hprh0"] <- "pmm"
meth["aglh0"] <- "pmm"
meth["agrh0"] <- "pmm"
meth["hplh2"] <- "pmm"
meth["hprh2"] <- "pmm"
meth["aglh2"] <- "pmm"
meth["agrh2"] <- "pmm"
meth["Co3"] <- "pmm"
meth["Cschool"] <- "pmm"
meth["Co3Cschool"] <- "pmm"
meth["totalvolume2"] <- "pmm"
meth["totalcog2"] <- "pmm"
meth["totalcog0"] <- "pmm"


pred = ini$pred
pred


# Specifying parameters for the imputation
post <- mice( dat_mod1, meth = meth, pred = pred, seed = 111,
              m = 1, maxit = 0)$post


bpmtot.imp1 <- mice( dat_mod1, meth = meth, pred = pred, post = post,
                     seed = 1111,
                     m = n.imp, maxit = n.iter)

library(data.table)
library(miceadds)
bpmtot.complete1 <- complete(bpmtot.imp1, action = "long")  
bpmtot.df1 <- as.data.frame(bpmtot.complete1)  
names(bpmtot.df1)
str(bpmtot.df1)


fit_m1 <-lm(hplh2 ~ Co3 * Cschool + hplh0 + 
              race + sex + age2 + s_type2 +totalvolume2,
            data = bpmtot.df1)
summary(fit_m1)



fit_y1 <-lm(totalcog2 ~ Co3 * Cschool + hplh2 + totalcog0 +
              race + sex + age2 , data = bpmtot.df1)
summary(fit_y1)



low.school<-mean(bpmtot.df1$Cschool,na.rm = TRUE)-sd(bpmtot.df1$Cschool,na.rm = TRUE) 
low.school #Check value of variable
library(mediation)
Mod.Med.low.school1 <- mediate(fit_m1, fit_y1,    
                               covariates = list(Cschool = low.school), 
                               sims = 1000, treat='Co3', mediator='hplh2',
                               cluster = bpmtot.df1$site_id0)
summary(Mod.Med.low.school1)




high.school<- mean(bpmtot.df1$Cschool,na.rm = TRUE)+sd(bpmtot.df1$Cschool,na.rm = TRUE) 
high.school #Check value of variable
library(mediation)
Mod.Med.high.school1 <- mediate(fit_m1, fit_y1,    
                                covariates = list(Cschool = high.school),    
                                sims = 1000, treat='Co3', mediator='hplh2',
                                cluster = bpmtot.df1$site_id0)
summary(Mod.Med.high.school1)


# tests whether the difference between indirect effects at each level of the moderator is significantly different from zero
Mod.Med.Testschool1 <- mediate(model.m = fit_m1, model.y = fit_y1, sims = 1000, 
                               treat='Co3', mediator='hplh2',
                               cluster = bpmtot.df1$site_id0)

test.modmed(Mod.Med.Testschool1, covariates.1 = list(Cschool = low.school),   
            covariates.2 = list(Cschool = high.school), sims = 1000) 
#Here we specify both levels of the moderator that we want to test

####psychotic-like experiences###
names(participant_structure_cognitive)
###summarize missingness for the variables listed
dat_nms = c("site_id0", "family_id0", "age0", "age2", 
            "race", "sex","s_type2",
            "hplh0","hplh2",  "hprh0","hprh2",
            "agrh0","agrh2",  "aglh0","aglh2","totalvolume2",
            "Cschool","Co3", "Co3Cschool" ,
            "ples2","ples0")

if (sum(as.numeric(dat_nms %in% names(participant_structure_cognitive))) != length(dat_nms))
  print("Error: missing core demographics. Add those first")
dat = data.table(participant_structure_cognitive[,dat_nms])
for (m in dat_nms) print(paste("number missing for ",m,": ",sum(is.na(participant_structure_cognitive[[m]]) | (participant_structure_cognitive[m] == "")),sep=""))

# Number of multiple imputed datasets & maximum number of iterations
n.imp = 20 
n.iter = 5



#set the variable list
var.ls13 <- c("site_id0", "family_id0", "age0", "age2", 
              "race", "sex","s_type2",
              "hplh0","hplh2",  "hprh0","hprh2",
              "agrh0","agrh2",  "aglh0","aglh2","totalvolume2",
              "Cschool","Co3", "Co3Cschool" ,
              "ples2","ples0")
dat_mod13 <- dat[, var.ls13, with = FALSE ]

str(dat_mod13)
summary(dat_mod13)
names(dat_mod13)


ini <- mice( dat_mod13, m = 1, maxit = 0 )
meth = ini$meth


#put variables in that you want to impute

meth["age0"] <- "pmm" 
meth["age2"] <- "pmm" 
meth["s_type2"] <- "pmm" 
meth["hplh0"] <- "pmm"
meth["hprh0"] <- "pmm"
meth["aglh0"] <- "pmm"
meth["agrh0"] <- "pmm"
meth["hplh2"] <- "pmm"
meth["hprh2"] <- "pmm"
meth["aglh2"] <- "pmm"
meth["agrh2"] <- "pmm"
meth["Co3"] <- "pmm"
meth["Cschool"] <- "pmm"
meth["Co3Cschool"] <- "pmm"
meth["totalvolume2"] <- "pmm"
meth["ples2"] <- "pmm"
meth["ples0"] <- "pmm"


pred = ini$pred
pred


# Specifying parameters for the imputation
post <- mice( dat_mod13, meth = meth, pred = pred, seed = 111,
              m = 1, maxit = 0)$post


bpmtot.imp13 <- mice( dat_mod13, meth = meth, pred = pred, post = post,
                      seed = 1111,
                      m = n.imp, maxit = n.iter)

library(data.table)
library(miceadds)
bpmtot.complete13 <- complete(bpmtot.imp13, action = "long")  
bpmtot.df13 <- as.data.frame(bpmtot.complete13)  
names(bpmtot.df13)
str(bpmtot.df13)



fit_m20 <-lm(hplh2 ~ Co3 * Cschool + hplh0 + 
               race + sex + age2 + s_type2 +totalvolume2,
             data = bpmtot.df13)
summary(fit_m20)


fit_y20 <-lm(ples2 ~ Co3 * Cschool + hplh2 + ples0 +
               race + sex + age2  , data = bpmtot.df13)
summary(fit_y20)


low.school<-mean(bpmtot.df13$Cschool)-sd(bpmtot.df13$Cschool) 
low.school #Check value of variable
library(mediation)
Mod.Med.low.school20 <- mediate(fit_m20, fit_y20,    
                                covariates = list(Cschool = low.school),  
                                sims = 1000, treat='Co3', mediator='hplh2',
                                cluster = bpmtot.df13$site_id0)
summary(Mod.Med.low.school20)




high.school<-mean(bpmtot.df13$Cschool)+sd(bpmtot.df13$Cschool) 
high.school #Check value of variable
library(mediation)
Mod.Med.high.school20 <- mediate(fit_m20, fit_y20,    
                                 covariates = list(Cschool = high.school),    
                                 sims = 1000, treat='Co3', mediator='hplh2',
                                 cluster = bpmtot.df13$site_id0)
summary(Mod.Med.high.school20)


# tests whether the difference between indirect effects at each level of the moderator is significantly different from zero
Mod.Med.Testschool20 <- mediate(model.m = fit_m20, model.y = fit_y20, sims = 1000, 
                                treat='Co3', mediator='hplh2',
                                cluster = bpmtot.df13$site_id0)
summary(Mod.Med.Testschool20)
test.modmed(Mod.Med.Testschool20, covariates.1 = list(Cschool = low.school),   
            covariates.2 = list(Cschool = high.school), sims = 1000) 
#Here we specify both levels of the moderator that we want to test




##Standardized Coefficients
###Moderated Mediation Model
###List-wise Deletion
library(data.table)
library(dplyr)
library(ggplot2)
library(mice)
library(lme4)
library(lmerTest)
library(miceadds)
library(broom.mixed)

library(haven)
participant_structure_cognitive <- read_sav("D:/ME/BNU/ABCD/environment-culture-brain/participant_structure_total_brain.sav")
View(participant_structure_cognitive)


participant_structure_cognitive <- replace(participant_structure_cognitive, participant_structure_cognitive  ==  999.000, NA);participant_structure_cognitive
summary(participant_structure_cognitive)
names(participant_structure_cognitive)
str(participant_structure_cognitive)



names(participant_structure_cognitive)
###summarize missingness for the variables listed
dat_nms = c("site_id0", "family_id0", "age0", "age2", 
            "race", "sex","s_type2",
            "hplh0","hplh2",  "hprh0","hprh2",
            "agrh0","agrh2",  "aglh0","aglh2","totalvolume2",
            "neighbor","o3", 
            "totalcog2","totalcog0")

if (sum(as.numeric(dat_nms %in% names(participant_structure_cognitive))) != length(dat_nms))
  print("Error: missing core demographics. Add those first")
dat = data.table(participant_structure_cognitive[,dat_nms])
for (m in dat_nms) print(paste("number missing for ",m,": ",sum(is.na(participant_structure_cognitive[[m]]) | (participant_structure_cognitive[m] == "")),sep=""))

# Number of multiple imputed datasets & maximum number of iterations
n.imp = 20 
n.iter = 5


#set the variable list
var.ls1 <- c("site_id0", "family_id0", "age0", "age2", 
             "race", "sex","s_type2",
             "hplh0","hplh2",  "hprh0","hprh2",
             "agrh0","agrh2",  "aglh0","aglh2","totalvolume2",
             "neighbor","o3", 
             "totalcog2","totalcog0")
dat_mod1 <- dat[, var.ls1, with = FALSE ]

str(dat_mod1)
summary(dat_mod1)
names(dat_mod1)



newdata1<-dat_mod1[complete.cases(dat_mod1),]
newdata1<-na.omit(dat_mod1)
summary(newdata1)
sum(is.na(newdata1))
str(newdata1)


standardized_newdata1 <- scale(newdata1)
standardized_newdata1 <- as.data.frame(standardized_newdata1)
View(standardized_newdata1)



fit_m1 <-lm(agrh2 ~ o3 * neighbor + agrh0 + 
              race + sex + age2 + s_type2 +totalvolume2,
            data = standardized_newdata1)
summary(fit_m1)


fit_y1 <-lm(totalcog2 ~ o3 * neighbor + agrh2 + totalcog0 +
              race + sex + age2 , data = standardized_newdata1)
summary(fit_y1)


low.neighbor<-mean(standardized_newdata1$neighbor,na.rm = TRUE)-sd(standardized_newdata1$neighbor,na.rm = TRUE) 
low.neighbor #Check value of variable
library(mediation)
Mod.Med.low.neighbor1 <- mediate(fit_m1, fit_y1,    
                                 covariates = list(neighbor = low.neighbor), 
                                 sims = 4000, treat='o3', mediator='agrh2',
                                 cluster = standardized_newdata1$site_id0)
summary(Mod.Med.low.neighbor1,options(digits=10))
##Extract Results
acme_ci <- c(Mod.Med.low.neighbor1$d0.ci[1], Mod.Med.low.neighbor1$d0.ci[2])
ade_ci <- c(Mod.Med.low.neighbor1$z0.ci[1], Mod.Med.low.neighbor1$z0.ci[2])

# Format Decimal Places to 4 Digits
acme_ci <- sprintf("%.4f", acme_ci)
ade_ci <- sprintf("%.4f", ade_ci)

# Output the Formatted Results
cat("ACME 95% CI:", acme_ci[1], "to", acme_ci[2], "\n")
cat("ADE 95% CI:", ade_ci[1], "to", ade_ci[2], "\n")


options(digits=10)
high.neighbor<- mean(standardized_newdata1$neighbor,na.rm = TRUE)+sd(standardized_newdata1$neighbor,na.rm = TRUE) 
high.neighbor #Check value of variable
library(mediation)
Mod.Med.high.neighbor1 <- mediate(fit_m1, fit_y1,    
                                  covariates = list(neighbor = high.neighbor),    
                                  sims = 4000, treat='o3', mediator='agrh2',
                                  cluster = standardized_newdata1$site_id0)
summary(Mod.Med.high.neighbor1)

acme_ci <- c(Mod.Med.high.neighbor1$d0.ci[1], Mod.Med.high.neighbor1$d0.ci[2])
ade_ci <- c(Mod.Med.high.neighbor1$z0.ci[1], Mod.Med.high.neighbor1$z0.ci[2])


acme_ci <- sprintf("%.4f", acme_ci)
ade_ci <- sprintf("%.4f", ade_ci)


cat("ACME 95% CI:", acme_ci[1], "to", acme_ci[2], "\n")
cat("ADE 95% CI:", ade_ci[1], "to", ade_ci[2], "\n")

# tests whether the difference between indirect effects at each level of the moderator is significantly different from zero
Mod.Med.Testneighbor1 <- mediate(model.m = fit_m1, model.y = fit_y1, sims = 4000, 
                                 treat='o3', mediator='agrh2',
                                 cluster = standardized_newdata1$site_id0)

test.modmed(Mod.Med.Testneighbor1, covariates.1 = list(neighbor = low.neighbor),   
            covariates.2 = list(neighbor = high.neighbor), sims = 4000) 
#Here we specify both levels of the moderator that we want to test



####psychotic-like experiences as DV###
names(participant_structure_cognitive)
###summarize missingness for the variables listed
dat_nms = c("site_id0", "family_id0", "age0", "age2", 
            "race", "sex","s_type2",
            "hplh0","hplh2",  "hprh0","hprh2",
            "agrh0","agrh2",  "aglh0","aglh2",
            "neighbor","o3","totalvolume2",
            "ples2","ples0")

if (sum(as.numeric(dat_nms %in% names(participant_structure_cognitive))) != length(dat_nms))
  print("Error: missing core demographics. Add those first")
dat = data.table(participant_structure_cognitive[,dat_nms])
for (m in dat_nms) print(paste("number missing for ",m,": ",sum(is.na(participant_structure_cognitive[[m]]) | (participant_structure_cognitive[m] == "")),sep=""))

# Number of multiple imputed datasets & maximum number of iterations
n.imp = 20 
n.iter = 5


#set the variable list
var.ls13 <- c("site_id0", "family_id0", "age0", "age2", 
              "race", "sex","s_type2",
              "hplh0","hplh2",  "hprh0","hprh2",
              "agrh0","agrh2",  "aglh0","aglh2","totalvolume2",
              "neighbor","o3",
              "ples2","ples0")
dat_mod13 <- dat[, var.ls13, with = FALSE ]

str(dat_mod13)
summary(dat_mod13)
names(dat_mod13)

newdata13<-dat_mod13[complete.cases(dat_mod13),]
newdata13<-na.omit(dat_mod13)
summary(newdata13)
sum(is.na(newdata13))
str(newdata13)


standardized_newdata13 <- scale(newdata13)
standardized_newdata13 <- as.data.frame(standardized_newdata13)
View(standardized_newdata13)



fit_m20 <-lm(agrh2 ~ o3 * neighbor + agrh0 + 
               race + sex + age2 + s_type2 +totalvolume2,
             data = standardized_newdata13)
summary(fit_m20)



fit_y20 <-lm(ples2 ~ o3 * neighbor + agrh2 + ples0 +
               race + sex + age2  , data = standardized_newdata13)
summary(fit_y20)


low.neighbor<-mean(standardized_newdata13$neighbor)-sd(standardized_newdata13$neighbor) 
low.neighbor #Check value of variable
library(mediation)
Mod.Med.low.neighbor20 <- mediate(fit_m20, fit_y20,    
                                  covariates = list(neighbor = low.neighbor),  
                                  sims = 4000, treat='o3', mediator='agrh2',
                                  cluster = standardized_newdata13$site_id0)
summary(Mod.Med.low.neighbor20)

acme_ci <- c(Mod.Med.low.neighbor20$d0.ci[1], Mod.Med.low.neighbor20$d0.ci[2])
ade_ci <- c(Mod.Med.low.neighbor20$z0.ci[1], Mod.Med.low.neighbor20$z0.ci[2])


acme_ci <- sprintf("%.4f", acme_ci)
ade_ci <- sprintf("%.4f", ade_ci)


cat("ACME 95% CI:", acme_ci[1], "to", acme_ci[2], "\n")
cat("ADE 95% CI:", ade_ci[1], "to", ade_ci[2], "\n")



high.neighbor<-mean(standardized_newdata13$neighbor)+sd(standardized_newdata13$neighbor) 
high.neighbor #Check value of variable
library(mediation)
Mod.Med.high.neighbor20 <- mediate(fit_m20, fit_y20,    
                                   covariates = list(neighbor = high.neighbor),    
                                   sims = 4000, treat='o3', mediator='agrh2',
                                   cluster = standardized_newdata13$site_id0)
summary(Mod.Med.high.neighbor20)

acme_ci <- c(Mod.Med.high.neighbor20$d0.ci[1], Mod.Med.high.neighbor20$d0.ci[2])
ade_ci <- c(Mod.Med.high.neighbor20$z0.ci[1], Mod.Med.high.neighbor20$z0.ci[2])


acme_ci <- sprintf("%.4f", acme_ci)
ade_ci <- sprintf("%.4f", ade_ci)


cat("ACME 95% CI:", acme_ci[1], "to", acme_ci[2], "\n")
cat("ADE 95% CI:", ade_ci[1], "to", ade_ci[2], "\n")

# tests whether the difference between indirect effects at each level of the moderator is significantly different from zero
Mod.Med.Testneighbor20 <- mediate(model.m = fit_m20, model.y = fit_y20, sims = 4000, 
                                  treat='o3', mediator='agrh2',
                                  cluster = standardized_newdata13$site_id0)
summary(Mod.Med.Testneighbor20)
test.modmed(Mod.Med.Testneighbor20, covariates.1 = list(neighbor = low.neighbor),   
            covariates.2 = list(neighbor = high.neighbor), sims = 4000) 
#Here we specify both levels of the moderator that we want to test


### total cognition scores as DV
names(participant_structure_cognitive)
###summarize missingness for the variables listed
dat_nms = c("site_id0", "family_id0", "age0", "age2", 
            "race", "sex","s_type2",
            "hplh0","hplh2",  "hprh0","hprh2",
            "agrh0","agrh2",  "aglh0","aglh2","totalvolume2",
            "school","o3", 
            "totalcog2","totalcog0")

if (sum(as.numeric(dat_nms %in% names(participant_structure_cognitive))) != length(dat_nms))
  print("Error: missing core demographics. Add those first")
dat = data.table(participant_structure_cognitive[,dat_nms])
for (m in dat_nms) print(paste("number missing for ",m,": ",sum(is.na(participant_structure_cognitive[[m]]) | (participant_structure_cognitive[m] == "")),sep=""))

# Number of multiple imputed datasets & maximum number of iterations
n.imp = 20 
n.iter = 5



#set the variable list
var.ls1 <- c("site_id0", "family_id0", "age0", "age2", 
             "race", "sex","s_type2",
             "hplh0","hplh2",  "hprh0","hprh2",
             "agrh0","agrh2",  "aglh0","aglh2","totalvolume2",
             "school","o3", 
             "totalcog2","totalcog0")
dat_mod1 <- dat[, var.ls1, with = FALSE ]

str(dat_mod1)
summary(dat_mod1)
names(dat_mod1)

newdata1<-dat_mod1[complete.cases(dat_mod1),]
newdata1<-na.omit(dat_mod1)
summary(newdata1)
sum(is.na(newdata1))
str(newdata1)


standardized_newdata1 <- scale(newdata1)
standardized_newdata1 <- as.data.frame(standardized_newdata1)
View(standardized_newdata1)



fit_m1 <-lm(hplh2 ~ o3 * school + hplh0 + 
              race + sex + age2 + s_type2 +totalvolume2,
            data = standardized_newdata1)
summary(fit_m1)


fit_y1 <-lm(totalcog2 ~ o3 * school + hplh2 + totalcog0 +
              race + sex + age2 , data = standardized_newdata1)
summary(fit_y1)



low.school<-mean(standardized_newdata1$school,na.rm = TRUE)-sd(standardized_newdata1$school,na.rm = TRUE) 
low.school #Check value of variable
library(mediation)
Mod.Med.low.school1 <- mediate(fit_m1, fit_y1,    
                               covariates = list(school = low.school), 
                               sims = 4000, treat='o3', mediator='hplh2',
                               cluster = standardized_newdata1$site_id0)
summary(Mod.Med.low.school1)

acme_ci <- c(Mod.Med.low.school1$d0.ci[1], Mod.Med.low.school1$d0.ci[2])
ade_ci <- c(Mod.Med.low.school1$z0.ci[1], Mod.Med.low.school1$z0.ci[2])


acme_ci <- sprintf("%.4f", acme_ci)
ade_ci <- sprintf("%.4f", ade_ci)


cat("ACME 95% CI:", acme_ci[1], "to", acme_ci[2], "\n")
cat("ADE 95% CI:", ade_ci[1], "to", ade_ci[2], "\n")



high.school<- mean(standardized_newdata1$school,na.rm = TRUE)+sd(standardized_newdata1$school,na.rm = TRUE) 
high.school #Check value of variable
library(mediation)
Mod.Med.high.school1 <- mediate(fit_m1, fit_y1,    
                                covariates = list(school = high.school),    
                                sims = 4000, treat='o3', mediator='hplh2',
                                cluster = standardized_newdata1$site_id0)
summary(Mod.Med.high.school1)

acme_ci <- c(Mod.Med.high.school1$d0.ci[1], Mod.Med.high.school1$d0.ci[2])
ade_ci <- c(Mod.Med.high.school1$z0.ci[1], Mod.Med.high.school1$z0.ci[2])


acme_ci <- sprintf("%.4f", acme_ci)
ade_ci <- sprintf("%.4f", ade_ci)


cat("ACME 95% CI:", acme_ci[1], "to", acme_ci[2], "\n")
cat("ADE 95% CI:", ade_ci[1], "to", ade_ci[2], "\n")

# tests whether the difference between indirect effects at each level of the moderator is significantly different from zero
Mod.Med.Testschool1 <- mediate(model.m = fit_m1, model.y = fit_y1, sims = 4000, 
                               treat='o3', mediator='hplh2',
                               cluster = standardized_newdata1$site_id0)

test.modmed(Mod.Med.Testschool1, covariates.1 = list(school = low.school),   
            covariates.2 = list(school = high.school), sims = 4000) 
#Here we specify both levels of the moderator that we want to test



####psychotic-like experiences as DV##
names(participant_structure_cognitive)
###summarize missingness for the variables listed
dat_nms = c("site_id0", "family_id0", "age0", "age2", 
            "race", "sex","s_type2",
            "hplh0","hplh2",  "hprh0","hprh2",
            "agrh0","agrh2",  "aglh0","aglh2",
            "school","o3","totalvolume2",
            "ples2","ples0")

if (sum(as.numeric(dat_nms %in% names(participant_structure_cognitive))) != length(dat_nms))
  print("Error: missing core demographics. Add those first")
dat = data.table(participant_structure_cognitive[,dat_nms])
for (m in dat_nms) print(paste("number missing for ",m,": ",sum(is.na(participant_structure_cognitive[[m]]) | (participant_structure_cognitive[m] == "")),sep=""))

# Number of multiple imputed datasets & maximum number of iterations
n.imp = 20 
n.iter = 5



#set the variable list
var.ls13 <- c("site_id0", "family_id0", "age0", "age2", 
              "race", "sex","s_type2",
              "hplh0","hplh2",  "hprh0","hprh2",
              "agrh0","agrh2",  "aglh0","aglh2","totalvolume2",
              "school","o3",
              "ples2","ples0")
dat_mod13 <- dat[, var.ls13, with = FALSE ]

str(dat_mod13)
summary(dat_mod13)
names(dat_mod13)



newdata13<-dat_mod13[complete.cases(dat_mod13),]
newdata13<-na.omit(dat_mod13)
summary(newdata13)
sum(is.na(newdata13))
str(newdata13)


standardized_newdata13 <- scale(newdata13)
standardized_newdata13 <- as.data.frame(standardized_newdata13)
View(standardized_newdata13)


fit_m20 <-lm(hplh2 ~ o3 * school + hplh0 + 
               race + sex + age2 + s_type2 +totalvolume2,
             data = standardized_newdata13)
summary(fit_m20)



fit_y20 <-lm(ples2 ~ o3 * school + hplh2 + ples0 +
               race + sex + age2  , data = standardized_newdata13)
summary(fit_y20)


low.school<-mean(standardized_newdata13$school)-sd(standardized_newdata13$school) 
low.school #Check value of variable
library(mediation)
Mod.Med.low.school20 <- mediate(fit_m20, fit_y20,    
                                covariates = list(school = low.school),  
                                sims = 4000, treat='o3', mediator='hplh2',
                                cluster = standardized_newdata13$site_id0)
summary(Mod.Med.low.school20)

acme_ci <- c(Mod.Med.low.school20$d0.ci[1], Mod.Med.low.school20$d0.ci[2])
ade_ci <- c(Mod.Med.low.school20$z0.ci[1], Mod.Med.low.school20$z0.ci[2])


acme_ci <- sprintf("%.4f", acme_ci)
ade_ci <- sprintf("%.4f", ade_ci)


cat("ACME 95% CI:", acme_ci[1], "to", acme_ci[2], "\n")
cat("ADE 95% CI:", ade_ci[1], "to", ade_ci[2], "\n")



high.school<-mean(standardized_newdata13$school)+sd(standardized_newdata13$school) 
high.school #Check value of variable
library(mediation)
Mod.Med.high.school20 <- mediate(fit_m20, fit_y20,    
                                 covariates = list(school = high.school),    
                                 sims = 4000, treat='o3', mediator='hplh2',
                                 cluster = standardized_newdata13$site_id0)
summary(Mod.Med.high.school20)

acme_ci <- c(Mod.Med.high.school20$d0.ci[1], Mod.Med.high.school20$d0.ci[2])
ade_ci <- c(Mod.Med.high.school20$z0.ci[1], Mod.Med.high.school20$z0.ci[2])


acme_ci <- sprintf("%.4f", acme_ci)
ade_ci <- sprintf("%.4f", ade_ci)


cat("ACME 95% CI:", acme_ci[1], "to", acme_ci[2], "\n")
cat("ADE 95% CI:", ade_ci[1], "to", ade_ci[2], "\n")

# tests whether the difference between indirect effects at each level of the moderator is significantly different from zero
Mod.Med.Testschool20 <- mediate(model.m = fit_m20, model.y = fit_y20, sims = 4000, 
                                treat='o3', mediator='hplh2',
                                cluster = standardized_newdata13$site_id0)
summary(Mod.Med.Testschool20)
test.modmed(Mod.Med.Testschool20, covariates.1 = list(school = low.school),   
            covariates.2 = list(school = high.school), sims = 4000) 
#Here we specify both levels of the moderator that we want to test



##Standardized Coefficients
##Moderated Mediation Model
### MI Dataset
library(data.table)
library(dplyr)
library(ggplot2)
library(mice)
library(lme4)
library(lmerTest)
library(miceadds)
library(broom.mixed)

library(haven)
participant_structure_cognitive <- read_sav("D:/ME/BNU/ABCD/environment-culture-brain/participant_structure_total_brain.sav")
View(participant_structure_cognitive)


participant_structure_cognitive <- replace(participant_structure_cognitive, participant_structure_cognitive  ==  999.000, NA);participant_structure_cognitive
summary(participant_structure_cognitive)
names(participant_structure_cognitive)
str(participant_structure_cognitive)



### Neighbor as a Moderator Variable
### Total Cognitive Score as the Dependent Variable
names(participant_structure_cognitive)
###summarize missingness for the variables listed
dat_nms = c("site_id0", "family_id0", "age0", "age2", 
            "race", "sex","s_type2",
            "hplh0","hplh2",  "hprh0","hprh2",
            "agrh0","agrh2",  "aglh0","aglh2","totalvolume2",
            "neighbor","o3", 
            "totalcog2","totalcog0")

if (sum(as.numeric(dat_nms %in% names(participant_structure_cognitive))) != length(dat_nms))
  print("Error: missing core demographics. Add those first")
dat = data.table(participant_structure_cognitive[,dat_nms])
for (m in dat_nms) print(paste("number missing for ",m,": ",sum(is.na(participant_structure_cognitive[[m]]) | (participant_structure_cognitive[m] == "")),sep=""))

# Number of multiple imputed datasets & maximum number of iterations
n.imp = 20 
n.iter = 5



#set the variable list
var.ls1 <- c("site_id0", "family_id0", "age0", "age2", 
             "race", "sex","s_type2",
             "hplh0","hplh2",  "hprh0","hprh2",
             "agrh0","agrh2",  "aglh0","aglh2","totalvolume2",
             "neighbor","o3", 
             "totalcog2","totalcog0")
dat_mod1 <- dat[, var.ls1, with = FALSE ]

str(dat_mod1)
summary(dat_mod1)
names(dat_mod1)

ini <- mice( dat_mod1, m = 1, maxit = 0 )
meth = ini$meth


#put variables in that you want to impute

meth["age0"] <- "pmm" 
meth["age2"] <- "pmm" 
meth["s_type2"] <- "pmm" 
meth["hplh0"] <- "pmm"
meth["hprh0"] <- "pmm"
meth["aglh0"] <- "pmm"
meth["agrh0"] <- "pmm"
meth["hplh2"] <- "pmm"
meth["hprh2"] <- "pmm"
meth["aglh2"] <- "pmm"
meth["agrh2"] <- "pmm"
meth["o3"] <- "pmm"
meth["neighbor"] <- "pmm"

meth["totalvolume2"] <- "pmm"
meth["totalcog2"] <- "pmm"
meth["totalcog0"] <- "pmm"


pred = ini$pred
pred


# Specifying parameters for the imputation
post <- mice( dat_mod1, meth = meth, pred = pred, seed = 111,
              m = 1, maxit = 0)$post



bpmtot.imp1 <- mice( dat_mod1, meth = meth, pred = pred, post = post,
                     seed = 1111,
                     m = n.imp, maxit = n.iter)



library(data.table)
library(miceadds)
bpmtot.complete1 <- complete(bpmtot.imp1, action = "long")  
bpmtot.df1 <- as.data.frame(bpmtot.complete1) 
names(bpmtot.df1)
str(bpmtot.df1)


standardized_bpmtot.df1 <- scale(bpmtot.df1)
standardized_bpmtot.df1 <- as.data.frame(standardized_bpmtot.df1)
View(standardized_bpmtot.df1)



fit_m1 <-lm(agrh2 ~ o3 * neighbor + agrh0 + 
              race + sex + age2 + s_type2 +totalvolume2,
            data = standardized_bpmtot.df1)
summary(fit_m1)



fit_y1 <-lm(totalcog2 ~ o3 * neighbor + agrh2 + totalcog0 +
              race + sex + age2 , data = standardized_bpmtot.df1)
summary(fit_y1)


low.neighbor<-mean(standardized_bpmtot.df1$neighbor,na.rm = TRUE)-sd(standardized_bpmtot.df1$neighbor,na.rm = TRUE) 
low.neighbor #Check value of variable
library(mediation)
Mod.Med.low.neighbor1 <- mediate(fit_m1, fit_y1,    
                                 covariates = list(neighbor = low.neighbor), 
                                 sims = 1000, treat='o3', mediator='agrh2',
                                 cluster = standardized_bpmtot.df1$site_id0)
summary(Mod.Med.low.neighbor1)

acme_ci <- c(Mod.Med.low.neighbor1$d0.ci[1], Mod.Med.low.neighbor1$d0.ci[2])
ade_ci <- c(Mod.Med.low.neighbor1$z0.ci[1], Mod.Med.low.neighbor1$z0.ci[2])


acme_ci <- sprintf("%.4f", acme_ci)
ade_ci <- sprintf("%.4f", ade_ci)


cat("ACME 95% CI:", acme_ci[1], "to", acme_ci[2], "\n")
cat("ADE 95% CI:", ade_ci[1], "to", ade_ci[2], "\n")



high.neighbor<- mean(standardized_bpmtot.df1$neighbor,na.rm = TRUE)+sd(standardized_bpmtot.df1$neighbor,na.rm = TRUE) 
high.neighbor #Check value of variable
library(mediation)
Mod.Med.high.neighbor1 <- mediate(fit_m1, fit_y1,    
                                  covariates = list(neighbor = high.neighbor),    
                                  sims = 1000, treat='o3', mediator='agrh2',
                                  cluster = standardized_bpmtot.df1$site_id0)
summary(Mod.Med.high.neighbor1)


acme_ci <- c(Mod.Med.high.neighbor1$d0.ci[1], Mod.Med.high.neighbor1$d0.ci[2])
ade_ci <- c(Mod.Med.high.neighbor1$z0.ci[1], Mod.Med.high.neighbor1$z0.ci[2])


acme_ci <- sprintf("%.4f", acme_ci)
ade_ci <- sprintf("%.4f", ade_ci)


cat("ACME 95% CI:", acme_ci[1], "to", acme_ci[2], "\n")
cat("ADE 95% CI:", ade_ci[1], "to", ade_ci[2], "\n")

# tests whether the difference between indirect effects at each level of the moderator is significantly different from zero
Mod.Med.Testneighbor1 <- mediate(model.m = fit_m1, model.y = fit_y1, sims = 1000, 
                                 treat='o3', mediator='agrh2',
                                 cluster = standardized_bpmtot.df1$site_id0)

test.modmed(Mod.Med.Testneighbor1, covariates.1 = list(neighbor = low.neighbor),   
            covariates.2 = list(neighbor = high.neighbor), sims = 1000) 
#Here we specify both levels of the moderator that we want to test



####psychotic-like experiences as DV###
names(participant_structure_cognitive)
###summarize missingness for the variables listed
dat_nms = c("site_id0", "family_id0", "age0", "age2", 
            "race", "sex","s_type2",
            "hplh0","hplh2",  "hprh0","hprh2",
            "agrh0","agrh2",  "aglh0","aglh2","totalvolume2",
            "neighbor","o3",
            "ples2","ples0")

if (sum(as.numeric(dat_nms %in% names(participant_structure_cognitive))) != length(dat_nms))
  print("Error: missing core demographics. Add those first")
dat = data.table(participant_structure_cognitive[,dat_nms])
for (m in dat_nms) print(paste("number missing for ",m,": ",sum(is.na(participant_structure_cognitive[[m]]) | (participant_structure_cognitive[m] == "")),sep=""))

# Number of multiple imputed datasets & maximum number of iterations
n.imp = 20 
n.iter = 5

options(max.print = 20000)##显示20000行

#set the variable list
var.ls13 <- c("site_id0", "family_id0", "age0", "age2", 
              "race", "sex","s_type2",
              "hplh0","hplh2",  "hprh0","hprh2",
              "agrh0","agrh2",  "aglh0","aglh2","totalvolume2",
              "neighbor","o3",
              "ples2","ples0")
dat_mod13 <- dat[, var.ls13, with = FALSE ]

str(dat_mod13)
summary(dat_mod13)
names(dat_mod13)

ini <- mice( dat_mod13, m = 1, maxit = 0 )
meth = ini$meth


#put variables in that you want to impute

meth["age0"] <- "pmm" 
meth["age2"] <- "pmm" 
meth["s_type2"] <- "pmm" 
meth["hplh0"] <- "pmm"
meth["hprh0"] <- "pmm"
meth["aglh0"] <- "pmm"
meth["agrh0"] <- "pmm"
meth["hplh2"] <- "pmm"
meth["hprh2"] <- "pmm"
meth["aglh2"] <- "pmm"
meth["agrh2"] <- "pmm"
meth["o3"] <- "pmm"
meth["neighbor"] <- "pmm"

meth["totalvolume2"] <- "pmm"
meth["ples2"] <- "pmm"
meth["ples0"] <- "pmm"


pred = ini$pred
pred


# Specifying parameters for the imputation
post <- mice( dat_mod13, meth = meth, pred = pred, seed = 111,
              m = 1, maxit = 0)$post



bpmtot.imp13 <- mice( dat_mod13, meth = meth, pred = pred, post = post,
                      seed = 1111,
                      m = n.imp, maxit = n.iter)

library(data.table)
library(miceadds)
bpmtot.complete13 <- complete(bpmtot.imp13, action = "long")  
bpmtot.df13 <- as.data.frame(bpmtot.complete13)  
names(bpmtot.df13)
str(bpmtot.df13)



standardized_bpmtot.df13 <- scale(bpmtot.df13)
standardized_bpmtot.df13 <- as.data.frame(standardized_bpmtot.df13)
View(standardized_bpmtot.df13)


fit_m20 <-lm(agrh2 ~ o3 * neighbor + agrh0 + 
               race + sex + age2 + s_type2 +totalvolume2,
             data = standardized_bpmtot.df13)
summary(fit_m20)



fit_y20 <-lm(ples2 ~ o3 * neighbor + agrh2 + ples0 +
               race + sex + age2  , data = standardized_bpmtot.df13)
summary(fit_y20)


low.neighbor<-mean(standardized_bpmtot.df13$neighbor)-sd(standardized_bpmtot.df13$neighbor) 
low.neighbor #Check value of variable
library(mediation)
Mod.Med.low.neighbor20 <- mediate(fit_m20, fit_y20,    
                                  covariates = list(neighbor = low.neighbor),  
                                  sims = 1000, treat='o3', mediator='agrh2',
                                  cluster = standardized_bpmtot.df13$site_id0)
summary(Mod.Med.low.neighbor20)

acme_ci <- c(Mod.Med.low.neighbor20$d0.ci[1], Mod.Med.low.neighbor20$d0.ci[2])
ade_ci <- c(Mod.Med.low.neighbor20$z0.ci[1], Mod.Med.low.neighbor20$z0.ci[2])


acme_ci <- sprintf("%.4f", acme_ci)
ade_ci <- sprintf("%.4f", ade_ci)


cat("ACME 95% CI:", acme_ci[1], "to", acme_ci[2], "\n")
cat("ADE 95% CI:", ade_ci[1], "to", ade_ci[2], "\n")


high.neighbor<-mean(standardized_bpmtot.df13$neighbor)+sd(standardized_bpmtot.df13$neighbor) 
high.neighbor #Check value of variable
library(mediation)
Mod.Med.high.neighbor20 <- mediate(fit_m20, fit_y20,    
                                   covariates = list(neighbor = high.neighbor),    
                                   sims = 1000, treat='o3', mediator='agrh2',
                                   cluster = standardized_bpmtot.df13$site_id0)
summary(Mod.Med.high.neighbor20)

acme_ci <- c(Mod.Med.high.neighbor20$d0.ci[1], Mod.Med.high.neighbor20$d0.ci[2])
ade_ci <- c(Mod.Med.high.neighbor20$z0.ci[1], Mod.Med.high.neighbor20$z0.ci[2])


acme_ci <- sprintf("%.4f", acme_ci)
ade_ci <- sprintf("%.4f", ade_ci)


cat("ACME 95% CI:", acme_ci[1], "to", acme_ci[2], "\n")
cat("ADE 95% CI:", ade_ci[1], "to", ade_ci[2], "\n")

# tests whether the difference between indirect effects at each level of the moderator is significantly different from zero
Mod.Med.Testneighbor20 <- mediate(model.m = fit_m20, model.y = fit_y20, sims = 1000, 
                                  treat='o3', mediator='agrh2',
                                  cluster = standardized_bpmtot.df13$site_id0)
summary(Mod.Med.Testneighbor20)
test.modmed(Mod.Med.Testneighbor20, covariates.1 = list(neighbor = low.neighbor),   
            covariates.2 = list(neighbor = high.neighbor), sims = 1000) 
#Here we specify both levels of the moderator that we want to test



##School as a Moderator Variable
###Total Cognitive Score as the Dependent Variable

names(participant_structure_cognitive)
###summarize missingness for the variables listed
dat_nms = c("site_id0", "family_id0", "age0", "age2", 
            "race", "sex","s_type2",
            "hplh0","hplh2",  "hprh0","hprh2",
            "agrh0","agrh2",  "aglh0","aglh2","totalvolume2",
            "school","o3", 
            "totalcog2","totalcog0")

if (sum(as.numeric(dat_nms %in% names(participant_structure_cognitive))) != length(dat_nms))
  print("Error: missing core demographics. Add those first")
dat = data.table(participant_structure_cognitive[,dat_nms])
for (m in dat_nms) print(paste("number missing for ",m,": ",sum(is.na(participant_structure_cognitive[[m]]) | (participant_structure_cognitive[m] == "")),sep=""))

# Number of multiple imputed datasets & maximum number of iterations
n.imp = 20 
n.iter = 5


#set the variable list
var.ls1 <- c("site_id0", "family_id0", "age0", "age2", 
             "race", "sex","s_type2",
             "hplh0","hplh2",  "hprh0","hprh2",
             "agrh0","agrh2",  "aglh0","aglh2","totalvolume2",
             "school","o3",
             "totalcog2","totalcog0")
dat_mod1 <- dat[, var.ls1, with = FALSE ]

str(dat_mod1)
summary(dat_mod1)
names(dat_mod1)

ini <- mice( dat_mod1, m = 1, maxit = 0 )
meth = ini$meth


#put variables in that you want to impute

meth["age0"] <- "pmm" 
meth["age2"] <- "pmm" 
meth["s_type2"] <- "pmm" 
meth["hplh0"] <- "pmm"
meth["hprh0"] <- "pmm"
meth["aglh0"] <- "pmm"
meth["agrh0"] <- "pmm"
meth["hplh2"] <- "pmm"
meth["hprh2"] <- "pmm"
meth["aglh2"] <- "pmm"
meth["agrh2"] <- "pmm"
meth["o3"] <- "pmm"
meth["school"] <- "pmm"

meth["totalvolume2"] <- "pmm"
meth["totalcog2"] <- "pmm"
meth["totalcog0"] <- "pmm"


pred = ini$pred

pred


# Specifying parameters for the imputation
post <- mice( dat_mod1, meth = meth, pred = pred, seed = 111,
              m = 1, maxit = 0)$post



bpmtot.imp1 <- mice( dat_mod1, meth = meth, pred = pred, post = post,
                     seed = 1111,
                     m = n.imp, maxit = n.iter)

library(data.table)
library(miceadds)
bpmtot.complete1 <- complete(bpmtot.imp1, action = "long")  
bpmtot.df1 <- as.data.frame(bpmtot.complete1)  
names(bpmtot.df1)
str(bpmtot.df1)


standardized_bpmtot.df1 <- scale(bpmtot.df1)
standardized_bpmtot.df1 <- as.data.frame(standardized_bpmtot.df1)
View(standardized_bpmtot.df1)



fit_m1 <-lm(hplh2 ~ o3 * school + hplh0 + 
              race + sex + age2 + s_type2 +totalvolume2,
            data = standardized_bpmtot.df1)
summary(fit_m1)


fit_y1 <-lm(totalcog2 ~ o3 * school + hplh2 + totalcog0 +
              race + sex + age2 , data = standardized_bpmtot.df1)
summary(fit_y1)


low.school<-mean(standardized_bpmtot.df1$school,na.rm = TRUE)-sd(standardized_bpmtot.df1$school,na.rm = TRUE) 
low.school #Check value of variable
library(mediation)
Mod.Med.low.school1 <- mediate(fit_m1, fit_y1,    
                               covariates = list(school = low.school), 
                               sims = 1000, treat='o3', mediator='hplh2',
                               cluster = standardized_bpmtot.df1$site_id0)
summary(Mod.Med.low.school1)

acme_ci <- c(Mod.Med.low.school1$d0.ci[1], Mod.Med.low.school1$d0.ci[2])
ade_ci <- c(Mod.Med.low.school1$z0.ci[1], Mod.Med.low.school1$z0.ci[2])


acme_ci <- sprintf("%.4f", acme_ci)
ade_ci <- sprintf("%.4f", ade_ci)


cat("ACME 95% CI:", acme_ci[1], "to", acme_ci[2], "\n")
cat("ADE 95% CI:", ade_ci[1], "to", ade_ci[2], "\n")



high.school<- mean(standardized_bpmtot.df1$school,na.rm = TRUE)+sd(standardized_bpmtot.df1$school,na.rm = TRUE) 
high.school #Check value of variable
library(mediation)
Mod.Med.high.school1 <- mediate(fit_m1, fit_y1,    
                                covariates = list(school = high.school),    
                                sims = 1000, treat='o3', mediator='hplh2',
                                cluster = standardized_bpmtot.df1$site_id0)
summary(Mod.Med.high.school1)

acme_ci <- c(Mod.Med.high.school1$d0.ci[1], Mod.Med.high.school1$d0.ci[2])
ade_ci <- c(Mod.Med.high.school1$z0.ci[1], Mod.Med.high.school1$z0.ci[2])


acme_ci <- sprintf("%.4f", acme_ci)
ade_ci <- sprintf("%.4f", ade_ci)


cat("ACME 95% CI:", acme_ci[1], "to", acme_ci[2], "\n")
cat("ADE 95% CI:", ade_ci[1], "to", ade_ci[2], "\n")

# tests whether the difference between indirect effects at each level of the moderator is significantly different from zero
Mod.Med.Testschool1 <- mediate(model.m = fit_m1, model.y = fit_y1, sims = 1000, 
                               treat='o3', mediator='hplh2',
                               cluster = standardized_bpmtot.df1$site_id0)

test.modmed(Mod.Med.Testschool1, covariates.1 = list(school = low.school),   
            covariates.2 = list(school = high.school), sims = 1000) 
#Here we specify both levels of the moderator that we want to test


####psychotic-like experiences as DV ###
names(participant_structure_cognitive)
###summarize missingness for the variables listed
dat_nms = c("site_id0", "family_id0", "age0", "age2", 
            "race", "sex","s_type2",
            "hplh0","hplh2",  "hprh0","hprh2",
            "agrh0","agrh2",  "aglh0","aglh2","totalvolume2",
            "school","o3",
            "ples2","ples0")

if (sum(as.numeric(dat_nms %in% names(participant_structure_cognitive))) != length(dat_nms))
  print("Error: missing core demographics. Add those first")
dat = data.table(participant_structure_cognitive[,dat_nms])
for (m in dat_nms) print(paste("number missing for ",m,": ",sum(is.na(participant_structure_cognitive[[m]]) | (participant_structure_cognitive[m] == "")),sep=""))

# Number of multiple imputed datasets & maximum number of iterations
n.imp = 20 
n.iter = 5


#set the variable list
var.ls13 <- c("site_id0", "family_id0", "age0", "age2", 
              "race", "sex","s_type2",
              "hplh0","hplh2",  "hprh0","hprh2",
              "agrh0","agrh2",  "aglh0","aglh2","totalvolume2",
              "school","o3",
              "ples2","ples0")
dat_mod13 <- dat[, var.ls13, with = FALSE ]

str(dat_mod13)
summary(dat_mod13)
names(dat_mod13)

ini <- mice( dat_mod13, m = 1, maxit = 0 )
meth = ini$meth


#put variables in that you want to impute

meth["age0"] <- "pmm" 
meth["age2"] <- "pmm" 
meth["s_type2"] <- "pmm" 
meth["hplh0"] <- "pmm"
meth["hprh0"] <- "pmm"
meth["aglh0"] <- "pmm"
meth["agrh0"] <- "pmm"
meth["hplh2"] <- "pmm"
meth["hprh2"] <- "pmm"
meth["aglh2"] <- "pmm"
meth["agrh2"] <- "pmm"
meth["o3"] <- "pmm"
meth["school"] <- "pmm"

meth["totalvolume2"] <- "pmm"
meth["ples2"] <- "pmm"
meth["ples0"] <- "pmm"


pred = ini$pred

pred


# Specifying parameters for the imputation
post <- mice( dat_mod13, meth = meth, pred = pred, seed = 111,
              m = 1, maxit = 0)$post



bpmtot.imp13 <- mice( dat_mod13, meth = meth, pred = pred, post = post,
                      seed = 1111,
                      m = n.imp, maxit = n.iter)

library(data.table)
library(miceadds)
bpmtot.complete13 <- complete(bpmtot.imp13, action = "long")  
bpmtot.df13 <- as.data.frame(bpmtot.complete13) 
names(bpmtot.df13)
str(bpmtot.df13)



standardized_bpmtot.df13 <- scale(bpmtot.df13)
standardized_bpmtot.df13 <- as.data.frame(standardized_bpmtot.df13)
View(standardized_bpmtot.df13)


fit_m20 <-lm(hplh2 ~ o3 * school + hplh0 + 
               race + sex + age2 + s_type2 +totalvolume2,
             data = standardized_bpmtot.df13)
summary(fit_m20)


fit_y20 <-lm(ples2 ~ o3 * school + hplh2 + ples0 +
               race + sex + age2  , data = standardized_bpmtot.df13)
summary(fit_y20)


low.school<-mean(standardized_bpmtot.df13$school)-sd(standardized_bpmtot.df13$school) 
low.school #Check value of variable
library(mediation)
Mod.Med.low.school20 <- mediate(fit_m20, fit_y20,    
                                covariates = list(school = low.school),  
                                sims = 1000, treat='o3', mediator='hplh2',
                                cluster = standardized_bpmtot.df13$site_id0)
summary(Mod.Med.low.school20)

acme_ci <- c(Mod.Med.low.school20$d0.ci[1], Mod.Med.low.school20$d0.ci[2])
ade_ci <- c(Mod.Med.low.school20$z0.ci[1], Mod.Med.low.school20$z0.ci[2])


acme_ci <- sprintf("%.4f", acme_ci)
ade_ci <- sprintf("%.4f", ade_ci)


cat("ACME 95% CI:", acme_ci[1], "to", acme_ci[2], "\n")
cat("ADE 95% CI:", ade_ci[1], "to", ade_ci[2], "\n")



high.school<-mean(standardized_bpmtot.df13$school)+sd(standardized_bpmtot.df13$school) 
high.school #Check value of variable
library(mediation)
Mod.Med.high.school20 <- mediate(fit_m20, fit_y20,    
                                 covariates = list(school = high.school),    
                                 sims = 1000, treat='o3', mediator='hplh2',
                                 cluster = standardized_bpmtot.df13$site_id0)
summary(Mod.Med.high.school20)

acme_ci <- c(Mod.Med.high.school20$d0.ci[1], Mod.Med.high.school20$d0.ci[2])
ade_ci <- c(Mod.Med.high.school20$z0.ci[1], Mod.Med.high.school20$z0.ci[2])


acme_ci <- sprintf("%.4f", acme_ci)
ade_ci <- sprintf("%.4f", ade_ci)


cat("ACME 95% CI:", acme_ci[1], "to", acme_ci[2], "\n")
cat("ADE 95% CI:", ade_ci[1], "to", ade_ci[2], "\n")

# tests whether the difference between indirect effects at each level of the moderator is significantly different from zero
Mod.Med.Testschool20 <- mediate(model.m = fit_m20, model.y = fit_y20, sims = 1000, 
                                treat='o3', mediator='hplh2',
                                cluster = standardized_bpmtot.df13$site_id0)
summary(Mod.Med.Testschool20)
test.modmed(Mod.Med.Testschool20, covariates.1 = list(school = low.school),   
            covariates.2 = list(school = high.school), sims = 1000) 
#Here we specify both levels of the moderator that we want to test

