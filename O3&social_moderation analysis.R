### Unstandardized Coefficients
###Moderation Model
###List-wise Deletion
library(data.table)
library(dplyr)
library(mice)
library(lme4)
library(miceadds)
library(broom.mixed)
library(haven)
structure_pm25o3no2 <- read_sav("D:/ME/ABCD/environment-culture-brain/participant_structure_total_brain.sav")
View(structure_pm25o3no2)


structure_pm25o3no2 <- replace(structure_pm25o3no2, structure_pm25o3no2  ==  999.000, NA);structure_pm25o3no2
summary(structure_pm25o3no2)
names(structure_pm25o3no2)
str(structure_pm25o3no2)


####school as moderator
###summarize missingness for the variables listed
dat_nms = c( "site_id0", "family_id0", "age0", "age2" ,
             "race", "sex","s_type2",
             "hplh0","aglh0","hprh0","agrh0",
             "hplh2","aglh2","hprh2","agrh2", "totalvolume2",
             "Cschool","Co3")

if (sum(as.numeric(dat_nms %in% names(structure_pm25o3no2))) != length(dat_nms))
  print("Error: missing core demographics. Add those first")
dat = data.table(structure_pm25o3no2[,dat_nms])
for (m in dat_nms) print(paste("number missing for ",m,": ",sum(is.na(structure_pm25o3no2[[m]]) | (structure_pm25o3no2[m] == "")),sep=""))

options(max.print = 20000)

#set the variable list
var.ls61 <- c( "site_id0", "family_id0", "age0", "age2" ,
               "race", "sex","s_type2",
               "hplh0","aglh0","hprh0","agrh0",
               "hplh2","aglh2","hprh2","agrh2", "totalvolume2",
               "Cschool","Co3")
dat_mod61 <- dat[, var.ls61, with = FALSE ]

summary(dat_mod61)


newdata61<-dat_mod61[complete.cases(dat_mod61),]
newdata61<-na.omit(dat_mod61)
summary(newdata61)
sum(is.na(newdata61))
str(newdata61)
View(newdata61)



model399 <- lm(agrh2 ~ Co3 * Cschool + agrh0 + 
                 race + sex + age2 + s_type2 + totalvolume2 ,
               data = newdata61)
tidy(model399, conf.int = TRUE)


library(estimatr)  # Run models with lm_robust()

model_lmrobust_clustered399 <- lm_robust(agrh2 ~ Co3 * Cschool + agrh0 + 
                                           race + sex + age2 + s_type2 + totalvolume2 , 
                                         fixed_effects = ~ site_id0,
                                         se_type = "stata",
                                         clusters = site_id0,
                                         data = newdata61)

tidy(model_lmrobust_clustered399, conf.int = TRUE)



model400 <- lm(aglh2 ~ Co3 * Cschool + aglh0 + 
                 race + sex + age2 + s_type2 +totalvolume2 ,
               data = newdata61)
tidy(model400, conf.int = TRUE)


library(estimatr)  # Run models with lm_robust()

model_lmrobust_clustered400 <- lm_robust(aglh2 ~ Co3 * Cschool + aglh0 + 
                                           race + sex + age2 + s_type2+totalvolume2 , 
                                         fixed_effects = ~ site_id0,
                                         se_type = "stata",
                                         clusters = site_id0,
                                         data = newdata61)

tidy(model_lmrobust_clustered400, conf.int = TRUE)


model401 <- lm(hplh2 ~ Co3 * Cschool + hplh0 + 
                 race + sex + age2 + s_type2 +totalvolume2 ,
               data = newdata61)
tidy(model401, conf.int = TRUE)


library(estimatr)  # Run models with lm_robust()

model_lmrobust_clustered401 <- lm_robust(hplh2 ~ Co3 * Cschool + hplh0 + 
                                           race + sex + age2 + s_type2 +totalvolume2, 
                                         fixed_effects = ~ site_id0,
                                         se_type = "stata",
                                         clusters = site_id0,
                                         data = newdata61)

tidy(model_lmrobust_clustered401, conf.int = TRUE)

library(interactions)
johnson_neyman(model =model_lmrobust_clustered401 ,pred = Co3, modx = Cschool)


model402 <- lm(hprh2 ~ Co3 * Cschool + hprh0 + 
                 race + sex + age2 + s_type2 +totalvolume2,
               data = newdata61)
tidy(model402, conf.int = TRUE)


library(estimatr)  # Run models with lm_robust()

model_lmrobust_clustered402 <- lm_robust(hprh2 ~ Co3 * Cschool + hprh0 + 
                                           race + sex + age2 + s_type2 +totalvolume2 , 
                                         fixed_effects = ~ site_id0,
                                         se_type = "stata",
                                         clusters = site_id0,
                                         data = newdata61)

tidy(model_lmrobust_clustered402, conf.int = TRUE)


names(structure_pm25o3no2)
####neighbor as moderator
###summarize missingness for the variables listed
dat_nms = c( "site_id0", "family_id0", "age0", "age2" ,
             "race", "sex","s_type2",
             "hplh0","aglh0","hprh0","agrh0",
             "hplh2","aglh2","hprh2","agrh2", "totalvolume2",
             "Cneighbor","Co3")

if (sum(as.numeric(dat_nms %in% names(structure_pm25o3no2))) != length(dat_nms))
  print("Error: missing core demographics. Add those first")
dat = data.table(structure_pm25o3no2[,dat_nms])
for (m in dat_nms) print(paste("number missing for ",m,": ",sum(is.na(structure_pm25o3no2[[m]]) | (structure_pm25o3no2[m] == "")),sep=""))


options(max.print = 20000)

#set the variable list
var.ls63 <- c( "site_id0", "family_id0", "age0", "age2" ,
               "race", "sex","s_type2",
               "hplh0","aglh0","hprh0","agrh0",
               "hplh2","aglh2","hprh2","agrh2", "totalvolume2",
               "Cneighbor","Co3")
dat_mod63 <- dat[, var.ls63, with = FALSE ]

summary(dat_mod63)


newdata63 <- dat_mod63[complete.cases(dat_mod63),]
newdata63 <- na.omit(dat_mod63)
summary(newdata63)
sum(is.na(newdata63))
str(newdata63)



model407 <- lm(agrh2 ~ Co3 * Cneighbor + agrh0 + 
                 race + sex + age2 + s_type2 +totalvolume2 ,
               data = newdata63)
tidy(model407, conf.int = TRUE)


library(estimatr)  # Run models with lm_robust()

model_lmrobust_clustered407 <- lm_robust(agrh2 ~ Co3 * Cneighbor + agrh0 + 
                                           race + sex + age2 + s_type2 +totalvolume2 , 
                                         fixed_effects = ~ site_id0,
                                         se_type = "stata",
                                         clusters = site_id0,
                                         data = newdata63)

tidy(model_lmrobust_clustered407, conf.int = TRUE)
library(interactions)
johnson_neyman(model =model_lmrobust_clustered407 ,pred = Co3, modx = Cneighbor)



model408 <- lm(aglh2 ~ Co3 * Cneighbor + aglh0 + 
                 race + sex + age2 + s_type2 +totalvolume2,
               data = newdata63)
tidy(model408, conf.int = TRUE)


library(estimatr)  # Run models with lm_robust()

model_lmrobust_clustered408 <- lm_robust(aglh2 ~ Co3 * Cneighbor + aglh0 + 
                                           race + sex + age2 + s_type2 +totalvolume2, 
                                         fixed_effects = ~ site_id0,
                                         se_type = "stata",
                                         clusters = site_id0,
                                         data = newdata63)

tidy(model_lmrobust_clustered408, conf.int = TRUE)



model409 <- lm(hplh2 ~ Co3 * Cneighbor + hplh0 + 
                 race + sex + age2 + s_type2+totalvolume2,
               data = newdata63)
tidy(model409, conf.int = TRUE)


library(estimatr)  # Run models with lm_robust()

model_lmrobust_clustered409 <- lm_robust(hplh2 ~ Co3 * Cneighbor + hplh0 + 
                                           race + sex + age2 + s_type2 +totalvolume2, 
                                         fixed_effects = ~ site_id0,
                                         se_type = "stata",
                                         clusters = site_id0,
                                         data = newdata63)

tidy(model_lmrobust_clustered409, conf.int = TRUE)



model410 <- lm(hprh2 ~ Co3 * Cneighbor + hprh0 + 
                 race + sex + age2 + s_type2 +totalvolume2,
               data = newdata63)
tidy(model410, conf.int = TRUE)


library(estimatr)  # Run models with lm_robust()

model_lmrobust_clustered410 <- lm_robust(hprh2 ~ Co3 * Cneighbor + hprh0 + 
                                           race + sex + age2 + s_type2 +totalvolume2 , 
                                         fixed_effects = ~ site_id0,
                                         se_type = "stata",
                                         clusters = site_id0,
                                         data = newdata63)

tidy(model_lmrobust_clustered410, conf.int = TRUE)



### standardized Coefficients
###Moderation Model
###List-wise Deletion
library(data.table)
library(dplyr)
library(mice)
library(lme4)
library(miceadds)
library(broom.mixed)
library(haven)
structure_pm25o3no2 <- read_sav("D:/ME/ABCD/environment-culture-brain/participant_structure_total_brain.sav")
View(structure_pm25o3no2)


structure_pm25o3no2 <- replace(structure_pm25o3no2, structure_pm25o3no2  ==  999.000, NA);structure_pm25o3no2
summary(structure_pm25o3no2)
names(structure_pm25o3no2)
str(structure_pm25o3no2)


####school as moderator 
###summarize missingness for the variables listed
dat_nms = c( "site_id0", "family_id0", "age0", "age2" ,
             "race", "sex","s_type2",
             "hplh0","aglh0","hprh0","agrh0",
             "hplh2","aglh2","hprh2","agrh2", "totalvolume2",
             "school","o3")

if (sum(as.numeric(dat_nms %in% names(structure_pm25o3no2))) != length(dat_nms))
  print("Error: missing core demographics. Add those first")
dat = data.table(structure_pm25o3no2[,dat_nms])
for (m in dat_nms) print(paste("number missing for ",m,": ",sum(is.na(structure_pm25o3no2[[m]]) | (structure_pm25o3no2[m] == "")),sep=""))


options(max.print = 20000)

#set the variable list
var.ls61 <- c( "site_id0", "family_id0", "age0", "age2" ,
               "race", "sex","s_type2",
               "hplh0","aglh0","hprh0","agrh0",
               "hplh2","aglh2","hprh2","agrh2", "totalvolume2",
               "school","o3")
dat_mod61 <- dat[, var.ls61, with = FALSE ]
summary(dat_mod61)


newdata61<-dat_mod61[complete.cases(dat_mod61),]
newdata61<-na.omit(dat_mod61)
summary(newdata61)
sum(is.na(newdata61))
str(newdata61)
View(newdata61)


##Standardize variables using the scale() function
standardized_newdata61 <- scale(newdata61)
standardized_newdata61 <- as.data.frame(standardized_newdata61)
View(standardized_newdata61)


model399 <- lm(agrh2 ~ o3 * school + agrh0 + 
                 race + sex + age2 + s_type2 + totalvolume2 ,
               data = standardized_newdata61)
tidy(model399, conf.int = TRUE)


library(estimatr)  # Run models with lm_robust()

model_lmrobust_clustered399 <- lm_robust(agrh2 ~ o3 * school + agrh0 + 
                                           race + sex + age2 + s_type2 + totalvolume2 , 
                                         fixed_effects = ~ site_id0,
                                         se_type = "stata",
                                         clusters = site_id0,
                                         data = standardized_newdata61)

tidy(model_lmrobust_clustered399, conf.int = TRUE)


model400 <- lm(aglh2 ~ o3 * school + aglh0 + 
                 race + sex + age2 + s_type2 +totalvolume2 ,
               data = standardized_newdata61)
tidy(model400, conf.int = TRUE)


library(estimatr)  # Run models with lm_robust()

model_lmrobust_clustered400 <- lm_robust(aglh2 ~ o3 * school + aglh0 + 
                                           race + sex + age2 + s_type2+totalvolume2 , 
                                         fixed_effects = ~ site_id0,
                                         se_type = "stata",
                                         clusters = site_id0,
                                         data = standardized_newdata61)

tidy(model_lmrobust_clustered400, conf.int = TRUE)



model401 <- lm(hplh2 ~ o3 * school + hplh0 + 
                 race + sex + age2 + s_type2 +totalvolume2 ,
               data = standardized_newdata61)
tidy(model401, conf.int = TRUE)


library(estimatr)  # Run models with lm_robust()

model_lmrobust_clustered401 <- lm_robust(hplh2 ~ o3 * school + hplh0 + 
                                           race + sex + age2 + s_type2 +totalvolume2, 
                                         fixed_effects = ~ site_id0,
                                         se_type = "stata",
                                         clusters = site_id0,
                                         data = standardized_newdata61)

tidy(model_lmrobust_clustered401, conf.int = TRUE)

library(interactions)
johnson_neyman(model =model_lmrobust_clustered401 ,pred = o3, modx = school)




model402 <- lm(hprh2 ~ o3 * school + hprh0 + 
                 race + sex + age2 + s_type2 +totalvolume2,
               data = standardized_newdata61)
tidy(model402, conf.int = TRUE)


library(estimatr)  # Run models with lm_robust()

model_lmrobust_clustered402 <- lm_robust(hprh2 ~ o3 * school + hprh0 + 
                                           race + sex + age2 + s_type2 +totalvolume2 , 
                                         fixed_effects = ~ site_id0,
                                         se_type = "stata",
                                         clusters = site_id0,
                                         data = standardized_newdata61)

tidy(model_lmrobust_clustered402, conf.int = TRUE)


names(structure_pm25o3no2)
####neighbor as moderator
###summarize missingness for the variables listed
dat_nms = c( "site_id0", "family_id0", "age0", "age2" ,
             "race", "sex","s_type2",
             "hplh0","aglh0","hprh0","agrh0",
             "hplh2","aglh2","hprh2","agrh2", "totalvolume2",
             "neighbor","o3")

if (sum(as.numeric(dat_nms %in% names(structure_pm25o3no2))) != length(dat_nms))
  print("Error: missing core demographics. Add those first")
dat = data.table(structure_pm25o3no2[,dat_nms])
for (m in dat_nms) print(paste("number missing for ",m,": ",sum(is.na(structure_pm25o3no2[[m]]) | (structure_pm25o3no2[m] == "")),sep=""))


#set the variable list
var.ls63 <- c( "site_id0", "family_id0", "age0", "age2" ,
               "race", "sex","s_type2",
               "hplh0","aglh0","hprh0","agrh0",
               "hplh2","aglh2","hprh2","agrh2", "totalvolume2",
               "neighbor","o3")
dat_mod63 <- dat[, var.ls63, with = FALSE ]

summary(dat_mod63)


newdata63 <- dat_mod63[complete.cases(dat_mod63),]
newdata63 <- na.omit(dat_mod63)
summary(newdata63)
sum(is.na(newdata63))
str(newdata63)

##Standardize variables using the scale() function
standardized_newdata63 <- scale(newdata63)
standardized_newdata63 <- as.data.frame(standardized_newdata63)
View(standardized_newdata63)


model407 <- lm(agrh2 ~ o3 * neighbor + agrh0 + 
                 race + sex + age2 + s_type2 +totalvolume2 ,
               data = standardized_newdata63)
tidy(model407, conf.int = TRUE)


library(estimatr)  # Run models with lm_robust()

model_lmrobust_clustered407 <- lm_robust(agrh2 ~ o3 * neighbor + agrh0 + 
                                           race + sex + age2 + s_type2 +totalvolume2 , 
                                         fixed_effects = ~ site_id0,
                                         se_type = "stata",
                                         clusters = site_id0,
                                         data = standardized_newdata63)

tidy(model_lmrobust_clustered407, conf.int = TRUE)
library(interactions)
johnson_neyman(model =model_lmrobust_clustered407 ,pred = o3, modx = neighbor)


model408 <- lm(aglh2 ~ o3 * neighbor + aglh0 + 
                 race + sex + age2 + s_type2 +totalvolume2,
               data = standardized_newdata63)
tidy(model408, conf.int = TRUE)


library(estimatr)  # Run models with lm_robust()

model_lmrobust_clustered408 <- lm_robust(aglh2 ~ o3 * neighbor + aglh0 + 
                                           race + sex + age2 + s_type2 +totalvolume2, 
                                         fixed_effects = ~ site_id0,
                                         se_type = "stata",
                                         clusters = site_id0,
                                         data = standardized_newdata63)

tidy(model_lmrobust_clustered408, conf.int = TRUE)



model409 <- lm(hplh2 ~ o3 * neighbor + hplh0 + 
                 race + sex + age2 + s_type2+totalvolume2,
               data = standardized_newdata63)
tidy(model409, conf.int = TRUE)


library(estimatr)  # Run models with lm_robust()

model_lmrobust_clustered409 <- lm_robust(hplh2 ~ o3 * neighbor + hplh0 + 
                                           race + sex + age2 + s_type2 +totalvolume2, 
                                         fixed_effects = ~ site_id0,
                                         se_type = "stata",
                                         clusters = site_id0,
                                         data = standardized_newdata63)

tidy(model_lmrobust_clustered409, conf.int = TRUE)


model410 <- lm(hprh2 ~ o3 * neighbor + hprh0 + 
                 race + sex + age2 + s_type2 +totalvolume2,
               data = standardized_newdata63)
tidy(model410, conf.int = TRUE)


library(estimatr)  # Run models with lm_robust()

model_lmrobust_clustered410 <- lm_robust(hprh2 ~ o3 * neighbor + hprh0 + 
                                           race + sex + age2 + s_type2 +totalvolume2 , 
                                         fixed_effects = ~ site_id0,
                                         se_type = "stata",
                                         clusters = site_id0,
                                         data = standardized_newdata63)

tidy(model_lmrobust_clustered410, conf.int = TRUE)



###Unstandardized Coefficients
###multiple imputation Dataset

library(data.table)
library(dplyr)
library(mice)
library(lme4)
library(miceadds)
library(broom.mixed)
library(haven)
structure_pm25o3no2 <- read_sav("D:/ME/ABCD/environment-culture-brain/participant_structure_total_brain.sav")
View(structure_pm25o3no2)


structure_pm25o3no2 <- replace(structure_pm25o3no2, structure_pm25o3no2  ==  999.000, NA);structure_pm25o3no2
summary(structure_pm25o3no2)
names(structure_pm25o3no2)
str(structure_pm25o3no2)


####o3#####
####school as moderator
###summarize missingness for the variables listed
dat_nms = c( "site_id0", "family_id0", "age0", "age2" ,
             "race", "sex","s_type2",
             "hplh0","aglh0","hprh0","agrh0",
             "hplh2","aglh2","hprh2","agrh2", "totalvolume2",
             "Cschool","Co3","Co3Cschool")

if (sum(as.numeric(dat_nms %in% names(structure_pm25o3no2))) != length(dat_nms))
  print("Error: missing core demographics. Add those first")
dat = data.table(structure_pm25o3no2[,dat_nms])
for (m in dat_nms) print(paste("number missing for ",m,": ",sum(is.na(structure_pm25o3no2[[m]]) | (structure_pm25o3no2[m] == "")),sep=""))

# Number of multiple imputed datasets & maximum number of iterations
n.imp = 20 # Number of Imputations
n.iter = 5# Maximum Iterations



#set the variable list
var.ls61 <- c( "site_id0", "family_id0", "age0", "age2" ,
               "race", "sex","s_type2",
               "hplh0","aglh0","hprh0","agrh0",
               "hplh2","aglh2","hprh2","agrh2", "totalvolume2",
               "Cschool","Co3","Co3Cschool")
dat_mod61 <- dat[, var.ls61, with = FALSE ]

summary(dat_mod61)


ini <- mice( dat_mod61, m = 1, maxit = 0 )
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


pred = ini$pred

pred


# Specifying parameters for the imputation
post <- mice( dat_mod61, meth = meth, pred = pred, seed = 111,
              m = 1, maxit = 0)$post



bpmtot.imp61 <- mice( dat_mod61, meth = meth, pred = pred, post = post,
                      seed = 1111,
                      m = n.imp, maxit = n.iter)


#####Convert Imputed Dataset to data.frame Format
library(data.table)
library(miceadds)
bpmtot.complete61 <- complete(bpmtot.imp61, action = "long")  
bpmtot.df61 <- as.data.frame(bpmtot.complete61) 
names(bpmtot.df61)
str(bpmtot.df61)



model399 <- lm(agrh2 ~ Co3 * Cschool + agrh0 + 
                 race + sex + age2 + s_type2 + totalvolume2 ,
               data = bpmtot.df61)
tidy(model399, conf.int = TRUE)


library(estimatr)  # Run models with lm_robust()

model_lmrobust_clustered399 <- lm_robust(agrh2 ~ Co3 * Cschool + agrh0 + 
                                           race + sex + age2 + s_type2 + totalvolume2 , 
                                         fixed_effects = ~ site_id0,
                                         se_type = "stata",
                                         clusters = site_id0,
                                         data = bpmtot.df61)

tidy(model_lmrobust_clustered399, conf.int = TRUE)



model400 <- lm(aglh2 ~ Co3 * Cschool + aglh0 + 
                 race + sex + age2 + s_type2 +totalvolume2 ,
               data = bpmtot.df61)
tidy(model400, conf.int = TRUE)


library(estimatr)  # Run models with lm_robust()

model_lmrobust_clustered400 <- lm_robust(aglh2 ~ Co3 * Cschool + aglh0 + 
                                           race + sex + age2 + s_type2+totalvolume2 , 
                                         fixed_effects = ~ site_id0,
                                         se_type = "stata",
                                         clusters = site_id0,
                                         data = bpmtot.df61)

tidy(model_lmrobust_clustered400, conf.int = TRUE)



model401 <- lm(hplh2 ~ Co3 * Cschool + hplh0 + 
                 race + sex + age2 + s_type2 +totalvolume2 ,
               data = bpmtot.df61)
tidy(model401, conf.int = TRUE)


library(estimatr)  # Run models with lm_robust()

model_lmrobust_clustered401 <- lm_robust(hplh2 ~ Co3 * Cschool + hplh0 + 
                                           race + sex + age2 + s_type2 +totalvolume2, 
                                         fixed_effects = ~ site_id0,
                                         se_type = "stata",
                                         clusters = site_id0,
                                         data = bpmtot.df61)

tidy(model_lmrobust_clustered401, conf.int = TRUE)
library(interactions)
johnson_neyman(model =model_lmrobust_clustered401 ,pred = o3, modx = school)


model402 <- lm(hprh2 ~ Co3 * Cschool + hprh0 + 
                 race + sex + age2 + s_type2 +totalvolume2,
               data = bpmtot.df61)
tidy(model402, conf.int = TRUE)


library(estimatr)  # Run models with lm_robust()

model_lmrobust_clustered402 <- lm_robust(hprh2 ~ Co3 * Cschool + hprh0 + 
                                           race + sex + age2 + s_type2 +totalvolume2 , 
                                         fixed_effects = ~ site_id0,
                                         se_type = "stata",
                                         clusters = site_id0,
                                         data = bpmtot.df61)

tidy(model_lmrobust_clustered402, conf.int = TRUE)



names(structure_pm25o3no2)
####neighbor as moderator
###summarize missingness for the variables listed
dat_nms = c( "site_id0", "family_id0", "age0", "age2" ,
             "race", "sex","s_type2",
             "hplh0","aglh0","hprh0","agrh0",
             "hplh2","aglh2","hprh2","agrh2", "totalvolume2",
             "Cneighbor","Co3","Co3Cneigh")

if (sum(as.numeric(dat_nms %in% names(structure_pm25o3no2))) != length(dat_nms))
  print("Error: missing core demographics. Add those first")
dat = data.table(structure_pm25o3no2[,dat_nms])
for (m in dat_nms) print(paste("number missing for ",m,": ",sum(is.na(structure_pm25o3no2[[m]]) | (structure_pm25o3no2[m] == "")),sep=""))

# Number of multiple imputed datasets & maximum number of iterations
n.imp = 20 
n.iter = 5


options(max.print = 20000)

#set the variable list
var.ls63 <- c( "site_id0", "family_id0", "age0", "age2" ,
               "race", "sex","s_type2",
               "hplh0","aglh0","hprh0","agrh0",
               "hplh2","aglh2","hprh2","agrh2", "totalvolume2",
               "Cneighbor","Co3","Co3Cneigh")
dat_mod63 <- dat[, var.ls63, with = FALSE ]

summary(dat_mod63)


ini <- mice( dat_mod63, m = 1, maxit = 0 )
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


pred = ini$pred
pred


# Specifying parameters for the imputation
post <- mice( dat_mod63, meth = meth, pred = pred, seed = 111,
              m = 1, maxit = 0)$post



bpmtot.imp63 <- mice( dat_mod63, meth = meth, pred = pred, post = post,
                      seed = 1111,
                      m = n.imp, maxit = n.iter)

# Convert Imputed Dataset to data.frame Format
library(data.table)
library(miceadds)
bpmtot.complete63 <- complete(bpmtot.imp63, action = "long")  
bpmtot.df63 <- as.data.frame(bpmtot.complete63)  # Convert Imputed Dataset to data.frame Format
names(bpmtot.df63)
str(bpmtot.df63)




model407 <- lm(agrh2 ~ Co3 * Cneighbor + agrh0 + 
                 race + sex + age2 + s_type2 +totalvolume2 ,
               data = bpmtot.df63)
tidy(model407, conf.int = TRUE)


library(estimatr)  # Run models with lm_robust()

model_lmrobust_clustered407 <- lm_robust(agrh2 ~ Co3 * Cneighbor + agrh0 + 
                                           race + sex + age2 + s_type2 +totalvolume2 , 
                                         fixed_effects = ~ site_id0,
                                         se_type = "stata",
                                         clusters = site_id0,
                                         data = bpmtot.df63)

tidy(model_lmrobust_clustered407, conf.int = TRUE)
library(interactions)
johnson_neyman(model =model_lmrobust_clustered407 ,pred = o3, modx = neighbor)




model408 <- lm(aglh2 ~ Co3 * Cneighbor + aglh0 + 
                 race + sex + age2 + s_type2 +totalvolume2,
               data = bpmtot.df63)
tidy(model408, conf.int = TRUE)


library(estimatr)  # Run models with lm_robust()

model_lmrobust_clustered408 <- lm_robust(aglh2 ~ Co3 * Cneighbor + aglh0 + 
                                           race + sex + age2 + s_type2 +totalvolume2, 
                                         fixed_effects = ~ site_id0,
                                         se_type = "stata",
                                         clusters = site_id0,
                                         data = bpmtot.df63)

tidy(model_lmrobust_clustered408, conf.int = TRUE)



model409 <- lm(hplh2 ~ Co3 * Cneighbor + hplh0 + 
                 race + sex + age2 + s_type2+totalvolume2,
               data = bpmtot.df63)
tidy(model409, conf.int = TRUE)


library(estimatr)  # Run models with lm_robust()

model_lmrobust_clustered409 <- lm_robust(hplh2 ~ Co3 * Cneighbor + hplh0 + 
                                           race + sex + age2 + s_type2 +totalvolume2, 
                                         fixed_effects = ~ site_id0,
                                         se_type = "stata",
                                         clusters = site_id0,
                                         data = bpmtot.df63)

tidy(model_lmrobust_clustered409, conf.int = TRUE)



model410 <- lm(hprh2 ~ Co3 * Cneighbor + hprh0 + 
                 race + sex + age2 + s_type2 +totalvolume2,
               data = bpmtot.df63)
tidy(model410, conf.int = TRUE)


library(estimatr)  # Run models with lm_robust()

model_lmrobust_clustered410 <- lm_robust(hprh2 ~ Co3 * Cneighbor + hprh0 + 
                                           race + sex + age2 + s_type2 +totalvolume2 , 
                                         fixed_effects = ~ site_id0,
                                         se_type = "stata",
                                         clusters = site_id0,
                                         data = bpmtot.df63)

tidy(model_lmrobust_clustered410, conf.int = TRUE)


## Standardization
###MI Dataset
###Moderation Model
library(data.table)
library(dplyr)
library(mice)
library(lme4)
library(miceadds)
library(broom.mixed)
library(haven)
structure_pm25o3no2 <- read_sav("D:/ME/ABCD/environment-culture-brain/participant_structure_total_brain.sav")
View(structure_pm25o3no2)



structure_pm25o3no2 <- replace(structure_pm25o3no2, structure_pm25o3no2  ==  999.000, NA);structure_pm25o3no2
summary(structure_pm25o3no2)
names(structure_pm25o3no2)
str(structure_pm25o3no2)


####o3#####
####school as moderator
###summarize missingness for the variables listed
dat_nms = c( "site_id0", "family_id0", "age0", "age2" ,
             "race", "sex","s_type2",
             "hplh0","aglh0","hprh0","agrh0",
             "hplh2","aglh2","hprh2","agrh2", "totalvolume2",
             "school","o3")

if (sum(as.numeric(dat_nms %in% names(structure_pm25o3no2))) != length(dat_nms))
  print("Error: missing core demographics. Add those first")
dat = data.table(structure_pm25o3no2[,dat_nms])
for (m in dat_nms) print(paste("number missing for ",m,": ",sum(is.na(structure_pm25o3no2[[m]]) | (structure_pm25o3no2[m] == "")),sep=""))

# Number of multiple imputed datasets & maximum number of iterations
n.imp = 20 
n.iter = 5

options(max.print = 20000)##显示20000行

#set the variable list
var.ls61 <- c( "site_id0", "family_id0", "age0", "age2" ,
               "race", "sex","s_type2",
               "hplh0","aglh0","hprh0","agrh0",
               "hplh2","aglh2","hprh2","agrh2", "totalvolume2",
               "school","o3")
dat_mod61 <- dat[, var.ls61, with = FALSE ]

summary(dat_mod61)


ini <- mice( dat_mod61, m = 1, maxit = 0 )
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


pred = ini$pred
pred


# Specifying parameters for the imputation
post <- mice( dat_mod61, meth = meth, pred = pred, seed = 111,
              m = 1, maxit = 0)$post



bpmtot.imp61 <- mice( dat_mod61, meth = meth, pred = pred, post = post,
                      seed = 1111,
                      m = n.imp, maxit = n.iter)

library(data.table)
library(miceadds)
bpmtot.complete61 <- complete(bpmtot.imp61, action = "long")  
bpmtot.df61 <- as.data.frame(bpmtot.complete61)  
names(bpmtot.df61)
str(bpmtot.df61)

library(CDM)
library(TAM)
library(tidyverse)
tocenterlist.ls <- c("school", "o3", "totalvolume2")
View(tocenterlist.ls)
bpmtot.dat <- miceadds::mids2datlist(bpmtot.imp61)
bpmtot.df61_z <- scale_datlist(datlist=bpmtot.dat,orig_var=tocenterlist.ls,trafo_var = paste0(tocenterlist.ls,"_z"),weights=NULL,M=0,SD=1,digits = NULL)
View(bpmtot.df61_z)



standardized_bpmtot.df61 <- scale(bpmtot.df61)
standardized_bpmtot.df61 <- as.data.frame(standardized_bpmtot.df61)
View(standardized_bpmtot.df61)


model399 <- lm(agrh2 ~ o3 * school + agrh0 + 
                 race + sex + age2 + s_type2 + totalvolume2 ,
               data = standardized_bpmtot.df61)
tidy(model399, conf.int = TRUE)


library(estimatr)  # Run models with lm_robust()

model_lmrobust_clustered399 <- lm_robust(agrh2 ~ o3 * school + agrh0 + 
                                           race + sex + age2 + s_type2 + totalvolume2 , 
                                         fixed_effects = ~ site_id0,
                                         se_type = "stata",
                                         clusters = site_id0,
                                         data = standardized_bpmtot.df61)

tidy(model_lmrobust_clustered399, conf.int = TRUE)



model400 <- lm(aglh2 ~ o3 * school + aglh0 + 
                 race + sex + age2 + s_type2 +totalvolume2 ,
               data = standardized_bpmtot.df61)
tidy(model400, conf.int = TRUE)


library(estimatr)  # Run models with lm_robust()

model_lmrobust_clustered400 <- lm_robust(aglh2 ~ o3 * school + aglh0 + 
                                           race + sex + age2 + s_type2+totalvolume2 , 
                                         fixed_effects = ~ site_id0,
                                         se_type = "stata",
                                         clusters = site_id0,
                                         data = standardized_bpmtot.df61)

tidy(model_lmrobust_clustered400, conf.int = TRUE)



model401 <- lm(hplh2 ~ o3 * school + hplh0 + 
                 race + sex + age2 + s_type2 +totalvolume2 ,
               data = standardized_bpmtot.df61)
tidy(model401, conf.int = TRUE)


library(estimatr)  # Run models with lm_robust()

model_lmrobust_clustered401 <- lm_robust(hplh2 ~ o3 * school + hplh0 + 
                                           race + sex + age2 + s_type2 +totalvolume2, 
                                         fixed_effects = ~ site_id0,
                                         se_type = "stata",
                                         clusters = site_id0,
                                         data = standardized_bpmtot.df61)

tidy(model_lmrobust_clustered401, conf.int = TRUE)
library(interactions)
johnson_neyman(model =model_lmrobust_clustered401 ,pred = o3, modx = school)



model402 <- lm(hprh2 ~ o3 * school + hprh0 + 
                 race + sex + age2 + s_type2 +totalvolume2,
               data = standardized_bpmtot.df61)
tidy(model402, conf.int = TRUE)


library(estimatr)  # Run models with lm_robust()

model_lmrobust_clustered402 <- lm_robust(hprh2 ~ o3 * school + hprh0 + 
                                           race + sex + age2 + s_type2 +totalvolume2 , 
                                         fixed_effects = ~ site_id0,
                                         se_type = "stata",
                                         clusters = site_id0,
                                         data = standardized_bpmtot.df61)

tidy(model_lmrobust_clustered402, conf.int = TRUE)


names(structure_pm25o3no2)
####neighbor as moderator
###summarize missingness for the variables listed
dat_nms = c( "site_id0", "family_id0", "age0", "age2" ,
             "race", "sex","s_type2",
             "hplh0","aglh0","hprh0","agrh0",
             "hplh2","aglh2","hprh2","agrh2", "totalvolume2",
             "neighbor","o3")

if (sum(as.numeric(dat_nms %in% names(structure_pm25o3no2))) != length(dat_nms))
  print("Error: missing core demographics. Add those first")
dat = data.table(structure_pm25o3no2[,dat_nms])
for (m in dat_nms) print(paste("number missing for ",m,": ",sum(is.na(structure_pm25o3no2[[m]]) | (structure_pm25o3no2[m] == "")),sep=""))

# Number of multiple imputed datasets & maximum number of iterations
n.imp = 20 
n.iter = 5


#set the variable list
var.ls63 <- c( "site_id0", "family_id0", "age0", "age2" ,
               "race", "sex","s_type2",
               "hplh0","aglh0","hprh0","agrh0",
               "hplh2","aglh2","hprh2","agrh2", "totalvolume2",
               "neighbor","o3")
dat_mod63 <- dat[, var.ls63, with = FALSE ]

summary(dat_mod63)


ini <- mice( dat_mod63, m = 1, maxit = 0 )
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


pred = ini$pred
pred


# Specifying parameters for the imputation
post <- mice( dat_mod63, meth = meth, pred = pred, seed = 111,
              m = 1, maxit = 0)$post



bpmtot.imp63 <- mice( dat_mod63, meth = meth, pred = pred, post = post,
                      seed = 1111,
                      m = n.imp, maxit = n.iter)

library(data.table)
library(miceadds)
bpmtot.complete63 <- complete(bpmtot.imp63, action = "long")  
bpmtot.df63 <- as.data.frame(bpmtot.complete63)  
names(bpmtot.df63)
str(bpmtot.df63)


standardized_bpmtot.df63 <- scale(bpmtot.df63)
standardized_bpmtot.df63 <- as.data.frame(standardized_bpmtot.df63)
View(standardized_bpmtot.df63)



model407 <- lm(agrh2 ~ o3 * neighbor + agrh0 + 
                 race + sex + age2 + s_type2 +totalvolume2 ,
               data = standardized_bpmtot.df63)
tidy(model407, conf.int = TRUE)


library(estimatr)  # Run models with lm_robust()

model_lmrobust_clustered407 <- lm_robust(agrh2 ~ o3 * neighbor + agrh0 + 
                                           race + sex + age2 + s_type2 +totalvolume2 , 
                                         fixed_effects = ~ site_id0,
                                         se_type = "stata",
                                         clusters = site_id0,
                                         data = standardized_bpmtot.df63)

tidy(model_lmrobust_clustered407, conf.int = TRUE)
library(interactions)
johnson_neyman(model =model_lmrobust_clustered407 ,pred = o3, modx = neighbor)




model408 <- lm(aglh2 ~ o3 * neighbor + aglh0 + 
                 race + sex + age2 + s_type2 +totalvolume2,
               data = standardized_bpmtot.df63)
tidy(model408, conf.int = TRUE)


library(estimatr)  # Run models with lm_robust()

model_lmrobust_clustered408 <- lm_robust(aglh2 ~ o3 * neighbor + aglh0 + 
                                           race + sex + age2 + s_type2 +totalvolume2, 
                                         fixed_effects = ~ site_id0,
                                         se_type = "stata",
                                         clusters = site_id0,
                                         data = standardized_bpmtot.df63)

tidy(model_lmrobust_clustered408, conf.int = TRUE)



model409 <- lm(hplh2 ~ o3 * neighbor + hplh0 + 
                 race + sex + age2 + s_type2+totalvolume2,
               data = standardized_bpmtot.df63)
tidy(model409, conf.int = TRUE)


library(estimatr)  # Run models with lm_robust()

model_lmrobust_clustered409 <- lm_robust(hplh2 ~ o3 * neighbor + hplh0 + 
                                           race + sex + age2 + s_type2 +totalvolume2, 
                                         fixed_effects = ~ site_id0,
                                         se_type = "stata",
                                         clusters = site_id0,
                                         data = standardized_bpmtot.df63)

tidy(model_lmrobust_clustered409, conf.int = TRUE)




model410 <- lm(hprh2 ~ o3 * neighbor + hprh0 + 
                 race + sex + age2 + s_type2 +totalvolume2,
               data = standardized_bpmtot.df63)
tidy(model410, conf.int = TRUE)


library(estimatr)  # Run models with lm_robust()

model_lmrobust_clustered410 <- lm_robust(hprh2 ~ o3 * neighbor + hprh0 + 
                                           race + sex + age2 + s_type2 +totalvolume2 , 
                                         fixed_effects = ~ site_id0,
                                         se_type = "stata",
                                         clusters = site_id0,
                                         data = standardized_bpmtot.df63)

tidy(model_lmrobust_clustered410, conf.int = TRUE)
