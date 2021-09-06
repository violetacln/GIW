
##-------- main packages -----------------------------------------------------------------------
library(tidyverse)
library(rstanarm)
library(lme4)
library(lmerTest)
library(stargazer)
library(furniture)
library(report)
library(parameters)
library(performance)
library(effectsize)
#library(correlation)
library(bayestestR)
library(insight)
#library(see)
library(margins)
library(ggplot2)
library(sjmisc)
library(DescTools)
library(car)
library(sqldf)
#library(RODBC) # not needed on linux
library(odbc)     
library(DBI)      


##-------- get data ------------------------------------------------------------------------------------------
## if on linux, talking to SQL server  ------------------------------------------
con <- dbConnect(odbc(),
                 Driver = "FreeTDS",
                 Server = "XXXX",
                 Database = "XXXX",
                 UID = "XXXXX",
                 PWD = rstudioapi::askForPassword("Database password"),
                 Port = 1433)
result <- dbSendQuery(con,"SELECT * FROM YYYYYY")
micro <- dbFetch(result)

## if on windows, talking to SQL-server ------------------
# dbhandle <- RODBC::odbcDriverConnect('driver={SQL Server};server=XXXX;database=XXXXXX;trusted_connection=true')
# micro <- RODBC::sqlQuery(dbhandle, "SELECT  * FROM YYYYYY")


##--------- processing data ---------------------------------------------------------------------------------

# # LOG, HOURLY WAGES
micro$wageHourly <- log(micro$wageHourly0)
#
# # GENDER
micro$gender <- as.factor(micro$gender)
#
# # TIME  (change origin to first year in data)
micro$time <- micro$time0 - min(micro$time0)   ### rescaled, it starts at time zero which is 2008
micro$timef <- as.factor(micro$time)
#
# # EDUCATION
micro$education <- as.factor(micro$education)
micro$educ1 <- as.factor(car::recode(micro$education,
                                     " 10:29='e2'; 30:49='e3'; 50:69='e4'; 70:89='e5' ") )
#
#
# # EMPLOYMENT -----------
#
# # COMPANY
# ## size: make groups by size as <50, 50-249, >249
micro$sizeCompanycateg <- as.factor(car::recode(micro$sizeCompany,
                                                "  lo:49='small'; 50:249='medium'; 250:hi='high'  " ))
micro$capitalareaComp <- as.factor(micro$capitalareaComp)
#
###----grouping factors -------------
## companies
micro$company <- as.factor(micro$company)
## individuals 
micro$id <- as.factor(micro$id)
## occupations
micro$occupation1 <- as.factor(micro$occupation1)
## or
micro$occupation4 <- as.factor(micro$occupation4)
###---------- attributes of occupation (1,2,3= low, medium, high): female-dominance and older-dominance
micro$categ_propF <- as.factor(micro$categ_propF)
micro$categ_propY <- as.factor(micro$categ_propY)
## economic activity
micro$nace2 <- as.factor(micro$nace2)  ## econ activity 2 digits
## or
micro$nace1 <- as.factor(micro$nace1) ## use only this OR the 2-digits above, not both
## ---- employment attribute: economic sector
micro$econSector <- as.factor(micro$econSector)
##---more attributes of employees--------
## work related
micro$regapprentice <- as.factor(micro$regapprentice)
micro$regstudent <- as.factor(micro$regstudent)
micro$inlabunion <- as.factor(micro$inlabunion)
micro$fulltime <- as.factor(micro$fulltime)
micro$supervisor <- as.factor(micro$supervisor)
micro$ctworker <- as.factor(micro$ctworker)
micro$monthlyEarn <- as.factor(micro$monthlyEarn)
micro$shiftPremium <- as.factor(micro$shiftPremium)
## demography
micro$marital <- as.factor(micro$marital)
micro$backgr <- as.factor(micro$backgr)
micro$childage0to2 <- as.factor(micro$childage0to2)
micro$childage2to5 <- as.factor(micro$childage2to5)
micro$childage6to16 <- as.factor(micro$childage6to16)
##------------- company attributes
micro$equalpaycert <- as.factor(micro$equalpaycert)


##------if working with all data -------------------------------
## when possible: using all data
m0 <- micro
##------if sampling, for speedy first tests --------------------
# m2 <- sqldf::sqldf("select distinct id, count(time) as nid from micro group by id")
# micro <- merge(micro, m2 , by="id")
# set.seed(999)
# m0 <- dplyr::filter(micro, micro$id %in% sample(micro$id[micro$nid>5], size=50000) )

##--- clean---------
rm(micro)



##------ for analysis most variables, but maybe smaller sample for speed: ------------------------------------
m1 <- dplyr::select(m0, c(
  #time:
  time,
  # (obs) grouping into clusters:
  id, company , nace2, occupation4,               
  # individual attributes :
  wageHourly,  
  
  gender,
  educ1, 
  
  lenEmployComp, totalHoursScaled, 
  age, 
  #age2Scaled, # or maybe build it into model, from age_within, age_between ?!
  
  fulltime, inlabunion,
  regapprentice, regstudent,
  backgr,
  supervisor, ctworker, monthlyEarn, shiftPremium,
  
  marital, childage0to2, childage2to5, childage6to16,
  # company attributes:
  econSector, 
  sizeCompanycateg, capitalareaComp, equalpaycert,
  # occupation attributes
  categ_propY, categ_propF
))


##--------releveling factors, for easy interpretation of  results when modelling ----------------------
## tried to have same levels as previous studies ------------------------------------------------------
m1$fulltime <- relevel(m1$fulltime, ref="0")
m1$inlabunion <- relevel(m1$inlabunion, ref="0")
m1$capitalareaComp <- relevel(m1$capitalareaComp, ref="0")
m1$monthlyEarn <- relevel(m1$monthlyEarn, ref="0")


##-------- create within/between split of individual characteristics which change with time, if needed
library(parameters)
## test for heterogeneity
# parameters::check_heterogeneity(m1, select= c("age", "lenEmployComp", "totalHoursScaled"), group = "id")

## calculate within/between-subject demeaned/mean values of time dependent numeric variables
##, needed for 3-level models, time-dependent
m1bw <- cbind(m1, parameters::demean(m1, 
                                     select = c("age", 
                                                ##"age2Scaled", 
                                                "lenEmployComp", "totalHoursScaled"), 
                                     group = "id",
                                     suffix_demean = "_within",
                                     suffix_groupmean = "_between",
                                     add_attributes = TRUE,
                                     verbose = TRUE))
rm(m0)

#---- recoding more: the municipality sector including capital area and the others------------------
m1bw$econSect <- as.factor(car::recode(m1bw$econSector,
                                       " 'B'='M'; 'S'='M' " ))

##-- for some of the EDA, a simpler dataset -------------------------------------------------------
mred <- dplyr::select(m1bw, c(wageHourly, gender, educ1, backgr,
                              econSect, supervisor, fulltime,
                              categ_propF, categ_propY))


##-------------------------------------------------------------------------------------------------------
##------------ exploratory and summary-------------------------------------------------------------------
##-------------------------------------------------------------------------------------------------------


##-----correlation heatmap style ---- FIG.1 ---------
DataExplorer::plot_correlation(mred, type=c("all"))
##---descriptive 
gtsummary::tbl_summary(dplyr::select(m1bw, wageHourly, time, gender, 
                                     #nace2, occupation4, 
                                     educ1, age, 
                                     lenEmployComp, totalHoursScaled,
                                     fulltime, inlabunion, regstudent, regapprentice,
                                     backgr, supervisor, ctworker, monthlyEarn,
                                     shiftPremium, 
                                     marital, childage0to2, childage6to16, childage2to5,
                                     econSect, sizeCompanycateg, capitalareaComp,
                                     equalpaycert, categ_propY, categ_propF
)
)

gtsummary::tbl_summary(dplyr::select(m1bw, wageHourly, time, gender, 
                                     educ1, age, 
                                     lenEmployComp, totalHoursScaled,
                                     fulltime, inlabunion, regstudent, regapprentice,
                                     backgr, supervisor, ctworker, monthlyEarn,
                                     shiftPremium, 
                                     marital, childage0to2, childage6to16, childage2to5,
                                     econSect, sizeCompanycateg, capitalareaComp,
                                     equalpaycert, categ_propY, categ_propF,
                                     nace2, 
                                     occupation4), 
                       by=gender)

## more detailed descriptive, longer to run and with huge output -----
# data_wide <- m1bw %>%
#   dplyr::select(id, gender, wageHourly, time)  %>%
#   tidyr::pivot_wider(names_from = time,
#                      names_prefix = "time_",
#                      values_from = wageHourly)
# 
# data_wide %>%
#   dplyr::group_by(gender) %>%
#   furniture::table1(
#     "year 0" = time_0,
#     "year 1" = time_1,
#     "year 2" = time_2,
#     "year 3" = time_3,
#     "year 4" = time_4,
#     "year 5" = time_5,
#     "year 6" = time_6,
#     "year 7" = time_7,
#     "year 8" = time_8,
#     "year 9" = time_9,
#     "year 10" = time_10,
#     "year 11" = time_11,
#     "year 12" = time_12,
#     total = TRUE,
#     test = TRUE,
#     digits = 2,
#     na.rm=FALSE,
#     output = "markdown",
#     caption = "Differences in hourly wages across time, by gender")


##------  more plots -----------------------------------------------

## -------------------- FIG.2 --------------------------
ggplot2::ggplot(m1, aes(x=wageHourly, colour=gender) ) + 
  geom_density()  +
  labs(x= "log hourly wages") +
  theme(legend.position = "bottom") +
  scale_colour_discrete(name="Gender", breaks=c(0,1), labels=c("Males", "Females"))


##-------------------- FIG.3 ---------------------------
m1_tl <- m1  
m1_tl$time <- car::recode(m1$time, "
                    0='2008'; 1='2009'; 2='2010';
                    3='2011'; 4='2012'; 5='2013';
                    6='2014'; 7='2015'; 8='2016';
                    9='2017'; 10='2018'; 11='2019';
                    12='2020'
                          ")
ggplot2::ggplot(m1_tl, aes(x=wageHourly, colour=gender)) + 
  geom_density()+
  labs(x= "log hourly wages") +
  facet_wrap(~as.factor(time), labeller=labeller(time1=
                                                   c("2008", "2009", "2010", "2011",
                                                     "2012", "2013", "2014", "2015",
                                                     "2016", "2017", "2018", "2019", "2020"
                                                   ))) +
  theme(legend.position = "bottom") +
  scale_colour_discrete(name="Gender", breaks=c(0,1), labels=c("Males", "Females"))


##------------------- FIG.4 ----------------------------------
ggplot2::ggplot(m1, aes(x=wageHourly, colour=gender)) + 
  geom_density()+
  labs(x= "log hourly wages") +
  facet_wrap(~educ1) +
  theme(legend.position = "bottom") +
  scale_colour_discrete(name="Gender", breaks=c(0,1), labels=c("Males", "Females"))

#-------------------- FIG. 5 ---------------------------------
ggplot2::ggplot(m1bw, aes(x=wageHourly, colour=gender)) + 
  geom_density()+
  labs(x= "log hourly wages") +
  facet_wrap(~as.factor(econSect)) +
  theme(legend.position = "bottom") +
  scale_colour_discrete(name="Gender", breaks=c(0,1), labels=c("Males", "Females"))

## ------------ optional more plots (many!) ----------------
# ggplot2::ggplot(
#   m1bw,
#   aes(y=wageHourly, colour=gender)) +
#   theme_bw()+
#   geom_boxplot() +
#   facet_wrap(~occupation4)  
# ## or
# ggplot2::ggplot(
#   m1bw,
#   aes(y=wageHourly, x=gender)) +
#   theme_bw()+
#   geom_boxplot()+
#  facet_wrap(~as.factor(nace2))  

##---------- PRELIMINARY approximative evaluation of variables --------------------------
## Pre-modeling using Bayesian Adaptive Sampling -------------
## (with Bayesian Variable Selection and Model Averaging)
# model_BAS <- BAS::bas.lm(
#                    wageHourly ~
#                      time+ gender+
#                      I(age-mean(age)) + I(lenEmployComp- mean(lenEmployComp))+
#                      I(totalHoursScaled-mean(totalHoursScaled)) + 
#                      educ1 +
#                      fulltime+ inlabunion + regapprentice + regstudent + 
#                      supervisor + ctworker + monthlyEarn + shiftPremium+
#                      backgr + marital+ childage0to2+childage2to5+ childage6to16+
#                      econSector+
#                     sizeCompanycateg+ capitalareaComp+ equalpaycert+
#                     categ_propF + categ_propY,
#                     data = m1bw)
# 
# # plot(model_BAS, ask=F)
# model_BAS
# options(width = 80)
# knitr::kable(summary(model_BAS) )   
# plot(coef(model_BAS), ask = F)  ## subset = c(1:2)
# confint(coef(model_BAS))
# plot(confint(coef(model_BAS)))  ##, estimator = "HPM") or estimator = "MPM")





##------------------------------------------------------------------------------
##-------------------------- MODELLING ------------------------------------------
##-------------------------------------------------------------------------------


##------------------------ 0.0.----------------------------------------------

##----the "null-null" models: only gender!, lm, for each time point, 
## with/without economic sector differences --------------------

modelsNull <- list()
for(t in 0:12) {
  M000 <-lm(wageHourly ~
              gender 
            ## with/without economic sector differences:
            ## + gender*econSect
            , data=subset(m1bw, time == t))
  #summary(M000)
  modelsNull[[t+1]] <- M000
}

sjPlot::tab_model(
  modelsNull[[1]], modelsNull[[2]], modelsNull[[3]], 
  modelsNull[[4]], modelsNull[[5]],modelsNull[[6]],
  modelsNull[[7]], modelsNull[[8]], modelsNull[[9]], 
  modelsNull[[10]],modelsNull[[11]],modelsNull[[12]],
  modelsNull[[13]],
  ## one option for multiplicity adjustments
  p.adjust = "holm",  
  show.ci = FALSE, show.reflvl = TRUE,
  show.se = TRUE, auto.label = FALSE,
  string.se = "SE", show.icc = TRUE,
  show.aic = TRUE, digits=3,
  dv.labels = c("2008", "2009", "2010", "2011", 
                "2012", "2013", "2014", "2015",
                "2016", "2017", "2018", "2019",
                "2020")
  ### ,file="...."   ## if we want the output in a file which may even be open in word 
  
)

## or separately, for each economic sector null-null models only gender, for each time point
## check 
modelsNullecM <- list()
for(t in 0:12) {
  M000ecM <-lm(wageHourly ~
                 gender 
               , data=subset(m1bw, time == t & econSect=='M'))   ## or any other
  #summary(M000ecM)
  modelsNullecM[[t+1]] <- M000ecM
}


##---------------------------------0.1.-------------------------------------------------

## -----"unconditional MLM models": gender as fixed effect, although include random effects for: 
## company, economic activity, occupation
## time_fixed points, where possible (so no correlation of observations within individuals)
## for speed of computation
## and fast interpretation --------------------------------------------------------------

modelsUnCond <- list()
for(t in 8:12) {
  M0 <-lmerTest::lmer(wageHourly ~ 
                        gender 
                      + (1|company)+ (1|nace2)+ (1|occupation4)  
                      , data=subset(m1bw, time == t), 
                      control = lmerControl(optimizer ="Nelder_Mead"),
                      REML=T)
  # summary(M0)
  modelsUnCond[[t-8 +1]] <- M0
}


##---------------------- comparing several gender-additive models, for fixed time ----------------
## showing how the fixed time,  two-level models improve with adding covariates
## use ML for fitting now, in order to compare performances of models with different sstructures


##--- most covariates ----------------------------------------------
M0_ftf_ml <-lmerTest::lmer(wageHourly ~ 
                             gender + 
                             ( 
                               I(age-mean(age)) 
                               + I((age-mean(age))^2)
                               
                               + I(lenEmployComp-mean(lenEmployComp))
                               + I((lenEmployComp-mean(lenEmployComp))^2)
                               
                               + I(totalHoursScaled-mean(totalHoursScaled))
                               
                               + educ1
                               
                               + regstudent + regapprentice
                               + marital + childage0to2 + childage6to16 + childage2to5
                               + inlabunion +fulltime 
                               + backgr
                               + supervisor + ctworker + monthlyEarn + shiftPremium
                               + capitalareaComp + categ_propF + categ_propY 
                               + equalpaycert + sizeCompanycateg
                               ##   + econSect
                             )
                           + (1|company)+ (1|nace2)+ (1|occupation4)  
                           , data=subset(m1bw, time == 11), 
                           control = lmerControl(optimizer ="Nelder_Mead"),
                           REML=FALSE)

summary(M0_ftf_ml)


## restricted model restr1: ---------------------------------------------
## age, education, fulltime, lenghtEmploymentInCompany, economic activity, occupation, totalHoursWorked 
M0_ftf_ml_restr1 <-lmerTest::lmer(wageHourly ~ 
                                    gender +   
                                    ( 
                                      I(age-mean(age)) 
                                      + I((age-mean(age))^2)
                                      
                                      + I(lenEmployComp-mean(lenEmployComp))
                                      + I((lenEmployComp-mean(lenEmployComp))^2)
                                      
                                      + I(totalHoursScaled-mean(totalHoursScaled))
                                      
                                      + educ1
                                      
                                      +fulltime 
                                      
                                      ##  + econSect
                                    )
                                  + (1|company)+ (1|nace2)+ (1|occupation4)  
                                  , data=subset(m1bw, time == 11), 
                                  control = lmerControl(optimizer ="Nelder_Mead"),
                                  REML=FALSE)

summary(M0_ftf_ml_restr1)

## restricted model restr1a: --------------------------------------------------------
## same as restr1, but without including the company effect
M0_ftf_ml_restr1a <-lmerTest::lmer(wageHourly ~ gender +   
                                     ( 
                                       I(age-mean(age)) 
                                       + I((age-mean(age))^2)
                                       
                                       + I(lenEmployComp-mean(lenEmployComp))
                                       + I((lenEmployComp-mean(lenEmployComp))^2)
                                       
                                       + I(totalHoursScaled-mean(totalHoursScaled))
                                       
                                       + educ1
                                       
                                       +fulltime 
                                       
                                       ##  + econSect
                                     )
                                   + (1|company)+ (1|nace2)+ (1|occupation4)  
                                   , data=subset(m1bw, time == 11), 
                                   control = lmerControl(optimizer ="Nelder_Mead"),
                                   REML=FALSE)

summary(M0_ftf_ml_restr1a)


## restricted model restr2: -------------------------------------------------                                     
## age, experience with company, education, PLUS marital status and having children factors
M0_ftf_ml_restr2 <-lmerTest::lmer(wageHourly ~ 
                                    gender + 
                                    ( 
                                      I(age-mean(age)) 
                                      + I((age-mean(age))^2)
                                      
                                      + I(lenEmployComp-mean(lenEmployComp))
                                      + I((lenEmployComp-mean(lenEmployComp))^2)
                                      
                                      + I(totalHoursScaled-mean(totalHoursScaled))
                                      
                                      + educ1
                                      
                                      + marital                                      
                                      + childage0to2 + childage6to16 + childage2to5  
                                      
                                      +fulltime 
                                      + backgr                                
                                      
                                      ##    + econSect
                                    )
                                  
                                  + (1|company)+ (1|nace2)+ (1|occupation4)  
                                  , data=subset(m1bw, time == 11), 
                                  control = lmerControl(optimizer ="Nelder_Mead"),
                                  REML=FALSE)

summary(M0_ftf_re_restr2)



## restricted model restr3:--------------------------------------------
M0_ftf_ml_restr3 <-lmerTest::lmer(wageHourly ~ 
                                    gender +   
                                    ( 
                                      I(age-mean(age)) 
                                      + I((age-mean(age))^2)
                                      
                                      + I(lenEmployComp-mean(lenEmployComp))
                                      + I((lenEmployComp-mean(lenEmployComp))^2)
                                      
                                      + I(totalHoursScaled-mean(totalHoursScaled))
                                      
                                      + educ1
                                      
                                      + regstudent + regapprentice
                                      
                                      + marital 
                                      + childage0to2 + childage6to16 + childage2to5
                                      
                                      + inlabunion 
                                      +fulltime 
                                      + backgr
                                      + supervisor + ctworker + monthlyEarn + shiftPremium                                  
                                      
                                      ##  + econSect
                                    )
                                  + (1|company)+ (1|nace2)+ (1|occupation4)  
                                  , data=subset(m1bw, time == 11), 
                                  control = lmerControl(optimizer ="Nelder_Mead"),
                                  REML=FALSE)

summary(M0_ftf_ml_restr3)


##---------------- compare performance of additive models and report --------------------------------

## performance ---------------------------------------
performance::compare_performance( 
  M0_ftf_ml_restr1,
  M0_ftf_ml_restr1a, 
  M0_ftf_ml_restr2,
  M0_ftf_ml_restr3,
  M0_ftf_ml,
  rank = TRUE)

## printing model results viewer / in file  -------------
sjPlot::tab_model(
  M0_ftf_ml_restr1a,
  M0_ftf_ml_restr1,
  M0_ftf_ml_restr2,
  M0_ftf_ml_restr3,
  M0_ftf_ml,
  show.ci = FALSE,
  show.reflvl = TRUE,
  show.se = TRUE,
  auto.label = FALSE,
  string.se = "SE",
  show.icc = TRUE,
  show.aic = TRUE, 
  digits=3,
  dv.labels = c("M1a","M1", "M2", "M3", "M")
  ##  ,file="tab_model_build_Y2019.doc"
)



##---------------------------------- 0.2 -------------------------------------------------
## --------additive, simple MODELS, for several time values--------------------------------
models <- list()
for(t in 8:12) {
  M0_ftf_re <-lmerTest::lmer(wageHourly ~ 
                               gender +   
                               ( 
                                 I(age-mean(age)) 
                                 + I((age-mean(age))^2)
                                 
                                 + I(lenEmployComp-mean(lenEmployComp))
                                 + I((lenEmployComp-mean(lenEmployComp))^2)
                                 
                                 
                                 + I(totalHoursScaled-mean(totalHoursScaled))
                                 
                                 + educ1
                                 
                                 + regstudent + regapprentice
                                 + marital + childage0to2 + childage6to16 + childage2to5
                                 + inlabunion +fulltime 
                                 + backgr
                                 + supervisor + ctworker + monthlyEarn + shiftPremium
                                 
                                 + capitalareaComp + categ_propF + categ_propY 
                                 + equalpaycert + sizeCompanycateg
                                 ## with/without econSect; w/wo inter of gender with econSect
                                 + econSect  
                               )
                             
                             + (1|company)+ (1|nace2)+ (1|occupation4)  
                             ## with/without restricted set
                             , data=subset(m1bw, time == t),   ## & econSect == 'M')  
                             control = lmerControl(optimizer ="Nelder_Mead"),
                             REML=T)
  models[[t-8 +1]] <- M0_ftf_re
}


##----- additive, restricted (less covariates) models, several time values --------------------
modelsM1 <- list()
for(t in 8:12) {
  M0_ftf_reM1 <-lmerTest::lmer(wageHourly ~ 
                                 gender +   
                                 ( 
                                   I(age-mean(age)) 
                                   + I((age-mean(age))^2)
                                   
                                   + I(lenEmployComp-mean(lenEmployComp))
                                   + I((lenEmployComp-mean(lenEmployComp))^2)
                                   + I(totalHoursScaled-mean(totalHoursScaled))
                                   
                                   + educ1
                                   
                                   # without(for restr1-models); with, for restr2-models, restr3-models
                                   + marital + childage0to2 + childage6to16 + childage2to5 + backgr
                                   # without(for restr1-models); with, for restr1-models and for restr2-models, restr-3 models
                                   +fulltime 
                                   # without(for restr1-models); with, for restr3-models:
                                   + regstudent + regapprentice + inlabunion 
                                   + supervisor + ctworker + monthlyEarn + shiftPremium
                                   
                                 )
                               
                               + (1|company)+ (1|nace2)+ (1|occupation4)  
                               ## with/without restricted set
                               , data=subset(m1bw, time == t    ) ## & econSect == 'M')
                               , control = lmerControl(optimizer ="Nelder_Mead"),
                               REML=T)
  modelsM1[[t-8 +1]] <- M0_ftf_reM1
}


##---------------------------------------- 0.3. ---------------------------------------
##-------- unique, time growth curve with additive gender effect, 
## random intercepts, w/wo random gender-slope ----------------------------------------
## less fast computations, slightly more involved interpretations but not too difficult

M_tot_re_Ec <-lmerTest::lmer(wageHourly ~
                               time*
                               (gender +
                                  
                                  age_within
                                + age_between
                                
                                + I(age_within^2)
                                + I(age_between^2)
                                
                                + lenEmployComp_within
                                + lenEmployComp_between
                                
                                + I(lenEmployComp_within^2)
                                + I(lenEmployComp_between^2)
                                
                                + totalHoursScaled_within
                                + totalHoursScaled_between
                                
                                + educ1
                                
                                + regstudent + regapprentice
                                + marital + childage0to2 + childage6to16 + childage2to5
                                + backgr
                                + inlabunion +fulltime
                                + supervisor + ctworker + monthlyEarn + shiftPremium
                                + capitalareaComp + categ_propF + categ_propY
                                + equalpaycert + sizeCompanycateg
                                ## w/wo for total or by econSect
                                + econSect        
                                ## w/wo for total or by econSect
                                + gender:econSect      
                               )
                             ## only random intercepts:   
                             + (1|id)    
                             ### might like to include (age_within|id) type of terms as well **
                             + (1|company)+ (1|nace2)+ (1|occupation4)
                             ## w/wo random slopes, simplest type
                             ##  + (0+gender|nace2) + (0+gender|occupation4)
                             , data= m1bw    
                             , control = lmerControl(optimizer ="Nelder_Mead"),
                             REML=T)

summary(M_tot_re_Ec)

##---unique, restricted (less covariates), time growth curve with additive gender effect, 
## random intercepts, w/wo random gender-slope --------------------------------------------
## less fast computations, slightly more involved interpretations but not too difficult

M_tot_re_Ec_restricted <-lmerTest::lmer(wageHourly ~
                                          
                                          time*
                                          (gender +
                                             
                                             age_within
                                           + age_between
                                           
                                           
                                           + I(age_within^2)
                                           + I(age_between^2)
                                           
                                           
                                           + lenEmployComp_within
                                           + lenEmployComp_between
                                           
                                           
                                           + I(lenEmployComp_within^2)
                                           + I(lenEmployComp_between^2)
                                           
                                           
                                           + totalHoursScaled_within
                                           + totalHoursScaled_between
                                           
                                           # without(for restr1-models); with, for restr2-models, restr3-models
                                           + marital + childage0to2 + childage6to16 + childage2to5 + backgr
                                           # without(for restr1-models); with, for restr1-models and for restr2-models, restr-3 models
                                           +fulltime 
                                           # without(for restr1-models); with, for restr3-models:
                                           + regstudent + regapprentice + inlabunion 
                                           + supervisor + ctworker + monthlyEarn + shiftPremium
                                           ## w/wo for total or by econSect
                                           + econSect   
                                           ## wo w/ for total or by econSect
                                           + gender:econSect      
                                          )
                                        
                                        + (1|id)    
                                        ## might like to include (age_within|id) type of terms as well  **
                                        + (1|company)+ (1|nace2)+ (1|occupation4)
                                        ## w/wo random slopes, simplest type
                                        ##  + (0+gender|nace2) + (0+gender|occupation4)
                                        , data= m1bw    ####subset(m1bw, time == t)
                                        , control = lmerControl(optimizer ="Nelder_Mead"),
                                        REML=T)

summary(M_tot_re_Ec_restricted)



##-------------------------------------0.4----------------------------------------------------
##----------------------------- INTERACTIVE MODELS--------------------------------------------


## maximal interaction model, fixed time ---------------------
## fitted with REML/ML
M0_ftf_re_interact_max <-lmerTest::lmer(wageHourly ~
                                          gender *(
                                            I(age-mean(age))
                                            + I((age-mean(age))^2)
                                            
                                            + I(lenEmployComp-mean(lenEmployComp))
                                            + I((lenEmployComp-mean(lenEmployComp))^2)
                                            
                                            + I(totalHoursScaled-mean(totalHoursScaled))
                                            
                                            + educ1
                                            
                                            + regstudent + regapprentice
                                            + marital + childage0to2 + childage6to16 + childage2to5
                                            + inlabunion +fulltime
                                            + backgr
                                            + supervisor + ctworker + monthlyEarn + shiftPremium
                                            + capitalareaComp + categ_propF + categ_propY
                                            + equalpaycert +  sizeCompanycateg
                                            
                                            ##  + econSect
                                          )
                                        + (1|company)+ (1|nace2)+ (1|occupation4)
                                        ## w/wo random slopes, simplest type
                                        ##  + (0+gender|nace2) + (0+gender|occupation4)
                                        , data=subset(m1bw, time==11),
                                        control = lmerControl(optimizer ="Nelder_Mead"),
                                        ##REML=F
                                        REML=T)

summary(M0_ftf_re_interact_max)

##--- check significance of terms by building credible intervals, using simulations ---------------------
model <- M0_ftf_re_interact_max
library(arm)
sim_fit <- arm::sim( model, 10000)  
bayestestR::hdi(sim_fit)
bayestestR::eti(sim_fit)
xf <- bayestestR::ci(sim_fit, ci = c(.5, .8, .95), effects = "fixed")
## as in Figure 6 ------------------------------
plot(xf) #------------------------------------------------------------------------------------------------


## interactive model, without the interactions which proved to be non-significant --------------
## fitted with REML/ML
M0_ftf_re_interact <-lmerTest::lmer(wageHourly ~
                                      gender +
                                      
                                      I(age-mean(age))
                                    + I((age-mean(age))^2)
                                    
                                    + I(lenEmployComp-mean(lenEmployComp))
                                    + I((lenEmployComp-mean(lenEmployComp))^2)
                                    
                                    + I(totalHoursScaled-mean(totalHoursScaled))
                                    
                                    + educ1
                                    
                                    + regstudent + regapprentice
                                    + marital + childage0to2 + childage6to16 + childage2to5
                                    + inlabunion +fulltime
                                    + backgr
                                    + supervisor + ctworker + monthlyEarn + shiftPremium
                                    + capitalareaComp + categ_propF + categ_propY
                                    + equalpaycert +  sizeCompanycateg
                                    
                                    + econSect
                                    +
                                      gender:(
                                        I(age-mean(age))
                                        + I((age-mean(age))^2)
                                        
                                        + I(lenEmployComp-mean(lenEmployComp))
                                        + I((lenEmployComp-mean(lenEmployComp))^2)
                                        
                                        + I(totalHoursScaled-mean(totalHoursScaled))
                                        
                                        + educ1
                                        
                                        + marital + childage0to2 + childage6to16 
                                        + regapprentice + inlabunion + supervisor + monthlyEarn 
                                        + equalpaycert + sizeCompanycateg
                                        + econSect
                                      )                                    
                                    + (1|company)+ (1|nace2)+ (1|occupation4)
                                    ## w/wo random slopes, simplest type
                                    ##  + (0+gender|nace2) + (0+gender|occupation4)
                                    , data=subset(m1bw, time==11),
                                    control = lmerControl(optimizer ="Nelder_Mead"),
                                    ##REML=F
                                    REML=T)
summary(M0_ftf_re_interact)


#--- restricted interaction model -------------------------------------------------------
M0_restr_ftf_re_interact <-lmerTest::lmer(wageHourly ~
                                            gender +
                                            I(age-mean(age))
                                          + I((age-mean(age))^2)
                                          + I(lenEmployComp-mean(lenEmployComp))
                                          + I((lenEmployComp-mean(lenEmployComp))^2)
                                          + I(totalHoursScaled-mean(totalHoursScaled))
                                          + educ1
                                          + regstudent + regapprentice + fulltime
                                          ##  + econSect
                                          +
                                            gender:(
                                              I(age-mean(age))
                                              + I((age-mean(age))^2)
                                              + I(lenEmployComp-mean(lenEmployComp))
                                              + I((lenEmployComp-mean(lenEmployComp))^2)
                                              + I(totalHoursScaled-mean(totalHoursScaled))
                                              + educ1
                                              + regapprentice + inlabunion 
                                              ##    + econSect
                                            )  
                                          + (1|company)+ (1|nace2)+ (1|occupation4)
                                          ## w/wo random slopes, simplest type
                                          ##  + (0+gender|nace2) + (0+gender|occupation4)
                                          , data=subset(m1bw, time==11),
                                          control = lmerControl(optimizer ="Nelder_Mead"),
                                          ##REML=F
                                          REML=T)

summary(M0_restr_ftf_re_interact)

##---- main plots ----------------------------------------------
library(merTools)
library(ggplot2)
model <- M0_restr_ftf_re_interact
#fixed effects visualisation, as in Figure 7 --------------------
feEx <- FEsim(model, 10000)
plotFEsim(feEx) +
  theme_bw() + labs(title = "Coefficient Plot",
                    y = "Median Effect Estimate", x = "")

#random effects, as in Figure 8 ---------------------------------
reEx <- REsim(model)
#head(reEx)
plotREsim(reEx)


##----- basic checks of model --------------------------------------
model <- M0_restr_ftf_re_interact
performance::r2(model, by_group = TRUE) 
#F-test with Satterthwaite adjusted degrees of freedom:
anova(model) 
# likelihood ratio tests (Deviance Difference Test) by single term deletion 
lmerTest::ranova(model)


## ---- separate models (equivalent to the interaction model): -------

## Females employees data: separate model ----------------------------
M0_f_F_re <-lmerTest::lmer(wageHourly ~ 
                             ( 
                               I(age-mean(age)) 
                               + I((age-mean(age))^2)
                               
                               + I(lenEmployComp-mean(lenEmployComp))
                               + I((lenEmployComp-mean(lenEmployComp))^2)
                               
                               + I(totalHoursScaled-mean(totalHoursScaled))
                               
                               + regstudent + regapprentice
                               + marital + childage0to2 + childage6to16 + childage2to5
                               + inlabunion + fulltime 
                               + backgr
                               + supervisor + ctworker + monthlyEarn + shiftPremium
                               + capitalareaComp 
                               + categ_propF + categ_propY 
                               + equalpaycert + sizeCompanycateg
                               + educ1
                               ## + econSect
                             )
                           + (1|company)+ (1|nace2)+ (1|occupation4)  
                           , data=subset(m1bw, gender=="1" & time == 11), 
                           control = lmerControl(optimizer ="Nelder_Mead"),
                           ##REML=F
                           REML=T)

summary(M0_f_F_re)

## Males employees data: separate model ------------------------- 
## fitting with ML/REML
M0_f_M_re <-lmerTest::lmer(wageHourly ~ 
                             ( 
                               I(age-mean(age)) 
                               + I((age-mean(age))^2)
                               
                               + I(lenEmployComp-mean(lenEmployComp))
                               + I((lenEmployComp-mean(lenEmployComp))^2)
                               
                               + I(totalHoursScaled-mean(totalHoursScaled))
                               
                               + regstudent + regapprentice
                               + marital + childage0to2 + childage6to16 + childage2to5
                               + inlabunion +fulltime 
                               + backgr
                               + supervisor + ctworker + monthlyEarn + shiftPremium
                               + capitalareaComp + categ_propF + categ_propY 
                               + equalpaycert + sizeCompanycateg
                               + educ1
                               + econSect
                             )
                           + (1|company)+ (1|nace2)+ (1|occupation4)  
                           , data=subset(m1bw, gender=="0" & time == 11), 
                           control = lmerControl(optimizer ="Nelder_Mead"),
                           ##REML=F
                           REML=T)

summary(M0_f_M_re)

##-------------- reporting on interaction, additive and separate models (fixed time) ----------
## w/wo output in file
sjPlot::tab_model(
  M0_ftf_ml,  
  M0_f_F_ml, 
  M0_f_M_ml,
  M0_ftf_ml_interact, 
  ## M0_ftf_ml_interact_slope,
  show.p = FALSE,
  # p.adjust = "holm",
  show.ci = FALSE,
  show.reflvl = TRUE,
  show.se = TRUE,
  auto.label = FALSE,
  string.se = "SE",
  show.icc = TRUE,
  show.aic = TRUE, 
  digits=3,
  dv.labels = c("additive", "F", "M", "interaction"
                ##, "inter.and.slope"
  )
  ## ,file="table_F_M_ad_int_Y2019.doc"
  
)



##---models with interactions, several fixed time values ----------------------------
modelsInteract_totEcon <- list()
for(t in 8:12) {
  M0_ftf_re_interact <-lmerTest::lmer(wageHourly ~
                                        gender +
                                        
                                        I(age-mean(age))
                                      + I((age-mean(age))^2)
                                      
                                      + I(lenEmployComp-mean(lenEmployComp))
                                      + I((lenEmployComp-mean(lenEmployComp))^2)
                                      
                                      + I(totalHoursScaled-mean(totalHoursScaled))
                                      
                                      + regstudent + regapprentice
                                      + marital + childage0to2 + childage6to16 + childage2to5
                                      + inlabunion +fulltime
                                      + backgr
                                      + supervisor + ctworker + monthlyEarn + shiftPremium
                                      + capitalareaComp + categ_propF + categ_propY
                                      + equalpaycert +  sizeCompanycateg
                                      + educ1
                                      + econSect
                                      +
                                        gender:(
                                          I(age-mean(age))
                                          + I((age-mean(age))^2)
                                          
                                          + I(lenEmployComp-mean(lenEmployComp))
                                          + I((lenEmployComp-mean(lenEmployComp))^2)
                                          
                                          + I(totalHoursScaled-mean(totalHoursScaled))
                                          
                                          + educ1
                                          
                                          + regapprentice
                                          + marital + childage0to2 + childage6to16 
                                          
                                          + inlabunion 
                                          + supervisor 
                                          + monthlyEarn 
                                          + equalpaycert + sizeCompanycateg
                                          
                                          ###    w/wo 
                                          + econSect
                                        )
                                      + (1|company)+ (1|nace2)+ (1|occupation4)
                                      ##  + (0+gender|nace2) + (0+gender|occupation4)
                                      , data=subset(m1bw, time==t),
                                      control = lmerControl(optimizer ="Nelder_Mead"),
                                      REML=T)
  modelsInteract_totEcon[[t-8+1]] <- M0_ftf_re_interact
}


##------- UNIQUE, time growth MODELS, interactive -------------------------
## with random gender intercepts and w/wo random gender-slopes
M_tot_inter_re_Ec <-lmerTest::lmer(wageHourly ~
                                     time*
                                     (gender +
                                        
                                        age_within
                                      + age_between
                                      
                                      + I(age_within^2)
                                      + I(age_between^2)
                                      
                                      + lenEmployComp_within
                                      + lenEmployComp_between
                                      
                                      + I(lenEmployComp_within^2)
                                      + I(lenEmployComp_between^2)
                                      
                                      + totalHoursScaled_within
                                      + totalHoursScaled_between
                                      
                                      + regstudent + regapprentice
                                      + marital + childage0to2 + childage6to16 + childage2to5
                                      + inlabunion +fulltime
                                      + backgr
                                      + supervisor + ctworker + monthlyEarn + shiftPremium
                                      + capitalareaComp + categ_propF + categ_propY
                                      + equalpaycert + sizeCompanycateg
                                      + educ1
                                      + econSect             #w/wo for total or by econSect
                                      + gender:econSect      # w/ for total or by econSect
                                      + 
                                        gender :(
                                          age_within
                                          + age_between
                                          
                                          
                                          + I(age_within^2)
                                          + I(age_between^2)
                                          
                                          
                                          + lenEmployComp_within
                                          + lenEmployComp_between
                                          
                                          
                                          + I(lenEmployComp_within^2)
                                          + I(lenEmployComp_between^2)
                                          
                                          
                                          + totalHoursScaled_within
                                          + totalHoursScaled_between
                                          
                                          + regapprentice
                                          + marital + childage0to2 + childage6to16 
                                          + inlabunion 
                                          + supervisor + ctworker + monthlyEarn
                                          + equalpaycert + sizeCompanycateg
                                          + educ1
                                          ## w/wo for total or by econSect
                                          + econSect      
                                          ##wo/w for total or by econSect
                                          + gender:econSect
                                        )
                                     )
                                   + (1|id)
                                   ## might include (age_within|id) type of terms as well  **
                                   + (1|company)+ (1|nace2)+ (1|occupation4)
                                   ## w/wo random slopes, simplest type
                                   ##  + (0+gender|nace2) + (0+gender|occupation4)
                                   , data= m1bw    ####subset(m1bw, time == t)
                                   , control = lmerControl(optimizer ="Nelder_Mead"),
                                   REML=T)
summary(M_tot_inter_re_Ec)




##-------------------------------- 0.5 ------------------------------------------------------
##------------ more VISUALISATION of results ------------------------------------------------

##--- figure 9a-----------------------------------------
test <- (exp(fixef(M0_ftf_re_interact))-1)[33:50]
test1 <- (test[-c(6, 13)])[-c(2,4)]
barplot(test1, las=3, col="blue", ylim=c(-0.05, 0.1),
        names.arg= c(
          "age", 
          "l.empl.",
          "t.hours.sc",
          "married",
          "child.less2",
          "child6to16",
          "lab.union",
          "superv.",
          "month.e.",
          "m.comp.", 
          "s.comp.",
          "e3",
          "e4",
          "e5" ))  

##--- figure 9b -----------------------------------------------
nn <- c( "age",  "l.empl.", "t.hours.sc",
         "married", "child.less2", "child6to16",
         "lab.union",
         "superv.",
         "month.e.",
         "m.comp.",  "s.comp.",  "e3", "e4", "e5" )
xx <- nn
yy <- nn
dataM0 <- expand.grid(X=xx, Y=yy)
# put the real values of the sums of coefficients:
dataM0$Z <- c(test1+test1[1], test1+test1[2], test1+test1[3],
              test1+test1[4], test1+test1[5], test1+test1[6],
              test1+test1[7], test1+test1[8], test1+test1[9],
              test1+test1[10], test1+test1[11], test1+test1[12],
              test1+test1[13], test1+test1[14])
ggplot2::ggplot(dataM0, aes(X, Y, fill= Z)) + 
  geom_tile() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank() ,
    axis.text.x=element_text(angle=90) ,
    axis.ticks.x=element_blank()
  )

##---- figure 10a---------------------------------------------
testadd <- exp(fixef(M0_ftf_ml)[-c( 1,2, 4, 6, 8, 25 )]) -1
barplot(testadd, las=3, ylim=c(-0.2, 0.5), col="blue", axes=TRUE,
        names.arg=
          c("age", 
            "l.empl.",
            "t.hours.sc",
            "apprent.",
            "married",
            "child.less2",
            "child6to16",
            "child2to5",
            "lab.union",
            "fulltime",
            "n.I.backgr",
            "superv.",
            "craft.w.",
            "month.e.",
            "shift.prem.",
            "comp.cap.",
            "mixt",
            "domin.M",
            "domin.o.35",
            "m.comp.", 
            "s.comp.",
            "e3",
            "e4",
            "e5",
            "municipal.", 
            "governm."))

## ----- figure 10b --------------------------------------------------
testadd1 <- exp( fixef(M0_ftf_re_restr1)[-c(1, 2, 4,6 )] )-1
barplot(testadd1, las=3, ylim=c(-0.01,0.2), col="blue",
        names.arg=c(
          "age", 
          "l.empl.",
          "t.hours.sc", "fulltime",
          "e3", "e4", "e5" 
        ))



##---------------------------------------------------------------------------
## ----- developing Oaxaca-Blinder decomposition for MLM models --------------
##---------------------------------------------------------------------------

##----- fixed time MLM
##............

##----- time dependent MLM
##..............














