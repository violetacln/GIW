
## Main packages - loading -----------------

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

##library(RODBC) # not needed on linux version
library(odbc)     
library(DBI)      

library(DescTools)
library(car)
library(sqldf)

library(ggeffects)



### ------------Get data -------------------------------------------------------

### on windows ---------------------------
dbhandle <- RODBC::odbcDriverConnect('driver={SQL Server};server=zeus;database=adferdir;trusted_connection=true')
micro <- RODBC::sqlQuery(dbhandle, "SELECT * from dbo.gpg2024")



###---------- Data processing --------------------------------------------------

# # LOG, HOURLY WAGES
micro$wageHourly <- log(micro$wageHourly0)
#
# # GENDER
micro$gender <- as.factor(micro$gender)
#
# # TIME  (change origin to first year in data)
micro$time <- micro$time0 - min(micro$time0)   ### re-scaled, it starts at time zero which is 2008
micro$timef <- as.factor(micro$time)

# # Length of employment (calculated)
# check:
length(micro[micro$time0 - micro$lenEmployComp < 0 ,]$id) ## ------- there is one record -------------
micro[micro$time0 - micro$lenEmployComp < 0 ,]$lenEmployComp <- micro[micro$time0 - micro$lenEmployComp < 0 ,]$time0
micro$lenEmployComp <- micro$time0 - micro$lenEmployComp
#summary(micro$lenEmployComp)
#
# # EDUCATION
micro$education <- as.factor(micro$education)
micro$educ1 <- as.factor(car::recode(micro$education,
                                     " 10:29='e2'; 30:49='e3'; 50:69='e4'; 70:89='e5' ") )

#
# # EMPLOYMENT -----------
#
#
# # COMPANY
# ## size: make groups by size as <50, 50-249, >249
micro$sizeCompanycateg <- as.factor(car::recode(micro$sizeCompany,
                                                "  lo:49='small'; 50:249='medium'; 250:hi='high'  " ))
micro$capitalareaComp <- as.factor(micro$capitalareaComp)
#
# ## grouping factors -------------
micro$company <- as.factor(micro$company)
micro$id <- as.factor(micro$id)
#
#
# # OCCUPATION ---------
#
# ## groups
micro$occupation1 <- as.factor(micro$occupation1)
micro$occupation4 <- as.factor(micro$occupation4)
micro$occupation3 <- as.factor(substring(micro$occupation4, 1,3))
## in general we want 2 digits
micro$occupation2re <- as.factor(substring(micro$occupation4, 1,2))
#
## but some categories need 3 digits, grouped as follows
micro$occupation3re <- as.factor(car::recode(micro$occupation3,
                                             "
                   233:235='233to235'; 342:347='342to347';
                   511:512='511to512'; 513:514='513to514'; 515:516='515to516'

                   "))
#
# # now we build the occupation categories with 2-digits except for the list where we use 3 digits
micro$occupation23[substring(micro$occupation4, 1,3) %in% c('22', '23', '34', '51')] <-
  micro$occupation3re[substring(micro$occupation4, 1,3) %in% c('22', '23', '34', '51')]
# #plus
micro$occupation23[!(substring(micro$occupation4, 1,3) %in% c('22', '23', '34', '51'))] <-
  micro$occupation2re[!(substring(micro$occupation4, 1,3) %in% c('22', '23', '34', '51'))]
# # so the useful (26 categories) variable, formed by using 2 and 3 digits classifications:
micro$occupation23 <- as.factor(micro$occupation23) ######## does make mapping difficult to understand!
#
#
# ## attributes of occupation (1,2,3= low, medium, high): female-dominance and youth-dominance
micro$categ_propF <- as.factor(micro$categ_propF)
micro$categ_propY <- as.factor(micro$categ_propY)
#
#

## ECONOMIC ACTIVITY: use one OR the other of the following
micro$nace2 <- as.factor(micro$nace2)  ## econ activity 2 digits
micro$nace1 <- as.factor(micro$nace1) ## use only this OR the 2-digits above, not both
#

## ECONOMIC SECTOR
micro$econSector <- as.factor(micro$econSector)

##--- new grouping of economic sectors: M= municipalities (B+S: local and Reykjavik)-----------
micro$econSect <- as.factor(car::recode(micro$econSector,
                                        " 'B'='M'; 'S'='M' " ))

##-->>>> binary version of A, R, M econ sectors-----------
micro$econSectA <- as.factor(ifelse(micro$econSect=='A', 1, 0))
micro$econSectR <- as.factor(ifelse(micro$econSect=='R', 1, 0))
micro$econSectM <- as.factor(ifelse(micro$econSect=='M', 1, 0))


#
# #OTHER attributes-------- of individuals
micro$regapprentice <- as.factor(micro$regapprentice)
micro$regstudent <- as.factor(micro$regstudent)
#
micro$marital <- as.factor(micro$marital)
micro$childage0to2 <- as.factor(micro$childage0to2)
micro$childage2to5 <- as.factor(micro$childage2to5)
micro$childage6to16 <- as.factor(micro$childage6to16)
#
micro$inlabunion <- as.factor(micro$inlabunion)
micro$fulltime <- as.factor(micro$fulltime)
#
micro$equalpaycert <- as.factor(micro$equalpaycert)

### more variables
micro$backgr <- as.factor(micro$backgr)
micro$supervisor <- as.factor(micro$supervisor)
micro$ctworker <- as.factor(micro$ctworker)
micro$monthlyEarn <- as.factor(micro$monthlyEarn)
micro$shiftPremium <- as.factor(micro$shiftPremium)

#
# # new rescaled attributes---------------
# # time is rescaled above already
micro$age2Scaled <- (micro$age^2)/100
micro$totalHoursScaled <- (micro$totalHours)/365
# lenEmployComp does not need, since acceptable range
#

#
##--- re-leveling factors, for easy interpretation of  results when modeling ---------
## in order to have same levels as previous studies
micro$fulltime <- relevel(micro$fulltime, ref="0")
micro$inlabunion <- relevel(micro$inlabunion, ref="0")
micro$capitalareaComp <- relevel(micro$capitalareaComp, ref="0")
micro$monthlyEarn <- relevel(micro$monthlyEarn, ref="0")
#


###---------------------end processing-------------------------------------------------


##------- additional processing --------------------------------


# #--------- if need of sampling ------------------
# m2 <- sqldf::sqldf("select distinct id, count(time) as nid from micro group by id")
# micro <- merge(micro, m2 , by="id")
# 
# set.seed(999)
# m0 <- dplyr::filter(micro, micro$id %in% sample(micro$id[micro$nid>5], size=20000) )


#------------ if keep all data ----------------------
#when possible: using all data ############################
m0 <- micro


##  for analysis most variables/ most data: ---------
m1 <- dplyr::select(m0, c(
  time,
  weights,
  # (obs) grouping into clusters:
  id, company , nace2, occupation4,               
  # individual attributes :
  wageHourly,  
  
  gender,
  educ1, education,
  
  lenEmployComp, totalHoursScaled, 
  age, 
  age2Scaled, # or maybe build it into model, from age_within, age_between ?!
  
  fulltime, inlabunion,
  regapprentice, regstudent,
  backgr,
  supervisor, ctworker, monthlyEarn, shiftPremium,
  
  marital, childage0to2, childage2to5, childage6to16,
  # company attributes:
  econSect, 
  econSectA, econSectR, econSectM,
  sizeCompanycateg, capitalareaComp, equalpaycert,
  # occupation attributes
  categ_propY, categ_propF
))


library(parameters)
#parameters::check_heterogeneity(m1, select= c("age", "lenEmployComp", "totalHoursScaled"), group = "id")
#--- calculate within/between-subject demeaned/mean values, needed for growth models-------------------------
m1bw <- cbind(m1, parameters::demean(m1, 
                                     select = c("age", "age2Scaled", "lenEmployComp", "totalHoursScaled"), 
                                     by = "id",
                                     suffix_demean = "_within",
                                     suffix_groupmean = "_between",
                                     add_attributes = TRUE,
                                     verbose = TRUE))


## not needed anymore:
rm(m1)
rm(m0)
rm(micro)


### --------------- exploration ---------------------------------

# mred <- dplyr::select(m1bw, c(wageHourly, gender, 
#                               educ1, backgr, age, lenEmployComp,
#                               equalpaycert,
#                               inlabunion,
#                               econSect, supervisor, fulltime,
#                               categ_propF, categ_propY))
# 
# #nice, expected correlations
# DataExplorer::plot_correlation(mred, type=c("all"))

### additional plots, as in previous modelling/analysis script.{....}


### weighted descriptive statistics; last 5 years of data pooled in time; or all data pooled in time.---------
library(gtsummary)
table_descriptive_weighted_stat <- survey::svydesign(~1, data = subset(m1bw, time>=11), weights = ~weights) |>
  tbl_svysummary(by = gender, 
                 include = c(
                   wageHourly, time, gender,
                   educ1, age,
                   lenEmployComp, totalHoursScaled,
                   fulltime, inlabunion, regstudent, regapprentice,
                   backgr, supervisor, ctworker, monthlyEarn,
                   shiftPremium,
                   marital, childage0to2, childage6to16, childage2to5,
                   econSect,
                   econSectM, econSectR, econSectA,
                   sizeCompanycateg, capitalareaComp,
                   equalpaycert, categ_propY, categ_propF
                   ##, nace2
                   ##, occupation4
                 )) 
# |>
# add_p()



##--------------------------------------------------------------------------------------------------------------------------
###----***----- unadjusted gap and total expl/unexpl parts, direct from data and counterfactual predictions --------***--------

unadj_gap_direct <- c()
expl_part_M <- c()
expl_part_FM <- c()
yM_M <- c()
yF_F <- c()
yM_F <- c()
time_point <- c()
unexpl_part_FM <- c()
unexpl_part_M <- c()

## or from tt in 9:15 but not earlier for M, unless t-dependent and derived, or pooled years
## tt=11 means year 2019; tt=15 means year 2023.

for(tt in 11:15) {
  
  dataM <- 
    subset(m1bw, gender==0 & time==tt)  ################ total economical sector
  ##subset(m1bw, gender==0 & time==tt & econSect=='M')  ############ by sector
  
  dataF <- 
    subset(m1bw, gender==1 & time==tt)   ################ total
  ##subset(m1bw, gender==1 & time==tt & econSect=='M')   ############ by sector
  
  dataFM <- 
    subset(m1bw, time==tt)   ################ total
  ## subset(m1bw, time==tt & econSect=='M')   ############ by sector
  
  
  ## real data----------------------------------------------------*** 0 *** ----------------------
  yM_dataM <- weighted.mean(exp(dataM$wageHourly), dataM$weights )
  
  yF_dataF <- weighted.mean(exp(dataF$wageHourly), dataF$weights)
  
  
  unadj_gap_direct <- cbind( unadj_gap_direct, (yF_dataF/yM_dataM -1) )
  
  
  ## model of reference: Males -----------------------------------*** 1 *** ---------------
  ## reordered predictors
  model_M <- lmerTest::lmer( wageHourly~
                               
                               educ1
                             
                             + marital
                             + childage0to2
                             + childage6to16
                             + childage2to5
                             + I(age - mean(age))
                             + I(age - mean(age))^2
                             + backgr
                             
                             + capitalareaComp
                             + sizeCompanycateg
                             + categ_propF
                             + categ_propY
                             + equalpaycert
                             
                             + I(totalHoursScaled - mean(totalHoursScaled))
                             + I(lenEmployComp - mean(lenEmployComp))
                             + I(lenEmployComp - mean(lenEmployComp))^2
                             + fulltime
                             + inlabunion
                             + regapprentice
                             + ctworker
                             + shiftPremium
                             + monthlyEarn
                             + supervisor
                             + regstudent
                             
                             + (1|company)+ (1|nace2)+ (1|occupation4)
                             , data=dataM,
                             control = lmerControl(optimizer ="Nelder_Mead"),
                             REML=TRUE)
  
  
  ## model of reference: Females ------------------------------------*** 1 bis *** ---------------
  model_F <- lmerTest::lmer( wageHourly~
                               
                               educ1
                             
                             + marital
                             + childage0to2
                             + childage6to16
                             + childage2to5
                             + I(age - mean(age))
                             + I(age - mean(age))^2
                             + backgr
                             
                             + capitalareaComp
                             + sizeCompanycateg
                             + categ_propF
                             + categ_propY
                             + equalpaycert
                             
                             + I(totalHoursScaled - mean(totalHoursScaled))
                             + I(lenEmployComp - mean(lenEmployComp))
                             + I(lenEmployComp - mean(lenEmployComp))^2
                             + fulltime
                             + inlabunion
                             + regapprentice
                             + ctworker
                             + shiftPremium
                             + monthlyEarn
                             + supervisor
                             + regstudent
                             
                             + (1|company)+ (1|nace2)+ (1|occupation4)
                             , data=dataF,
                             control = lmerControl(optimizer ="Nelder_Mead"),
                             REML=TRUE)
  
  
  yM_dataF <- weighted.mean(exp(predict(model_M,
                                        newdata=dataF, allow.new.levels = TRUE))
                            , dataF$weights)
  
  expl_part_M <- cbind( expl_part_M, yM_dataF/yM_dataM -1 )  
  
  unexpl_part_M <- cbind( unexpl_part_M  , ( yF_dataF/yM_dataM -1 )  - ( yM_dataF/yM_dataM -1 ))
  
  
  yM_M <- cbind( yM_M, yM_dataM )
  yF_F <- cbind( yF_F, yF_dataF )
  yM_F <-cbind(yM_F, yM_dataF)     ### the counter-factual data when Males-model is reference
  
  
  ###--model of reference: pooled with group label, i.e. "additive" model -----*** 2 ***----------
  model_FM <- lmerTest::lmer( wageHourly~
                                
                                gender
                              
                              + educ1
                              
                              + marital
                              + childage0to2
                              + childage6to16
                              + childage2to5
                              + I(age - mean(age))
                              + I(age - mean(age))^2
                              + backgr
                              
                              + capitalareaComp
                              + sizeCompanycateg
                              + categ_propF
                              + categ_propY
                              + equalpaycert
                              
                              + I(totalHoursScaled - mean(totalHoursScaled))
                              + I(lenEmployComp - mean(lenEmployComp))
                              + I(lenEmployComp - mean(lenEmployComp))^2
                              + fulltime
                              + inlabunion
                         ## on economical sector M should remove:      
                         ##      + regapprentice
                              + ctworker
                              + shiftPremium
                              + monthlyEarn
                              + supervisor
                              + regstudent
                              
                              + (1|company)+ (1|nace2)+ (1|occupation4)
                              , data=dataFM,
                              control = lmerControl(optimizer ="Nelder_Mead"),
                              REML=TRUE)
  
  
  coef_gender <- (fixef(model_FM)[names(fixef(model_FM))=='gender1'])
  
  ## directly:
  unexpl_part_FM <- cbind(unexpl_part_FM, exp(coef_gender)-1)
  
  time_point <- cbind(time_point, tt)
  
}

yM_M
yF_F
yM_F

### unadjusted
yF_F/yM_M -1
### explained version 1
yM_F/yM_M -1
### explained version 2
expl_part_FM <- (yF_F/yM_M -1) - unexpl_part_FM  ### i.e. (yF_dataF/yM_dataM -1) - exp(coef_gender)+1


# ### overall "Oaxaca decomposition" of unadjusted gap into "explained" and rest; the adjusted gpg also:
df_direct <- cbind(matrix(unadj_gap_direct), matrix(expl_part_M), matrix(expl_part_FM), matrix(unexpl_part_M), matrix(unexpl_part_FM), matrix(time_point))
###matrix(gpg_adjusted), matrix(time_point))
colnames(df_direct) <- c("unadj_gap_direct", "expl_part_M", "expl_part_FM", "unexpl_part_M", "usual_adjusted_gap=unexpl_part_FM", "time_point")
####"gpg_adjusted", "time_point")
df_direct



# 
# 
## save, after running the above code for each of: unrestricted econSect, then econSect=A, R, M ------------

# df_direct_tot <- df_direct

# df_direct_A <- df_direct

# df_direct_R <- df_direct

# df_direct_M <- df_direct  

result_gap_econSect_time <-
  rbind(
    cbind(data.frame(df_direct_tot), econSect='total'),
    cbind(data.frame(df_direct_A),  econSect='A'),
    cbind(data.frame(df_direct_R),  econSect='R'),
    cbind(data.frame(df_direct_M),  econSect='M')
  )

result_gap_econSect_time$year <- result_gap_econSect_time$time_point + 2008
result_gap_econSect_time


### save it into database adferdir as a table
## dbhandle <- RODBC::odbcDriverConnect('driver={SQL Server};server=zeus;database=adferdir;trusted_connection=true')
## RODBC::sqlSave(dbhandle, result_gap_econSect_time)

## or/and
## xlsx::write.xlsx2(total_effects, file='result_gap_econSect_time.xlsx')


###------------- Publishing the model fitting results as tables ----------------###

## run the models defined above, for tt<- 11 then for  tt<- 15, then run the following piece in order to view/save it as files:

sjPlot::tab_model(model_FM, model_M, model_F 
                  , digits=3
                  
                  ## , title="2023"
                  ## , title="2019"
                  
                  , dv.labels = c("additive model", "Model fitted on Males-data", "Model fitted on Females-data")
                  
                  ## saved:
                  ## , file="table_FM_M_F_2023_ordered.doc"  
                  ##, file="table_FM_M_F_2019_ordered.doc"
                  
)

###-----------------------------------------------------------------------------###



###------------------------------------------------------------------------------------------------------------###
###---------------------detailed decomposition (of "explained"="compositional effect" on the gap)-------------###


## fiX the time: tt<-15 or tt<-11 and fit the model model_FM as above (or we can choose another reference model, e.g. model_M or model_F) ***
## due to the used notation (for the model in decomposition) in what follows, we need to re-nme it:
M0_inter <- model_FM

### then ####m1bw <- subset(m1bw_reserve, econSect=='M')  ## only if we run the code below for a sector; otherwise no need.

### --->>>>> need to do: create functions to do the below


### for the random effects -------------------------------------------------

### nace2 ------------
re_nace2 <- data.frame((ranef(M0_inter)[names(ranef(M0_inter))=='nace2']))

u_nace2_e <- c()
for (i2 in levels(m1bw$nace2)){
  x <-  ifelse(
    rownames(re_nace2)== i2,
    re_nace2[rownames(re_nace2)== i2,]
    ,0)
  
  u_nace2_e <- cbind(u_nace2_e, 
                     
                     x*
                       ## weighted -------
                     ( 
                       sum(subset(m1bw, time==tt & gender== '0' & nace2 == i2)$weights)/sum(subset(m1bw, time==tt & gender== '0')$weights )
                       -
                         sum(subset(m1bw, time==tt & gender== '1' & nace2 == i2)$weights)/sum(subset(m1bw, time==tt & gender== '1')$weights)
                     )
                     
  )
}

explained_by_nace2 <- - sum(u_nace2_e)  ##### the sign had to be changed here because we need mean_F-mean_M above, for my convention   



### Occupation4 --------------------------
re_occupation4 <- data.frame((ranef(M0_inter)[names(ranef(M0_inter))=='occupation4']))

u_occupation4_e <- c()

for (i2 in levels(m1bw$occupation4)){
  x <-  ifelse(
    rownames(re_occupation4)== i2,
    re_occupation4[rownames(re_occupation4)== i2,]
    ,0)
  
  u_occupation4_e <- cbind(u_occupation4_e,
                           x*
                             ( sum(subset(m1bw, time==tt & gender== '0' & occupation4 == i2)$weights)/sum(subset(m1bw, time==tt & gender== '0')$weights)
                               -
                                 sum(subset(m1bw, time==tt & gender== '1' & occupation4 == i2)$weights)/sum(subset(m1bw, time==tt & gender== '1')$weights)
                             )
                           
  )
  
}

explained_by_occupation4 <- - sum(u_occupation4_e)    ##### the sign had to be changed here because we need mean_F-mean_M above, for my convention 



### company -------------------------

re_company <- data.frame((ranef(M0_inter)[names(ranef(M0_inter))=='company']))

u_company_e <- c()
for (i2 in levels(m1bw$company)){
  x <-  ifelse(rownames(re_company)==i2,
               re_company[rownames(re_company)==i2,]
               , 0)
  
  u_company_e <- cbind(u_company_e, 
                       x*
                         ( sum(subset(m1bw, time==tt & gender== '0' & company == i2)$weights)/sum(subset(m1bw, time==tt & gender== '0')$weights)
                           -
                             sum(subset(m1bw, time==tt & gender== '1' & company == i2)$weights)/sum(subset(m1bw, time==tt & gender== '1')$weights)
                         )
                       
  )
}

explained_by_company <- - sum(u_company_e)  ##### the sign had to be changed here because we need mean_F-mean_M above, for my convention   


### fixed ----------------------sign already changed also below according to the convention I apply------------------------------------

printing_FE <- c()

u1e <- - (fixef(M0_inter)[names(fixef(M0_inter))=='marital1'])*
  ( sum(subset(m1bw, gender== '0' & marital == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights)
    -
      sum(subset(m1bw, gender== '1' & marital == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights)
  )



u2e <- - (fixef(M0_inter)[names(fixef(M0_inter))=='regapprentice1'])*
  ( sum(subset(m1bw, gender== '0' & regapprentice == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights)
    -
      sum(subset(m1bw, gender== '1' & regapprentice == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights)
  )

## on M:
# u2e <-0
# printing_FE <- rbind( printing_FE,
#                       cbind(
#                         0,0,0, 0
#                            )
# )



u2_bis_e <- - (fixef(M0_inter)[names(fixef(M0_inter))=='regstudent1'])*
  ( sum(subset(m1bw, gender== '0' & regstudent == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights)
    -
      sum(subset(m1bw, gender== '1' & regstudent == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights)
  )



u3e <- - (fixef(M0_inter)[names(fixef(M0_inter))=='childage0to21'])*
  ( sum(subset(m1bw, gender== '0' & childage0to2 == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights)
    -
      sum(subset(m1bw, gender== '1' & childage0to2 == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights)
  )



#-------------------


u4e <- - (fixef(M0_inter)[names(fixef(M0_inter))=='childage6to161'])*
  ( sum(subset(m1bw, gender== '0' & childage6to16 == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights)
    -
      sum(subset(m1bw, gender== '1' & childage6to16 == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights)
  )



#-----------------------------------------

u44e <- - (fixef(M0_inter)[names(fixef(M0_inter))=='childage2to51'])*
  ( sum(subset(m1bw, gender== '0' & childage2to5 == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights)
    -
      sum(subset(m1bw, gender== '1' & childage2to5 == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights)
  )


#------------------------------------


u5e <- - (fixef(M0_inter)[names(fixef(M0_inter))=='inlabunion1'])*
  ( sum(subset(m1bw, gender== '0' & inlabunion == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights)
    -
      sum(subset(m1bw, gender== '1' & inlabunion == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights)
  )


#------------------------------------------


u6e <- - (fixef(M0_inter)[names(fixef(M0_inter))=='supervisor1'])*
  ( sum(subset(m1bw, gender== '0' & supervisor == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights)
    -
      sum(subset(m1bw, gender== '1' & supervisor == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights)
  )


#-----------------------------------------------------


u7e <- - (fixef(M0_inter)[names(fixef(M0_inter))=='monthlyEarn1'])*
  ( sum(subset(m1bw, gender== '0' & monthlyEarn == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights)
    -
      sum(subset(m1bw, gender== '1' & monthlyEarn == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights)
  )


#----------------------------------------------


u8e <- - (fixef(M0_inter)[names(fixef(M0_inter))=='equalpaycert1'])*
  ( sum(subset(m1bw, gender== '0' & equalpaycert == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights)
    -
      sum(subset(m1bw, gender== '1' & equalpaycert == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights)
  )

#----------------------------------------------------

u9e <- - (fixef(M0_inter)[names(fixef(M0_inter))=='sizeCompanycategmedium'])*
  ( sum(subset(m1bw, gender== '0' & sizeCompanycateg == 'medium' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights)
    -
      sum(subset(m1bw, gender== '1' & sizeCompanycateg == 'medium' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights)
  )



#--------------------------------------------------


u10e <- - (fixef(M0_inter)[names(fixef(M0_inter))=='sizeCompanycategsmall'])*
  ( sum(subset(m1bw, gender== '0' & sizeCompanycateg == 'small' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights)
    -
      sum(subset(m1bw, gender== '1' & sizeCompanycateg == 'small' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights)
  )

## for M-sector:
# printing_FE <- rbind( printing_FE,
#                       cbind(
#                         0,0,0,0
#                       )
# )


#-------------------------------------------------------


u11e <- - (fixef(M0_inter)[names(fixef(M0_inter))=='educ1e3'])*
  ( sum(subset(m1bw, gender== '0' & educ1 == 'e3' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights)
    -
      sum(subset(m1bw, gender== '1' & educ1 == 'e3' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights)
  )


#-------------------------------------------------------


u12e <- - (fixef(M0_inter)[names(fixef(M0_inter))=='educ1e4'])*
  ( sum(subset(m1bw, gender== '0' & educ1 == 'e4' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights)
    -
      sum(subset(m1bw, gender== '1' & educ1 == 'e4' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights)
  )



#-----------------------------------------------------


u13e <- - (fixef(M0_inter)[names(fixef(M0_inter))=='educ1e5'])*
  ( sum(subset(m1bw, gender== '0' & educ1 == 'e5' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights)
    -
      sum(subset(m1bw, gender== '1' & educ1 == 'e5' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights)
  )


#----------------------------------------------------

u16e <- - (fixef(M0_inter)[names(fixef(M0_inter))=='backgr1'])*
  ( sum(subset(m1bw, gender== '0' & backgr == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights)
    -
      sum(subset(m1bw, gender== '1' & backgr == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights)
  )


#------------------------------------------------------


u17e <- - (fixef(M0_inter)[names(fixef(M0_inter))=='ctworker1'])*
  ( sum(subset(m1bw, gender== '0' & ctworker == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights)
    -
      sum(subset(m1bw, gender== '1' & ctworker == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights)
  )


#-------------------------------------------------


u18e <- - (fixef(M0_inter)[names(fixef(M0_inter))=='fulltime1'])*
  ( sum(subset(m1bw, gender== '0' & fulltime == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights)
    -
      sum(subset(m1bw, gender== '1' & fulltime == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights)
  )


#--------------------------------------------


u19e <- - (fixef(M0_inter)[names(fixef(M0_inter))=='shiftPremium1'])*
  ( sum(subset(m1bw, gender== '0' & shiftPremium == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights)
    -
      sum(subset(m1bw, gender== '1' & shiftPremium == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights)
  )



#-------------------------------------------------------


u20e <- - (fixef(M0_inter)[names(fixef(M0_inter))=='capitalareaComp1'])*
  ( sum(subset(m1bw, gender== '0' & capitalareaComp == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights)
    -
      sum(subset(m1bw, gender== '1' & capitalareaComp == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights)
  )




#---------------------------------------------------------


u21e <- - (fixef(M0_inter)[names(fixef(M0_inter))=='categ_propF2'])*
  ( sum(subset(m1bw, gender== '0' & categ_propF == 2 & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights)
    -
      sum(subset(m1bw, gender== '1' & categ_propF == 2 & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights)
  )



#--------------------------------------------------


u22e <- -  (fixef(M0_inter)[names(fixef(M0_inter))=='categ_propF3'])*
  ( sum(subset(m1bw, gender== '0' & categ_propF == 3 & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights)
    -
      sum(subset(m1bw, gender== '1' & categ_propF == 3 & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights)
  )


#---------------------------------------------------



u23e <- - (fixef(M0_inter)[names(fixef(M0_inter))=='categ_propY3'])*
  ( sum(subset(m1bw, gender== '0' & categ_propY == 3 & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights)
    -
      sum(subset(m1bw, gender== '1' & categ_propY == 3 & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights)
  )



#-----------------------------------


u24e <- - (fixef(M0_inter)[names(fixef(M0_inter))=='I(age - mean(age))'])*
  (
    weighted.mean( subset(m1bw, gender=='0' & time==tt)$age, subset(m1bw, gender=='0' & time==tt)$weights ) 
    - 
      weighted.mean( subset(m1bw, gender=='1' & time==tt)$age, subset(m1bw, gender=='1' & time==tt)$weights )
  ) 
## + 0 ## of the part corresponding to "mean(age)" 


#-------------------------------


u25e <- - (fixef(M0_inter)[names(fixef(M0_inter))=='I(lenEmployComp - mean(lenEmployComp))'])*
  (
    weighted.mean( subset(m1bw, gender=='0' & time==tt)$lenEmployComp , subset(m1bw, gender=='0' & time==tt)$weights) 
    - 
      weighted.mean( subset(m1bw, gender=='1' & time==tt)$lenEmployComp , subset(m1bw, gender=='1' & time==tt)$weights)
  ) 

## + 0


#--------------------------------------------------


u26e <- - (fixef(M0_inter)[names(fixef(M0_inter))=='I(totalHoursScaled - mean(totalHoursScaled))'])*
  (
    weighted.mean( subset(m1bw, gender=='0' & time==tt)$totalHoursScaled , subset(m1bw, gender=='0' & time==tt)$weights) 
    - 
      weighted.mean( subset(m1bw, gender=='1' & time==tt)$totalHoursScaled,  subset(m1bw, gender=='1' & time==tt)$weights)
  ) 



####---------------------------------------------

#>>>>>>>>>>>>>>>>>>>>>>
### to re-print, in the right order, all of these components 


printing_FE_ordered <- c()

printing_FE_ordered <- rbind( printing_FE_ordered,
                              cbind(
                                (fixef(M0_inter)[names(fixef(M0_inter))=='educ1e3']), 
                                sum(subset(m1bw, gender== '0' & educ1 == 'e3' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights),
                                sum(subset(m1bw, gender== '1' & educ1 == 'e3' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights),
                                u11e
                              )
)


printing_FE_ordered <- rbind( printing_FE_ordered,
                              cbind(
                                (fixef(M0_inter)[names(fixef(M0_inter))=='educ1e4']), 
                                sum(subset(m1bw, gender== '0' & educ1 == 'e4' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights),
                                sum(subset(m1bw, gender== '1' & educ1 == 'e4' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights),
                                u12e
                              )
)


printing_FE_ordered <- rbind( printing_FE_ordered,
                              cbind(
                                (fixef(M0_inter)[names(fixef(M0_inter))=='educ1e5']), 
                                sum(subset(m1bw, gender== '0' & educ1 == 'e5' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights),
                                sum(subset(m1bw, gender== '1' & educ1 == 'e5' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights),
                                u13e
                              )
)

printing_FE_ordered <- rbind( printing_FE_ordered,
                              cbind(
                                (fixef(M0_inter)[names(fixef(M0_inter))=='marital1']), 
                                sum(subset(m1bw, gender== '0' & marital == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights),
                                sum(subset(m1bw, gender== '1' & marital == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights),
                                u1e
                              )
)


printing_FE_ordered <- rbind( printing_FE_ordered,
                              cbind(
                                (fixef(M0_inter)[names(fixef(M0_inter))=='childage0to21']), 
                                sum(subset(m1bw, gender== '0' & childage0to2 == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights),
                                sum(subset(m1bw, gender== '1' & childage0to2 == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights),
                                u3e
                              )
)


printing_FE_ordered <- rbind( printing_FE_ordered,
                              cbind(
                                (fixef(M0_inter)[names(fixef(M0_inter))=='childage6to161']), 
                                sum(subset(m1bw, gender== '0' & childage6to16 == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights),
                                sum(subset(m1bw, gender== '1' & childage6to16 == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights),
                                u4e
                              )
)


printing_FE_ordered <- rbind( printing_FE_ordered,
                              cbind(
                                (fixef(M0_inter)[names(fixef(M0_inter))=='childage2to51']), 
                                sum(subset(m1bw, gender== '0' & childage2to5 == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights),
                                sum(subset(m1bw, gender== '1' & childage2to5 == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights),
                                u44e
                              )
)


printing_FE_ordered <- rbind( printing_FE_ordered,
                              cbind(
                                (fixef(M0_inter)[names(fixef(M0_inter))=='I(age - mean(age))']), 
                                weighted.mean( subset(m1bw, gender=='0' & time==tt)$age, subset(m1bw, gender=='0' & time==tt)$weights ),
                                weighted.mean( subset(m1bw, gender=='1' & time==tt)$age, subset(m1bw, gender=='1' & time==tt)$weights ),
                                u24e
                              )
)


printing_FE_ordered <- rbind( printing_FE_ordered,
                              cbind(
                                (fixef(M0_inter)[names(fixef(M0_inter))=='backgr1']), 
                                sum(subset(m1bw, gender== '0' & backgr == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights),
                                sum(subset(m1bw, gender== '1' & backgr == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights),
                                u16e
                              )
)


printing_FE_ordered <- rbind( printing_FE_ordered,
                              cbind(
                                (fixef(M0_inter)[names(fixef(M0_inter))=='capitalareaComp1']), 
                                sum(subset(m1bw, gender== '0' & capitalareaComp == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights),
                                sum(subset(m1bw, gender== '1' & capitalareaComp == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights),
                                u20e
                              )
)


printing_FE_ordered <- rbind( printing_FE_ordered,
                              cbind(
                                (fixef(M0_inter)[names(fixef(M0_inter))=='sizeCompanycategmedium']), 
                                sum(subset(m1bw, gender== '0' & sizeCompanycateg == 'medium' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights),
                                sum(subset(m1bw, gender== '1' & sizeCompanycateg == 'medium' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights),
                                u9e
                              )
)


printing_FE_ordered <- rbind( printing_FE_ordered,
                              cbind(
                                (fixef(M0_inter)[names(fixef(M0_inter))=='sizeCompanycategsmall']),
                                sum(subset(m1bw, gender== '0' & sizeCompanycateg == 'small' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights),
                                sum(subset(m1bw, gender== '1' & sizeCompanycateg == 'small' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights),
                                u10e
                              )
)

## on M
# printing_FE_ordered <- rbind( printing_FE_ordered,
#                       cbind(
#                         0,0,0,0
#                       )
# )


printing_FE_ordered <- rbind( printing_FE_ordered,
                              cbind(
                                (fixef(M0_inter)[names(fixef(M0_inter))=='categ_propF2']), 
                                sum(subset(m1bw, gender== '0' & categ_propF == 2 & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights),
                                sum(subset(m1bw, gender== '1' & categ_propF == 2 & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights),
                                u21e
                              )
)


printing_FE_ordered <- rbind( printing_FE_ordered,
                              cbind(
                                (fixef(M0_inter)[names(fixef(M0_inter))=='categ_propF3']), 
                                sum(subset(m1bw, gender== '0' & categ_propF == 3 & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights),
                                sum(subset(m1bw, gender== '1' & categ_propF == 3 & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights),
                                u22e
                              )
)


printing_FE_ordered <- rbind( printing_FE_ordered,
                              cbind(
                                (fixef(M0_inter)[names(fixef(M0_inter))=='categ_propY3']), 
                                sum(subset(m1bw, gender== '0' & categ_propY == 3 & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights),
                                sum(subset(m1bw, gender== '1' & categ_propY == 3 & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights),
                                u23e
                              )
)


printing_FE_ordered <- rbind( printing_FE_ordered,
                              cbind(
                                (fixef(M0_inter)[names(fixef(M0_inter))=='equalpaycert1']), 
                                sum(subset(m1bw, gender== '0' & equalpaycert == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights),
                                sum(subset(m1bw, gender== '1' & equalpaycert == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights),
                                u8e
                              )
)

printing_FE_ordered <- rbind( printing_FE_ordered,
                              cbind(
                                (fixef(M0_inter)[names(fixef(M0_inter))=='I(totalHoursScaled - mean(totalHoursScaled))']), 
                                weighted.mean( subset(m1bw, gender=='0' & time==tt)$totalHoursScaled, subset(m1bw, gender=='0' & time==tt)$weights ),
                                weighted.mean( subset(m1bw, gender=='1' & time==tt)$totalHoursScaled, subset(m1bw, gender=='1' & time==tt)$weights ),
                                u26e
                              )
)


printing_FE_ordered <- rbind( printing_FE_ordered,
                              cbind(
                                (fixef(M0_inter)[names(fixef(M0_inter))=='I(lenEmployComp - mean(lenEmployComp))']), 
                                weighted.mean( subset(m1bw, gender=='0' & time==tt)$lenEmployComp, subset(m1bw, gender=='0' & time==tt)$weights ),
                                weighted.mean( subset(m1bw, gender=='1' & time==tt)$lenEmployComp, subset(m1bw, gender=='1' & time==tt)$weights ),
                                u25e
                              )
)

printing_FE_ordered <- rbind( printing_FE_ordered,
                              cbind(
                                (fixef(M0_inter)[names(fixef(M0_inter))=='fulltime1']), 
                                sum(subset(m1bw, gender== '0' & fulltime == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights),
                                sum(subset(m1bw, gender== '1' & fulltime == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights),
                                u18e
                              )
)


printing_FE_ordered <- rbind( printing_FE_ordered,
                              cbind(
                                (fixef(M0_inter)[names(fixef(M0_inter))=='inlabunion1']), 
                                sum(subset(m1bw, gender== '0' & inlabunion == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights),
                                sum(subset(m1bw, gender== '1' & inlabunion == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights),
                                u5e
                              )
)


printing_FE_ordered <- rbind( printing_FE_ordered,
                              cbind(
                                (fixef(M0_inter)[names(fixef(M0_inter))=='regapprentice1']),
                                sum(subset(m1bw, gender== '0' & regapprentice == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights),
                                sum(subset(m1bw, gender== '1' & regapprentice == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights),
                                u2e
                              )
)

## on M 
# printing_FE_ordered <- rbind( printing_FE_ordered,
#                       cbind(
#                         0,0,0, 0
#                       )
# )



printing_FE_ordered <- rbind( printing_FE_ordered,
                              cbind(
                                (fixef(M0_inter)[names(fixef(M0_inter))=='ctworker1']), 
                                sum(subset(m1bw, gender== '0' & ctworker == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights),
                                sum(subset(m1bw, gender== '1' & ctworker == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights),
                                u17e
                              )
)


printing_FE_ordered <- rbind( printing_FE_ordered,
                              cbind(
                                (fixef(M0_inter)[names(fixef(M0_inter))=='shiftPremium1']), 
                                sum(subset(m1bw, gender== '0' & shiftPremium == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights),
                                sum(subset(m1bw, gender== '1' & shiftPremium == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights),
                                u19e
                              )
)


printing_FE_ordered <- rbind( printing_FE_ordered,
                              cbind(
                                (fixef(M0_inter)[names(fixef(M0_inter))=='monthlyEarn1']), 
                                sum(subset(m1bw, gender== '0' & monthlyEarn == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights),
                                sum(subset(m1bw, gender== '1' & monthlyEarn == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights),
                                u7e
                              )
)


printing_FE_ordered <- rbind( printing_FE_ordered,
                              cbind(
                                (fixef(M0_inter)[names(fixef(M0_inter))=='supervisor1']), 
                                sum(subset(m1bw, gender== '0' & supervisor == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights),
                                sum(subset(m1bw, gender== '1' & supervisor == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights),
                                u6e
                              )
)


printing_FE_ordered <- rbind( printing_FE_ordered,
                              cbind(
                                (fixef(M0_inter)[names(fixef(M0_inter))=='regstudent1']), 
                                sum(subset(m1bw, gender== '0' & regstudent == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '0' & time==tt)$weights),
                                sum(subset(m1bw, gender== '1' & regstudent == '1' & time==tt)$weights)/sum(subset(m1bw, gender== '1' & time==tt)$weights),
                                u2_bis_e
                              )
)


colnames(printing_FE_ordered) <- c("coef", "M_mean", "F_mean", "composition_effect")

printing_FE_ordered


#-----------------------------------------------

## save these --------------------------

## xlsx::write.xlsx2(printing_FE_ordered, file='composition_ref_FM_2023_ordered.xlsx')

## xlsx::write.xlsx2(printing_FE_ordered, file='composition_ref_FM_2019_ordered.xlsx')


## xlsx::write.xlsx2(printing_FE_ordered, file='comp_ref_FM_2023_ordered_econSect_M.xlsx')

## xlsx::write.xlsx2(printing_FE_ordered, file='comp_ref_FM_2019_ordered_econSect_M.xlsx')


#### copy-paste into the resulted file also: the explained_by_nace2, ... etc calculated above:

explained_by_nace2 

explained_by_occupation4

explained_by_company


##--------------------------------------------------


