####### combinbing MIDs and ISW directed datasets #####

library(foreign)
library(readstata13)
library(caret)
library(rpart)
library(randomForest)
library(stats)
library(rattle)
library(Cubist)
library(nnet)
require(MASS)
require(dplyr)
require(plyr)
library(gmailr)
library(foreach)
library(beepr)

# load in nondirected datasets
setwd("F:/Dropbox/FSU/FSU_Fall_2017/Paper_1/")

# load in directed mids
load("Final_Data/mids.allies1.RData")

# load in directed isw
load("Final_Data/isw.allies1.RData")


# trim
M.allies<-data.table(dplyr::select(mids.allies1, a_ccode, b_ccode, dyad_ccode_year, a_fatality, maxdur, styear,a_irst, a_milex, a_milper, a_pec, a_tpop, a_upop, a_lcinc, a_polity2, 
                                   a_ally_irst, a_ally_milex, a_ally_milper , a_ally_pec, a_ally_tpop, a_ally_upop, a_ally_lcinc, a_ally_polity2,
                                   b_irst, b_milex, b_milper, b_pec, b_tpop, b_upop, b_lcinc, b_polity2, conttype, participants, terriss, riveriss, mariss, icowsal, icowsalc))

I.allies<-data.table(dplyr::select(isw.allies1, a_ccode, b_ccode, dyad_ccode_year, a_BatDeath, duration, StartYear1, a_irst, a_milex, a_milper, a_pec, a_tpop, a_upop, a_lcinc, a_polity2,
                                   a_ally_irst, a_ally_milex, a_ally_milper , a_ally_pec, a_ally_tpop, a_ally_upop, a_ally_lcinc, a_ally_polity2,
                                   b_irst, b_milex, b_milper, b_pec, b_tpop, b_upop, b_lcinc, b_polity2, conttype, participants, terriss, riveriss, mariss, icowsal, icowsalc))

# check MIDs to see if there is overlap with ISW
# keep the ISW obs, drop the MIDs
M.duplicates<-which(M.allies$dyad_ccode_year %in% I.allies$dyad_ccode_year)
I.duplicates<-which(I.allies$dyad_ccode_year %in% M.allies$dyad_ccode_year)

# drop the MID observations which appear in ISW
M.drop<-M.allies[-M.duplicates,]

# Change the DV
dv<-M.drop$a_fatality
dv<-ifelse(dv==-9, NA, dv)
dv<-ifelse(dv==0, 0, dv)
dv<-ifelse(dv==1, 13, dv)
dv<-ifelse(dv==2, 63, dv)
dv<-ifelse(dv==3, 175, dv)
dv<-ifelse(dv==4, 375, dv)
dv<-ifelse(dv==5, 750 , dv)
dv<-ifelse(dv==-9, NA, dv)

a_FatalM<-dv

# Random
a_FatalR<-foreach(i=1:nrow(M.drop)) %do% {
        out=i
        out<-if(M.drop[i,]$a_fatality==0) {0} else {out}
        out<-if(M.drop[i,]$a_fatality==1) {round(sample(rnorm(length(seq(1,25)), median(seq(1,25)), 3), 1), 0)} else {out}
        out<-if(M.drop[i,]$a_fatality==2) {round(sample(rnorm(length(seq(26,100)), median(seq(26,100)), 12), 1), 0)} else {out}
        out<-if(M.drop[i,]$a_fatality==3) {round(sample(rnorm(length(seq(101,250)), median(seq(101,250)), 25), 1), 0)} else {out}
        out<-if(M.drop[i,]$a_fatality==4) {round(sample(rnorm(length(seq(251,500)), median(seq(251,500)), 41), 1), 0)} else {out}
        out<-if(M.drop[i,]$a_fatality==5) {round(sample(rnorm(length(seq(501,999)), median(seq(501,999)), 83), 1), 0)} else {out}
        out<-if(M.drop[i,]$a_fatality==-9) {NA} else {out}
        out
}

a_FatalR<-do.call(rbind, a_FatalR)

# bind these into the MID data
M.drop$a_fatality<-a_FatalM
M.allies.M<-M.drop

M.drop$a_fatality<-a_FatalR
M.allies.R<-M.drop

# check for -9s in MIDs
which(M.allies.M$a_fatality==-9)

# recode -9s in ISW
I.allies[which(I.allies$a_BatDeath==-9),]$a_BatDeath<-NA


# rename columns
colnames(M.allies.M)[c(4,5, 6)]<-c("a_BatDeath", "Duration", "Year")
colnames(M.allies.R)[c(4,5, 6)]<-c("a_BatDeath", "Duration", "Year")
colnames(I.allies)[c(4,5, 6)]<-c("a_BatDeath", "Duration", "Year")


# Save MIDs
save(M.allies.M, file="Final_Data/M.allies.M.RData")
save(M.allies.R, file="Final_Data/M.allies.R.Rdata")

# Combine 
C.allies.M<-rbind.data.frame(M.allies.M, I.allies)
C.allies.R<-rbind.data.frame(M.allies.M, I.allies)

# 
save(C.allies.M, file="Final_Data/C.allies.M.RData")
save(C.allies.R, file="Final_Data/C.allies.R.RData")

# check for duplicates and issues here



### IHS transformation here

# inverse hyperbolic sine transformation
IHS <- function(x, theta){
        log(theta * x + sqrt((theta * x)^2 + 1))
}


# KS Function
ks.test.stat <- function(x, theta){ 
        newvar <- IHS(x, theta) 
        
        ks_stat <- function(x) {
                x <- x[!is.na(x)]
                ans <- ecdf(x)(x) - pnorm(x, mean = mean(x), sd = sd(x))
                max(abs(ans))
        }
        
        ks_stat(newvar)
}

# hyperbolic sine function
HS = function(x, theta) {
        sqrt((exp(2*x)-1)/((2*theta)^2))
}


look<-data.table(C.allies.R)
write.dta(look, "look.dta")


# Which are NAs?
NAs<-look[which(is.na(look$a_BatDeath)),]
write.dta(NAs, "NAs.dta")


# which are duplicates
duplicates<-look[which(duplicated(look$dyad_ccode_year)==T),]

dupe.dyads<-duplicates$dyad_ccode_year

# keep only only the duplicate with highest conflict

drop<-foreach(i=1:length(dupe.dyads)) %do% {
        
        # among the duplicated dyads, grab each one individually
        obs<-which(look$dyad_ccode_year==dupe.dyads[i])
        bar<-na.omit(look[obs,])
        
        # within the dyad, which observation has the most a) battle deaths or b) duration
        bd<-which(bar$a_BatDeath==max(bar$a_BatDeath))
        dur<-which(bar$Duration==max(bar$Duration))
        
        # if the number of battle deaths is equal in both obs, proceed to duration as the tiebreka

        num<-bd
        
        if (length(bd)>1) {num<-dur}
        if (length(dur)>1) {num<-dur[1]}
        
        # observation to keep
        bar[num]
        obs[num]
        
        # observations to drop
        obs[-num]

}

drop.num<-unlist(drop)

data<-look[-unique(sort(drop.num)),]


# Clear out the duplicates in the other two datasets

allies<-data.table(C.allies.R)
allies<-allies[-unique(sort(drop.num)),]
allies<-na.omit(allies)


# make the IHS transformation
# Run over a_BatDeath
theta<-optimize(ks.test.stat, lower=0, upper=2^10, x=allies$a_BatDeath, maximum=F)$minimum

# Make the transformation
a_BatDeath_IHS<-IHS(allies$a_BatDeath, theta)


# attach
allies1<-data.table(allies, a_BatDeath_IHS)

save(allies1, file="Final_Data/allies1.RData")


   
   
   
   
   
   
  


