####### combinbing MIDs and ISW directed datasets #####

cat("05:  combining MIDs and ISW")

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

# load in directed interstate war
isw.dir.strongest<-foreach(i=1:10)  %do% {
        a <- get(load(paste("Final_data/isw.dir.strongest", i, ".Rdata", sep="")))
        a
}

isw.dir.average<-foreach(i=1:10) %do% {
        a<-get(load(paste("Final_data/isw.dir.average", i, ".Rdata", sep="")))
        a
}

isw.dir.aggregate<-foreach(i=1:10) %do% {
        a<-get(load(paste("Final_data/isw.dir.aggregate", i, ".Rdata", sep="")))
        a
}

# load in directed mids
mids.dir.strongest<-foreach(i=1:10) %do% {
        a<-get(load(paste("Final_data/mids.dir.strongest", i, ".Rdata", sep="")))
        a
}

mids.dir.average<-foreach(i=1:10) %do% {
        a<-get(load(paste("Final_data/mids.dir.average", i, ".Rdata", sep="")))
        a
}

mids.dir.aggregate<-foreach(i=1:10) %do% {
        a<-get(load(paste("Final_data/mids.dir.aggregate", i, ".Rdata", sep="")))
        a
}

foreach(h=1:10) %do% {
        
        # strongest
        M.strong<-data.table(dplyr::select(mids.dir.strongest[[h]], a_ccode, b_ccode, dyad_ccode_year, a_fatality, maxdur, styear,a_irst, a_milex, a_milper, a_pec, a_tpop, a_upop, a_lcinc, a_polity2, 
                                           b_irst, b_milex, b_milper, b_pec, b_tpop, b_upop, b_lcinc, b_polity2, conttype, participants, terriss, riveriss, mariss, icowsal, icowsalc))
        I.strong<-data.table(dplyr::select(isw.dir.strongest[[h]], a_ccode, b_ccode, dyad_ccode_year, a_BatDeath, duration, StartYear1, a_irst, a_milex, a_milper, a_pec, a_tpop, a_upop, a_lcinc, a_polity2, 
                                           b_irst, b_milex, b_milper, b_pec, b_tpop, b_upop, b_lcinc, b_polity2, conttype, participants, terriss, riveriss, mariss, icowsal, icowsalc))
        
        # average
        M.average<-data.table(dplyr::select(mids.dir.average[[h]], a_ccode, b_ccode, dyad_ccode_year, a_fatality, maxdur, styear,a_irst, a_milex, a_milper, a_pec, a_tpop, a_upop, a_lcinc, a_polity2, 
                                            b_irst, b_milex, b_milper, b_pec, b_tpop, b_upop, b_lcinc, b_polity2, conttype, participants, terriss, riveriss, mariss, icowsal, icowsalc))
        I.average<-data.table(dplyr::select(isw.dir.average[[h]], a_ccode, b_ccode, dyad_ccode_year, a_BatDeath, duration, StartYear1, a_irst, a_milex, a_milper, a_pec, a_tpop, a_upop, a_lcinc, a_polity2, 
                                            b_irst, b_milex, b_milper, b_pec, b_tpop, b_upop, b_lcinc, b_polity2, conttype, participants, terriss, riveriss, mariss, icowsal, icowsalc))
        
        # opponent
        M.aggregate<-data.table(dplyr::select(mids.dir.aggregate[[h]], a_ccode, b_ccode, dyad_ccode_year, a_fatality, maxdur, styear,a_irst, a_milex, a_milper, a_pec, a_tpop, a_upop, a_lcinc, a_polity2, 
                                              b_irst, b_milex, b_milper, b_pec, b_tpop, b_upop, b_lcinc, b_polity2, conttype, participants, terriss, riveriss, mariss, icowsal, icowsalc))
        I.aggregate<-data.table(dplyr::select(isw.dir.aggregate[[h]], a_ccode, b_ccode, dyad_ccode_year, a_BatDeath, duration, StartYear1, a_irst, a_milex, a_milper, a_pec, a_tpop, a_upop, a_lcinc, a_polity2, 
                                              b_irst, b_milex, b_milper, b_pec, b_tpop, b_upop, b_lcinc, b_polity2, conttype, participants, terriss, riveriss, mariss, icowsal, icowsalc))
        
        # check MIDs to see if there is overlap with ISW
        # keep the ISW obs, drop the MIDs
        M.duplicates<-which(M.strong$dyad_ccode_year %in% I.strong$dyad_ccode_year)
        I.duplicates<-which(I.strong$dyad_ccode_year %in% M.strong$dyad_ccode_year)
        
        # drop the MID observations which appear in ISW
        # no duplicates
        
        M.strong.drop<-M.strong[-M.duplicates,]
        M.average.drop<-M.average[-M.duplicates,]
        M.aggregate.drop<-M.aggregate[-M.duplicates]
        
        # Change the DV
        dv<-M.strong.drop$a_fatality
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
        a_FatalR<-foreach(i=1:nrow(M.strong.drop)) %do% {
                out=i
                out<-if(M.strong.drop[i,]$a_fatality==0) {0} else {out}
                out<-if(M.strong.drop[i,]$a_fatality==1) {round(sample(rnorm(length(seq(1,25)), median(seq(1,25)), 3), 1), 0)} else {out}
                out<-if(M.strong.drop[i,]$a_fatality==2) {round(sample(rnorm(length(seq(26,100)), median(seq(26,100)), 12), 1), 0)} else {out}
                out<-if(M.strong.drop[i,]$a_fatality==3) {round(sample(rnorm(length(seq(101,250)), median(seq(101,250)), 25), 1), 0)} else {out}
                out<-if(M.strong.drop[i,]$a_fatality==4) {round(sample(rnorm(length(seq(251,500)), median(seq(251,500)), 41), 1), 0)} else {out}
                out<-if(M.strong.drop[i,]$a_fatality==5) {round(sample(rnorm(length(seq(501,999)), median(seq(501,999)), 83), 1), 0)} else {out}
                out<-if(M.strong.drop[i,]$a_fatality==-9) {NA} else {out}
                out
        }
        
        a_FatalR<-do.call(rbind, a_FatalR)
        
        # bind these into the MID data
        # strong
        M.strong.drop$a_fatality<-a_FatalM
        M.strong.M<-M.strong.drop
        
        M.strong.drop$a_fatality<-a_FatalR
        M.strong.R<-M.strong.drop
        
        # average
        M.average.drop$a_fatality<-a_FatalM
        M.average.M<-M.average.drop
        
        M.average.drop$a_fatality<-a_FatalR
        M.average.R<-M.average.drop
        
        # aggregate
        M.aggregate.drop$a_fatality<-a_FatalM
        M.aggregate.M<-M.aggregate.drop
        
        M.aggregate.drop$a_fatality<-a_FatalR
        M.aggregate.R<-M.aggregate.drop
        
        # check for -9s in MIDs
        which(M.strong.M$a_fatality==-9)
        which(M.average.M$a_fatality==-9)
        which(M.aggregate.M$a_fatality==-9)
        
        # recode -9s in ISW
        I.strong[which(I.strong$a_BatDeath==-9),]$a_BatDeath<-NA
        I.average[which(I.average$a_BatDeath==-9),]$a_BatDeath<-NA
        I.aggregate[which(I.aggregate$a_BatDeath==-9),]$a_BatDeath<-NA
        
        # rename columns
        colnames(M.strong.M)[c(4,5, 6)]<-c("a_BatDeath", "Duration", "Year")
        colnames(M.strong.R)[c(4,5, 6)]<-c("a_BatDeath", "Duration", "Year")
        colnames(I.strong)[c(4,5, 6)]<-c("a_BatDeath", "Duration", "Year")
        
        colnames(M.average.M)[c(4,5, 6)]<-c("a_BatDeath", "Duration", "Year")
        colnames(M.average.R)[c(4,5, 6)]<-c("a_BatDeath", "Duration", "Year")
        colnames(I.average)[c(4,5, 6)]<-c("a_BatDeath", "Duration", "Year")
        
        colnames(M.aggregate.M)[c(4,5, 6)]<-c("a_BatDeath", "Duration", "Year")
        colnames(M.aggregate.R)[c(4,5, 6)]<-c("a_BatDeath", "Duration", "Year")
        colnames(I.aggregate)[c(4,5, 6)]<-c("a_BatDeath", "Duration", "Year")
        
        # Save MIDs
        # Combine 
        C.strong.M<-rbind.data.frame(M.strong.M, I.strong)
        C.strong.R<-rbind.data.frame(M.strong.M, I.strong)
        
        C.average.M<-rbind.data.frame(M.average.M, I.average)
        C.average.R<-rbind.data.frame(M.average.M, I.average)
        
        C.aggregate.M<-rbind.data.frame(M.aggregate.M, I.aggregate)
        C.aggregate.R<-rbind.data.frame(M.aggregate.M, I.aggregate)
        
        
        # 
        # check for duplicates and issues here
        
        # Which are NAs?
        look<-data.table(C.strong.R)
        
        NAs<-look[which(is.na(look$a_BatDeath)),]
        
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
        
        missing<-cbind(round(colMeans(is.na(C.strong.M)), 3))*100
        missing
        
        strong<-data.table(C.strong.M)
        strong<-strong[-unique(sort(drop.num)),]
        strong<-na.omit(strong)
        
        average<-data.table(C.average.M)
        average<-average[-unique(sort(drop.num)),]
        average<-na.omit(average)
        
        aggregate<-data.table(C.aggregate.M)
        aggregate<-aggregate[-unique(sort(drop.num)),]
        aggregate<-na.omit(aggregate)
        
        
        # make the IHS transformation
        # Run over a_BatDeath
        theta<-optimize(ks.test.stat, lower=0, upper=2^10, x=strong$a_BatDeath, maximum=F)$minimum
        
        # Make the transformation
        a_BatDeath_IHS<-IHS(strong$a_BatDeath, theta)
        
        
        # attach
        strong1<-data.table(strong, a_BatDeath_IHS)
        average1<-data.table(average, a_BatDeath_IHS)
        aggregate1<-data.table(aggregate, a_BatDeath_IHS)
        
        save(strong1, file=paste("Final_Data/strong", h,".Rdata", sep=""))
        save(average1, file=paste("Final_Data/average", h,".Rdata", sep=""))
        save(aggregate1, file=paste("Final_Data/aggregate", h,".Rdata", sep=""))
        
}
   
cat("05:  done")

rm(list = setdiff(ls(), lsf.str()))

