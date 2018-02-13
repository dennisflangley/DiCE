#############################
### Make COW Dyads #######
#############################
# The purpose of this function is to create directed dyads for all possible country pairs in the cow universe

# Inputs: A_ccode, Year_T, A_Features_T
#         B_ccode, Year_T, B_Features_T

require(MASS)
require(dplyr)
library(gmailr)
library(foreach)
library(doParallel)
library(data.table)

#setwd("C:/Users/Phil/Dropbox/")
setwd("F:/Dropbox/FSU/FSU_Fall_2017/Paper_1/")
setwd("/Users/philhenrickson/Dropbox/FSU/FSU_Fall_2017/Paper_1/")

# Output: clean_A_ccode, clean_B_ccode, Year_T, A_Features, B_Features, dyad_ccode

makeCOWdyads<-function(data, year){
        
        require(dplyr)
        require(plyr)
        require(foreach)
        
        dat<-data
        years<-year
        
        out<-foreach(i=1:length(unique(years))) %do% {
                
                ccodes<-dat %>%
                        filter(year==years[i]) %>%
                        dplyr::select(ccode) %>%
                        t() %>%
                        as.vector()
                
                hold<-foreach(j=1:length(ccodes)) %do% {
                        
                        atemp<-dat %>%
                                filter(year %in% years[i]) %>%
                                filter(ccode %in% ccodes[j])
                        
                        colnames(atemp)<-c("a_ccode", "year", paste("a",colnames(dplyr::select(atemp, -ccode, -year)), sep="_"))
                        
                        btemp<-dat %>%
                                filter(year %in% years[i]) %>%
                                filter(ccode %in% ccodes[-j])
                        
                        colnames(btemp)<-c("b_ccode", "year", paste("b",colnames(dplyr::select(btemp, -ccode, -year)), sep="_"))
                        
                        cbind(atemp, dplyr::select(btemp, -year))
                }
                
                print(years[i])
                
                do.call(rbind,hold)
        }
        
        ddyad<-do.call(rbind, out)
        
        ddyad
}

dump("makeCOWdyads.R")

library(dplyr)
library(foreach)
library(doParallel)

# let's run this thing for real
library(foreign)

load("Cleaned_Data/Imputations/impute.clean1.Rdata")
load("Cleaned_Data/Imputations/impute.clean2.Rdata")
load("Cleaned_Data/Imputations/impute.clean3.Rdata")
load("Cleaned_Data/Imputations/impute.clean4.Rdata")
load("Cleaned_Data/Imputations/impute.clean5.Rdata")
load("Cleaned_Data/Imputations/impute.clean6.Rdata")
load("Cleaned_Data/Imputations/impute.clean7.Rdata")
load("Cleaned_Data/Imputations/impute.clean8.Rdata")
load("Cleaned_Data/Imputations/impute.clean9.Rdata")
load("Cleaned_Data/Imputations/impute.clean10.Rdata")


# functions needed
source("cleanCCode.R")
source("makeCYear.R")
source("cleanCYear.R")
source("tidyCYear.R")


#
impute.cleaned<-list(impute.clean1, impute.clean2, impute.clean3, impute.clean4,impute.clean5,
                     impute.clean6, impute.clean7, impute.clean8, impute.clean9, impute.clean10)


out<-foreach(i=1:length(impute.cleaned)) %do% {

        
        nmc_polity<-impute.cleaned[[i]]
        
        ugh<-filter(nmc_polity, ccode==365)
        
        nmc_polity$ccode<-substr(nmc_polity$ccode_year, 1, 3)
        
        # trim down to only necessary features
        trimmed<-dplyr::select(nmc_polity, ccode,
                               year, 
                               polity2, 
                               irst, 
                               milex, 
                               milper, 
                               pec, 
                               tpop, 
                               upop,
                               lcinc)
        
        # need to make a quick change here to account an issue with germany in 1990
        a<-which(trimmed$ccode==255 & trimmed$year==1990)
        b<-which(trimmed$ccode==260 & trimmed$year==1990)[-1]
        
        # drop the 255s, keep first 260
        trimmed<-trimmed[-c(a, b),]
        
        # use the function
        system.time(foo<-makeCOWdyads(trimmed, trimmed$year))
        foo<-data.table(foo)
        
        source("cleanCCode.R")
        
        # clean up the ccodes
        a_ccode<-lapply(foo$a_ccode, FUN=function(x) 
                
        {length<-nchar(x) 
        
        if (length==1) {paste("00", sep="", x)} else 
                
                if (length==2) {paste("0", sep="", x)}  else
                        
                        if (length>=3) {as.character(x)} else
                                
                                # NA or Negative
                                if (is.na(x)==T | x<=0) {x}})
        
        
        # clean up the ccodes
        b_ccode<-lapply(foo$b_ccode, FUN=function(x) 
                
        {length<-nchar(x) 
        
        if (length==1) {paste("00", sep="", x)} else 
                
                if (length==2) {paste("0", sep="", x)}  else
                        
                        if (length>=3) {as.character(x)} else
                                
                                # NA or Negative
                                if (is.na(x)==T | x<=0) {x}})
        
        
        dyad_ccode<-paste(a_ccode, b_ccode, sep="_")
        dyad_ccode_year<-paste(dyad_ccode, foo$year, sep="_")
        
        final<-data.table(foo, dyad_ccode_year)
        
        # Merge with contiguity
        load("Cleaned_Data/contig.Rdata")
        
        # Merge
        final.merged<-join(final, dplyr::select(contig, conttype, dyad_ccode_year), type="left")
        
        final.out<-final.merged %>%
                mutate(conttype=replace(conttype, is.na(conttype), 6))
        
        final.out<-data.table(final.out)
        
        COWdyads<-final.out
        
        # Merge with ICOW
        #### Merge with ICOW data
        ICOW<-read.dta("Data/Predictors/ICOW/test.dta")
        
        
        
        ICOW_sub<-dplyr::select(ICOW, issue, terriss, riveriss, mariss, region, claim, chal, tgt, icowsal, icowsalc, year)
        
        # clean up ccodes
        ccodea<-tidyCYear(ICOW_sub$chal, ICOW_sub$year)
        ccodeb<-tidyCYear(ICOW_sub$tgt, ICOW_sub$year)
        dyad_ccode<-paste(substr(ccodea, 1, 3), substr(ccodeb, 1, 3), sep="")
        dyad_ccode_year<-paste(dyad_ccode, ICOW_sub$year, sep="_")
        
        foo<-data.table(ICOW_sub, dyad_ccode_year)
        
        # flip
        ICOW_flip<-ICOW_sub
        ICOW_flip$chal<-ICOW_sub$tgt
        ICOW_flip$tgt<-ICOW_sub$chal
        
        ccodea<-tidyCYear(ICOW_flip$chal, ICOW_flip$year)
        ccodeb<-tidyCYear(ICOW_flip$tgt, ICOW_flip$year)
        dyad_ccode<-paste(substr(ccodea, 1, 3), substr(ccodeb, 1, 3), sep="")
        dyad_ccode_year<-paste(dyad_ccode, ICOW_sub$year, sep="_")
        
        bar<-data.table(ICOW_flip, dyad_ccode_year)
        
        # stack
        ICOW_stack<-rbind.data.frame(foo, bar)
        ICOW_stack<-dplyr::select(ICOW_stack, -year)
        
        # merge
        COWdyads_final<-join(COWdyads,  ICOW_stack, type="left", by="dyad_ccode_year", match="first")
        
        # recode
        COWdyads_final[is.na(COWdyads_final)] <- 0
        
        # make dyad_ccode_year numeric?
        COWdyads_final$dyad_ccode
        
        COWdyads_final$dyad_ccode<-as.factor(paste(dyad_ccode, COWdyads$year, sep="_"))
        
        # save
        save(COWdyads_final, file=paste("Final_Data/COWdyads_final_", i, ".Rdata", sep=""))
        
        COWdyads_final

}

