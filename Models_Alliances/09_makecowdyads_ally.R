#############################
### Make DiCE_Alliance ######
#############################

library(dplyr)

# Load in a previously made universe of cowcodes
setwd("F:/Dropbox/FSU/FSU_Fall_2017/Paper_1/")

# load in COW universe
load("Final_Data/COWdyads_final.Rdata")
names(COWdyads_final)[names(COWdyads_final)=='year']<-'Year'

# load in ensembled cow models and weights
load("Output/model_list_allies.RData")
load("Output/weights.allies.out.RData")

# The purpose of this function is to predict battle deaths for hypothetical sides, similar to the previous work
# but in this case I have to write a function which will allow the user to input

# Year
# Side A
# Side B

# Dropping these in, set it up so that it will produce this

# A features, A_ally features, B features, dyad features, year
# B features, B_ally features, A features, dyad features, year

# Then, with these in hand, predict battle deaths incorporating alliances

# requires COWdyads
a_countries<-c("020", "200")
b_countries<-c("365", "750")
years<-1995

# inputs: 
DiCE_Allies<-function(a_countries, b_countries, years) {

        require(dplyr)
        require(foreach)
        require(data.table)
        
        dat<- foreach(i=1:length(years), .combine=rbind.data.frame) %do% {
                COWdyads_final %>%
                        filter(Year==years)
        }
        
        # search through COWdyads
        a_side<-foreach(i=1:length(a_countries), .combine=rbind.data.frame) %do%{
                dat%>%
                        filter(a_ccode==a_countries[i]) %>%
                        filter(b_ccode==b_countries[i])
        }
        
        b_side<-foreach(i=1:length(b_countries), .combine=rbind.data.frame) %do%{
                dat%>%
                        filter(a_ccode==b_countries[i]) %>%
                        filter(b_ccode==a_countries[i])
        }

        
        a_aggregate<-a_side%>%
                mutate(b_lcinc, b_lcinc=exp(b_lcinc)) %>%
                mutate_each(funs(sum), b_irst, b_milex, b_milper, b_pec, b_tpop, b_upop, b_lcinc) %>%
                mutate(b_lcinc, b_lcinc=log(b_lcinc)) %>%
                mutate(b_polity2=min(b_polity2))
        
        b_aggregate<-b_side%>%
                mutate(b_lcinc, b_lcinc=exp(b_lcinc)) %>%
                mutate_each(funs(sum), b_irst, b_milex, b_milper, b_pec, b_tpop, b_upop, b_lcinc) %>%
                mutate(b_lcinc, b_lcinc=log(b_lcinc)) %>%
                mutate(b_polity2=min(b_polity2))
        
        
        a_ally<-foreach(i=1:length(a_countries), .combine=rbind.data.frame) %do% {
                a_side %>%
                        filter(a_ccode!=a_countries[i])%>%
                        dplyr::select(a_irst, a_milex, a_milper, a_pec, a_tpop, a_upop, a_lcinc, a_polity2)%>%
                        mutate(a_lcinc, a_lcinc=exp(a_lcinc)) %>%
                        mutate_each(funs(sum), a_irst, a_milex, a_milper, a_pec, a_tpop, a_upop, a_lcinc) %>%
                        mutate(a_lcinc, a_lcinc=log(a_lcinc)) %>%
                        mutate(a_polity2=min(a_polity2))
        }
        
        names(a_ally)<-c("irst", "milex", "milper", "pec", "tpop", "upop", "lcinc", "polity2")
        names(a_ally)<-c(paste("a_ally", names(a_ally), sep="_"))
        
        
        b_ally<-foreach(i=1:length(b_countries), .combine=rbind.data.frame) %do% {
                b_side %>%
                        filter(a_ccode!=b_countries[i])%>%
                        dplyr::select(a_irst, a_milex, a_milper, a_pec, a_tpop, a_upop, a_lcinc, a_polity2)%>%
                        mutate(a_lcinc, a_lcinc=exp(a_lcinc)) %>%
                        mutate_each(funs(sum), a_irst, a_milex, a_milper, a_pec, a_tpop, a_upop, a_lcinc) %>%
                        mutate(a_lcinc, a_lcinc=log(a_lcinc)) %>%
                        mutate(a_polity2=min(a_polity2))
        }
        
        names(b_ally)<-c("irst", "milex", "milper", "pec", "tpop", "upop", "lcinc", "polity2")
        names(b_ally)<-c(paste("a_ally", names(b_ally), sep="_"))

        
        a<-cbind.data.frame(a_aggregate, a_ally)
        b<-cbind.data.frame(b_aggregate, b_ally)
        
        out<-rbind.data.frame(a, b)
        
        participants<-nrow(out)
        
        out_final<-cbind.data.frame(out, participants)
        
        # load in ensemble models
        
        # predict a_battle_deaths
        pred<-foreach(i=1:length(model_list), .combine=cbind) %do% {
                predict.train(model_list[[i]], newdata=out_final)
        }
        
        # bootstrap?
        pred
        
}







# The purpose of this function is to create directed dyads for all possible country pairs in the cow universe

# Inputs: A_ccode, Year_T, A_Features_T
#         B_ccode, Year_T, B_Features_T

#setwd("C:/Users/Phil/Dropbox/")
setwd("F:/Dropbox/FSU/FSU_Fall_2017/Paper_1/")

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
nmc_polity<-impute.clean1

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

# drop the 255sl keep first 260
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

source("cleanCCode.R")
source("makeCYear.R")
source("cleanCYear.R")
source("tidyCYear.R")

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
