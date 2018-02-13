###########################################################################
#### Use best duration model to produce expected duration for COW universe
###########################################################################

#setwd("C:/Users/Phil/Dropbox/")
setwd("F:/Dropbox/FSU/FSU_Fall_2017/Paper_1/")


library(foreach)
library(dplyr)
library(plyr)
library(doParallel)
library(data.table)

# load data sans predicted duration
load("Cleaned_Data/mids_merged1.Rdata")

uniq.mids<-unique(mids_merged1$dispnum)


# Creating directed dyads
directeddyads.mids.allies<-function(mid, data){
        
        dat<-data
        mid<-mid
        
        require(dplyr)
        require(plyr)
        
        # extract relevant info about the war
        dat<-subset(dat, dispnum==mid)
        details<-dplyr::select(dat, styear, dispnum, hiact, hostlev, maxdur, outcome, participants)
        details<-details[1,]
        
        # split into sides and grab relevant variables
        a<-dat%>%
                filter(dispnum==mid & sidea==1) %>%
                dplyr::select(ccode_year, stabb, ccode, ccode_year, fatality, irst, milex, milper, pec, tpop, upop, lcinc, polity2, dispnum_side, orig)
        
        b<-dat%>%
                filter(dispnum==mid & sidea==0) %>%
                dplyr::select(ccode_year, stabb, ccode, ccode_year, fatality, irst, milex, milper, pec, tpop, upop, lcinc, polity2, dispnum_side, orig)
        
        #### 1) Strongest opponent
        strong.a<-a[which(a$lcinc==max(a$lcinc)),]
        
        a.list<-list()
        if(nrow(a) == 1){a.list[[1]]<-strong.a} else {
                for (i in 1:nrow(a)){
                        a.list[[i]]<-a[i,]
                }
        }
        
        strong.b<-b[which(b$lcinc==max(b$lcinc)),]
        
        b.list<-list()
        if(nrow(b) == 1){b.list[[1]]<-strong.b} else {
                for (i in 1:nrow(b)){
                        b.list[[i]]<-b[i,]
                }
        }
        
        ### 3) Aggregate opponent
        aggregate.a<-dat%>%
                filter(dispnum==mid & sidea==1) %>%
                dplyr::select(ccode_year, stabb, ccode, ccode_year, fatality, irst, milex, milper, pec, tpop, upop, lcinc, polity2, dispnum_side, orig) %>%
                mutate(lcinc=exp(lcinc)) %>%
                mutate_each(funs(sum), fatality, irst, milex, milper, pec, tpop, upop, lcinc, polity2) %>%
                mutate(lcinc=log(lcinc)) %>%
                mutate(stabb=replace(stabb, 1, paste(a$stabb, collapse=""))) %>%
                # replace ccode and ccode_year with strongest ccode and ccode_year
                mutate(ccode=replace(ccode, 1, strong.a$ccode)) %>%
                mutate(ccode_year=replace(ccode,1, strong.a$ccode_year)) %>%
                # Coded as 1 if there's an originator on their side
                mutate(orig=min(a$orig)) %>%
                # Code polity and polity2 as lowest in the group
                mutate(polity2=min(a$polity))
        
        aggregate.a<-aggregate.a[1,]
        
        # 
        allies.a<-dat%>%
                filter(dispnum==mid & sidea==1) %>%
                dplyr::select(irst, milex, milper, pec, tpop, upop, lcinc, polity2)  
        
        aggregate.b<-dat%>%
                filter(dispnum==mid & sidea==0) %>%
                dplyr::select(ccode_year, stabb, ccode, ccode_year, fatality, irst, milex, milper, pec, tpop, upop, lcinc, polity2, dispnum_side, orig) %>%
                mutate(lcinc=exp(lcinc)) %>%
                mutate_each(funs(sum), fatality, irst, milex, milper, pec, tpop, upop, lcinc, polity2) %>%
                mutate(lcinc=log(lcinc)) %>%
                mutate(stabb=replace(stabb, 1, paste(b$stabb, collapse=""))) %>%
                # replace ccode and ccode_year with strongest ccode and ccode_year
                mutate(ccode=replace(ccode, 1, strong.b$ccode)) %>%
                mutate(ccode_year=replace(ccode,1, strong.b$ccode_year)) %>%
                # Coded as 1 if there's an originator on their side
                mutate(orig=min(b$orig)) %>%
                # Code polity and polity2 as lowest in the group
                mutate(polity2=min(b$polity))
        
        aggregate.b<-aggregate.b[1,]
        
        allies.b<-dat%>%
                filter(dispnum==mid & sidea==0) %>%
                dplyr::select(irst, milex, milper, pec, tpop, upop, lcinc, polity2)
        
        
        directed.a.agg<-foreach(i=1:nrow(a), .combine=rbind.data.frame) %do% {
                
                allies<-allies.a[-i,]
                
                a.allies<-allies %>%
                        mutate(lcinc, lcinc=exp(lcinc)) %>%
                        mutate_each(funs(sum), irst, milex, milper, pec, tpop, upop, lcinc) %>%
                        mutate(lcinc, lcinc=log(lcinc)) %>%
                        mutate(polity2=min(polity2))
                
                a.allies<-a.allies[1,]
                a.allies[is.na(a.allies)]<-0
                
                names(a.allies)<-c(paste("ally", names(a.allies), sep="_"))
                
                dir.a.agg<-data.table(details, a.list[[i]], a.allies, aggregate.b, paste(a.list[[i]]$ccode , aggregate.b$ccode, aggregate.b$year, sep=""), a.list[[i]]$lcinc/aggregate.b$lcinc)
                names(dir.a.agg)<-c(names(details), paste("a", sep="_", names(a)), paste("a", sep="_", names(a.allies)),paste("b", sep="_", names(b)), "dyad_ccode", "cinc_ratio")
                
                dir.a.agg
                
        }
        
        directed.b.agg<-foreach(i=1:nrow(b), .combine=rbind.data.frame) %do% {
                
                allies<-allies.b[-i,]
                
                b.allies<-allies %>%
                        mutate(lcinc, lcinc=exp(lcinc)) %>%
                        mutate_each(funs(sum), irst, milex, milper, pec, tpop, upop, lcinc) %>%
                        mutate(lcinc, lcinc=log(lcinc)) %>%
                        mutate(polity2=min(polity2))
                
                b.allies<-b.allies[1,]
                b.allies[is.na(b.allies)]<-0
                
                names(b.allies)<-c(paste("ally", names(b.allies), sep="_"))
                
                dir.b.agg<-data.table(details, b.list[[i]], b.allies, aggregate.a, paste(b.list[[i]]$ccode , aggregate.a$ccode, aggregate.a$year, sep=""), b.list[[i]]$lcinc/aggregate.a$lcinc)
                names(dir.b.agg)<-c(names(details), paste("a", sep="_", names(a)), paste("a", sep="_", names(a.allies)), paste("b", sep="_", names(b)), "dyad_ccode", "cinc_ratio")
                
                dir.b.agg
                
        }
        
        # Output to list
        directed.agg<-rbind.data.frame(directed.a.agg, directed.b.agg)
        directed.agg
        
}

# 
uniq.mids<-unique(mids_merged1$dispnum)

# Using the function
mids.allies<-foreach(i=1:length(uniq.mids), .combine=rbind.data.frame) %do% {
        out<-directeddyads.mids.allies(uniq.mids[i], mids_merged1)
        print(i)
        out
}

# Merge with contig
source("cleanCCode.R")
source("makeCYear.R")
source("cleanCYear.R")
source("tidyCYear.R")

## Contiguity
load("Cleaned_Data/contig.Rdata")

# Create a dyad_ccode-year variable
# aggregate
dyad_ccode<-paste(mids.allies$a_ccode, mids.allies$b_ccode, sep="")
dyad_ccode_year<-paste(dyad_ccode, mids.allies$styear, sep="_")
mids.allies<-data.frame(mids.allies, dyad_ccode_year)

mids.allies<-join(mids.allies,  dplyr::select(contig, conttype, dyad_ccode_year), type="left", match="first")
mids.allies<-mids.allies%>%
        mutate(conttype=replace(conttype, is.na(conttype), 6))

save(mids.allies, file="Final_Data/mids.allies.RData")


# Merge with ICOW
#### Merge with ICOW data
library(foreign)
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
mids.allies1<-join(mids.allies,  ICOW_stack, type="left", by="dyad_ccode_year", match="first")

# recode NAs to zero -> purely because if something is NA it means that the issue isnt present
mids.allies1[is.na(mids.allies1)] <- 0


# How many -9s are there?
table(mids.allies1$a_fatality)


# save
save(mids.allies1, file="Final_Data/mids.allies1.RData")

