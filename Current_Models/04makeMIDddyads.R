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

cat("04:  now making mids directed dyads")

# load data sans predicted duration
load("Cleaned_Data/mids_merged1.Rdata")

uniq.mids<-unique(mids_merged1$dispnum)


# Creating directed dyads
directeddyads.mids<-function(mid, data){
        
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
        
        
        dir.a.s<-list()
        dir.b.s<-list()
        
        for (i in 1:nrow(a)){
                dir.a.s[[i]]<-data.table(details, a.list[[i]], strong.b, paste(a.list[[i]]$ccode , strong.b$ccode, strong.b$year, sep=""), a.list[[i]]$lcinc/strong.b$lcinc)
                names(dir.a.s[[i]])<-c(names(details), paste("a", sep="_", names(a)), paste("b", sep="_", names(b)), "dyad_ccode", "cinc_ratio")
        }
        
        for (i in 1:nrow(b)){
                dir.b.s[[i]]<-data.table(details, b.list[[i]], strong.a, paste(b.list[[i]]$ccode , strong.a$ccode, strong.a$year, sep=""), b.list[[i]]$lcinc/strong.a$lcinc)
                names(dir.b.s[[i]])<-c(names(details), paste("a", sep="_", names(a)), paste("b", sep="_", names(b)), "dyad_ccode", "cinc_ratio")
        }
        
        # Output to list
        directed.a.s<-do.call(rbind.data.frame, dir.a.s)
        directed.b.s<-do.call(rbind.data.frame, dir.b.s)
        
        directed.strongest<-rbind.data.frame(directed.a.s, directed.b.s)
        
        
        ### 2) Average opponent
        average.a<-dat%>%
                filter(dispnum==mid & sidea==1) %>%
                dplyr::select(ccode_year, stabb, ccode, ccode_year, fatality, irst, milex, milper, pec, tpop, upop, lcinc, polity2, dispnum_side, orig) %>%
                mutate(lcinc=exp(lcinc)) %>%
                mutate_each(funs(mean), fatality, irst, milex, milper, pec, tpop, upop, lcinc, polity2) %>%
                mutate(lcinc=log(lcinc)) %>%
                mutate(stabb=replace(stabb, 1, paste(a$stabb, collapse=""))) %>%
                # replace ccode and ccode_year with strongest ccode and ccode_year
                mutate(ccode=replace(ccode, 1, strong.a$ccode)) %>%
                mutate(ccode_year=replace(ccode,1, strong.a$ccode_year)) %>%
                # Coded as 1 if there's an originator on their side
                mutate(orig=min(a$orig)) %>%
                # Code polity and polity2 as lowest in the group
                mutate(polity2=min(a$polity))
        
        
        average.a<-average.a[1,]
        
        
        average.b<-dat%>%
                filter(dispnum==mid & sidea==0) %>%
                dplyr::select(ccode_year, stabb, ccode, ccode_year, fatality, irst, milex, milper, pec, tpop, upop, lcinc, polity2, dispnum_side, orig) %>%
                mutate(lcinc=exp(lcinc)) %>%
                mutate_each(funs(mean), fatality, irst, milex, milper, pec, tpop, upop, lcinc, polity2) %>%
                mutate(lcinc=log(lcinc)) %>%
                mutate(stabb=replace(stabb, 1, paste(b$stabb, collapse=""))) %>%
                # replace ccode and ccode_year with strongest ccode and ccode_year
                mutate(ccode=replace(ccode, 1, strong.b$ccode)) %>%
                mutate(ccode_year=replace(ccode,1, strong.b$ccode_year)) %>%
                # Coded as 1 if there's an originator on their side
                mutate(orig=min(b$orig)) %>%
                # Code polity and polity2 as lowest in the group
                mutate(polity2=min(b$polity))
        
        average.b<-average.b[1,]
        
        dir.a.avg<-list()
        dir.b.avg<-list()
        
        for (i in 1:nrow(a)){
                dir.a.avg[[i]]<-data.table(details, a.list[[i]], average.b, paste(a.list[[i]]$ccode , average.b$ccode, average.b$year, sep=""), a.list[[i]]$lcinc/average.b$lcinc)
                
                names(dir.a.avg[[i]])<-c(names(details), paste("a", sep="_", names(a)), paste("b", sep="_", names(b)), "dyad_ccode", "cinc_ratio")
        }
        
        for (i in 1:nrow(b)){
                dir.b.avg[[i]]<-data.table(details, b.list[[i]], average.a, paste(b.list[[i]]$ccode , average.a$ccode, average.a$year, sep=""), b.list[[i]]$lcinc/average.a$lcinc)
                names(dir.b.avg[[i]])<-c(names(details), paste("a", sep="_", names(a)), paste("b", sep="_", names(b)), "dyad_ccode", "cinc_ratio")
        }
        
        directed.a.avg<-do.call(rbind.data.frame, dir.a.avg)
        directed.b.avg<-do.call(rbind.data.frame, dir.b.avg)
        
        directed.average<-rbind.data.frame(directed.a.avg, directed.b.avg)
        
        
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
        
        
        dir.a.agg<-list()
        dir.b.agg<-list()
        
        for (i in 1:nrow(a)){
                dir.a.agg[[i]]<-data.table(details, a.list[[i]], aggregate.b, paste(a.list[[i]]$ccode , aggregate.b$ccode, aggregate.b$year, sep=""), a.list[[i]]$lcinc/aggregate.b$lcinc)
                names(dir.a.agg[[i]])<-c(names(details), paste("a", sep="_", names(a)), paste("b", sep="_", names(b)), "dyad_ccode", "cinc_ratio")
        }
        
        for (i in 1:nrow(b)){
                dir.b.agg[[i]]<-data.table(details, b.list[[i]], aggregate.a, paste(b.list[[i]]$ccode , aggregate.a$ccode, aggregate.a$year, sep=""), b.list[[i]]$lcinc/aggregate.a$lcinc)
                names(dir.b.agg[[i]])<-c(names(details), paste("a", sep="_", names(a)), paste("b", sep="_", names(b)), "dyad_ccode", "cinc_ratio")
        }
        
        
        # Output to list
        directed.a.agg<-do.call(rbind.data.frame, dir.a.agg)
        directed.b.agg<-do.call(rbind.data.frame, dir.b.agg)
        
        directed.agg<-rbind.data.frame(directed.a.agg, directed.b.agg)
        
        # Final Output
        out<-list("strongest" = directed.strongest, "average" = directed.average, "aggregate"=directed.agg)
        
}

# 
# load dataset
load("Cleaned_Data/mids_merged1.Rdata")
load("Cleaned_Data/mids_merged2.Rdata")
load("Cleaned_Data/mids_merged3.Rdata")
load("Cleaned_Data/mids_merged4.Rdata")
load("Cleaned_Data/mids_merged5.Rdata")
load("Cleaned_Data/mids_merged6.Rdata")
load("Cleaned_Data/mids_merged7.Rdata")
load("Cleaned_Data/mids_merged8.Rdata")
load("Cleaned_Data/mids_merged9.Rdata")
load("Cleaned_Data/mids_merged10.Rdata")

imputations<-list(mids_merged1,
                  mids_merged2,
                  mids_merged3,
                  mids_merged4,
                  mids_merged5,
                  mids_merged6,
                  mids_merged7,
                  mids_merged8,
                  mids_merged9,
                  mids_merged10)

# Dropping a bunch of wars because these only have one participant
uniq.mids<-unique(mids_merged1$dispnum)

# Merge with contig
source("cleanCCode.R")
source("makeCYear.R")
source("cleanCYear.R")
source("tidyCYear.R")

## Contiguity
load("Cleaned_Data/contig.Rdata")

# Using the function
foreach(h=1:length(imputations)) %do% { 
        out.1<-list()
        out.2<-list()
        out.3<-list()
        
        for (i in 1:length(uniq.mids)){
                out.1[[i]]<-directeddyads.mids(uniq.mids[i], imputations[[h]])$strongest
                out.2[[i]]<-directeddyads.mids(uniq.mids[i], imputations[[h]])$average
                out.3[[i]]<-directeddyads.mids(uniq.mids[i], imputations[[h]])$aggregate
                print(i)
        }
        
        mids.dir.strongest<-do.call(rbind.data.frame,out.1)
        mids.dir.average<-do.call(rbind.data.frame,out.2)
        mids.dir.aggregate<-do.call(rbind.data.frame, out.3)
        
        # Merge
        # strongest
        dyad_ccode<-paste(mids.dir.strongest$a_ccode, mids.dir.strongest$b_ccode, sep="")
        dyad_ccode_year<-paste(dyad_ccode, mids.dir.strongest$styear, sep="_")
        mids.dir.strongest<-data.frame(mids.dir.strongest, dyad_ccode_year)
        
        mids.dir.strongest<-join(mids.dir.strongest, dplyr::select(contig, conttype, dyad_ccode_year), type="left")
        mids.dir.strongest<-mids.dir.strongest %>%
                mutate(conttype=replace(conttype, is.na(conttype), 6))
        
        # average
        dyad_ccode<-paste(mids.dir.average$a_ccode, mids.dir.average$b_ccode, sep="")
        dyad_ccode_year<-paste(dyad_ccode, mids.dir.average$styear, sep="_")
        mids.dir.average<-data.frame(mids.dir.average, dyad_ccode_year)
        
        mids.dir.average<-join(mids.dir.average, dplyr::select(contig, conttype, dyad_ccode_year), type="left")
        mids.dir.average<-mids.dir.average %>%
                mutate(conttype=replace(conttype, is.na(conttype), 6))
        
        # aggregate
        dyad_ccode<-paste(mids.dir.aggregate$a_ccode, mids.dir.aggregate$b_ccode, sep="")
        dyad_ccode_year<-paste(dyad_ccode, mids.dir.aggregate$styear, sep="_")
        mids.dir.aggregate<-data.frame(mids.dir.aggregate, dyad_ccode_year)
        
        mids.dir.aggregate<-join(mids.dir.aggregate, dplyr::select(contig, conttype, dyad_ccode_year), type="left")
        mids.dir.aggregate<-mids.dir.aggregate %>%
                mutate(conttype=replace(conttype, is.na(conttype), 6))
        
        
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
        mids.dir.strongest1<-join(mids.dir.strongest,  ICOW_stack, type="left", by="dyad_ccode_year", match="first")
        mids.dir.average1<-join(mids.dir.average, ICOW_stack, type="left", by="dyad_ccode_year", match="first")
        mids.dir.aggregate1<-join(mids.dir.aggregate,  ICOW_stack, type="left", by="dyad_ccode_year", match="first")
        
        # check missigness
        colMeans(is.na(mids.dir.strongest1))
        
        # recode NAs to zero -> purely because if something is NA it means that the issue isnt present
        mids.dir.strongest1[is.na(mids.dir.strongest1)] <- 0
        mids.dir.average1[is.na(mids.dir.average1)] <- 0
        mids.dir.aggregate1[is.na(mids.dir.aggregate1)] <- 0
        
        # save
        save(mids.dir.strongest1, file=paste("Final_Data/mids.dir.strongest", h,".Rdata", sep=""))
        save(mids.dir.average1, file=paste("Final_Data/mids.dir.average", h,".Rdata", sep=""))
        save(mids.dir.aggregate1, file=paste("Final_Data/mids.dir.aggregate", h,".Rdata", sep=""))
}

cat("04:  done")

rm(list = setdiff(ls(), lsf.str()))
rm(list = setdiff(ls(), lsf.str()))
