########################################
#### Create Directed ISW Data #########
#######################################

#setwd("C:/Users/Phil/Dropbox/")
setwd("F:/Dropbox/FSU/FSU_Fall_2017/Paper_1/")

library(foreach)
library(dplyr)
library(plyr)
library(doParallel)

cat("04:  now making interstate war directed dyads")

# Give the function the dataset
# The goal of the function is to take a dataset with a warnumber, then create directed dyads from it
# I then iterate over all warnums in the dataset 

# The other goal is to set it up so that I can feed it the selection of the dyads

directeddyads<-function(war, data){
        
        require(dplyr)
        require(plyr)
        
        dat<-data
        war<-war
        
        # extract relevant info about the war
        dat<-subset(dat, WarNum==war)
        details<-dplyr::select(dat, WarNum, WarName, WarType, StartYear1, duration, participants, multiple, WhereFought)
        details<-details[1,]
        
        # split into sides and grab relevant variables
        a<-dat%>%
                filter(WarNum==war & Side==1) %>%
                dplyr::select(ccode_year, BatDeath, StateName, ccode, irst, milex, milper, pec, tpop, upop, lcinc, polity2, Side, warnum_side, Initiator)
        
        b<-dat%>%
                filter(WarNum==war & Side==2) %>%
                dplyr::select(ccode_year, BatDeath, StateName, ccode, irst, milex, milper, pec, tpop, upop, lcinc, polity2, Side, warnum_side, Initiator)
        
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
                filter(WarNum==war & Side==1) %>%
                dplyr::select(ccode_year, BatDeath, StateName, ccode, irst, milex, milper, pec, tpop, upop, lcinc, polity2, Side, warnum_side, Initiator) %>%
                mutate(lcinc=exp(lcinc)) %>%
                mutate_each(funs(mean), BatDeath, irst, milex, milper, pec, tpop, upop, lcinc, polity2) %>%
                mutate(lcinc=log(lcinc)) %>%
                mutate(StateName=replace(StateName, 1, paste(a$StateName, collapse=""))) %>%
                # replace ccode and ccode_year with strongest ccode and ccode_year
                mutate(ccode=replace(ccode, 1, strong.a$ccode)) %>%
                mutate(ccode_year=replace(ccode,1, strong.a$ccode_year)) %>%
                # Coded as 1 if there's an initiator on their side
                mutate(Initiator=min(a$Initiator)) %>%
                # Code polity and polity2 as lowest in the group
                mutate(polity2=min(a$polity))
        
        
        average.a<-average.a[1,]
        
        
        average.b<-dat%>%
                filter(WarNum==war & Side==2) %>%
                dplyr::select(ccode_year, BatDeath, StateName, ccode, irst, milex, milper, pec, tpop, upop, lcinc, polity2, Side, warnum_side, Initiator) %>%
                mutate(lcinc=exp(lcinc)) %>%
                mutate_each(funs(mean), BatDeath, irst, milex, milper, pec, tpop, upop, lcinc, polity2) %>%
                mutate(lcinc=log(lcinc)) %>%
                mutate(StateName=replace(StateName, 1, paste(b$StateName, collapse=""))) %>%
                # replace ccode and ccode_year with strongest ccode and ccode_year
                mutate(ccode=replace(ccode, 1, strong.b$ccode)) %>%
                mutate(ccode_year=replace(ccode,1, strong.b$ccode_year)) %>%
                # Coded as 1 if there's an initiator on their side
                mutate(Initiator=min(b$Initiator)) %>%
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
        
        
        ### 3) Aggregate Opponent
        aggregate.a<-dat%>%
                filter(WarNum==war & Side==1) %>%
                dplyr::select(ccode_year, BatDeath, StateName, ccode, irst, milex, milper, pec,tpop, upop, lcinc, polity2, Side, warnum_side, Initiator) %>%
                mutate(lcinc, lcinc=exp(lcinc)) %>%
                mutate_each(funs(sum), BatDeath, irst, milex, milper, pec, tpop, upop, lcinc, polity2) %>%
                mutate(lcinc, lcinc=log(lcinc)) %>%
                mutate(StateName=replace(StateName, 1, paste(a$StateName, collapse=""))) %>%
                # replace ccode and ccode_year with strongest ccode and ccode_year
                mutate(ccode=replace(ccode, 1, strong.a$ccode)) %>%
                mutate(ccode_year=replace(ccode,1, strong.a$ccode_year)) %>%
                # Coded as 1 if there's an initiator on their side
                mutate(Initiator=min(a$Initiator)) %>%
                # Code polity and polity2 as lowest in the group
                mutate(polity2=min(a$polity))
        
        aggregate.a<-aggregate.a[1,]
        
        aggregate.b<-dat%>%
                filter(WarNum==war & Side==2) %>%
                dplyr::select(ccode_year, BatDeath, StateName, ccode, irst, milex, milper, pec,tpop, upop, lcinc, polity2, Side, warnum_side, Initiator) %>%
                mutate(lcinc, lcinc=exp(lcinc)) %>%
                mutate_each(funs(sum), BatDeath, irst, milex, milper, pec, tpop, upop, lcinc, polity2) %>%
                mutate(lcinc, lcinc=log(lcinc)) %>%
                mutate(StateName=replace(StateName, 1, paste(b$StateName, collapse=""))) %>%
                # replace ccode and ccode_year with strongest ccode and ccode_year
                mutate(ccode=replace(ccode, 1, strong.b$ccode)) %>%
                mutate(ccode_year=replace(ccode,1, strong.b$ccode_year)) %>%
                # Coded as 1 if there's an initiator on their side
                mutate(Initiator=min(b$Initiator)) %>%
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



# load dataset
load("Cleaned_Data/isw_merged1.Rdata")
load("Cleaned_Data/isw_merged2.Rdata")
load("Cleaned_Data/isw_merged3.Rdata")
load("Cleaned_Data/isw_merged4.Rdata")
load("Cleaned_Data/isw_merged5.Rdata")
load("Cleaned_Data/isw_merged6.Rdata")
load("Cleaned_Data/isw_merged7.Rdata")
load("Cleaned_Data/isw_merged8.Rdata")
load("Cleaned_Data/isw_merged9.Rdata")
load("Cleaned_Data/isw_merged10.Rdata")

imputations<-list(isw_merged1,
                  isw_merged2,
                  isw_merged3,
                  isw_merged4,
                  isw_merged5,
                  isw_merged6,
                  isw_merged7,
                  isw_merged8,
                  isw_merged9,
                  isw_merged10)

# Dropping a bunch of wars because these only have one participant
uniq<-sort(unique(isw_merged1$WarNum))

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
        
        for (i in 1:length(uniq)){
                out.1[[i]]<-directeddyads(uniq[i], imputations[[h]])$strongest
                out.2[[i]]<-directeddyads(uniq[i], imputations[[h]])$average
                out.3[[i]]<-directeddyads(uniq[i], imputations[[h]])$aggregate
                print(i)
        }
        
        isw.dir.strongest<-do.call(rbind.data.frame,out.1)
        isw.dir.average<-do.call(rbind.data.frame,out.2)
        isw.dir.aggregate<-do.call(rbind.data.frame, out.3)
        
        # Merge
        # strongest
        dyad_ccode<-paste(isw.dir.strongest$a_ccode, isw.dir.strongest$b_ccode, sep="")
        dyad_ccode_year<-paste(dyad_ccode, isw.dir.strongest$StartYear1, sep="_")
        isw.dir.strongest<-data.frame(isw.dir.strongest, dyad_ccode_year)
        
        isw.dir.strongest<-join(isw.dir.strongest, dplyr::select(contig, conttype, dyad_ccode_year), type="left")
        isw.dir.strongest<-isw.dir.strongest %>%
                mutate(conttype=replace(conttype, is.na(conttype), 6))
        
        # average
        dyad_ccode<-paste(isw.dir.average$a_ccode, isw.dir.average$b_ccode, sep="")
        dyad_ccode_year<-paste(dyad_ccode, isw.dir.average$StartYear1, sep="_")
        isw.dir.average<-data.frame(isw.dir.average, dyad_ccode_year)
        
        isw.dir.average<-join(isw.dir.average, dplyr::select(contig, conttype, dyad_ccode_year), type="left")
        isw.dir.average<-isw.dir.average %>%
                mutate(conttype=replace(conttype, is.na(conttype), 6))
        
        # aggregate
        dyad_ccode<-paste(isw.dir.aggregate$a_ccode, isw.dir.aggregate$b_ccode, sep="")
        dyad_ccode_year<-paste(dyad_ccode, isw.dir.aggregate$StartYear1, sep="_")
        isw.dir.aggregate<-data.frame(isw.dir.aggregate, dyad_ccode_year)
        
        isw.dir.aggregate<-join(isw.dir.aggregate, dplyr::select(contig, conttype, dyad_ccode_year), type="left")
        isw.dir.aggregate<-isw.dir.aggregate %>%
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
        isw.dir.strongest1<-join(isw.dir.strongest,  ICOW_stack, type="left", by="dyad_ccode_year", match="first")
        isw.dir.average1<-join(isw.dir.average, ICOW_stack, type="left", by="dyad_ccode_year", match="first")
        isw.dir.aggregate1<-join(isw.dir.aggregate,  ICOW_stack, type="left", by="dyad_ccode_year", match="first")
        
        # recode NAs to zero -> purely because if something is NA it means that the issue isnt present
        isw.dir.strongest1[is.na(isw.dir.strongest1)] <- 0
        isw.dir.average1[is.na(isw.dir.average1)] <- 0
        isw.dir.aggregate1[is.na(isw.dir.aggregate1)] <- 0
        
        # save
        save(isw.dir.strongest1, file=paste("Final_Data/isw.dir.strongest", h,".Rdata", sep=""))
        save(isw.dir.average1, file=paste("Final_Data/isw.dir.average", h,".Rdata", sep=""))
        save(isw.dir.aggregate1, file=paste("Final_Data/isw.dir.aggregate", h,".Rdata", sep=""))
}

cat("04:  done")

rm(list = setdiff(ls(), lsf.str()))


