########################################
#### Create Directed ISW Data #########
#######################################

#setwd("C:/Users/Phil/Dropbox/")
setwd("F:/Dropbox/FSU/FSU_Fall_2017/Paper_1/")

library(foreach)
library(dplyr)
library(plyr)
library(doParallel)

# load dataset
load("Cleaned_Data/isw_merged1.Rdata")

uniq<-sort(unique(isw_merged1$WarNum))


# Give the function the dataset
# The goal of the function is to take a dataset with a warnumber, then create directed dyads from it
# I then iterate over all warnums in the dataset 

# The other goal is to set it up so that I can feed it the selection of the dyads

directeddyads_allies<-function(war, data){
        
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
        
        ### 3) Aggregate Opponent
        # Side A
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
        
        allies.a<-dat%>%
                filter(WarNum==war & Side==1) %>%
                dplyr::select(irst, milex, milper, pec, tpop, upop, lcinc, polity2)  
        
        # Side B
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
        
        allies.b<-dat%>%
                filter(WarNum==war & Side==2) %>%
                dplyr::select(irst, milex, milper, pec, tpop, upop, lcinc, polity2)     
        
        # 
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


# Dropping a bunch of wars because these only have one participant
uniq<-sort(unique(isw_merged1$WarNum))

# Using the function

isw.allies<-foreach(i=1:length(uniq), .combine=rbind.data.frame) %do% {
        out<-directeddyads_allies(uniq[i], isw_merged1)
        print(i)
        out
}

# Save
save(isw.dir.strongest, file="Dyadic_Data/Dir.Strongest.RData")
save(isw.dir.average, file="Dyadic_Data/Dir.Average.RData")
save(isw.dir.aggregate, file="Dyadic_Data/Dir.Aggregate.RData")

# Merge with contig
source("cleanCCode.R")
source("makeCYear.R")
source("cleanCYear.R")
source("tidyCYear.R")

## Contiguity
load("Cleaned_Data/contig.Rdata")

# Merge
# aggregate
dyad_ccode<-paste(isw.allies$a_ccode, isw.allies$b_ccode, sep="")
dyad_ccode_year<-paste(dyad_ccode, isw.allies$StartYear1, sep="_")
isw.allies<-data.frame(isw.allies, dyad_ccode_year)

isw.allies<-join(isw.allies, dplyr::select(contig, conttype, dyad_ccode_year), type="left")
isw.allies<-isw.allies %>%
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
isw.allies1<-join(isw.allies,  ICOW_stack, type="left", by="dyad_ccode_year", match="first")

# recode NAs to zero -> purely because if something is NA it means that the issue isnt present
isw.allies1[is.na(isw.allies1)] <- 0

# save
save(isw.allies1, file="Final_Data/isw.allies1.RData")
