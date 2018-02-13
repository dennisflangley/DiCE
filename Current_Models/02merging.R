### Merge imputed datasets with ISW and MID Data

#setwd("C:/Users/Phil/Dropbox/")
setwd("F:/Dropbox/FSU/FSU_Fall_2017/Paper_1/")

cat("02:  now merging with imputations")

library(dplyr)
library(plyr)
library(foreach)
library(data.table)

source("cleanCCode.R")
source("makeCYear.R")
source("cleanCYear.R")
source("tidyCYear.R")

# Load in ISW and MIDs data
interstatewar<-read.csv("Data/Outcomes/Inter-StateWarData_v4.0.csv")
ccode_year<-tidyCYear(interstatewar$ccode, interstatewar$StartYear1)
interstatewar<-data.table(interstatewar, ccode_year)
save(interstatewar, file="Cleaned_Data/interstatewar.RData")

# MIDs
mids<-read.csv("Data/Outcomes/gml-midb-2.0.csv")

# drop if too recent (dont have NMC data on these)
mids<-subset(mids, styear<2008)

uniq.mids<-unique(mids$dispnum)

# only keep mids if at least one person was killed in the MID
# to this end, keep the mid if at least one country involved has greater than 0
fatal<-seq(1,6,1)

# here's the loop that does that
hold<-foreach(i=1:length(uniq.mids), .combine=rbind) %do% {
        temp<-filter(mids, dispnum==uniq.mids[i])
        na.temp<-matrix(NA, 1, 20)
        colnames(na.temp)<-colnames(temp)
        if (length(grep("TRUE", fatal %in% temp$fatality))==0) {out<-na.temp} else {out<-temp}
        out
}

# omit NAs
fatmids<-na.omit(hold)
fatmids<-data.table(fatmids)

uniq.fatmids<-unique(fatmids$dispnum)

# Drop if a fatality in the war equals 6, as this dispute should be captured in the ISW data
fatal<-6
hold.fat<-foreach(i=1:length(uniq.fatmids), .combine=rbind) %do% {
        temp<-filter(fatmids, dispnum==uniq.fatmids[i])
        na.temp<-matrix(NA, 1, 20)
        colnames(na.temp)<-colnames(temp)
        if (length(grep("TRUE", fatal %in% temp$fatality))==1) {out<-na.temp} else {out<-temp}
        out
}

fatmids<-na.omit(hold.fat)
fatmids<-data.table(fatmids)


# run through cleaning
ccode_year<-tidyCYear(fatmids$ccode, fatmids$styear)
fatmids<-data.table(fatmids, ccode_year)
save(fatmids, file="Data/Outcomes/fatmids.Rdata")


# Load imputed NMC+polity data
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


# For each imputation:

# merge with ISW and MIDS
# Merge these with NMC and Polity
interstatewar<-dplyr::select(interstatewar, -ccode)
fatmids<-dplyr::select(fatmids, -ccode)

# MIDs
mids_merged1<-join(fatmids, impute.clean1, type="left", by="ccode_year")


# ISW
isw_merged1<-join(interstatewar, impute.clean1, type="left", by="ccode_year")
isw_merged2<-join(interstatewar, impute.clean2, type="left", by="ccode_year")
isw_merged3<-join(interstatewar, impute.clean3, type="left", by="ccode_year")
isw_merged4<-join(interstatewar, impute.clean4, type="left", by="ccode_year")
isw_merged5<-join(interstatewar, impute.clean5, type="left", by="ccode_year")
isw_merged6<-join(interstatewar, impute.clean6, type="left", by="ccode_year")
isw_merged7<-join(interstatewar, impute.clean7, type="left", by="ccode_year")
isw_merged8<-join(interstatewar, impute.clean8, type="left", by="ccode_year")
isw_merged9<-join(interstatewar, impute.clean9, type="left", by="ccode_year")
isw_merged10<-join(interstatewar, impute.clean10, type="left", by="ccode_year")

# clean the ccodes
isw_merged1$ccode<-substr(isw_merged1$ccode_year, 1, 3)
isw_merged2$ccode<-substr(isw_merged1$ccode_year, 1, 3)
isw_merged3$ccode<-substr(isw_merged1$ccode_year, 1, 3)
isw_merged4$ccode<-substr(isw_merged1$ccode_year, 1, 3)
isw_merged5$ccode<-substr(isw_merged1$ccode_year, 1, 3)
isw_merged6$ccode<-substr(isw_merged1$ccode_year, 1, 3)
isw_merged7$ccode<-substr(isw_merged1$ccode_year, 1, 3)
isw_merged8$ccode<-substr(isw_merged1$ccode_year, 1, 3)
isw_merged9$ccode<-substr(isw_merged1$ccode_year, 1, 3)
isw_merged10$ccode<-substr(isw_merged1$ccode_year, 1, 3)


# change statename to a character, ugh
isw_merged1$StateName<-as.character(isw_merged1$StateName)
isw_merged2$StateName<-as.character(isw_merged2$StateName)
isw_merged3$StateName<-as.character(isw_merged3$StateName)
isw_merged4$StateName<-as.character(isw_merged4$StateName)
isw_merged5$StateName<-as.character(isw_merged5$StateName)
isw_merged6$StateName<-as.character(isw_merged6$StateName)
isw_merged7$StateName<-as.character(isw_merged7$StateName)
isw_merged8$StateName<-as.character(isw_merged8$StateName)
isw_merged9$StateName<-as.character(isw_merged9$StateName)
isw_merged10$StateName<-as.character(isw_merged10$StateName)

load("Data/Outcomes/fatmids.Rdata")

fatmids<-dplyr::select(fatmids, -ccode)

# MIDs
mids_merged1<-join(fatmids, impute.clean1, type="left", by="ccode_year")
mids_merged2<-join(fatmids, impute.clean2, type="left", by="ccode_year")
mids_merged3<-join(fatmids, impute.clean3, type="left", by="ccode_year")
mids_merged4<-join(fatmids, impute.clean4, type="left", by="ccode_year")
mids_merged5<-join(fatmids, impute.clean5, type="left", by="ccode_year")
mids_merged6<-join(fatmids, impute.clean6, type="left", by="ccode_year")
mids_merged7<-join(fatmids, impute.clean7, type="left", by="ccode_year")
mids_merged8<-join(fatmids, impute.clean8, type="left", by="ccode_year")
mids_merged9<-join(fatmids, impute.clean9, type="left", by="ccode_year")
mids_merged10<-join(fatmids, impute.clean10, type="left", by="ccode_year")

# clean the ccodes
mids_merged1$ccode<-substr(mids_merged1$ccode_year, 1, 3)
mids_merged2$ccode<-substr(mids_merged1$ccode_year, 1, 3)
mids_merged3$ccode<-substr(mids_merged1$ccode_year, 1, 3)
mids_merged4$ccode<-substr(mids_merged1$ccode_year, 1, 3)
mids_merged5$ccode<-substr(mids_merged1$ccode_year, 1, 3)
mids_merged6$ccode<-substr(mids_merged1$ccode_year, 1, 3)
mids_merged7$ccode<-substr(mids_merged1$ccode_year, 1, 3)
mids_merged8$ccode<-substr(mids_merged1$ccode_year, 1, 3)
mids_merged9$ccode<-substr(mids_merged1$ccode_year, 1, 3)
mids_merged10$ccode<-substr(mids_merged1$ccode_year, 1, 3)

# change stabb to character
mids_merged1$stabb<-as.character(mids_merged1$stabb)
mids_merged2$stabb<-as.character(mids_merged2$stabb)
mids_merged3$stabb<-as.character(mids_merged3$stabb)
mids_merged4$stabb<-as.character(mids_merged4$stabb)
mids_merged5$stabb<-as.character(mids_merged5$stabb)
mids_merged6$stabb<-as.character(mids_merged6$stabb)
mids_merged7$stabb<-as.character(mids_merged7$stabb)
mids_merged8$stabb<-as.character(mids_merged8$stabb)
mids_merged9$stabb<-as.character(mids_merged9$stabb)
mids_merged10$stabb<-as.character(mids_merged10$stabb)


### Save ISW
save(isw_merged1, file="Cleaned_Data/isw_merged1.Rdata")
save(isw_merged2, file="Cleaned_Data/isw_merged2.Rdata")
save(isw_merged3, file="Cleaned_Data/isw_merged3.Rdata")
save(isw_merged4, file="Cleaned_Data/isw_merged4.Rdata")
save(isw_merged5, file="Cleaned_Data/isw_merged5.Rdata")
save(isw_merged6, file="Cleaned_Data/isw_merged6.Rdata")
save(isw_merged7, file="Cleaned_Data/isw_merged7.Rdata")
save(isw_merged8, file="Cleaned_Data/isw_merged8.Rdata")
save(isw_merged9, file="Cleaned_Data/isw_merged9.Rdata")
save(isw_merged10, file="Cleaned_Data/isw_merged10.Rdata")


### Save MIDs
save(mids_merged1, file="Cleaned_Data/mids_merged1.Rdata")
save(mids_merged2, file="Cleaned_Data/mids_merged2.Rdata")
save(mids_merged3, file="Cleaned_Data/mids_merged3.Rdata")
save(mids_merged4, file="Cleaned_Data/mids_merged4.Rdata")
save(mids_merged5, file="Cleaned_Data/mids_merged5.Rdata")
save(mids_merged6, file="Cleaned_Data/mids_merged6.Rdata")
save(mids_merged7, file="Cleaned_Data/mids_merged7.Rdata")
save(mids_merged8, file="Cleaned_Data/mids_merged8.Rdata")
save(mids_merged9, file="Cleaned_Data/mids_merged9.Rdata")
save(mids_merged10, file="Cleaned_Data/mids_merged10.Rdata")

cat("02:  done")

rm(list = setdiff(ls(), lsf.str()))

