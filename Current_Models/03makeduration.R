######################################################
## 5. Create Duration Measure for ISw and MIDs ######
######################################################
#setwd("C:/Users/Phil/Dropbox/")
setwd("F:/Dropbox/FSU/FSU_Fall_2017/Paper_1/")

library(dplyr)
library(data.table)

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

cat("03:  now adding duration and participants")

attach(isw_merged1)

duration<-ISOdate(EndYear1, EndMonth1, EndDay1)-ISOdate(StartYear1, StartMonth1, StartDay1)

detach(isw_merged1)

duration<-as.numeric(duration)

# bind to existing dataset
isw_merged1<-data.table(isw_merged1, duration)
isw_merged2<-data.table(isw_merged2, duration)
isw_merged3<-data.table(isw_merged3, duration)
isw_merged4<-data.table(isw_merged4, duration)
isw_merged5<-data.table(isw_merged5, duration)
isw_merged6<-data.table(isw_merged6, duration)
isw_merged7<-data.table(isw_merged7, duration)
isw_merged8<-data.table(isw_merged8, duration)
isw_merged9<-data.table(isw_merged9, duration)
isw_merged10<-data.table(isw_merged10, duration)

wars<-isw_merged1$WarNum

# how many participants in each war
participants<-vector()
for (i in 1:length(wars)){
        participants[i]<-length(which(isw_merged1$WarNum==wars[i]))
}

# Wars with multiple partipants
multiple<-ifelse(participants>2, 1, 0)

# Warnum with side 
warnum_side<-paste(isw_merged1$WarNum, sep="_",isw_merged1$Side)

# add to datatable
isw_merged1<-data.table(isw_merged1, participants, multiple, warnum_side)
isw_merged2<-data.table(isw_merged2, participants, multiple, warnum_side)
isw_merged3<-data.table(isw_merged3, participants, multiple, warnum_side)
isw_merged4<-data.table(isw_merged4, participants, multiple, warnum_side)
isw_merged5<-data.table(isw_merged5, participants, multiple, warnum_side)
isw_merged6<-data.table(isw_merged6, participants, multiple, warnum_side)
isw_merged7<-data.table(isw_merged7, participants, multiple, warnum_side)
isw_merged8<-data.table(isw_merged8, participants, multiple, warnum_side)
isw_merged9<-data.table(isw_merged9, participants, multiple, warnum_side)
isw_merged10<-data.table(isw_merged10, participants, multiple, warnum_side)


# Save ISW as RData
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



# Now for MIDs
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


mida<-read.csv("Data/Outcomes/gml-mida-2.0.csv")
names(mida)[names(mida) == 'dispnum3'] <- 'dispnum'
mida_sub<-dplyr::select(mida, dispnum, outcome, mindur, maxdur)

mids_merged1<-join(mids_merged1, mida_sub, type="left", by="dispnum")
mids_merged2<-join(mids_merged2, mida_sub, type="left", by="dispnum")
mids_merged3<-join(mids_merged3, mida_sub, type="left", by="dispnum")
mids_merged4<-join(mids_merged4, mida_sub, type="left", by="dispnum")
mids_merged5<-join(mids_merged5, mida_sub, type="left", by="dispnum")
mids_merged6<-join(mids_merged6, mida_sub, type="left", by="dispnum")
mids_merged7<-join(mids_merged7, mida_sub, type="left", by="dispnum")
mids_merged8<-join(mids_merged8, mida_sub, type="left", by="dispnum")
mids_merged9<-join(mids_merged9, mida_sub, type="left", by="dispnum")
mids_merged10<-join(mids_merged10, mida_sub, type="left", by="dispnum")


# dispute number
disputes<-(mids_merged1$dispnum)

# how many participants in each MID
participants<- foreach(i=1:length(disputes), .combine=rbind) %do%
        length(which(mids_merged1$dispnum==disputes[i]))

# Wars with multiple partipants
multiple<-ifelse(participants>2, 1, 0)

# Warnum with side 
dispnum_side<-paste(mids_merged1$dispnum, sep="_", mids_merged1$sidea)

# bind these in
mids_merged1<-data.table(mids_merged1, participants, multiple, dispnum_side)
mids_merged2<-data.table(mids_merged2, participants, multiple, dispnum_side)
mids_merged3<-data.table(mids_merged3, participants, multiple, dispnum_side)
mids_merged4<-data.table(mids_merged4, participants, multiple, dispnum_side)
mids_merged5<-data.table(mids_merged5, participants, multiple, dispnum_side)
mids_merged6<-data.table(mids_merged6, participants, multiple, dispnum_side)
mids_merged7<-data.table(mids_merged7, participants, multiple, dispnum_side)
mids_merged8<-data.table(mids_merged8, participants, multiple, dispnum_side)
mids_merged9<-data.table(mids_merged9, participants, multiple, dispnum_side)
mids_merged10<-data.table(mids_merged10, participants, multiple, dispnum_side)

# rename for some reason
names(mids_merged1)[37] <- 'participants'
names(mids_merged1)[38] <- 'multiple'
names(mids_merged2)[37] <- 'participants'
names(mids_merged2)[38] <- 'multiple'
names(mids_merged3)[37] <- 'participants'
names(mids_merged3)[38] <- 'multiple'
names(mids_merged4)[37] <- 'participants'
names(mids_merged4)[38] <- 'multiple'
names(mids_merged5)[37] <- 'participants'
names(mids_merged5)[38] <- 'multiple'
names(mids_merged6)[37] <- 'participants'
names(mids_merged6)[38] <- 'multiple'
names(mids_merged7)[37] <- 'participants'
names(mids_merged7)[38] <- 'multiple'
names(mids_merged8)[37] <- 'participants'
names(mids_merged8)[38] <- 'multiple'
names(mids_merged9)[37] <- 'participants'
names(mids_merged9)[38] <- 'multiple'
names(mids_merged10)[37] <- 'participants'
names(mids_merged10)[38] <- 'multiple'

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

cat("03:  done")

rm(list = setdiff(ls(), lsf.str()))


