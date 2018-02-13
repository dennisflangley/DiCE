############################################
## 1 Multiple Imputation for NMC and Polity
############################################
cat("01:  now imputing NMC and polity")

#setwd("C:/Users/Phil/Dropbox/")
setwd("F:/Dropbox/FSU/FSU_Fall_2017/Paper_1/")

load("Cleaned_Data/country.data.Rdata")

#### packages ####
library(dplyr)
library(tidyr)
library(data.table)
library(foreach)
library(Amelia)
library(doParallel)

library(Amelia)

impute.out<-amelia(dplyr::select(country.data,-ccode_year), 
                   m = 10, 
                   ts = "year", 
                   cs = "ccode",
                   polytime=3,
                   intercs=T,
                   max.resample=1000,
                   empri = 0.001 * nrow(country.data))

# save imputed data
save(impute.out, file="Cleaned_Data/Imputations/imputations.Rdata")

load("Cleaned_Data/Imputations/imputations.Rdata")

impute.out1<-impute.out$imputations$imp1
impute.out2<-impute.out$imputations$imp2
impute.out3<-impute.out$imputations$imp3
impute.out4<-impute.out$imputations$imp4
impute.out5<-impute.out$imputations$imp5
impute.out6<-impute.out$imputations$imp6
impute.out7<-impute.out$imputations$imp7
impute.out8<-impute.out$imputations$imp8
impute.out9<-impute.out$imputations$imp9
impute.out10<-impute.out$imputations$imp10


# recall functions

source("cleanCCode.R")
source("makeCYear.R")
source("cleanCYear.R")
source("tidyCYear.R")

library(data.table)

ccode_year<-tidyCYear(as.numeric(impute.out1$ccode), impute.out1$year)

impute.clean1<-data.frame(impute.out1, ccode_year)
impute.clean2<-data.frame(impute.out2, ccode_year)
impute.clean3<-data.frame(impute.out3, ccode_year)
impute.clean4<-data.frame(impute.out4, ccode_year)
impute.clean5<-data.frame(impute.out5, ccode_year)
impute.clean6<-data.frame(impute.out6, ccode_year)
impute.clean7<-data.frame(impute.out7, ccode_year)
impute.clean8<-data.frame(impute.out8, ccode_year)
impute.clean9<-data.frame(impute.out9, ccode_year)
impute.clean10<-data.frame(impute.out10, ccode_year)


## Lagging Variables ##
library(DataCombine)

data.obs<-list(impute.clean1, impute.clean2, impute.clean3, impute.clean4, impute.clean5, impute.clean6, impute.clean7, impute.clean8, impute.clean9, impute.clean10)
               
#    , impute.clean2, impute.clean3, impute.clean4, impute.clean5, impute.clean6, impute.clean7, impute.clean8, impute.clean9, impute.clean10)
names(data.obs)<-c("impute.clean1")

#, "impute.clean2", "impute.clean3", "impute.clean4", "impute.clean5", "impute.clean6", "impute.clean7", "impute.clean8", "impute.clean9", "impute.clean10")

newdat<-list()

for (j in 1:length(data.obs)){
        # select the dataset
        data<-data.obs[[j]]
        
        lpolity<-dplyr::select(slide(impute.clean1, GroupVar="ccode", TimeVar="year", Var="polity", NewVar="lpolity", slideBy=-1), lpolity)
        names(lpolity)<-"lpolity"
        
        lpolity2<-dplyr::select(slide(impute.clean1, GroupVar="ccode", TimeVar="year", Var="polity2", NewVar="lpolity2", slideBy=-1), lpolity2)
        names(lpolity2)<-"lpolity2"
        
        data<-data.frame(data, lpolity, lpolity2)
        
        newdat[[j]]<-data
}

impute.clean1<-newdat[[1]]
impute.clean2<-newdat[[2]]
impute.clean3<-newdat[[3]]
impute.clean4<-newdat[[4]]
impute.clean5<-newdat[[5]]
impute.clean6<-newdat[[6]]
impute.clean7<-newdat[[7]]
impute.clean8<-newdat[[8]]
impute.clean9<-newdat[[9]]
impute.clean10<-newdat[[10]]



# Save these
save(impute.clean1, file="Cleaned_Data/Imputations/impute.clean1.Rdata")
save(impute.clean2, file="Cleaned_Data/Imputations/impute.clean2.Rdata")
save(impute.clean3, file="Cleaned_Data/Imputations/impute.clean3.Rdata")
save(impute.clean4, file="Cleaned_Data/Imputations/impute.clean4.Rdata")
save(impute.clean5, file="Cleaned_Data/Imputations/impute.clean5.Rdata")
save(impute.clean6, file="Cleaned_Data/Imputations/impute.clean6.Rdata")
save(impute.clean7, file="Cleaned_Data/Imputations/impute.clean7.Rdata")
save(impute.clean8, file="Cleaned_Data/Imputations/impute.clean8.Rdata")
save(impute.clean9, file="Cleaned_Data/Imputations/impute.clean9.Rdata")
save(impute.clean10, file="Cleaned_Data/Imputations/impute.clean10.Rdata")


cat("01:  done")

rm(list = setdiff(ls(), lsf.str()))

