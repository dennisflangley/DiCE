##########################################
## Data Cleaning
##########################################

library(foreign)
library(tidyr)
library(plyr)
library(dplyr)
library(Hmisc)
library(readstata13)

cat("00:  fixing ccodes and making IHS transformation")

##########################################
## 1. Make Functions 
##########################################

#setwd("C:/Users/Phil/Dropbox/")
setwd("F:/Dropbox/FSU/FSU_Fall_2017/Paper_1/")

source("cleanCCode.R")
source("makeCYear.R")
source("cleanCYear.R")
source("tidyCYear.R")

####################################
## 2. Data Transformations for NMC
####################################

nmc<-read.csv("Data/Predictors/Capabilities/NMC_v4_0.csv", na.strings = "-9")

# omit unnecessary variables
nmc <- dplyr::select(nmc, -version, -stateabb)

# identify NAs and Zeroes in NMC
table<-matrix(NA, 6, 3)

rownames(table)<-colnames(nmc)[3:8]
colnames(table)<-c("Pr(Miss)", "Pr(Zero)", "Theta")

n<-nrow(nmc)

# irst
irst.nmiss<-length(which(is.na(nmc$irst)))
irst.nzero<-length(which(nmc$irst==0))

table[1,1]<- irst.nmiss/n
table[1,2]<- irst.nzero/n

# milex
milex.nmiss<-length(which(is.na(nmc$milex)))
milex.nzero<-length(which(nmc$milex==0))

table[2,1]<-milex.nmiss/n
table[2,2]<-milex.nzero/n

# milper
milper.nmiss<-length(which(is.na(nmc$milper)))
milper.nzero<-length(which(nmc$milper==0))

table[3,1]<-milper.nmiss/n
table[3,2]<-milper.nzero/n

# pec
pec.nmiss<-length(which(is.na(nmc$pec)))
pec.nzero<-length(which(nmc$pec==0))

table[4,1]<-pec.nmiss/n
table[4,2]<-pec.nzero/n

# tpop
tpop.nmiss<-length(which(is.na(nmc$tpop)))
tpop.nzero<-length(which(nmc$tpop==0))

table[5,1]<-tpop.nmiss/n
table[5,2]<-tpop.nzero/n

# upop
upop.nmiss<-length(which(is.na(nmc$upop)))
upop.nzero<-length(which(nmc$upop==0))

table[6,1]<-upop.nmiss/n
table[6,2]<-upop.nzero/n


# IHS Transformation
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

# Run over the six variables in NMC
t.nmc<-nmc[,3:8]

thetas<-rep(NA, ncol(t.nmc))


set.seed(1999)
for (i in 1:length(thetas)){
        thetas[i]<-optimize(ks.test.stat, lower=0, upper=2^10, x=t.nmc[,i], maximum=F)$minimum
}

# store theta values in original table
table[,3]<-thetas

table<-round(table,3)

transformed.nmc<-matrix(NA, nrow(t.nmc), ncol(t.nmc))
colnames(transformed.nmc)<-colnames(t.nmc)

# make the transformations
for (i in 1:ncol(t.nmc)){
        transformed.nmc[,i]<-IHS(t.nmc[,i], table[i,3])
}

# log transform CINC (even though I likely will not use it)
lcinc<-log(nmc$cinc)

# final dataset
cleaned.nmc<-cbind(nmc[,1:2], transformed.nmc, lcinc)

# visualize each variable
par(mfrow=c(2,4))

vis.nmc<-cleaned.nmc[,3:9]

for(i in 1:ncol(vis.nmc)){
        d<-density(na.omit(vis.nmc[,i]))
        plot(d, main=paste(colnames(vis.nmc[i])))
}

# save
transformed.nmc<-cleaned.nmc



###################################################
## 3. Run data through the ccode cleaning functions
###################################################

# NMC
nmc<-transformed.nmc
ccode_year<-tidyCYear(nmc$ccode, nmc$year)
nmc<-data.frame(nmc, ccode_year)
save(nmc, file="Cleaned_Data/transformed_nmc.Rdata")


# polity IV
polity<-read.dta("Data/Predictors/Institutions/PolityIV.dta")
ccode_year<-tidyCYear(polity$ccode, polity$year)
polity<-cbind(polity, ccode_year)
save(polity, file="Cleaned_Data/PolityIV.Rdata")


pol<-dplyr::select(polity, ccode_year, ccode, year, polity, polity2)

# using Robs code


## 300 (AUH).  so, COW has AUS from 1919-present and HUN from 1918-present, 
## with AUH from 1816-1918 (all missing)
## POL has Austria 1816-present and Hungary 1867-present.  
## Austria is -10, 1816-1847; -6 1848-1860; -4 1861-1917; and -88 in 1918
## Hungary is -4, 1867-1917; and -88, 1918.
## so, there is agreement between austria and hungary 1861-1917.
#foo <- data.frame(ccode = "300",
 #                 year = 1816:1847,
  #                polity = -10, polity2 = -10)
#foo <- rbind.data.frame(foo,
 #                       data.frame(ccode = "300",
  #                                 year = 1848:1860,
   #                                polity = -6, polity2 = -6))
#foo <- rbind.data.frame(foo,
 #                       data.frame(ccode = "300",
  #                                 year = 1861:1917,
   #                                polity = -4, polity2 = -4))
#foo <- rbind.data.frame(foo,
 #                       data.frame(ccode = "300",
  #                                 year = 1918,
   #                                polity = -88, polity2 = -4))
#foo$ccode_year <- paste(foo$ccode, foo$year, sep = "_")
#pol <- rbind.data.frame(pol, foo)
#rm(foo)

## 345 and 347 (YUG).  always a joy.  bad 1878-1920 and 2007-2011.
## polity has 347 going from 1991-2002 and 345 going from 1921-1990
## but from 1868 onward, there was Kingdom of Croatia-Slavonia not in polity
## however, serbia (kingdom of, principality of) goes back in data and seems
## to be the only long-term autonomous state in region at the time
#foo <- filter(bar, country == "Serbia" & year %in% 1878:1920) %>%
 #       select(-country) %>%
  #      mutate(ccode = "345",
   #            cyear = paste(ccode, year, sep = "_"))
#pol <- rbind.data.frame(pol, foo)
#rm(foo)

## 365 (RUS) from 1923-1939.  polity gives USSR ccode 364
#foo <- filter(bar, ccode == "364" & year %in% 1923:1939) %>%
 #       select(-country) %>%
  #      mutate(ccode = "365",
   #            cyear = paste(ccode, year, sep = "_"))
#pol <- rbind.data.frame(pol, foo)
#rm(foo)

## 367 (LAT) -- just enters data at a different year
foo <- data.frame(ccode_year = c("367_1918", "367_1919"),
                  ccode = "367",
                  year = 1918:1919,
                  polity = 7, polity2 = 7)
pol <- rbind.data.frame(pol, foo)
rm(foo)

## 452 (GHA) 1957-1959 -- just enters at different time
foo <- data.frame(ccode_year = c("452_1957", "452_1958", "452_1959"),
                  ccode = "452",
                  year = 1957:1959,
                  polity = -8, polity2 = -8)
pol <- rbind.data.frame(pol, foo)
rm(foo)

## 552 (ZIM), 1965-1969 -- just enters at different time
foo <- data.frame(ccode = "552",
                  year = 1965:1969,
                  polity = 4, polity2 = 4)
foo <- foo %>%
        mutate(ccode_year = paste(ccode, year, sep = "_")) %>%
        dplyr::select(ccode_year, ccode, year, polity, polity2)
pol <- rbind.data.frame(pol, foo)
rm(foo)

## 651 (EGY) 1855-1882.  Egypt isn't made an autonomous khedivate until 
## 1867, though Ottoman rule was still only nominal.  British ocupation
## begins in 1882.  Ottomans influenced government, but Egypt could be 
## attacked separately.  call it a -10, a la Ottomans
#foo <- filter(bar, country == "Turkey" & year %in% 1855:1882) %>%
 #       mutate(ccode = "651",
  #             ccode_year = paste(ccode, year, sep = "_")) %>%
   #     select(-country)
#pol <- rbind.data.frame(pol, foo)
#rm(foo)

## 690 (KUW) 1961-2.  enters at different year
foo <- data.frame(ccode_year = c("690_1961", "690_1962"),
                  ccode = "690",
                  year = 1961:1962,
                  polity = -8, polity2 = -8)
pol <- rbind.data.frame(pol, foo)
rm(foo)

## 712 (MON) 1921-1923.  enters at different year
foo <- data.frame(ccode_year = c("712_1921", "712_1922", "712_1923"),
                  ccode = "712",
                  year = 1921:1923,
                  polity = -7, polity2 = -7)
pol <- rbind.data.frame(pol, foo)
rm(foo)

## 750 (IND), 1947-1949.  enters at different year
foo <- data.frame(ccode_year = c("750_1947", "750_1948", "750_1949"),
                  ccode = "750",
                  year = 1947:1949,
                  polity = 9, polity2 = 9)
pol <- rbind.data.frame(pol, foo)
rm(foo)

## 812 (LAO) 1953
pol <- rbind.data.frame(pol,
                        data.frame(ccode_year = "812_1953",
                                   ccode = "812",
                                   year = 1953,
                                   polity = -88, polity2 = -2))

## 817 (RVN) 1954
pol <- rbind.data.frame(pol,
                        data.frame(ccode_year = "817_1954",
                                   ccode = "817",
                                   year = 1954,
                                   polity = -3, polity2 = -3))

## 947 (TUV) 2000-2011.  Tuvalu is never in the polity data.  drop it
#dat <- filter(dat, ccode != "947")

### merge with NMC
country.data<-join(nmc, dplyr::select(pol, ccode_year, polity, polity2), type="left")


save(country.data, file="Cleaned_Data/country.data.Rdata")


rm(list = setdiff(ls(), lsf.str()))

cat("00:  done")


