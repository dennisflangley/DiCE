####
##
##  name:  doWork.R
##  date:  2017 - 11 - 27
##  what:  this is the make file
##
####

setwd("F:/Dropbox/FSU/FSU_Fall_2017/Paper_1/Current_Models")
source("00ccodesIHS.R")

setwd("F:/Dropbox/FSU/FSU_Fall_2017/Paper_1/Current_Models")
source("01imputation.R")

setwd("F:/Dropbox/FSU/FSU_Fall_2017/Paper_1/Current_Models")
source("02merging.R")

setwd("F:/Dropbox/FSU/FSU_Fall_2017/Paper_1/Current_Models")
source("03makeduration.R")   

setwd("F:/Dropbox/FSU/FSU_Fall_2017/Paper_1/Current_Models")
source("04makeISWddyads.R")   

setwd("F:/Dropbox/FSU/FSU_Fall_2017/Paper_1/Current_Models")
source("04makeMIDddyads.R")

setwd("F:/Dropbox/FSU/FSU_Fall_2017/Paper_1/Current_Models")
source("05_combineMIDsISW.R")