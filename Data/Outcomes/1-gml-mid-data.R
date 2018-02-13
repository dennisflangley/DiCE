#
# Script to Generate Gibler-Miller-Little MID Data
# -----------------------------------------------------
# Steve Miller
# Date: 24 August 2017
# License: MIT
# Full citation: Gibler, Douglas M., Steven V. Miller, and Erin K. Little. 2016. "An Analysis of the Militarized Interstate Dispute (MID) Dataset, 1816–2001." International Studies Quarterly 60(4): 719-730. 

setwd("~/Dropbox/projects/mid-project/gml-mid-data")

# 1) Load packages and custom function -----

library(sqldf)
library(tidyverse)
library(stringr)
library(lubridate)

# This will help us calculate mindur/maxdur in the event of missing days...
eom <- function(date) {
  # date character string containing POSIXct date
  date.lt <- as.POSIXlt(date) # add a month, then subtract a day:
  mon <- date.lt$mon + 2 
  year <- date.lt$year
  year <- year + as.integer(mon==13) # if month was December add a year
  mon[mon==13] <- 1
  iso = ISOdate(1900+year, mon, 1, hour=0, tz=attr(date,"tz"))
  result = as.POSIXct(iso) - 86400 # subtract one day
  result + (as.POSIXlt(iso)$isdst - as.POSIXlt(result)$isdst)*3600
}

# 2) Load data -----

cowdisp <- read_csv("~/Dropbox/data/MID/MIDA_4.01.csv")
dispchange <- readxl::read_xlsx("../internal/dispute changes.xlsx") %>%
  rename(dispnum3 = DispNum)

cowpart <- read_csv("~/Dropbox/data/MID/MIDB_4.01.csv") %>%
  setNames(., tolower(names(.))) %>%
  # We're going to create participant unique identifiers...
  group_by(dispnum3, ccode) %>%
  mutate(entry = row_number()) %>%
  ungroup() %>%
  mutate(puid = str_c(1, str_pad(ccode, 3, pad="0"), entry)) %>%
  select(-entry)

partchange <- readxl::read_xlsx("../internal/participant changes.xlsx") %>%
  setNames(., tolower(names(.))) %>%
  mutate(`midb_3.10::dispnum` = as.integer(`midb_3.10::dispnum`)) %>%
  # We're going to create participant unique identifiers...
  group_by(`midb_3.10::dispnum`, `midb_3.10::ccode`) %>% 
  mutate(entry = row_number()) %>%
  ungroup() %>%
  mutate(puid = str_c(1, str_pad(`midb_3.10::ccode`, 3, pad="0"), entry),
         `midb_3.10::ccode` = ifelse(is.na(`midb_3.10::ccode`), 0, `midb_3.10::ccode`)) %>%
  select(-entry)



# 3) Clean dispute level data. -----

Disp <- left_join(cowdisp, dispchange)

names(Disp) <- tolower(names(Disp))

# Rename for convenience...
Disp %>%
  rename("c_drop" = "corr.drop?",
         "c_stday" = "corr.stday",
         "c_stmon" = "corr.stmon",
         "c_styear" = "corr.styear",
         "c_endday" = "corr.endday",
         "c_endmon" = "corr.endmon",
         "c_endyear" = "corr.endyear",
         "c_fatality" = "corr.fatality",
         "c_fatalpre" = "corr.fatalpre",
         "c_hiact" = "corr.hiact",
         "c_hostlev" = "corr.hostlev",
         "c_numa" = "corr.numa",
         "c_numb" = "corr.numb",
         "c_outcome" = "corr.outcome",
         "c_recip" = "corr.recip",
         "c_settle" = "corr.settle") -> Disp

# Fudge how Filemaker processed hiact and fatality changes.
str_sub(Disp$c_hiact, -1, -1) <- ""
Disp %>%
  mutate(c_fatality = str_sub(c_fatality, 1, 1),
         c_hiact = as.numeric(c_hiact)) -> Disp


# The bulk of the dispute changes happen here.
Disp %>%
  mutate(stday = ifelse(!is.na(c_stday), c_stday, stday),
         stmon = ifelse(!is.na(c_stmon), c_stmon, stmon),
         styear = ifelse(!is.na(c_styear), c_styear, styear),
         endday = ifelse(!is.na(c_endday), c_endday, endday),
         endmon = ifelse(!is.na(c_endmon), c_endmon, endmon),
         endyear = ifelse(!is.na(c_endyear), c_endyear, endyear),
         outcome = ifelse(!is.na(c_outcome), c_outcome, outcome),
         settle = ifelse(!is.na(c_settle), c_settle, settle),
         fatality = ifelse(!is.na(c_fatality), c_fatality, fatality),
         fatalpre = ifelse(!is.na(c_fatalpre), c_fatalpre, fatalpre),
         hiact = ifelse(!is.na(c_hiact), c_hiact, hiact),
         hostlev = ifelse(!is.na(c_hostlev), c_hostlev, hostlev),
         recip = ifelse(!is.na(c_recip), c_recip, recip),
         numa = ifelse(!is.na(c_numa), c_numa, numa),
         numb = ifelse(!is.na(c_stmon), c_stmon, numb)
         ) -> Disp

# Create mindur/maxdur while accounting for missing data.
Disp %<>%
  mutate(stday = ifelse(stday == -9, NA, stday),
         endday = ifelse(endday == -9, NA, endday),
         minstday = ifelse(is.na(stday), 1, NA),
         minendday = ifelse(is.na(endday), 1, NA),
         stdate = make_datetime(styear, stmon, stday),
         enddate = make_datetime(endyear, endmon, endday),
         stdate2 = make_datetime(styear, stmon, minstday),
         enddate2 = make_datetime(endyear, endmon, minendday),
         # Basic mindur, which will equal maxdur with complete dates.
         mindur = as.numeric(difftime(enddate, stdate, units="days")),
         # Mindur iff just the stday is missing.
         mindur = ifelse(is.na(mindur),
                         as.numeric(difftime(enddate, eom(stdate2), units="days")),
                         mindur),
         # Mindur iff just the endday is missing.
         mindur = ifelse(is.na(mindur),
                         as.numeric(difftime(enddate2, stdate, units="days")),
                         mindur),
         # Mindur iff *both* endday and stday are missing
         mindur = ifelse(is.na(mindur),
                         as.numeric(difftime(enddate2, eom(stdate2), units="days")),
                         mindur),
         # Basic maxdur, which will equal mindur with complete dates.
         maxdur = as.numeric(difftime(enddate, stdate, units="days")),
         # Maxdur iff just the stday is missing.
         maxdur = ifelse(is.na(maxdur),
                         as.numeric(difftime(enddate, stdate2, units="days")),
                         maxdur),
         # Maxdur iff just the endday is missing.
         maxdur = ifelse(is.na(maxdur),
                         as.numeric(difftime(eom(enddate2), stdate, units="days")),
                         maxdur),
         # Maxdur iff *both* endday and stday are missing
         maxdur = ifelse(is.na(maxdur),
                         as.numeric(difftime(eom(enddate2), stdate2, units="days")),
                         maxdur)) %>%
  select(-minstday, -minendday, -stdate, -enddate, -stdate2, -enddate2)

# 4) Clean participant data. -----


Part <- full_join(partchange, cowpart, 
                  by=c(`midb_3.10::dispnum` = "dispnum3",
                       `midb_3.10::ccode` = "ccode",
                       "puid" = "puid"))


Part %>%
  rename("dispnum" = "midb_3.10::dispnum",
         "ccode" = "midb_3.10::ccode",
         "c_stabb" = "midb_3.10::corr.stabb",
         "c_ccode" = "midb_3.10::corr.ccode",
         "c_stmon" = "midb_3.10::corr.stmon",
         "c_styear" = "midb_3.10::corr.styear",
         "c_stday" = "midb_3.10::corr.stday",
         "c_sidea" = "midb_3.10::corr.sidea",
         "c_orig" = "midb_3.10::corr.orig",
         "c_revstate" = "midb_3.10::corr.revstate",
         "c_endday" = "midb_3.10::corr.endday",
         "c_endmon" = "midb_3.10::corr.endmon",
         "c_endyear" = "midb_3.10::corr.endyear",
         "c_fatality" = "midb_3.10::corr.fatality",
         "c_fatalpre" = "midb_3.10::corr.fatalpre",
         "c_hiact" = "midb_3.10::corr.hiact",
         "c_hostlev" = "midb_3.10::corr.hostlev",
         "c_revtype1" = "midb_3.10::corr.revtype1",
         "c_revtype2" = "midb_3.10::corr.revtype2") -> Part



# Think I need to start by reading in the drop/mergecase variables from Disp
Disp %>%
  select(dispnum3, c_drop, mergecase) %>%
  rename(dispnum = dispnum3) -> dropmerge

Part %>%
  left_join(., dropmerge) %>%
  select(dispnum, c_drop, mergecase, everything()) -> Part

# Next: fudge with how Filemaker handled fatality/hiact/hostlev.
Part %>%
  mutate(c_hostlev = as.numeric(str_sub(c_hiact, -2, -2)),
         c_hiact = as.numeric(str_sub(c_hiact, 1, 2)),
         c_fatality = as.numeric(str_sub(c_fatality, 1, 2)),
         c_fatalpre = ifelse(c_fatality == 0, 0, c_fatalpre),
         c_fatalpre = ifelse(c_fatality == -9, -9, c_fatalpre)) -> Part

# The bulk of the participant changes will happen here.
Part %>%
#  rename(fatalpre = fatapre) %>%
  mutate(# Added participants
         ccode = ifelse(ccode == 0, c_ccode, ccode), 
         # Misidentified participants and ones we'll remove soon.
         ccode = ifelse(!is.na(c_ccode), c_ccode, ccode), 
         stday = ifelse(!is.na(c_stday), c_stday, stday),
         stmon = ifelse(!is.na(c_stmon), c_stmon, stmon),
         styear = ifelse(!is.na(c_styear), c_styear, styear),
         endday = ifelse(!is.na(c_endday), c_endday, endday),
         endmon = ifelse(!is.na(c_endmon), c_endmon, endmon),
         endyear = ifelse(!is.na(c_endyear), c_endyear, endyear),
         sidea = ifelse(!is.na(c_sidea), c_sidea, sidea),
         revstate = ifelse(!is.na(c_revstate), c_revstate, revstate),
         revtype1 = ifelse(!is.na(c_revtype1), c_revtype1, revtype1),
         revtype2 = ifelse(!is.na(c_revtype2), c_revtype2, revtype2),
         fatality = ifelse(!is.na(c_fatality), c_fatality, fatality),
         fatalpre = ifelse(!is.na(c_fatalpre), c_fatalpre, fatalpre),
         hiact = ifelse(!is.na(c_hiact), c_hiact, hiact),
         hostlev = ifelse(!is.na(c_hostlev), c_hostlev, hostlev),
         orig = ifelse(!is.na(c_orig), c_orig, orig)) -> Part


# CoW identified five of our proposed participant changes from version 4.0 to 4.01:
# This is YPR in 2783, PRK in 2963 and 2974, Taiwan in 3211, and Niger in 4312.
# This creates duplicate rows for these entries, one of which is almost entirely missing beyond the ccode
# In other words, there were some changes here we should account.



# 5) Let's get rid of the drops/merge cases. ----
# Dispute-level: simple identifiers
# If c_drop is "Yes": it's a simple drop.
# If c_drop is "Merge": we're folding it into another MID, so remove.
# If c_drop is "Could Not Find": remove for an inability to replicate.

Disp %>%
  filter(is.na(c_drop) | c_drop == "No") %>%
  arrange(-mergecase) %>%
  select(dispnum3, dispnum4, c_drop, mergecase, stday:version)  %>%
  write_csv(., "../internal/disp-diagnostic-check.csv")

Disp %>%
  filter(is.na(c_drop) | c_drop == "No") %>%
  select(dispnum3, dispnum4, stday:version) -> Disp

# Participant-level: some added wrinkles
# if ccode == 0, we removed the participant but kept the MID.
Part %>%
  filter(is.na(c_drop) | c_drop == "No") %>%
  filter(ccode != 0) %>%
  arrange(-mergecase) %>%
  select(dispnum, dispnum4, ccode, stabb, c_drop, mergecase, stday:version) %>%
  write_csv(., "../internal/part-diagnostic-check.csv")

Part %>%
  filter(is.na(c_drop) | c_drop == "No") %>%
  filter(ccode != 0) %>%
  select(dispnum, dispnum4, ccode, stabb, stday:version) -> Part

# 6) Write dispute/participant data. -----

Disp %>%
  mutate(version = "gml-2.0") %>%
  write_csv(., "gml-mida-2.0.csv")

Part %>%
  mutate(version = "gml-2.0") %>%
  write_csv(., "gml-midb-2.0.csv")

# 7) Let's create non-directed dispute dyad-years first. -----

# This will convert participant-level data into country-year observations.
# Similar to: https://gist.github.com/svmiller/68c95dbde4d3d2af1aa86f3b8c437ae6
Part %>%
  rowwise() %>%
  mutate(year = list(seq(styear, endyear))) %>%
  ungroup() %>%
  unnest() %>%
  arrange(dispnum, year, ccode) %>%
  select(dispnum, year, ccode, everything()) -> Part

# I only know of an SQL solution to this.
# Basically: we're going to to treat `Part` as two data frames and select everything from them.
# Then: we drop where two ccodes equal each other and where B.ccode > A.ccode
NDY <- sqldf("select A.dispnum dispnum, A.ccode ccode1, B.ccode ccode2, 
      A.year year1, B.year year2, A.sidea sidea1, B.sidea sidea2, 
             A.revstate revstate1, B.revstate revstate2, 
             A.revtype1 revtype11, B.revtype1 revtype12, 
             A.revtype2 revtype21, B.revtype2 revtype22, 
             A.fatality fatality1, B.fatality fatality2, 
             A.fatalpre fatalpre1, B.fatalpre fatalpre2, 
             A.hiact hiact1, B.hiact hiact2, A.hostlev hostlev1, B.hostlev hostlev2, 
             A.orig orig1, B.orig orig2
             from Part A join Part B using (dispnum) 
             where A.ccode != B.ccode AND A.ccode < B.ccode AND 
             sidea1 != sidea2 AND year1 == year2
             order by A.ccode, B.ccode") %>% tbl_df() %>%
  # Some nip-and-tuck...
  rename(year = year1) %>%
  select(-year2) %>%
  arrange(dispnum)

# Note: There are three duplicate dyad-years in this frame, all in WWII.
# Basically: Bulgaria (355), Romania (360), and Finland (375) switched sides in 1944.
# Bulgaria and Romania left 0 (Axis) on (d/m/y) 25/8/1944 and 23/8/1944.
# Both reappeared on 1 (Allies) on 5/9/1944 and 9/9/1944.
# In essence: there should be no Bulgaria-Romania dyadic entry in WWII.
# Finland left 0 (Axis) on 19/9/1944 and re-entered on 1 (Allies) on 20/9/1944
# i.e.: there was no point in the war where BUL, RUM, or FIN were on opposing sides of WWII.

NDY %<>%
  filter(!(dispnum == 258 & ccode1 == 355 & ccode2 == 360 & year == 1944)) %>%
  filter(!(dispnum == 258 & ccode1 == 355 & ccode2 == 375 & year == 1944)) %>%
  filter(!(dispnum == 258 & ccode1 == 360 & ccode2 == 375 & year == 1944))

# Let's make some MID onsets now.

NDY$midongoing <- 1

NDY$midonset <- with(NDY, ave(year, dispnum, FUN = function(x)
  as.integer(c(TRUE, tail(x, -1L) != head(x, -1L) + 1L))))

# Need to merge in dispute-level information now.
Disp %>%
  select(dispnum3, hiact, hostlev, mindur, maxdur, outcome, settle, fatality, fatalpre, stmon, endmon, version) %>%
  rename(dispnum = dispnum3) -> Disps

NDY <- left_join(NDY, Disps) %>%
  mutate(version = "gml-2.0")

write_csv(NDY, "gml-ndy-disputes-2.0.csv")

# 8) Now create directed dispute dyad-years. -----

DDY <- sqldf("select A.dispnum dispnum, A.ccode ccode1, B.ccode ccode2, 
      A.year year1, B.year year2, A.sidea sidea1, B.sidea sidea2, 
             A.revstate revstate1, B.revstate revstate2, 
             A.revtype1 revtype11, B.revtype1 revtype12, 
             A.revtype2 revtype21, B.revtype2 revtype22, 
             A.fatality fatality1, B.fatality fatality2, 
             A.fatalpre fatalpre1, B.fatalpre fatalpre2, 
             A.hiact hiact1, B.hiact hiact2, A.hostlev hostlev1, B.hostlev hostlev2, 
             A.orig orig1, B.orig orig2
             from Part A join Part B using (dispnum) 
             where A.ccode != B.ccode AND 
             sidea1 != sidea2 AND year1 == year2
             order by A.ccode, B.ccode") %>% tbl_df() %>%
  # Some nip-and-tuck...
  rename(year = year1) %>%
  select(-year2) %>%
  arrange(dispnum)

# Correct for those peculiarities of Bulgaria, Romania, and Finland in WWII in 1944.

DDY %<>%
  filter(!(dispnum == 258 & ccode1 == 355 & ccode2 == 360 & year == 1944)) %>%
  filter(!(dispnum == 258 & ccode1 == 355 & ccode2 == 375 & year == 1944)) %>%
  filter(!(dispnum == 258 & ccode1 == 360 & ccode2 == 375 & year == 1944)) %>%
  filter(!(dispnum == 258 & ccode1 == 360 & ccode2 == 355 & year == 1944)) %>%
  filter(!(dispnum == 258 & ccode1 == 375 & ccode2 == 355 & year == 1944)) %>%
  filter(!(dispnum == 258 & ccode1 == 375 & ccode2 == 360 & year == 1944))

# Let's make some MID onsets now.

DDY$midongoing <- 1

DDY$midonset <- with(DDY, ave(year, dispnum, FUN = function(x)
  as.integer(c(TRUE, tail(x, -1L) != head(x, -1L) + 1L))))

DDY <- left_join(DDY, Disps) %>%
  mutate(version = "gml-2.0")

write_csv(DDY, "gml-ddy-disputes-2.0.csv")
