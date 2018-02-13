# Gibler-Miller-Little (GML) MID Data

[![GitHub (pre-)release](https://img.shields.io/github/release/svmiller/gml-mid-data/all.svg)]()

This Github repository contains files for the [Gibler](http://dmgibler.people.ua.edu/)-[Miller](http://svmiller.com)-[Little](https://www.erinklittle.com/) MID data. This finished project follows generous support from two National Science Foundation Grants (SES#0923406 and SES#1260492) and the ultimate publications of our findings in *International Studies Quarterly*. Users should include the following citation in the bibliographies of their analyses of these data:

> Gibler, Douglas M., Steven V. Miller, and Erin K. Little. 2016. "[An Analysis of the Militarized
Interstate Dispute (MID) Dataset, 1816–2001](https://academic.oup.com/isq/article-abstract/60/4/719/2918882/An-Analysis-of-the-Militarized-Interstate-Dispute?redirectedFrom=fulltext)." *International Studies Quarterly* 60(4): 719-730.

Contents of this Github repository include:

1. `1-gml-mid-data.R`: an R script that forks our data from version 4.01 of the Correlates of War's (CoW) Militarized Interstate Dispute (MID) data. The script leans on data transformation by `lubridate`, `sqldf`, `stringr` and `tidyverse` to transform CoW's dispute and participant data with our corrections into the GML MID data at the dispute-level and participant-level.
2. `gml-mida-2.0.csv`: version 2.0 of the dispute-level GML MID data, forked from CoW's version 4.01.
3. `gml-midb-2.0.csv`: version 2.0 of the participant-level GML MID data, forked from CoW's version 4.01.
4. `gml-ddy-disputes-2.0.csv`: version 2.0 of the directed dispute-year GML MID data.
4. `gml-ndy-disputes-2.0.csv`: version 2.0 of the nondirected dispute-year GML data.

Please contact me (svmille@clemson.edu) with any inquiries about the script and the data it produces. Please contact Doug Gibler (dmgibler@ua.edu) with any inquiries about the overall re-evaluation and extension of the MID project. He maintains [an active section on his website](http://dmgibler.people.ua.edu/mid-data.html) that contains additional documentation.