# Predict COWDyads

library(caret)
library(rpart)
library(randomForest)
library(stats)
library(foreign)
library(readstata13)
library(randomForest)
library(stats)
library(plyr)
library(dplyr)
library(rattle)
library(Cubist)
library(nnet)
require(MASS)
require(dplyr)
library(gmailr)
library(foreach)
library(doParallel)
library(data.table)
library(Rsolnp)
library(tidyr)
library(caretEnsemble)
library(beepr)

#setwd("C:/Users/Phil/Dropbox/")
setwd("F:/Dropbox/")

# save this
load("Output/COW_DiCE.Rdata")


library(plyr)
library(ggplot2)

sort<-data.table(arrange(COW_DiCE, Year))

# look 
p<-ggplot(sort, aes(x=Year, y=DiCE))+
        geom_point(color="blue", alpha=0.009)+
        ylab("Expected Battle Deaths (IHS)")
p

# scale by joint polity score
Dyad<-rep(0, nrow(sort))

Dyad<-ifelse(sort$a_polity2>6 & sort$b_polity2>6, "Democratic", Dyad)
Dyad<-ifelse(sort$a_polity2< -6 & sort$b_polity2< -6, "Autocratic", Dyad)
Dyad<-ifelse(Dyad==0, "Mixed", Dyad)

sort.dem<-sort
sort.dem$dem<-ifelse(sort$a_polity2>6 & sort$b_polity2>6, 1, 0)
sort.dem$Dyad<-Dyad


# facet regime type
ggplot(sort.dem, aes(x=Year, y=DiCE))+
        geom_point(aes(color=Dyad), alpha=0.01)+
        facet_grid(Dyad~ .)+
        scale_colour_manual(values=c("red", "blue", "purple4"), guide=FALSE)


# color scale polity
sort.dem$Joint_Polity<-sort$a_polity2+sort$b_polity2

ggplot(sort.dem, aes(x=Year, y=DiCE, group=Joint_Polity))+
        geom_point(aes(color=Joint_Polity), alpha=0.01)+
        scale_color_gradient(low="red", high="blue")+
        ylab("Expected Battle Deaths (IHS)")



# breaks with loess lines
sort.pre1825<-subset(sort, Year<=1825)
sort.pre1919<-subset(sort, Year>1825& Year<=1919)
sort.pre1950<-subset(sort, Year>=1920 & Year<1950)
sort.post1950<-subset(sort, Year>=1950)

# create empty plot
df <- data.frame()
set<-ggplot(df) + geom_point() + xlim(1816, 2007) + ylim(0, 12.5)


set+ geom_point(data=sort.pre1825, aes(x=Year, y=DiCE), alpha=0.01)+
        stat_smooth(data=sort.pre1825, aes(x=Year, y=DiCE))+
        geom_vline(xintercept=1825, linetype="longdash")+
        geom_point(data=sort.pre1919, aes(x=Year, y=DiCE), alpha=0.01)+
        stat_smooth(data=sort.pre1919, aes(x=Year, y=DiCE))+
        geom_vline(xintercept=1919, linetype="longdash")+
        geom_point(data=sort.pre1950, aes(x=Year, y=DiCE), alpha=0.01)+
        stat_smooth(data=sort.pre1950, aes(x=Year, y=DiCE))+
        geom_vline(xintercept=1950, linetype="longdash")+
        geom_point(data=sort.post1950, aes(x=Year, y=DiCE), alpha=0.01)+
        stat_smooth(data=sort.post1950, aes(x=Year, y=DiCE))+
        ylab("Expected Battle Deaths (IHS)")



# grab specific countries
# us and russia
dat<-sort

us_rus<-filter(sort, a_ccode=="002" & b_ccode=="365")
rus_us<-filter(sort, a_ccode=="365" & b_ccode=="002")

sorted<-data.table(cbind(us_rus$DiCE, rus_us$DiCE), us_rus$Year)

colnames(sorted)<-c("US", "Russia", "Year")

g<-gather(sorted, 
          value="DiCE",
          key="Country",
          US, Russia)

out<-ggplot(sort, aes(x=Year, y=DiCE))+
        ylab("Expected Battle Deaths (IHS)")+
        coord_cartesian(ylim=c(0,13.5))+
        geom_point(data=g, aes(x=Year, y=DiCE, color=Country), size=0.9)+
        geom_line(data=g, aes(x=Year, y=DiCE, color=Country), size=1)

out+scale_colour_manual(values=c("red", "blue"))


# france and germany
dat<-sort

france_germ<-filter(sort, a_ccode=="220" & b_ccode=="255")
germ_france<-filter(sort, a_ccode=="255" & b_ccode=="220")

sorted<-data.table(cbind(france_germ$DiCE, germ_france$DiCE, france_germ$Year))
colnames(sorted)<-c("France", "Germany", "Year")

g<-gather(sorted, 
          value="DiCE",
          key="Country",
          France, Germany)

g1<-gather(subset(sorted, Year<1946),
          value="DiCE",
          key="Country",
          France, Germany)

g2<-gather(subset(sorted, Year>1990),
           value="DiCE",
           key="Country",
           France, Germany)

out<-ggplot(sort, aes(x=Year, y=DiCE))+
        ylab("Expected Battle Deaths (IHS)")+
        coord_cartesian(ylim=c(0,13.5))+
        geom_point(data=g, aes(x=Year, y=DiCE, color=Country), size=0.9)+
        geom_line(data=g1, aes(x=Year, y=DiCE, color=Country), size=1)+
        geom_line(data=g2, aes(x=Year, y=DiCE, color=Country), size=1)

out+scale_colour_manual(values=c("red", "black"))


# China Japan
dat<-sort

a_b<-filter(sort, a_ccode=="710" & b_ccode=="740")
b_a<-filter(sort, a_ccode=="740" & b_ccode=="710")

sorted<-data.table(cbind(a_b$DiCE, b_a$DiCE, a_b$Year))
colnames(sorted)<-c("China", "Japan", "Year")

g<-gather(sorted, 
          value="DiCE",
          key="Country",
          China, Japan)

out<-ggplot(sort, aes(x=Year, y=DiCE))+
        ylab("Expected Battle Deaths (IHS)")+
        coord_cartesian(ylim=c(0,13.5))+
        geom_point(data=g, aes(x=Year, y=DiCE, color=Country), size=0.9)+
        geom_line(data=g, aes(x=Year, y=DiCE, color=Country), size=1)

out+scale_colour_manual(values=c("black", "red"))


# Brazil Argentina
dat<-sort

a_b<-filter(sort, a_ccode=="140" & b_ccode=="160")
b_a<-filter(sort, a_ccode=="160" & b_ccode=="140")

sorted<-data.table(cbind(a_b$DiCE, b_a$DiCE, a_b$Year))
colnames(sorted)<-c("Brazil", "Argentina", "Year")

g<-gather(sorted, 
          value="DiCE",
          key="Country",
          Brazil, Argentina)

out<-ggplot(sort, aes(x=Year, y=DiCE))+
        ylab("Expected Battle Deaths (IHS)")+
        coord_cartesian(ylim=c(0,13.5))+
        geom_point(data=g, aes(x=Year, y=DiCE, color=Country), size=0.9)+
        geom_line(data=g, aes(x=Year, y=DiCE, color=Country), size=1)

out+scale_colour_manual(values=c("blue", "yellow"))


# US Mexico
dat<-sort

a_b<-filter(sort, a_ccode=="002" & b_ccode=="070")
b_a<-filter(sort, a_ccode=="070" & b_ccode=="002")

sorted<-data.table(cbind(a_b$DiCE, b_a$DiCE, a_b$Year))
colnames(sorted)<-c("US", "Mexico", "Year")

g<-gather(sorted, 
          value="DiCE",
          key="Country",
          US, Mexico)

out<-ggplot(sort, aes(x=Year, y=DiCE))+
        ylab("Expected Battle Deaths (IHS)")+
        coord_cartesian(ylim=c(0,13.5))+
        geom_point(data=g, aes(x=Year, y=DiCE, color=Country), size=0.9)+
        geom_line(data=g, aes(x=Year, y=DiCE, color=Country), size=1)

out+scale_colour_manual(values=c("green", "blue"))




# let's show some multilateral conflicts





# What are the top 5 costliest conflicts in specific years?

years<-seq(1820, 2005, by=5)

ugh<-foreach(i=1:length(years)) %do% {
        sub_year<-COW_DiCE %>%
                filter(Year==years[[i]])
        
        sub_arrange<-arrange(sub_year, desc(DiCE))
        
        out<-sub_arrange[1:3,]
        
        out$dyad_ccode_year
}


# top 5 costliest conflicts

top5<-sapply(ugh, substring, 1, 7)
colnames(top5)<-years


# make a function that can automatically replace each of these with their country names
cowcodes<-read.csv("C:/Users/phenrickson/Downloads/COW country codes.csv")

source("cleanCCode.R")

cowcodes$CCode<-cleanCCode(cowcodes$CCode)

save(cowcodes, file="cowcodes.Rdata")

findCname<-function (ccode) {
        code<-ccode
        
        out<-foreach(i=1:length(ccode)) %do% {
                name<-as.vector(cowcodes[which(cowcodes$CCode==code[i]), 3])[1]
                return(name)
        }
        
        unlist(out)
}




top5_names<-apply(top5, 2, function (x) {
        ccode<-x
        
        a<-substr(ccode, 1, 3)
        b<-substr(ccode, 5, 8)
        
        out<-foreach(i=1:length(a)) %do% {
                name_a<-as.vector(cowcodes[which(cowcodes$CCode==a[i]), 3])[1]
                name_b<-as.vector(cowcodes[which(cowcodes$CCode==b[i]), 3])[1]
                
                name<-paste(name_a, sep="-", name_b)
                return(name)
        }
        
        unlist(out)
}
)

library(Hmisc)
latex(t(top5_names), file="")


# Randomly sample, show predicted vs observed
# load in data
# load in datasets
load("FSU/FSU Fall 2016/Prospectus/Paper I - Costs of War/Final_Data/strong.RData")
load("FSU/FSU Fall 2016/Prospectus/Paper I - Costs of War/Final_Data/average.RData")
load("FSU/FSU Fall 2016/Prospectus/Paper I - Costs of War/Final_Data/aggregate.RData")


# randomly select 50 obs and compare
# load in models
load("Output/final.CS.year.RData")
load("Output/final.CAVG.year.RData")
load("Output/final.CAGG.year.RData")

# load in tuning parameters
load("Output/tune.CS.year.RData")
load("Output/tune.CAVG.year.Rdata")
load("Output/tune.CAGG.year.Rdata")


load("Output/weights.mat.RData")
weights<-weights.mat
weights[6,3]<-0.068

weights.ensemble<-weights[which(weights>0.001)]


# set tuning parameters
rf.tune.average<-tune.CAVG.year[[7]]
bst.tune.average<-tune.CAVG.year[[8]]

knn.tune.aggregate<-tune.CAGG.year[[5]]
rf.tune.aggregate<-tune.CAGG.year[[7]]
bst.tune.aggregate<-tune.CAGG.year[[8]]


# ensemble these
tune_control<-trainControl(method="cv", n=5, predictionBounds = c(0, NA), savePredictions="final")

dat<-strong.trim


set.seed(1999)
models_average<-caretList(a_BatDeath_IHS~.,
                          data=dat,
                          trControl=tune_control,
                          metric="RMSE",
                          tuneList=list(rf=caretModelSpec(method="rf", tuneGrid=rf.tune.average),
                                        bstree=caretModelSpec(method="bstTree", tuneGrid=bst.tune.average)))

dat<-aggregate.trim

set.seed(1999)
models_aggregate<-caretList(a_BatDeath_IHS~.,
                            data=dat,
                            trControl=tune_control,
                            metric="RMSE",
                            tuneList=list(knn=caretModelSpec(method="knn", tuneGrid=knn.tune.aggregate, preProcess=c("center","scale")),
                                          rf=caretModelSpec(method="rf", tuneGrid=rf.tune.aggregate),
                                          bstree=caretModelSpec(method="bstTree", tuneGrid=bst.tune.aggregate)))

# grab predictions from these models
pred.average<-foreach(i=1:length(models_average)) %do%
        predict(models_average[[i]], newdata=COWdyads)

pred.aggregate<-foreach(i=1:length(models_aggregate)) %do%
        predict(models_aggregate[[i]], newdata=COWdyads)


pred.out<-data.table(do.call(cbind, pred.average), do.call(cbind, pred.aggregate))

DiCE<-as.matrix(pred.out)%*%weights.ensemble




foo<-sample(1:nrow(aggregate), replace=T)[1:50]
aggregate.sample<-aggregate[foo,]





# What is the predicted cost of war for the US and Iraq in 2003?

us_iraq<-filter(COW_DiCE, a_ccode=="002" & b_ccode=="645" & Year==2003)


# multilateral conflict
iraq_us<-filter(COW_DiCE, a_ccode=="645" & b_ccode=="002" & Year==2003)
iraq_uk<-filter(COW_DiCE, a_ccode=="645" & b_ccode=="200" & Year==2003)
iraq_aus<-filter(COW_DiCE, a_ccode=="645" & b_ccode=="900" & Year==2003)


iraq<-dplyr::select(iraq_us, -b_polity2, -b_irst, -b_milex, -b_milper, -b_pec, -b_tpop, -b_upop, -b_lcinc, -conttype)
multi<-rbind(iraq_us, iraq_uk, iraq_aus)

coalition<-dplyr::select(multi, b_polity2, b_irst, b_milex, b_milper, b_pec, b_tpop, b_upop, b_lcinc, conttype)

foo<-t(as.matrix(apply(coalition, 2, sum)))
foo[,1]<-10
foo[,9]<-6

iraq03<-cbind(iraq, foo)
