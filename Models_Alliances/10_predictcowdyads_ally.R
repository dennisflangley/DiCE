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


# read in cowdyads universe
load("Output/COWdyads.Rdata")

head(COWdyads)
colnames(COWdyads)[2]<-c("Year")
COWdyads<-data.table(COWdyads)

COWdyads<-COWdyads %>%
        mutate(a_polity2=replace(a_polity2, a_polity2>10, 10)) %>%
        mutate(b_polity2=replace(b_polity2, b_polity2>10, 10)) %>%
        mutate(a_polity2=replace(a_polity2, a_polity2< -10, -10)) %>%
        mutate(b_polity2=replace(b_polity2, b_polity2< -10, -10))


# load in data
# load in datasets
load("FSU/FSU Fall 2016/Prospectus/Paper I - Costs of War/Final_Data/strong.RData")
load("FSU/FSU Fall 2016/Prospectus/Paper I - Costs of War/Final_Data/average.RData")
load("FSU/FSU Fall 2016/Prospectus/Paper I - Costs of War/Final_Data/aggregate.RData")


strong.trim<-dplyr::select(strong,a_BatDeath_IHS, Year, a_irst, a_milex, a_milper, a_pec, a_tpop, a_upop, a_lcinc, a_polity2, b_irst, b_milex, b_milper, b_pec, b_tpop, b_upop, b_lcinc, b_polity2, conttype)
average.trim<-dplyr::select(average, a_BatDeath_IHS, Year, a_irst, a_milex, a_milper, a_pec, a_tpop, a_upop, a_lcinc, a_polity2, b_irst, b_milex, b_milper, b_pec, b_tpop, b_upop, b_lcinc, b_polity2, conttype)
aggregate.trim<-dplyr::select(aggregate, a_BatDeath_IHS, Year, a_irst, a_milex, a_milper, a_pec, a_tpop, a_upop, a_lcinc, a_polity2, b_irst, b_milex, b_milper, b_pec, b_tpop, b_upop, b_lcinc, b_polity2, conttype)



# load in models
load("Output/ensemble.strong.RData")
load("Output/ensemble.average.RData")
load("Output/ensemble.aggregate.RData")

# load in tuning parameters
load("Output/par.strong.RData")
load("Output/par.average.Rdata")
load("Output/par.aggregate.Rdata")

### Model: Tuning Parameters


load("Output/weights.RData")

weights.ensemble<-weights[which(weights>0.001)]

# ensemble these
tune_control<-trainControl(method="cv", n=5, predictionBounds = c(0, NA), savePredictions="final")

dat<-average.trim

# set tuning parameters
tune<-par.average

rf.tune<-expand.grid(.mtry=tune$rf)
bst.tune<-expand.grid(.maxdepth=tune$bsttree[2], .mstop=tune$bsttree[1], .nu=tune$bsttree[3])

set.seed(1999)
rfModel<-train(a_BatDeath_IHS~., 
                       data=dat, 
                       method="rf", 
                       trControl=tune_control, 
                       tuneGrid=rf.tune)

set.seed(1999)
bstModel<-train(a_BatDeath_IHS~., 
               data=dat, 
               method="bstTree", 
               trControl=tune_control, 
               tuneGrid=bst.tune)

models_average<-list("rf"=rfModel, "bst"=bstModel)

dat<-aggregate.trim

# set tuning parameters
tune<-par.aggregate

rf.tune<-expand.grid(.mtry=8)
knn.tune<-expand.grid(.k=tune$knn)
bst.tune<-expand.grid(.maxdepth=tune$bsttree[2], .mstop=tune$bsttree[1], .nu=tune$bsttree[3])
nnet.tune<-expand.grid(.size = tune$nnet[1], .decay = tune$nnet[2], .bag=T)

set.seed(1999)
knnModel<-train(a_BatDeath_IHS~., 
               data=dat, 
               method="knn", 
               trControl=tune_control, 
               tuneGrid=knn.tune)

set.seed(1999)
rfModel<-train(a_BatDeath_IHS~., 
               data=dat, 
               method="rf", 
               trControl=tune_control, 
               tuneGrid=rf.tune)

set.seed(1999)
bstModel<-train(a_BatDeath_IHS~., 
                data=dat, 
                method="bstTree", 
                trControl=tune_control, 
                tuneGrid=bst.tune)

set.seed(1999)
nnetModel<-train(a_BatDeath_IHS~.,
                 data=dat,
                 trControl=tune_control,
                 tuneGrid=nnet.tune,
                 method="avNNet",
                 preProcess=c("center", "scale"),
                 linout=1,
                 trace=F)


models_aggregate<-list("knn"=knnModel, "rf"=rfModel, "bst"=bstModel, "nnet"=nnetModel)


# grab predictions from these models
pred.average<-foreach(i=1:length(models_average)) %do%
        predict(models_average[[i]], newdata=COWdyads)

pred.aggregate<-foreach(i=1:length(models_aggregate)) %do%
        predict(models_aggregate[[i]], newdata=COWdyads)


pred.out<-data.table(do.call(cbind, pred.average), do.call(cbind, pred.aggregate))

DiCE<-as.matrix(pred.out)%*%weights.ensemble


# bind this back onto original dataset

# Visualize
COW_DiCE<-data.table(COWdyads, DiCE)

colnames(COW_DiCE)[c(22)]<-c("DiCE")

# save this
save(COW_DiCE, file="Output/COW_DiCE.Rdata")
load("Output/COW_DiCE.Rdata")


library(plyr)
library(ggplot2)

# 
sort<-data.table(arrange(COW_DiCE, Year))

#
ggplot(sort, aes(x=Year, y=DiCE_rf))+
        geom_point(color="blue", alpha=0.01)+
        ylab("Expected Battle Deaths (IHS)")

# scale by joint polity score
Dyad<-rep(0, nrow(sort))
        
Dyad<-ifelse(sort$a_polity2>6 & sort$b_polity2>6, "Democratic", Dyad)
Dyad<-ifelse(sort$a_polity2< -6 & sort$b_polity2< -6, "Autocratic", Dyad)
Dyad<-ifelse(Dyad==0, "Mixed", Dyad)

sort.dem<-sort
sort.dem$dem<-ifelse(sort$a_polity2>6 & sort$b_polity2>6, 1, 0)
sort.dem$Dyad<-Dyad


# color scale polity
sort.dem$Joint_Polity<-sort$a_polity2+sort$b_polity2

ggplot(sort.dem, aes(x=Year, y=DiCE_rf, group=Joint_Polity))+
        geom_point(aes(color=Joint_Polity), alpha=0.01)+
        scale_color_gradient(low="red", high="blue")+
        ylab("Expected Battle Deaths (IHS)")


# facet regime type
ggplot(sort.dem, aes(x=Year, y=DiCE))+
        geom_point(aes(color=Dyad), alpha=0.01)+
        facet_grid(Dyad~ .)+
        scale_colour_manual(values=c("red", "blue", "purple4"), guide=FALSE)



# breaks with loess lines
sort.pre1826<-subset(sort, Year<=1826)
sort.pre1919<-subset(sort, Year>1826& Year<=1919)
sort.pre1950<-subset(sort, Year>=1920 & Year<1950)
sort.post1950<-subset(sort, Year>=1950)

# create empty plot
df <- data.frame()
set<-ggplot(df) + geom_point() + xlim(1816, 2007) + ylim(0, 12.5)
set

set+ geom_point(data=sort.pre1826, aes(x=Year, y=DiCE_rf), alpha=0.01)+
        stat_smooth(data=sort.pre1826, aes(x=Year, y=DiCE_rf))+
        geom_vline(xintercept=1826, linetype="longdash")+
        geom_point(data=sort.pre1919, aes(x=Year, y=DiCE_rf), alpha=0.01)+
        stat_smooth(data=sort.pre1919, aes(x=Year, y=DiCE_rf))+
        geom_vline(xintercept=1919, linetype="longdash")+
        geom_point(data=sort.pre1950, aes(x=Year, y=DiCE_rf), alpha=0.01)+
        stat_smooth(data=sort.pre1950, aes(x=Year, y=DiCE_rf))+
        geom_vline(xintercept=1950, linetype="longdash")+
        geom_point(data=sort.post1950, aes(x=Year, y=DiCE_rf), alpha=0.01)+
        stat_smooth(data=sort.post1950, aes(x=Year, y=DiCE_rf))+
        ylab("Expected Battle Deaths (IHS)")



# grab specific countries
# us and soviet union

colnames(sort)[22]<-"DiCE"

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
        geom_line(data=g, aes(x=Year, y=DiCE, color=Country), size=1.25)

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
        geom_line(data=g1, aes(x=Year, y=DiCE, color=Country), size=1.25)+
        geom_line(data=g2, aes(x=Year, y=DiCE, color=Country), size=1.25)

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
        geom_line(data=g, aes(x=Year, y=DiCE, color=Country), size=1.25)

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
        geom_line(data=g, aes(x=Year, y=DiCE, color=Country), size=1.25)

out+scale_colour_manual(values=c("blue", "yellow"))



# North Korea South Korea
dat<-sort

a_b<-filter(sort, a_ccode=="731" & b_ccode=="732")
b_a<-filter(sort, a_ccode=="732" & b_ccode=="731")

sorted<-data.table(cbind(a_b$DiCE, b_a$DiCE, a_b$Year))
colnames(sorted)<-c("North_Korea", "South_Korea", "Year")

g<-gather(sorted, 
          value="DiCE",
          key="Country",
          North_Korea, South_Korea)

out<-ggplot(sort, aes(x=Year, y=DiCE))+
        ylab("Expected Battle Deaths (IHS)")+
        coord_cartesian(ylim=c(0,13.5))+
        geom_line(data=g, aes(x=Year, y=DiCE, color=Country), size=1.25)

out+scale_colour_manual(values=c("red", "blue"))



# us china
dat<-sort

us_chi<-filter(sort, a_ccode=="002" & b_ccode=="710")
chi_us<-filter(sort, a_ccode=="710" & b_ccode=="002")

sorted<-data.table(cbind(us_chi$DiCE, chi_us$DiCE), us_chi$Year)

colnames(sorted)<-c("US", "China", "Year")

g<-gather(sorted, 
          value="DiCE",
          key="Country",
          US, China)

out<-ggplot(sort, aes(x=Year, y=DiCE))+
        ylab("Expected Battle Deaths (IHS)")+
        coord_cartesian(ylim=c(0,13.5))+
        geom_line(data=g, aes(x=Year, y=DiCE, color=Country), size=1.25)

out+scale_colour_manual(values=c("red", "blue"))






# lets create some hypothetical multilateral wars
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

# predict

# grab predictions from these models
a<-foreach(i=1:length(models_average)) %do%
        predict(models_average[[i]], newdata=iraq03)

b<-foreach(i=1:length(models_aggregate)) %do%
        predict(models_aggregate[[i]], newdata=iraq03)


pred.out<-data.table(do.call(cbind, a), do.call(cbind, b))

iraq03_dice<-as.matrix(pred.out)%*%weights.ensemble



dat<-sort

us_chi<-filter(sort, a_ccode=="002" & b_ccode=="710")
chi_us<-filter(sort, a_ccode=="710" & b_ccode=="002")

sorted<-data.table(cbind(us_chi$DiCE, chi_us$DiCE), us_chi$Year)

colnames(sorted)<-c("US", "China", "Year")

g<-gather(sorted, 
          value="DiCE",
          key="Country",
          US, China)

out<-ggplot(sort, aes(x=Year, y=DiCE))+
        ylab("Expected Battle Deaths (IHS)")+
        coord_cartesian(ylim=c(0,13.5))+
        geom_line(data=g, aes(x=Year, y=DiCE, color=Country), size=1.25)

out+scale_colour_manual(values=c("red", "blue"))



# What are the top 5 costliest conflicts in specific years?

years<-seq(1820, 2000, by=10)

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

# Let's grab the costliest conflicts in total

# run through each year, identify the pairs and then add the costs








# Let's grab the top 10 threats for the US for every year
years<-seq(1820, 2000, by=10)

US_sub<-COW_DiCE %>%
        filter(a_ccode=="002")

US_threat<-foreach(i=1:length(years)) %do% {
        sub_year<-US_sub %>%
                filter(Year==years[[i]])
        
        sub_arrange<-arrange(sub_year, DiCE)
        
        out<-sub_arrange[1:3,]
        
        out$dyad_ccode_year
}


US_threat








# bootstrap time
boots=1000

DiCE.boot<-foreach(j=1:1000) %do% {
        
        boot<-sample(1:nrow(average.trim), replace=T)
        
        dat<-average.trim[boot,]
        
        set.seed(1999)
        models_average<-caretList(a_BatDeath_IHS~.,
                                  data=dat,
                                  trControl=tune_control,
                                  metric="RMSE",
                                  tuneList=list(rf=caretModelSpec(method="rf", tuneGrid=rf.tune.average),
                                                bstree=caretModelSpec(method="bstTree", tuneGrid=bst.tune.average)))
        
        dat<-aggregate.trim[boot,]
        
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
        
        print(j)
        
        out<-DiCE
        
        out
}

DiCE.boots<-do.call(cbind, DiCE.boot)


# bootstrap aggregate random forest 1000 times
dat<-aggregate.trim


tune_control<-trainControl(method="boot", n=500, predictionBounds = c(0, NA), savePredictions="final")

set.seed(1999)
rfModel_boots<-train(a_BatDeath_IHS~., 
               data=dat, 
               method="rf", 
               trControl=tune_control, 
               tuneGrid=rf.tune)



# Grab specific wars and battle deaths for both sides


# Look at predictions for 6 post 1980s conflicts



plot(NULL,
     xlim=c(0,5.5),
     ylim=c(0,16),
     xaxt='n',
     xlab="",
     ylab="Battle Deaths (IHS)")


a<-iraq1980
b<-iran1980

points(rep(0.1, length(a$pred)), a$pred, col=rgb(0.5, 0.5, 0.5, 0.5))
points(rep(0.2, length(b$pred)), b$pred, col=rgb(0.5, 0.5, 0.5, 0.5))

points(0.1, a$obs[1], pch=19)
points(0.2, b$obs[1], pch=19)


a<-israel1982
b<-syria1982

points(rep(1.1, length(a$pred)), a$pred, col=rgb(0.5, 0.5, 0.5, 0.5))
points(rep(1.2, length(b$pred)), b$pred, col=rgb(0.5, 0.5, 0.5, 0.5))

points(1.1, a$obs[1], pch=19)
points(1.2, b$obs[1], pch=19)



a<-iraq1990
b<-us1990

points(rep(2.2, length(a$pred)), a$pred, col=rgb(0.5, 0.5, 0.5, 0.5))
points(rep(2.1, length(b$pred)), b$pred, col=rgb(0.5, 0.5, 0.5, 0.5))

points(2.2, a$obs[1], pch=19)
points(2.1, b$obs[1], pch=19)


a<-serb1999
b<-us1999

points(rep(3.2, length(a$pred)), a$pred, col=rgb(0.5, 0.5, 0.5, 0.5))
points(rep(3.1, length(b$pred)), b$pred, col=rgb(0.5, 0.5, 0.5, 0.5))

points(3.2, a$obs[1], pch=19)
points(3.1, b$obs[1], pch=19)


a<-afgh2001
b<-us2001

points(rep(4.2, length(a$pred)), a$pred, col=rgb(0.5, 0.5, 0.5, 0.5))
points(rep(4.1, length(b$pred)), b$pred, col=rgb(0.5, 0.5, 0.5, 0.5))

points(4.2, a$obs[1], pch=19)
points(4.1, b$obs[1], pch=19)


a<-iraq2003
b<-us2003

points(rep(5.2, length(a$pred)), a$pred, col=rgb(0.5, 0.5, 0.5, 0.5))
points(rep(5.1, length(b$pred)), b$pred, col=rgb(0.5, 0.5, 0.5, 0.5))

points(5.2, a$obs[1], pch=19)
points(5.1, b$obs[1], pch=19)



axis(1, at=c(0.15, 1.15, 2.15, 3.15, 4.15, 5.15), labels=c("Iran-Iraq 1980", "First Lebanon War", "US-Iraq 1990", "Kosovo War", "US-Afghanistan 2001", "US-Iraq 2003"))


# Now to make this in ggplot
# make the backdrop

df <- data.frame()
set<-ggplot(df) + geom_point() + xlim(0, 6) + ylim(0, 16)

set+ylab("Expected Battle Deaths (IHS)")

set+ggplot()


iraq_iran<-data.table("1980 Iran-Iraq",iraq1980[1:150,]$pred, iran1980[1:150,]$pred)
israel_syria<-data.table("1982 Israel-Syria", israel1982[1:150,]$pred, syria1982[1:150,]$pred)
us_iraq_1<-data.table("1990 US-Iraq", us1990[1:150,]$pred, iraq1990[1:150,]$pred)
us_yugo<-data.table("1999 US-Yugoslavia", us1999[1:150,]$pred, serb1999[1:150,]$pred)
us_afgh<-data.table("2001 US-Afghanistan", us2001[1:150,]$pred, afgh2001[1:150,]$pred)
us_iraq<-data.table("2003 US-Iraq", us2003[1:150,]$pred, iraq2003[1:150,]$pred)


conflict<-rbind(iraq_iran, israel_syria, us_iraq_1, us_yugo, us_afgh, us_iraq)
colnames(conflict)<-c("Conflict"," Initiator", "Defender")

melt<-melt(conflict, value.name = "Side", varnames=c('Initiator', 'Defender'))
colnames(melt)<-c("Conflict", "Side", "Battle_Deaths")


iraq_iran_obs<-data.table("1980 Iran-Iraq",iraq1980[1,]$obs, iran1980[1,]$obs)
israel_syria_obs<-data.table("1982 Israel-Syria", israel1982[1,]$obs, syria1982[1,]$obs)
us_iraq_obs_1<-data.table("1990 US-Iraq", us1990[1,]$obs, iraq1990[1,]$obs)
us_yugo_obs<-data.table("1999 US-Yugoslavia", us1999[1,]$obs, serb1999[1,]$obs)
us_afgh_obs<-data.table("2001 US-Afghanistan", us2001[1,]$obs, afgh2001[1,]$obs)
us_iraq_obs<-data.table("2003 US-Iraq", us2003[1,]$obs, iraq2003[1,]$obs)


conflict<-rbind(iraq_iran, israel_syria, us_iraq_1, us_yugo, us_afgh, us_iraq)
colnames(conflict)<-c("Conflict"," Initiator", "Defender")

conflict_obs<-rbind(iraq_iran_obs, israel_syria_obs, us_iraq_obs_1, us_yugo_obs, us_afgh_obs, us_iraq_obs)
colnames(conflict_obs)<-c("Conflict"," Initiator", "Defender")

melt<-melt(conflict, value.name = "Side", varnames=c('Initiator', 'Defender'))
colnames(melt)<-c("Conflict", "Predicted", "Battle_Deaths")

melt_obs<-melt(conflict_obs, value.name = "Side", varnames=c('Initiator', 'Defender'))
colnames(melt_obs)<-c("Conflict", "Observed", "Battle_Deaths")


ggplot(melt, aes(x = Conflict, y=Battle_Deaths, color = Predicted)) +
        geom_point(position=position_dodge(width=0.3), alpha=0.15)+
        coord_cartesian(ylim = c(0, 16))+
        ylab("Battle Deaths (IHS)")

ggplot(melt_obs, aes(x = Conflict, y=Battle_Deaths, color = Observed)) +
        geom_point(position=position_dodge(width=0.3), size=5)+
        geom_point(data=melt, aes(x = Conflict, y=Battle_Deaths, color = Predicted), alpha=0.15, position=position_dodge(width=0.3))+
        coord_cartesian(ylim = c(0, 16))+
        ylab("Battle Deaths (IHS)")


        
        


ggplot(data=melt_obs, aes(x = Conflict, y=Battle_Deaths, color = Observed), shape=21, size=3, fill="color", position=position_dodge(width=0.3))+
        geom_point()

        geom_point(data= melt_obs, shape=21, size=5, fill="black", position=position_dodge(width=0.3))

ggplot(melt_obs, aes(x = Conflict, y=Battle_Deaths), alpha=0.5) +
        geom_point(position=position_dodge(width=0.3))+
        ylab("Expected Battle Deaths (IHS)")


ggplot(melt, aes(x = Conflict, ymin=min(Battle_Deaths), ymax=max(Battle_Deaths), color = Side), alpha=0.5) +
        geom_errorbar(position=position_dodge(width=0.3))+
        ylab("Expected Battle Deaths (IHS)")
        
        



