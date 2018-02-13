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
library(dplyr)
library(tidyr)

#setwd("C:/Users/Phil/Dropbox/")
setwd("F:/Dropbox/FSU/FSU_Fall_2017/Paper_1/")
#setwd("/Users/philhenrickson/Dropbox/FSU/FSU_Fall_2017/Paper_1/")

# read in cowdyads universe
COWdyads<-foreach(i=1:10)  %do% {
        a <- get(load(paste("Final_Data/COWdyads_final_", i, ".Rdata", sep="")))
        a
        
        colnames(COWdyads_final)[3]<-c("Year")
        COWdyads_final<-data.table(COWdyads_final)
        
        COWdyads_final<-COWdyads_final %>%
                mutate(a_polity2=replace(a_polity2, a_polity2>10, 10)) %>%
                mutate(b_polity2=replace(b_polity2, b_polity2>10, 10)) %>%
                mutate(a_polity2=replace(a_polity2, a_polity2< -10, -10)) %>%
                mutate(b_polity2=replace(b_polity2, b_polity2< -10, -10))
        COWdyads_final
}
rm(a)

# load in imputations
# load in imputations
strong<-foreach(i=1:10)  %do% {
        a <- get(load(paste("Final_data/strong", i, ".Rdata", sep="")))
        a
}

average<-foreach(i=1:10) %do% {
        a<-get(load(paste("Final_data/average", i, ".Rdata", sep="")))
        a
}

aggregate<-foreach(i=1:10) %do% {
        a<-get(load(paste("Final_data/aggregate", i, ".Rdata", sep="")))
        a
}       

# variables to omit
omit<-c("a_BatDeath", "Duration", "icowsal", "dyad_ccode_year", "mariss", "terriss")



# load in weights from ensemble to select models to run
load("ensemble.weights.Rdata")
look<-round(ensemble.weights[which(ensemble.weights > 0.001)], 4)

# set tune control 
ctrlParallel<-trainControl(method="cv", 
                           n=5, 
                           predictionBounds = c(0, NA), 
                           savePredictions="all",
                           allowParallel=T)
# From strong:
# Run a random forest, neural net, and mars with the following tuning
s_marsGrid<-expand.grid(.nprune=26, .degree=1)
s_nnetGrid<-expand.grid(.size=11, .decay=0.06, .bag=F)
s_rfGrid<-expand.grid(.mtry=13)


h=1

set.seed(1999)
models_strong<-caretList(a_BatDeath_IHS~.,
                         data=dplyr::select(strong[[h]], -one_of(omit), -a_ccode, -b_ccode),
                         trControl=ctrlParallel,
                         metric="RMSE",
                         tuneList=list(earth=caretModelSpec(method="earth", tuneGrid=s_marsGrid, preProcess=c("center", "scale")),
                                       avNNet=caretModelSpec(method="avNNet", tuneGrid=s_nnetGrid, preProcess=c("center", "scale"), linout=1, trace=F),
                                       rf=caretModelSpec(method="rf", tuneGrid=s_rfGrid)))


# From average:
# Run a cubist, boosted trees, and random forest
avg_cubGrid<-expand.grid(.committees=20, .neighbors=1)
avg_gbmGrid<-expand.grid(.n.trees=200,
                     .interaction.depth=9,
                     .shrinkage=0.1,
                     .n.minobsinnode=10)
avg_rfGrid<-expand.grid(.mtry=13)


set.seed(1999)
models_average<-caretList(a_BatDeath_IHS~.,
                         data=dplyr::select(average[[h]], -one_of(omit), -a_ccode, -b_ccode),
                         trControl=ctrlParallel,
                         metric="RMSE",
                         tuneList=list(cubist=caretModelSpec(method="cubist", tuneGrid=avg_cubGrid),
                                       gbm=caretModelSpec(method="gbm", tuneGrid=avg_gbmGrid),
                                       rf=caretModelSpec(method="rf", tuneGrid=avg_rfGrid)))



# From aggregate:
# Run a cubist, two mars, a neural net, and a random forest
agg_cubGrid<-expand.grid(.committees=1, .neighbors=9)
agg_marsGrid1<-expand.grid(.nprune=22, .degree=1)
agg_marsGrid2<-expand.grid(.nprune=18, .degree=1)
agg_nnetGrid<-expand.grid(.size=7, .decay=0.04, .bag=F)
agg_rfGrid<-expand.grid(.mtry=5)


set.seed(1999)
models_aggregate<-caretList(a_BatDeath_IHS~.,
                         data=dplyr::select(aggregate[[h]], -one_of(omit), -a_ccode, -b_ccode),
                         trControl=ctrlParallel,
                         metric="RMSE",
                         tuneList=list(cubist=caretModelSpec(method="cubist", tuneGrid=agg_cubGrid),
                                       earth1=caretModelSpec(method="earth", tuneGrid=agg_marsGrid1, preProcess=c("center", "scale")),
                                       earth2=caretModelSpec(method="earth", tuneGrid=agg_marsGrid1, preProcess=c("center", "scale")),
                                       avNNet=caretModelSpec(method="avNNet", tuneGrid=agg_nnetGrid, preProcess=c("center", "scale"), linout=1, trace=F),
                                       rf=caretModelSpec(method="rf", tuneGrid=agg_rfGrid)))


### predict COWdyads for each imputation

# first, pull out conflicts which actually occured from CoWDyads
realized<-paste(strong[[h]]$a_ccode, strong[[h]]$b_ccode, strong[[h]]$Year, sep="_")
COW<-COWdyads[[h]][-which(COWdyads_final$dyad_ccode_year %in% realized==T),]


# grab predictions from these models
# bind participants, set to 2 for all COWdyads
participants<-rep(2, nrow(COW))
COWdyads<-data.table(COW, participants)
rm(COW)

pred.strong<-foreach(i=1:length(models_strong), .combine=cbind) %do%
        predict(models_strong[[i]], newdata=COWdyads)

pred.average<-foreach(i=1:length(models_average), .combine=cbind) %do%
        predict(models_average[[i]], newdata=COWdyads)

pred.aggregate<-foreach(i=1:length(models_aggregate), .combine=cbind) %do%
        predict(models_aggregate[[i]], newdata=COWdyads)

pred.out<-as.matrix(data.frame(pred.aggregate, pred.strong, pred.average))
pred.ensemble<-pred.out%*%as.vector(look)


DiCE_oos<-data.frame(dyad_ccode_year=COWdyads$dyad_ccode_year,
                     a_ccode=COWdyads$a_ccode,
                     b_ccode=COWdyads$b_ccode,
                     Year=COWdyads$Year,
                     DiCE=pred.ensemble)

# CV Predictions
fold_index<-models_strong$rf$pred$rowIndex

fold_obs<-models_strong$rf$pred$obs

fold_pred<-data.table(models_aggregate$cubist$pred$pred,
                      models_aggregate$earth1$pred$pred,
                      models_aggregate$earth2$pred$pred,
                      models_aggregate$avNNet$pred$pred,
                      models_aggregate$rf$pred$pred,
                      models_strong$earth$pred$pred,
                      models_strong$avNNet$pred$pred,
                      models_strong$rf$pred$pred,
                      models_average$cubist$pred$pred,
                      models_average$gbm$pred$pred,
                      models_average$rf$pred$pred)

fold_ensemble<-as.matrix(fold_pred)%*%as.vector(look)

data.table(fold_index, fold_obs, fold_ensemble)

DiCE_cv<-data.frame(dyad_ccode_year=strong[[h]][fold_index,]$dyad_ccode_year,
                 a_ccode=strong[[h]][fold_index,]$a_ccode,
                 b_ccode=strong[[h]][fold_index,]$b_ccode,
                 Year=strong[[h]][fold_index,]$Year,
                 DiCE=fold_ensemble)

# bind together
dice<-rbind.data.frame(DiCE_oos, DiCE_cv)
save(dice, file="Output/dice.RData")
           
# save as DD
dice_dd<- dice %>%
        dplyr::rename("A_Costs"=DiCE)

save(dice_dd, file="F:/Dropbox/FSU/FSU_Fall_2017/Paper_1/Output/dice_dd.Rdata")

# convert to UD
reverse<-dice

reverse$a_ccode<-dice$b_ccode
reverse$b_ccode<-dice$a_ccode

dice <- dice %>%
        dplyr::rename("A_Costs"= DiCE)

reverse<- reverse %>%
        dplyr::rename("B_Costs"= DiCE)

reverse$dyad_ccode_year<-paste(reverse$a_ccode, reverse$b_ccode, reverse$Year, sep="_")
reverse<-dplyr::select(reverse, -a_ccode, -b_ccode, -Year)

dice_ud<-join(dice,  reverse, by="dyad_ccode_year", type="left", match="first")

test<-dice_ud

test$a_ccode<-as.numeric(test$a_ccode)
test$b_ccode<-as.numeric(test$b_ccode)

source("F:/Dropbox/FSU/FSU_Fall_2017/Paper_2/Measure/fn-ccodeCleaner.R")

dice_ud <- test %>%
        filter(a_ccode < b_ccode & Year>=min(test$Year)) %>%
        dplyr::rename("A_ccode"= a_ccode, "B_ccode"=b_ccode, "year"=Year) %>%
        mutate(A_ccode = makeCCode(A_ccode), B_ccode = makeCCode(B_ccode),
               A_cyear = paste(A_ccode, year, sep = "_"),
               B_cyear = paste(B_ccode, year, sep = "_"),
               A_ccode = fixCYear(A_cyear)$fixedCCode,
               B_ccode = fixCYear(B_cyear)$fixedCCode,
               A_cyear = fixCYear(A_cyear)$fixedCYear,
               B_cyear = fixCYear(B_cyear)$fixedCYear) %>%
        filter(!is.na(A_ccode) & !is.na(B_ccode)) %>%
        mutate(dyad_ccode_year= paste(A_ccode, B_ccode, year, sep = "_")) %>% 
        dplyr::select(dyad_ccode_year, year, A_ccode, B_ccode, A_Costs, B_Costs)
        
save(dice_ud, file="F:/Dropbox/FSU/FSU_Fall_2017/Paper_1/Output/dice_ud.Rdata")


# Visualize
library(plyr)
library(ggplot2)

# 
sort<-data.table(arrange(dice, Year))

#
ggplot(sort, aes(x=Year, y=A_Costs))+
        geom_point(alpha=0.02)+
        ylab("Expected Battle Deaths (IHS)")+
        theme_bw()


# merge dice with COWdyads
foo<-left_join(dice, dplyr::select(COWdyads, dyad_ccode_year, a_polity2, b_polity2), by="dyad_ccode_year")

sort<-data.table(arrange(foo, Year))

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

ggplot(sort.dem, aes(x=Year, y=A_Costs, group=Joint_Polity))+
        geom_point(aes(color=Joint_Polity), alpha=0.01)+
        scale_color_gradient(low="red", high="blue")+
        ylab("Expected Battle Deaths (IHS)")+
        theme_bw()


# facet regime type
ggplot(sort.dem, aes(x=Year, y=DiCE))+
        geom_point(aes(color=Dyad), alpha=0.01)+
        facet_grid(Dyad~ .)+
        scale_colour_manual(values=c("red", "blue", "purple4"), guide=FALSE)



# breaks with loess lines
sort.pre1826<-subset(sort, Year<=1826)
sort.pre1919<-subset(sort, Year>1826 & Year<=1919)
sort.pre1930<-subset(sort, Year>=1920 & Year<1930)
sort.pre1950<-subset(sort, Year>=1929 & Year<1950)
sort.post1950<-subset(sort, Year>=1950)

# create empty plot
df <- data.frame()
set<-ggplot(df) + geom_point() + xlim(1816, 2007)
set

set+ geom_point(data=sort.pre1826, aes(x=Year, y=A_Costs), alpha=0.01)+
        stat_smooth(data=sort.pre1826, aes(x=Year, y=A_Costs))+
        geom_vline(xintercept=1826, linetype="longdash")+
        geom_point(data=sort.pre1919, aes(x=Year, y=A_Costs), alpha=0.01)+
        stat_smooth(data=sort.pre1919, aes(x=Year, y=A_Costs))+
        geom_vline(xintercept=1919, linetype="longdash")+
        geom_point(data=sort.pre1930, aes(x=Year, y=A_Costs), alpha=0.01)+
        stat_smooth(data=sort.pre1930, aes(x=Year, y=A_Costs))+
        geom_vline(xintercept=1930, linetype="longdash")+
        geom_point(data=sort.pre1950, aes(x=Year, y=A_Costs), alpha=0.01)+
        stat_smooth(data=sort.pre1950, aes(x=Year, y=A_Costs))+
        geom_vline(xintercept=1950, linetype="longdash")+
        geom_point(data=sort.post1950, aes(x=Year, y=A_Costs), alpha=0.01)+
        stat_smooth(data=sort.post1950, aes(x=Year, y=A_Costs))+
        ylab("Expected Battle Deaths (IHS)")+
        theme_bw()



# Predict specific countries
# us and soviet union

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


iraq<-dplyr::select(iraq_us, -b_polity2, -b_irst, -b_milex, -b_milper, -b_pec, -b_tpop, -b_upop, -b_lcinc, -conttype, -region, -claim, -chal, -tgt, -icowsal, -a_ccode, -b_ccode, -dyad_ccode_year, -issue, -DiCE)
multi<-rbind(iraq_us, iraq_uk, iraq_aus)

coalition<-dplyr::select(multi, b_polity2, b_irst, b_milex, b_milper, b_pec, b_tpop, b_upop, b_lcinc, conttype)

foo<-t(as.matrix(apply(coalition, 2, sum)))
foo[,1]<-10
foo[,9]<-6
participants<-4
iraq03<-data.frame(iraq, foo, participants)

# predict

# grab predictions from these models
a<-foreach(i=1:length(models_strong), .combine=cbind) %do%
        predict.train(models_strong[[i]], newdata=iraq03)

b<-foreach(i=1:length(models_average), .combine=cbind) %do%
        predict.train(models_average[[i]], newdata=iraq03)

c<-foreach(i=1:length(models_aggregate), .combine=cbind) %do%
        predict.train(models_aggregate[[i]], newdata=iraq03)

pred.out<-data.table(do.call(cbind, a), do.call(cbind, b), do.call(cbind, c))

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


# run random forest on aggregate again
dat<-aggregate.trim

tune_control<-trainControl(method="cv", n=5, predictionBounds = c(0, NA), savePredictions="final", allowParallel=T)

set.seed(1999)
rfModel<-train(a_BatDeath_IHS~., 
                     data=dat, 
                     method="rf", 
                     trControl=tune_control, 
                     tuneGrid=rf.tune,
                     parallel=T)

number<-rfModel$pred$rowIndex
pred<-rfModel$pred$pred
obs<-rfModel$pred$obs
foo<-rfModel$pred


# bootstrap aggregate random forest 1000 times

DiCE_rf<-predict(rfModel, dat=COWdyads1)

# boostrap
cl <- makeCluster(7)
registerDoParallel(cl)

tune_control<-trainControl(method="boot", n=1000, predictionBounds = c(0, NA), savePredictions="final", allowParallel=T)

set.seed(1999)
rfModel_boots<-train(a_BatDeath_IHS~., 
                     data=dat, 
                     method="rf", 
                     trControl=tune_control, 
                     tuneGrid=rf.tune,
                     parallel=T)

which(rfModel_boots$pred==5)

#
number_boots<-rfModel_boots$pred$rowIndex
pred_boots<-rfModel_boots$pred$pred
obs_boots<-rfModel_boots$pred$obs
foo<-rfModel_boots$pred


which(rfModel$pred$rowIndex==1138)
# 

# Grab specific conflicts
num_a<-which(aggregate1$a_ccode=="645"&aggregate1$Year==1980)
num_b<-which(aggregate1$a_ccode=="630"&aggregate1$Year==1980)

iraq1980<-cbind(aggregate1[num_a,], foo[which(foo$rowIndex==num_a),])
iran1980<-cbind(aggregate1[num_b,], foo[which(foo$rowIndex==num_b),])

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


# next conflict
num_a<-which(aggregate1$a_ccode=="666"&aggregate1$Year==1982)
num_b<-which(aggregate1$a_ccode=="652"&aggregate1$Year==1982)

israel1982<-cbind(aggregate1[num_a,], foo[which(foo$rowIndex==num_a),])
syria1982<-cbind(aggregate1[num_b,], foo[which(foo$rowIndex==num_b),])

a<-israel1982
b<-syria1982

points(rep(1.1, length(a$pred)), a$pred, col=rgb(0.5, 0.5, 0.5, 0.5))
points(rep(1.2, length(b$pred)), b$pred, col=rgb(0.5, 0.5, 0.5, 0.5))

points(1.1, a$obs[1], pch=19)
points(1.2, b$obs[1], pch=19)

#
num_a<-which(aggregate1$a_ccode=="645"&aggregate1$Year==1990)
num_b<-which(aggregate1$a_ccode=="002"&aggregate1$Year==1990)

iraq1990<-cbind(aggregate1[num_a,], foo[which(foo$rowIndex==num_a),])
us1990<-cbind(aggregate1[num_b,], foo[which(foo$rowIndex==num_b),])

a<-iraq1990
b<-us1990

points(rep(2.2, length(a$pred)), a$pred, col=rgb(0.5, 0.5, 0.5, 0.5))
points(rep(2.1, length(b$pred)), b$pred, col=rgb(0.5, 0.5, 0.5, 0.5))

points(2.2, a$obs[1], pch=19)
points(2.1, b$obs[1], pch=19)

#
num_a<-which(aggregate1$a_ccode=="340"&aggregate1$Year==1999)
num_b<-which(aggregate1$a_ccode=="002"&aggregate1$Year==1999)

serb1999<-cbind(aggregate1[num_a,], foo[which(foo$rowIndex==num_a),])
us1999<-cbind(aggregate1[num_b,], foo[which(foo$rowIndex==num_b),])

a<-serb1999
b<-us1999

points(rep(3.2, length(a$pred)), a$pred, col=rgb(0.5, 0.5, 0.5, 0.5))
points(rep(3.1, length(b$pred)), b$pred, col=rgb(0.5, 0.5, 0.5, 0.5))

points(3.2, a$obs[1], pch=19)
points(3.1, b$obs[1], pch=19)

#
num_a<-which(aggregate1$a_ccode=="700"&aggregate1$Year==2001)
num_b<-which(aggregate1$a_ccode=="002"&aggregate1$Year==2001)[2]

afgh2001<-cbind(aggregate1[num_a,], foo[which(foo$rowIndex==num_a),])
us2001<-cbind(aggregate1[num_b,], foo[which(foo$rowIndex==num_b),])

a<-afgh2001
b<-us2001

points(rep(4.2, length(a$pred)), a$pred, col=rgb(0.5, 0.5, 0.5, 0.5))
points(rep(4.1, length(b$pred)), b$pred, col=rgb(0.5, 0.5, 0.5, 0.5))

points(4.2, a$obs[1], pch=19)
points(4.1, b$obs[1], pch=19)

#
num_a<-which(aggregate1$a_ccode=="645"&aggregate1$Year==2003)[2]
num_b<-which(aggregate1$a_ccode=="002"&aggregate1$Year==2003)

iraq2003<-cbind(aggregate1[num_a,], foo[which(foo$rowIndex==num_a),])
us2003<-cbind(aggregate1[num_b,], foo[which(foo$rowIndex==num_b),])

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
        
        















# Misc

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



