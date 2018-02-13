####################################################################
###### Train Models on combined ISW and MID battle death data ######
####################################################################

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
library(Hmisc)
library(rpart)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(randomForestSRC)
library(ggRandomForests)

#Send yourself an email - specify your preferred email address, subject, and message. The password is fixed at "rmail".
#send_message(mime(from="phil.henrickson@gmail.com", to="phil.henrickson@gmail.com", subject="Code Finished", "Woo!"))

#setwd("C:/Users/Phil/Dropbox/")
setwd("F:/Dropbox/FSU/FSU_Fall_2017/Paper_1/")


# load in datasets
load("Final_Data/allies1.RData")

allies.trim<-dplyr::select(allies1, a_BatDeath_IHS, Year, a_irst, a_milex, a_milper, a_pec, a_tpop, a_upop, a_lcinc, a_polity2,
                           a_ally_irst, a_ally_milex, a_ally_milper , a_ally_pec, a_ally_tpop, a_ally_upop, a_ally_lcinc, a_ally_polity2,
                           b_irst, b_milex, b_milper, b_pec, b_tpop, b_upop, b_lcinc, b_polity2, conttype, participants, terriss, riveriss, mariss, icowsalc)

colnames(allies.trim)<-c("A_Battle_Deaths", #1
                         "Year", #2
                         "A_IronSteel", #3 
                         "A_Mil.Exp.", #4
                         "A_Mil.Pers.", #5
                         "A_EnergyCon.", #6 
                         "A_TotalPop.", #7
                         "A_UrbanPop.", #8
                         "A_CINC", #9
                         "A_Polity2", #10
                         "A_Ally_IronSteel",  #11                    "A_Mil.Exp.",
                         "A_Ally_Mil.Exp.", #12
                         "A_Ally_Mil.Pers.", #13
                         "A_Ally_EnergyConsump.", #14 
                         "A_Ally_TotalPop.", #15
                         "A_Ally_UrbanPop.", #16
                         "A_Ally_CINC", #17
                         "A_Ally_Polity2", #18
                         "B_IronSteel", #19
                         "B_Mil.Exp.", #20
                         "B_Mil.Pers.", #21
                         "B_EnergyCon", #22
                         "B_TotalPop.", #23
                         "B_UrbanPop.", #24
                         "B_CINC", #25
                         "B_Polity2", #26
                         "Contiguity", #27
                         "Participants", #28
                         "Territorial_Dispute", #29
                         "River_Dispute", #30
                         "Maritime_Dispute", #31
                         "ICOW_Salience") #32

dat<-allies.trim
dat.ny<-dplyr::select(allies.trim, -Year)

# fit carts to these datasets
tune_control<-trainControl(method="repeatedcv", n=5, repeats=5, predictionBounds = c(0, NA), savePredictions="final")

cart_year<-train(A_Battle_Deaths~.,
                 data=dat,
                 method="rpart2",
                 trControl=tune_control)

cart_noyear<-train(A_Battle_Deaths~.,
                   data=dat.ny,
                   method="rpart2",
                   trControl=tune_control)

fancyRpartPlot(cart_year$finalModel, sub="")
fancyRpartPlot(cart_noyear$finalModel, sub="")


# run random forests on these datasets

tune_control<-trainControl(method="repeatedcv", n=5, repeats=5, predictionBounds = c(0, NA), savePredictions="final")

rf_year<-rfsrc(A_Battle_Deaths~., 
               data=dat, 
               mtry=7, tree.err=T)

rf_noyear<-rfsrc(A_Battle_Deaths~.,
                 data=dat.ny,
                 mtry=7, tree.err=T)

# plot variable importance
vimp1<-plot(gg_vimp(rf_year), lbsl=st.labs)
vimp2<-plot(gg_vimp(rf_noyear), lbsl=st.labs)

grid.arrange(vimp1, vimp2, ncol=2)
ggsave("vimp_allies.PDF")


library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

# plot out of bag error rate
plot(gg_error(rf_year))
plot(gg_error(rf_noyear))

# 
plot(sqrt(gg_rfsrc(rf_year)), alpha=.5)
plot(sqrt(gg_rfsrc(rf_noyear)), alpha=.5)

# plot minimal depth selection
varsel_year<-var.select(rf_year)
gg_md<-gg_minimal_depth(varsel_year)
plot(gg_md)
ggsave("Figures/depth_allies.pdf")

# plot partial dependence
gg_v_year<-gg_variable(rf_year)
plot(gg_v_year, panel=T, k=5)
ggsave("Figures/partial_allies.pdf")

#colnames(gg_v_year)<-c("Year", "State A Iron/Steel", "State A Military Expenditures", "State A Military Expenditures", "State A Military Personnel", "State A Energy Consumption", "State A Total Population", "State A Urban Population", "State A CINC", "State A Polity2", "State B Iron/Steel", "State B Military Expenditures", "State B Military Personnel", "State B Energy Consumption", "State B Total Population", "State B Urban Population", "State B CINC", "State B Polity2", "Contiguity")

# examine interactions
interaction_year<-find.interaction(rf_year_aggregate)
plot(gg_interaction(interaction_year), panel=T)
ggsave("Figures/interactions_allies.pdf")

# Grab a quick lasso plot to show off

# Fit Lasso and Ridges to also show this
# 
preObj<-preProcess(dat[,-c(1,2)], method=c("center", "scale"))
newdat<-data.table(dat[,c(1,2)], predict(preObj, dat[,-c(1,2)]))
colnames(newdat)[c(1,2)]<-c("A_Battle_Deaths", "Year")

library(glmnet)

x<-as.matrix(dplyr::select(newdat, -A_Battle_Deaths))
y<-as.matrix(dplyr::select(newdat, A_Battle_Deaths))


library(plotmo)
# lasso

lasso<-glmnet(x, y, alpha=1, family="gaussian")

cv.lasso<-cv.glmnet(x, y, alpha=1, family="gaussian")
coef.lasso<-coef(cv.lasso, s="lambda.min")

plot(cv.lasso$glmnet.fit, "lambda", label=T)




library(lars)
ridge<-glmnet(x, y, alpha=0, family="gaussian")
cv.ridge<-cv.glmnet(x, y, alpha=0, family="gaussian")
coef.ridge<-coef(cv.ridge, s="lambda.min")

plot(cv.ridge$glmnet.fit, "lambda", label=T)


# make
plot_glmnet(lasso,'lambda', label=10)
abline(v=log(cv.lasso$lambda.min), lty=2)


plot_glmnet(ridge, 'lambda', label=10)
abline(v=log(cv.ridge$lambda.min), lty=2)




# OLS with variable imputations
# grab the variables
vars<-names(dat)

# these are the variables to add in, leaving the standard set out
foo<-seq(1, ncol(dat))[-c(1,2,9,25)]

# set the tuning
tune_control<-trainControl(method="repeatedcv", n=5, predictionBounds = c(0, NA), savePredictions="final", allowParallel=T)

# iterate over variables, bootstrap for CIs

cl <- makeCluster(7)
registerDoParallel(cl)

olsvar.rmse<-
        foreach(i=1:length(foo), .combine=cbind, .packages=c('dplyr', 'foreach', 'caret')) %:% 
                foreach(j=1:1000, .combine=rbind, .packages=c('dplyr', 'foreach', 'caret')) %dopar% {
                
                form<-as.formula(paste("A_Battle_Deaths~", paste(vars[c(2,9,17, foo[i])], collapse="+")))
        
                boot<-sample(1:nrow(dat), replace=T)
                dat.boot<-dat[boot,]

                olsvar<-train(form,
                              data=dat.boot,
                              method="lm",
                              trControl=tune_control)
                
                rmse<-olsvar$results[2]
                rmse
                }

colnames(olsvar.rmse)<-vars[foo]

# mean
olsvar.mean<-apply(olsvar.rmse, 2, mean)

# CIs
olsvar.sort<-apply(olsvar.rmse, 2, sort)
olsvar.low<-olsvar.sort[51,]
olsvar.high<-olsvar.sort[950,]

olsvar.tab<-data.frame(olsvar.mean, olsvar.low, olsvar.high)

olsvar.disp<-olsvar.tab[order(-olsvar.tab$olsvar.mean) , , drop=F]

y.plot<-seq(1, length(olsvar.mean))
olsvar.disp<-data.frame(olsvar.disp, y.plot)

# plot in ggplot
ggplot(olsvar.disp, aes(x=olsvar.mean, y=y.plot))+
        geom_point(color="blue", size=2)+
        geom_errorbarh(aes(xmin=olsvar.low, xmax=olsvar.high), 
                       colour="blue", alpha=0.5, height=0, lwd=1) +
        coord_cartesian(xlim = c(3.075, 3.52), ylim=c(1,20))+
        scale_y_continuous(breaks = pretty(olsvar.disp$y.plot, n = 20), labels=rownames(olsvar.disp))+        
        labs(x="RMSE")+
        labs(y="")+
        ggtitle("OLS Variable Permutations")
ggsave("Figures/ols_var.pdf")

# Random forests fit to different periods of time
dat<-dat.y.aggregate

dat1<-subset(dat, Year<=1850)
dat2<-subset(dat, Year<=1900 & Year>1850)
dat3<-subset(dat, Year>1900 & Year<=1950)
dat4<-subset(dat, Year>1950)


rf1<-rfsrc(A_Battle_Deaths~., 
           data=dat1, 
           mtry=7, tree.err=T)

rf2<-rfsrc(A_Battle_Deaths~., 
            data=dat2, 
            mtry=7, tree.err=T)

rf3<-rfsrc(A_Battle_Deaths~., 
            data=dat3, 
            mtry=7, tree.err=T)

rf4<-rfsrc(A_Battle_Deaths~., 
           data=dat4, 
           mtry=7, tree.err=T)

a<-plot(gg_vimp(rf1), lbsl=st.labs)+
        ggtitle("1816-1850")
b<-plot(gg_vimp(rf2), lbsl=st.labs)+
        ggtitle("1851-1900")
c<-plot(gg_vimp(rf3), lbsl=st.labs)+
        ggtitle("1901-1950")
d<-plot(gg_vimp(rf4), lbsl=st.labs)+
        ggtitle("1951-2007")

grid.arrange(a, b, c, d)
ggsave("Figures/rf_time.pdf")


# partial dependence
gg_v<-gg_variable(rf4)
plot(gg_v, panel=T, k=5)+
        coord_cartesian(ylim=c(0,15))


# Repeat for OLS
vars<-names(dat.y.aggregate)

# these are the variables to add in, leaving the standard set out
foo<-seq(1, ncol(dat.y.aggregate))[-c(1,2,9,17)]

# set the tuning
tune_control<-trainControl(method="repeatedcv", n=5, predictionBounds = c(0, NA), savePredictions="final", allowParallel=T)

# iterate over variables

cl <- makeCluster(7)
registerDoParallel(cl)

year.seq<-c(1850, 1900, 1950, 2007)

ols.periods<-
foreach(h=1:length(year.seq), .combine=rbind, .packages=c('dplyr', 'foreach', 'caret')) %:% 
        foreach(i=1:length(foo), .combine=cbind, .packages=c('dplyr', 'foreach', 'caret')) %dopar% {

                dat<-subset(dat.y.aggregate, Year<=year.seq[h])
                
                 form<-as.formula(paste("A_Battle_Deaths~", paste(vars[c(2,9,17, foo[i])], collapse="+")))
                
        #        boot<-sample(1:nrow(dat.y.aggregate), replace=T)
         #       dat.boot<-dat.y.aggregate[boot,]
                
                olsvar<-train(form,
                              data=dat,
                              method="lm",
                              trControl=tune_control)
                
                rmse<-olsvar$results[2]
                rmse
        }

colnames(ols.periods)<-vars[foo]
ols.disp<-t(ols.periods)

ols.periods.disp<-ols.periods[order(-ols.periods$RMSE) , , drop=F]

ggplot(olsvar.disp, aes(x=olsvar.mean, y=y.plot))+
        geom_point(color="blue", size=2)+
        geom_errorbarh(aes(xmin=olsvar.low, xmax=olsvar.high), 
                       colour="blue", alpha=0.5, height=0, lwd=1) +
        coord_cartesian(xlim = c(3.075, 3.52), ylim=c(1,20))+
        scale_y_continuous(breaks = pretty(olsvar.disp$y.plot, n = 20), labels=rownames(olsvar.disp))+        
        labs(x="RMSE")+
        labs(y="")+
        ggtitle("OLS Variable Permutations")


library(Hmisc)
latex(round(table, 3), file="")





