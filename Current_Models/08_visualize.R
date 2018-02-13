####
###
### 

### Make Figures

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
library(abind)

#setwd("C:/Users/Phil/Dropbox/")
setwd("F:/Dropbox/FSU/FSU_Fall_2017/Paper_1/")
#setwd("/Users/philhenrickson/Dropbox/FSU/FSU_Fall_2017/Paper_1/")

### Scatterplots of model performance
# load in predictions - we'll use aggregate because it showed best performance across the board
load("Output/bestPred_aggregate.Rdata")

# grab the actual dv
load("Output/eval.Rdata")
actual<-eval$actual

# scatter plot
# brute force the column names because I'm bad at this
colnames(bestPred_aggregate)<-c("CART", 
                     "CINC+Year",
                     "Cubist", 
                     "Elastic Net",
                     "Boosted Trees",
                     "KNN",
                     "MARS",
                     "Neural Nets",
                     "Null",
                     "OLS",
                     "PLS",
                     "Random Forest",
                     "SVM Radial")

# arrange by rmse
load("Output/rmse_aggregate.Rdata")

pred_dt<-bestPred_aggregate[, order(-rmse_aggregate[,2])]
rmse_order<-rmse_aggregate$rpart2..maxdepth7[order(-rmse_aggregate[,2])]

# pull out the null
pred_dt<-dplyr::select(pred_dt, -Null)
rmse_order<-rmse_order[-1]

hold<-list() # holder

# loop over
for (i in 1:ncol(pred.dt))
        local({
                i=i
        plot<-ggplot(pred_dt, aes(x=pred.dt[,i], y=actual))+
                geom_point(alpha=0.25)+
                geom_smooth()+
                coord_cartesian(xlim = c(0,18), ylim=c(0,18))+
                labs(y="Observed")+
                labs(x="Predicted")+
                ggtitle(paste(colnames(pred_dt[i]), round(rmse_order[i],3), sep="\nRMSE: "))+
                theme_bw()
        print(i)
        print(plot)
        
        hold[[i]]<<-plot
})


# test
# loop over
for (i in 1:ncol(pred.dt))
        local({
                i=i
                plot<-ggplot(pred_dt, aes(x=pred.dt[,i], y=actual))+
                        geom_point(alpha=0.25)+
                        geom_smooth()+
                        coord_cartesian(xlim = c(0,18), ylim=c(0,18))+
                        ggtitle(paste(colnames(pred_dt[i]), round(rmse_order[i],3), sep="\nRMSE: "))+
                        theme_bw()+
                        theme_update(plot.title = element_text(),
                                     axis.title.x=element_blank(),
                                     axis.title.y=element_blank()) +
                        theme_update()
                
                print(i)
                print(plot)
                
                hold[[i]]<<-plot
        })


# plot all
grid.arrange(hold[[1]],
             hold[[2]],
             hold[[3]],
             hold[[4]],
             hold[[5]],
             hold[[6]],
             hold[[7]],
             hold[[8]],
             hold[[9]],
             hold[[10]],
             hold[[11]],
             hold[[12]],
             ncol=3,
             bottom="Predicted",
             left="Observed")

# RMSE function
error<-function(pred.Y, Y) {
        sqrt(sum((pred.Y-Y)^2)/nrow(Y))
}

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

# combine
dat.approaches<-foreach(i=1:length(strong)) %do% {
        list(strong[[i]], average[[i]], aggregate[[i]])
}
dat.names<-c("Strong", "Average", "Aggregate")


# until imputations
h=3
i=1

# Omit variables
omit<-c("a_BatDeath", "Duration", "icowsal", "dyad_ccode_year", "mariss", "terriss", "riveriss")
dat_all<-dat.approaches[[h]][[i]]

# Battle Deaths
# Show with and without IHS
a<-ggplot(dat_all, aes(x=a_BatDeath))+
        geom_histogram(bins=50)+
        labs(x="Battle Deaths")+
        theme_classic()
a
#ggsave("Figures/hist_batdeath.pdf")

b<-ggplot(dat_all, aes(x=a_BatDeath_IHS))+
        geom_histogram(binwidth=1)+
        labs(x="Battle Deaths (IHS)")+
        theme_classic()
b
#ggsave("Figures/hist_batdeath.pdf")

grid.arrange(a,b, ncol=2)
ggsave("Figures/transformed.pdf")


# show over time
ggplot(dat_all, aes(x=Year, y=a_BatDeath_IHS))+
        geom_point(alpha=0.3)+
        labs(y="Battle Deaths (IHS)")+
        theme_classic()
ggsave("Figures/dv_time.pdf")
      

# Summary Stats
dat<-dplyr::select(dat.approaches[[h]][[i]], -one_of(omit),
                   -a_ccode,
                   -b_ccode,
                   -Year)

tmp <- do.call(data.frame, 
               list(mean = apply(dat, 2, mean),
                    sd = apply(dat, 2, sd),
                    median = apply(dat, 2, median),
                    min = apply(dat, 2, min),
                    max = apply(dat, 2, max),
                    n = apply(dat, 2, length)))

tab<- tmp
tab[8,4]<- -10
tab[8,5]<- 10
tab[16,4]<- -10


# Create cleaning function
cleanMod <- function(x){
        x <- gsub(pattern = "a_", replacement = "A ", x)
        x <- gsub(pattern = "b_", replacement = "B ", x)
        x <- gsub(pattern = "_", replacement = " ", x)
        x <- gsub(pattern = "irst", replacement = "Iron and Steel", x)
        x <- gsub(pattern = "milex", replacement = "Military Expenditures", x)
        x <- gsub(pattern = "milper", replacement = "Military Personnel", x)
        x <- gsub(pattern = "pec", replacement = "Energy Consumption", x)
        x <- gsub(pattern = "tpop", replacement = "Total Population", x)
        x <- gsub(pattern = "upop", replacement = "Urban Population", x)
        x <- gsub(pattern = "lcinc", replacement = "CINC", x)
        x <- gsub(pattern = "polity2", replacement = "Polity2", x)
        x <- gsub(pattern = "conttype", replacement = "Contiguity", x)
        x <- gsub(pattern = "participants", replacement = "Participants", x)
        x <- gsub(pattern = "terriss", replacement = "Territorial Issue", x)
        x <- gsub(pattern = "riveriss", replacement = "River Issues", x)
        x <- gsub(pattern = "icowsalc", replacement = "ICOW Salience", x)
        #x <- gsub(pattern = "BatDeath_IHS", replacement = "Battle Deaths (IHS)", x)
}

cleanModRun <- function(x){
        x <- gsub(pattern = "a_", replacement = "A_", x)
        x <- gsub(pattern = "b_", replacement = "B_", x)
        x <- gsub(pattern = "irst", replacement = "Iron_Steel", x)
        x <- gsub(pattern = "milex", replacement = "Military_Expenditures", x)
        x <- gsub(pattern = "milper", replacement = "Military_Personnel", x)
        x <- gsub(pattern = "pec", replacement = "Energy_Consumption", x)
        x <- gsub(pattern = "tpop", replacement = "Total_Population", x)
        x <- gsub(pattern = "upop", replacement = "Urban_Population", x)
        x <- gsub(pattern = "lcinc", replacement = "CINC", x)
        x <- gsub(pattern = "polity2", replacement = "Polity2", x)
        x <- gsub(pattern = "conttype", replacement = "Contiguity", x)
        x <- gsub(pattern = "participants", replacement = "Participants", x)
        x <- gsub(pattern = "terriss", replacement = "Territorial_Issue", x)
        x <- gsub(pattern = "riveriss", replacement = "River_Issus", x)
        x <- gsub(pattern = "icowsalc", replacement = "ICOW_Salience", x)
        x <- gsub(pattern = "BatDeath_IHS", replacement = "Battle_Deaths_IHS", x)
}

rownames(tab)<-cleanMod(rownames(tab))
colnames(tab)<-c("Mean", "SD", "Median", "Min", "Max", "N")

latex(round(tab, 3), file="")

### load folds

error<-function(pred.Y, Y) {
        sqrt(sum((pred.Y-Y)^2)/nrow(Y))
}


### Regularization - Lasso and Ridge
dat<-dplyr::select(dat.approaches[[h]][[1]], -one_of(omit),
                     -a_ccode,
                     -b_ccode)

preObj<-preProcess(dat[,-c(1)], method=c("center", "scale"))
newdat<-data.table(dat[,c(1)], predict(preObj, dat[,-c(1)]))
colnames(newdat)<-cleanModRun(colnames(newdat))

library(glmnet)
x<-as.matrix(dplyr::select(newdat, -A_Battle_Deaths_IHS))
y<-as.matrix(dplyr::select(newdat, A_Battle_Deaths_IHS))


library(plotmo)
# lasso

lasso<-glmnet(x, y, alpha=1, family="gaussian")

cv.lasso<-cv.glmnet(x, y, alpha=1, family="gaussian")
coef.lasso<-coef(cv.lasso, s="lambda.1se")

plot(cv.lasso$glmnet.fit, label=T)

library(lars)
ridge<-glmnet(x, y, alpha=0, family="gaussian")

cv.ridge.wi<-cv.glmnet(x, y, alpha=0, family="gaussian", intercept=T)
cv.ridge.ni<-cv.glmnet(x, y, alpha=0, family="gaussian", intercept=F, lambda=cv.ridge.wi$lambda)

coef.ridge<-coef(cv.ridge.wi, s="lambda.1se")

plot(cv.ridge.wi$glmnet.fit, label=T)


# make
plot_glmnet(lasso, "lambda", label=10)
abline(v=log(cv.lasso$lambda.1se), lty=2)


plot_glmnet(ridge, "lambda",label=10)
abline(v=log(cv.ridge.ni$lambda.1se), lty=2)


# compare ols, ridge, and lasso coefficients
set.seed(1999)
ols <-lm(y~x)

coef.ols<-coef(ols)


# coefficients from OLS, ridge, lasso
coef.mat<-cbind(coef.ols, coef.lasso, coef.ridge)
colnames(coef.mat)<-c("OLS", "Lasso", "Ridge")
coef.mat<-round(coef.mat, 3)
coef.mat

rownames(coef.mat)<-cleanMod(rownames(coef.mat))

latex(coef.mat, file="")


### Linear Models to evalaute in-sample fit
regOut<-suppressWarnings(foreach(h=1:length(aggregate)) %do% {
        
        dat<-aggregate[[h]]
        
        # linear model with year polynomial
        reg.orig<-lm(a_BatDeath_IHS~a_irst+a_milex+a_milper+a_pec+a_tpop+a_upop+a_lcinc+a_polity2+b_irst+b_milex+b_milper+b_pec+b_tpop+b_upop+b_lcinc+b_polity2+conttype+participants+icowsalc+poly(Year, 3), data=dat)
        
        boots=1000
        
        coef1<-foreach(i=1:boots, .combine=rbind) %do% {
                boot<-sample(1:nrow(dat), replace=T)
                dat.boot<-dat[boot,]
                
                reg<-lm(a_BatDeath_IHS~a_irst+a_milex+a_milper+a_pec+a_tpop+a_upop+a_lcinc+a_polity2+b_irst+b_milex+b_milper+b_pec+b_tpop+b_upop+b_lcinc+b_polity2+conttype+participants+icowsalc+poly(Year, 3), data=dat.boot)
                coef(reg)
        }
        
        coef1<-coef1[,1:23]
        coef1.mean<-apply(coef1, 2, mean)
        coef1.lb<-apply(coef1, 2, quantile, probs=c(0.05, 0.95))[1,]
        coef1.ub<-apply(coef1, 2, quantile, probs=c(0.05, 0.95))[2,]
        
        output1<-data.frame(coef(reg.orig), coef1.lb, coef1.ub)
        
        
        # with year polynomial and country fixed effects
        reg1.orig<-lm(a_BatDeath_IHS~a_irst+a_milex+a_milper+a_pec+a_tpop+a_upop+a_lcinc+a_polity2+b_irst+b_milex+b_milper+b_pec+b_tpop+b_upop+b_lcinc+b_polity2+conttype+participants+icowsalc+poly(Year,3)+factor(a_ccode), data=dat)
        
        
        coef2<-foreach(i=1:boots, .combine=rbind) %do% {
                boot<-sample(1:nrow(dat), replace=T)
                dat.boot<-dat[boot,]
                
                reg1<-lm(a_BatDeath_IHS~a_irst+a_milex+a_milper+a_pec+a_tpop+a_upop+a_lcinc+a_polity2+b_irst+b_milex+b_milper+b_pec+b_tpop+b_upop+b_lcinc+b_polity2+conttype+participants+icowsalc+poly(Year,3)+factor(a_ccode), data=dat.boot)
                coef(reg1)
        }
        
        coef2<-coef2[,1:23]
        coef2.mean<-apply(coef2, 2, mean)
        coef2.lb<-apply(coef2, 2, quantile, probs=c(0.05, 0.95))[1,]
        coef2.ub<-apply(coef2, 2, quantile, probs=c(0.05, 0.95))[2,]
        
        output2<-data.frame(coef(reg1.orig)[1:23], coef2.lb, coef2.ub)
        
        reg.table<-round(cbind(output1, output2), 3)
        reg.table

})


# combine into an array and average across
# average across
regTable<-apply(abind(regOut, along=3), 1:2, mean)


rownames(regTable)<-cleanMod(rownames(regTable))
colnames(regTable)<-c("Coef", "LB", "UB", "Coef",  "LB", "UB")

library(Hmisc)
latex(regTable, file="")
save(regTable, file="Output/regTable.Rdata")




##### Decision Tree
dat_ny<-dplyr::select(dat.approaches[[h]][[1]], -one_of(omit),
                   -a_ccode,
                   -b_ccode,
                   -Year)

dat_y<-dplyr::select(dat.approaches[[h]][[1]], -one_of(omit),
                      -a_ccode,
                      -b_ccode)

# fit carts to these datasets
tune_control<-trainControl(method="repeatedcv",
                           n=5, repeats=5,
                           predictionBounds = c(0, NA),
                           savePredictions="final")

#
colnames(dat_y)<-cleanModRun(colnames(dat_y))
set.seed(1999)
cart_y<-train(A_Battle_Deaths_IHS~.,
              data=dat_y,
              method="rpart2",
              tuneLength=20,
              trControl=tune_control)

cart_ny<-train(a_BatDeath_IHS~.,
                   data=dat_ny,
                   method="rpart2",
                   trControl=tune_control)


# 
library(rpart.plot)
prp(cart_y$finalModel, main="CART")

fancyRpartPlot(cart_y$finalModel, sub="", palettes=c("Greys"), tweak=0.65, cex=.85)
fancyRpartPlot(cart_ny$finalModel, sub="")




### Partial dependence plots
load("par_aggregate.Rdata")

dat<-dplyr::select(dat.approaches[[h]][[1]], -one_of(omit),
                   -a_ccode,
                   -b_ccode)

dat_y<-dat%>%
        mutate(a_polity2=replace(a_polity2, a_polity2< -10, -10)) %>%
        mutate(b_polity2=replace(b_polity2, b_polity2< -10, -10))

set.seed(1999)
rf_y<-rfsrc(a_BatDeath_IHS~., 
            data=dat_y, 
            mtry=10, 
            tree.err=T)

plot(gg_vimp(rf_y))
gg_v_year<-gg_variable(rf_y)

colnames(gg_v_year)<-cleanMod(colnames(gg_v_year))
plot(gg_v_year, panel=T,
     k=5,
     notch=T)
ggsave("Figures/partial.pdf")




######## Variable Importance
### OLS
# grab the variables
dat<-dplyr::select(aggregate[[1]], -one_of(omit),
                        -a_ccode,
                        -b_ccode)

dat_vars<-dplyr::select(aggregate[[1]], -one_of(omit),
                   -a_ccode,
                   -b_ccode,
                   -a_BatDeath_IHS)

vars<-colnames(dat_vars)

# these are the variables to add in, leaving the standard set out
# Leave a standard set of Year, A CINC, and B CINC
foo<-seq(1, length(vars))

# set the tuning
tune_control<-trainControl(method="repeatedcv", n=5, predictionBounds = c(0, NA), savePredictions="final", allowParallel=T)

# iterate over variables, bootstrap for CIs
# register parallel backend
cl <- makeCluster(7)
registerDoParallel(cl)

permVars_ols<-
        foreach(i=1:length(foo),.combine=rbind, .packages=c('dplyr', 'foreach', 'caret')) %:% 
        foreach(j=1:1000, .combine=rbind, .packages=c('dplyr', 'foreach', 'caret')) %dopar% {
                
                form<-as.formula(paste("a_BatDeath_IHS~", paste(vars[foo[i]], collapse="+")))
                
                boot<-sample(1:nrow(dat), replace=T)
                dat.boot<-dat[boot,]
                
                olsvar<-train(form,
                              data=dat.boot,
                              method="lm",
                              trControl=tune_control)
                
                # make data.frame here
                tabVars<-data.frame(Variable=vars[foo[i]],
                                    RMSE=olsvar$results[2])
                
                tabVars
        }

# extract
permTab<-permVars_ols %>%
        dplyr::group_by(Variable = as.character(cleanMod(Variable))) %>%
        dplyr::summarise(mean = mean(RMSE),
                         se = sd(RMSE)) %>%
        
        ungroup()


# order
olsTab<-permTab[order(-permTab$mean), , drop=F]
save(olsTab, file="Output/olsTab.Rdata")
load("Output/olsTab.Rdata")


# plot
y.plot<-seq(1, nrow(olsTab))
bar<-data.frame(olsTab, y.plot)
rm(y.plot)

# Null
form<-as.formula(paste("a_BatDeath_IHS~", paste(vars[c(1,8,16)], collapse="+")))

null<-train(a_BatDeath_IHS~1,
              data=dat,
              method="lm",
              trControl=tune_control)

base<-train(a_BatDeath_IHS~.,
            data=dat,
            method="lm",
            trControl=tune_control)

rmse.null<-null$results[2]
rmse.base<-base$results[2]


# plot in ggplot
olsPlot<-ggplot(bar, aes(x=mean, y=y.plot))+
        geom_point(size=1.5)+
        geom_errorbarh(aes(xmin=mean-se, xmax=mean+se), alpha=0.5, height=0, lwd=0.75) +
        coord_cartesian(xlim=c(3.25, 4),ylim=c(1,length(foo)))+
        scale_y_continuous(breaks = pretty(bar$y.plot, n = length(foo)), labels=bar$Variable)+        
        labs(x="RMSE")+
        labs(y="")+
        ggtitle("OLS")+
        geom_vline(xintercept = rmse.null$RMSE, "x")+
        theme_bw()
olsPlot

### Repeat for Cubist
load("par_aggregate.Rdata")
cub_tune<-expand.grid(.committees=10, .neighbors=9)

# set tune control
tune_control<-trainControl(method="cv", n=5,
                           predictionBounds = c(0, NA),
                           savePredictions="final",
                           allowParallel=T)

# register parallel backend
cl <- makeCluster(7)
registerDoParallel(cl)

# Repeat for Random Forests
permVars_cub<-
        foreach(j=1:1000, .combine=rbind, .packages=c('dplyr', 'foreach', 'caret')) %dopar% {
                
                boot<-sample(1:nrow(dat), replace=T)
                dat.boot<-dat[boot,]
                
                cubModel_boots<-train(a_BatDeath_IHS~., 
                                      data=dat.boot,  
                                      method="cubist", 
                                      trControl=tune_control, 
                                      tuneGrid = cub_tune,
                                      parallel=T)
                
                hold<-data.frame(variable=rownames(varImp(cubModel_boots, scale=F)$importance),
                                 importance=varImp(cubModel_boots, scale=F)$importance)
                hold
                                 
        }

permTab<-permVars_cub %>%
        dplyr::group_by(Variable = as.character(cleanMod(variable))) %>%
        dplyr::summarise(mean = mean(Overall),
                         se = sd(Overall)) %>%
        ungroup()

cubTab<-permTab[order(permTab$mean), , drop=F]
save(cubTab, file="Output/cubTab.Rdata")

load("Output/cubTab.Rdata")
# plot
y.plot<-seq(1, nrow(cubTab))
bar<-data.frame(cubTab, y.plot)
rm(y.plot)

# plot in ggplot
cubPlot<-ggplot(bar, aes(x=mean, y=y.plot))+
        geom_point(size=1.5)+
        geom_errorbarh(aes(xmin=mean-se, xmax=mean+se), alpha=0.5, height=0, lwd=0.75) +
        coord_cartesian(xlim=c(0, 100),ylim=c(1,length(foo)))+
        scale_y_continuous(breaks = pretty(bar$y.plot, n = length(foo)), labels=bar$Variable)+        
        labs(x="Importance")+
        labs(y="")+
        ggtitle("Cubist")+
        geom_vline(xintercept = 0, "x")+
        theme_bw()
cubPlot


### Random forest
load("par_aggregate.Rdata")
rf_tune<-expand.grid(.mtry=10)

# set tune control
tune_control<-trainControl(method="cv", n=5,
                           predictionBounds = c(0, NA),
                           savePredictions="final",
                           allowParallel=T)

# register parallel backend
cl <- makeCluster(7)
registerDoParallel(cl)

# Repeat for Random Forests
permVars_rf<-
        foreach(j=1:1000, .combine=rbind, .packages=c('dplyr', 'foreach', 'caret')) %dopar% {
                
                boot<-sample(1:nrow(dat), replace=T)
                dat.boot<-dat[boot,]
                
                rf_boots<-rfsrc(a_BatDeath_IHS~., 
                                data=dat.boot, 
                                mtry=10, 
                                tree.err=T)
                
                hold<-gg_vimp(rf_boots)
                
                hold<-data.frame(variable=hold$vars,
                                 importance=hold$vimp)
                
        }

# extract
permTab<-permVars_rf %>%
        dplyr::group_by(Variable = as.character(cleanMod(vars))) %>%
        dplyr::summarise(mean = mean(vimp),
                         se = sd(vimp)) %>%
        ungroup()

rfTab<-permTab[order(permTab$mean), , drop=F]
save(rfTab, file="Output/rfTab.Rdata")

load("Output/rfTab.Rdata")

# plot
y.plot<-seq(1, nrow(rfTab))
bar<-data.frame(rfTab, y.plot)
rm(y.plot)

# plot in ggplot
rfPlot<-ggplot(bar, aes(x=mean, y=y.plot))+
        geom_point(size=1.5)+
        geom_errorbarh(aes(xmin=mean-se, xmax=mean+se), alpha=0.5, height=0, lwd=0.75) +
        coord_cartesian(xlim=c(0, 100),ylim=c(1,length(foo)))+
        scale_y_continuous(breaks = pretty(bar$y.plot, n = length(foo)), labels=bar$Variable)+        
        labs(x="Importance")+
        labs(y="")+
        ggtitle("Random Forest")+
        geom_vline(xintercept = 0, "x")+
        theme_bw()
rfPlot


# combine into one plot
grid.arrange(olsPlot, cubPlot, rfPlot, ncol=1)

