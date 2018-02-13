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
library(e1071)
library(Metrics)

#Send yourself an email - specify your preferred email address, subject, and message. The password is fixed at "rmail".
#send_message(mime(from="phil.henrickson@gmail.com", to="phil.henrickson@gmail.com", subject="Code Finished", "Woo!"))

#setwd("C:/Users/Phil/Dropbox/")
setwd("F:/Dropbox/FSU/FSU_Fall_2017/Paper_1/")

load("Final_Data/allies1.RData")

# select features needed for modelling
allies.trim<-dplyr::select(allies1,a_BatDeath_IHS, Year, a_irst, a_milex, a_milper, a_pec, a_tpop, a_upop, a_lcinc, a_polity2,
                           a_ally_irst, a_ally_milex, a_ally_milper , a_ally_pec, a_ally_tpop, a_ally_upop, a_ally_lcinc, a_ally_polity2,
                           b_irst, b_milex, b_milper, b_pec, b_tpop, b_upop, b_lcinc, b_polity2, conttype, participants, terriss, riveriss, mariss, icowsalc)

# calculate test error

# RMSE
error<-function(pred.Y, Y) {
        sqrt(sum((pred.Y-Y)^2)/nrow(Y))
}

# strong opponent criterion
dat<-allies.trim

# Split data into five folds
set.seed(1999)
folds <- createFolds(dat$a_BatDeath_IHS, k = 5, list = TRUE, returnTrain = FALSE)
iter<-list(c(folds$Fold1),  c(folds$Fold2), c(folds$Fold3), c(folds$Fold4), c(folds$Fold5))

# set tuning
tune_control<-trainControl(method="cv", n=5, predictionBounds = c(0, NA), savePredictions="final", allowParallel=T)

# set tuning grids for each of the classifiers; these are selected based on initial runs
# of the learners in a separate script
plsGrid<-expand.grid(.ncomp=seq(1,16))
ridgeGrid<-expand.grid(.lambda=seq(0, 1, .02))
lassoGrid<-expand.grid(.fraction=seq(0, 1, .02))
enetGrid<-expand.grid(.fraction=c(0.5, 0.8, 1), .lambda=seq(0, 1, .02))
knnGrid<-expand.grid(.k=seq(1, 17))
cartGrid<-expand.grid(.maxdepth=1:17)
rfGrid<-expand.grid(.mtry=seq(2,17, 2))
bstGrid<-expand.grid(interaction.depth=c(1, 3, 5), n.trees = (0:50)*50,
                     shrinkage=c(0.01, 0.001),
                     n.minobsinnode=10)
svmGrid<-expand.grid(.C=c(0.25, 0.5, 0.75, 0.9, 1), .sigma=c(0.001, 0.005, 0.01))
cubGrid<-expand.grid(.committees = c(10, 25, 50, 100), .neighbors = c(0,3, 5, 9))
nnetGrid<-expand.grid(.decay = c(0.01, 0.1, 0.5, 0.9), .size = c(1, 2, 5), .bag=T)

model.names<-c("null", "null_cinc", "ols","pls", "ridge", "lasso", "enet", "knn", "cart", "rf", "bsttree", "cub", "svm", "nnet")


# register parallel backend
# 
cl <- makeCluster(7)
registerDoParallel(cl)

# selecting each fold as the test set
# then tuning via 
tune.allies<-suppressWarnings(foreach(i=1:length(iter), .packages=c('dplyr', 'foreach', 'caret')) %dopar% {
        
        test<-dat[iter[[i]],]
        train<-dat[-iter[[i]],]
        
        x.test<-dplyr::select(test, -a_BatDeath_IHS)
        y.test<-dplyr::select(test, a_BatDeath_IHS)
        
        # nested cross validation
        # 1 Null
        set.seed(1999)
        
        nullModel<-train(a_BatDeath_IHS~1, 
                         data=train, 
                         method="lm",
                         trControl=tune_control)
        
        # null_cinc
        set.seed(1999)
        
        nullcincModel<-train(a_BatDeath_IHS~a_lcinc+b_lcinc+Year, 
                         data=train, 
                         method="lm",
                         trControl=tune_control)
        
        # 2 Linear regression
        set.seed(1999)
        
        olsModel<-train(a_BatDeath_IHS~., 
                        data=train,
                        method="lm",
                        trControl=tune_control)
        
        # 3 PLS
        set.seed(1999)
        
        plsModel<-train(a_BatDeath_IHS~., 
                        data=train,
                        method="pls",
                        trControl=tune_control,
                        tuneGrid=plsGrid,
                        preProcess=c("center", "scale"))
        
        # 4 Ridge
        set.seed(1999)
        
        ridgeModel<-train(a_BatDeath_IHS~., 
                          data=train,
                          method="ridge",
                          trControl=tune_control,
                          tuneGrid=ridgeGrid,
                          preProcess=c("center", "scale"))
        
        # 5 Lasso
        set.seed(1999)
        
        lassoModel<-train(a_BatDeath_IHS~., 
                          data=train,
                          method="lasso",
                          trControl=tune_control,
                          tuneGrid=lassoGrid,
                          preProcess=c("center", "scale"))
        
        # 6 Elastic Net
        set.seed(1999)
        
        enetModel<-train(a_BatDeath_IHS~., 
                         data=train,
                         method="enet",
                         trControl=tune_control,
                         tuneGrid=enetGrid,
                         preProcess=c("center", "scale"))
        
        # 7 KNN
        set.seed(1999)
        
        knnModel<-train(a_BatDeath_IHS~., 
                        data=train, 
                        method="knn", 
                        tuneGrid=knnGrid, 
                        trControl=tune_control,
                        preProcess=c("center", "scale"))
        
        # 8 CART tuning over max depth
        set.seed(1999)
        
        cartModel<-train(a_BatDeath_IHS~., 
                         data=train, 
                         method="rpart2",
                         trControl=tune_control, 
                         tuneGrid=cartGrid)
        
        # 9 Random Forest
        set.seed(1999)
        
        rfModel<-train(a_BatDeath_IHS~., 
                       data=train, 
                       method="rf", 
                       trControl=tune_control, 
                       tuneGrid=rfGrid,
                       importance=T)
        
        # 10 Boosted Tree
        set.seed(1999)
        
        bstModel<-train(a_BatDeath_IHS~.,
                        data=train,
                        method="gbm",
                        tuneGrid=bstGrid,
                        trControl=tune_control, 
                        verbose=F)
        
        # 11 Cubist
        set.seed(1999)
        
        cubModel<-train(a_BatDeath_IHS~., 
                        data=train,  
                        method="cubist", 
                        trControl=tune_control, 
                        tuneGrid = cubGrid)
        
        # 12 SVM r
        set.seed(1999)
        
        svmModel<-train(a_BatDeath_IHS~.,
                        data=train,
                        trControl=tune_control,
                        method="svmRadial",
                        tuneGrid=svmGrid,
                        preProcess=c("center", "scale"))
        
        # 13 aNN
        set.seed(1999)
        
        nnetModel<-train(a_BatDeath_IHS~.,
                         data=train,
                         trControl=tune_control,
                         tuneGrid=nnetGrid,
                         method="avNNet",
                         preProcess=c("center", "scale"),
                         linout=1,
                         trace=F)
        
        # Preliminary Models
        model_fold<-list(nullModel,
                         nullcincModel,# 1
                     olsModel, # 2
                     plsModel,  # 3
                     ridgeModel, # 4
                     lassoModel, # 5
                     enetModel, # 6
                     knnModel,  # 7
                     cartModel, # 8
                     rfModel, # 9
                     bstModel, # 10
                     cubModel, # 11
                     svmModel, # 12
                     nnetModel) # 13
        
        
        # grab predictions
        pred.fold<-foreach(j=1:length(model_fold)) %do%
                predict.train(model_fold[[j]], x.test)
        
        pred.mat<-as.matrix(do.call(cbind, pred.fold))
        
        # rmse for the candidate learners
        rmse.learners<-foreach(j=1:length(model_fold)) %do%
                error(pred.mat[,j], y.test)
        
        rmse.mat<-do.call(rbind, rmse.learners)
        
        # tune
        par<-foreach(j=1:length(model_fold)) %do%
                model_fold[[j]]$bestTune
        
        #
        out<-list("rmse"=rmse.mat, "pred"=pred.mat, "observed"=y.test, "tune"=par)
        
        out
})

save(tune.allies, file="Output/tune.allies.Rdata")

load("Output/tune.allies.Rdata")


# Evaluate performance



# Evaluate stability of tuning parameters
# candidate tuning parameters
par.allies<-lapply(tune.allies, '[', 'tune')

# for storing
plsTune<-list()
ridgeTune<-list()
lassoTune<-list()
enetTune<-list()
knnTune<-list()
cartTune<-list()
rfTune<-list()
bstTune<-list()
cubTune<-list()
svmTune<-list()
nnetTune<-list()

for(i in 1: length(par.allies)){
        fold<-par.allies[[i]]
        
        plsTune[[i]]<-fold$tune[[4]]
        ridgeTune[[i]]<-fold$tune[[5]]
        lassoTune[[i]]<-fold$tune[[6]]
        enetTune[[i]]<-fold$tune[[7]]
        knnTune[[i]]<-fold$tune[[8]]
        cartTune[[i]]<-fold$tune[[9]]
        rfTune[[i]]<-fold$tune[[10]]
        bstTune[[i]]<-fold$tune[[11]]
        cubTune[[i]]<-fold$tune[[12]]
        svmTune[[i]]<-fold$tune[[13]]
        nnetTune[[i]]<-fold$tune[[14]]

}

tune.list<-list(plsTune, ridgeTune, lassoTune, enetTune, knnTune, cartTune, rfTune, bstTune, cubTune, svmTune, nnetTune)

# get rmse for each tune over all of the folds for each model with 
# the selected tuning parameters

final.allies<-suppressWarnings(foreach(i=1:length(iter), .packages=c('dplyr', 'foreach', 'caret')) %dopar%  {

        tune.fold<-foreach(j=1:length(folds)) %do% {
                test<-dat[iter[[j]],]
                train<-dat[-iter[[j]],]
                
                x.test<-dplyr::select(test, -a_BatDeath_IHS)
                y.test<-dplyr::select(test, a_BatDeath_IHS)
                
                set.seed(1999)
                
                # 1 Null
                nullModel<-train(a_BatDeath_IHS~1, 
                                 data=train, 
                                 method="lm", 
                                 trControl=tune_control)
                
                # Null cinc
                set.seed(1999)
                
                nullcincModel<-train(a_BatDeath_IHS~a_lcinc+b_lcinc+Year, 
                                     data=train, 
                                     method="lm",
                                     trControl=tune_control)
                
                # 2 Linear regression
                set.seed(1999)
                
                olsModel<-train(a_BatDeath_IHS~., 
                                data=train,
                                method="lm",
                                trControl=tune_control)
                
                # 3 PLS
                set.seed(1999)
                
                plsModel<-train(a_BatDeath_IHS~., 
                                data=train,
                                method="pls",
                                trControl=tune_control,
                                tuneGrid=plsTune[[i]],
                                preProcess=c("center", "scale"))
                
                # 4 Ridge
                set.seed(1999)
                
                ridgeModel<-train(a_BatDeath_IHS~., 
                                  data=train,
                                  method="ridge",
                                  trControl=tune_control,
                                  tuneGrid=ridgeTune[[i]],
                                  preProcess=c("center", "scale"))
                
                # 5 Lasso
                set.seed(1999)
                
                lassoModel<-train(a_BatDeath_IHS~., 
                                  data=train,
                                  method="lasso",
                                  trControl=tune_control,
                                  tuneGrid=lassoTune[[i]],
                                  preProcess=c("center", "scale"))
                
                # 6 Elastic Net
                set.seed(1999)
                
                enetModel<-train(a_BatDeath_IHS~., 
                                 data=train,
                                 method="enet",
                                 trControl=tune_control,
                                 tuneGrid=enetTune[[i]],
                                 preProcess=c("center", "scale"))
                
                # 7 KNN
                set.seed(1999)
                
                knnModel<-train(a_BatDeath_IHS~., 
                                data=train, 
                                method="knn", 
                                tuneGrid=knnTune[[i]], 
                                trControl=tune_control, 
                                preProcess=c("center", "scale"))
                
                # 8 CART tuning over max depth
                set.seed(1999)
                
                cartModel<-train(a_BatDeath_IHS~., 
                                 data=train, 
                                 method="rpart2",
                                 trControl=tune_control, 
                                 tuneGrid=cartTune[[i]])
                
                # 9 Random Forest
                set.seed(1999)
                
                rfModel<-train(a_BatDeath_IHS~., 
                               data=train, 
                               method="rf", 
                               trControl=tune_control, 
                               tuneGrid=rfTune[[i]])
        
                # 9 Boosted Tree
                set.seed(1999)
                
                bstModel<-train(a_BatDeath_IHS~.,
                                data=train,
                                method="gbm",
                                tuneGrid=bstTune[[i]],
                                trControl=tune_control)
                
                # 11 Cubist
                set.seed(1999)
                
                cubModel<-train(a_BatDeath_IHS~., 
                                data=train,  
                                method="cubist", 
                                trControl=tune_control, 
                                tuneGrid = cubTune[[i]])
                
                # 12 SVM r
                set.seed(1999)
                
                svmModel<-train(a_BatDeath_IHS~.,
                                data=train,
                                trControl=tune_control,
                                method="svmRadial",
                                tuneGrid=svmTune[[i]],
                                preProcess=c("center", "scale"))
                
                # 13 aNN
                set.seed(1999)
                
                nnetModel<-train(a_BatDeath_IHS~.,
                                 data=train,
                                 trControl=tune_control,
                                 tuneGrid=nnetTune[[i]],
                                 method="avNNet",
                                 preProcess=c("center", "scale"),
                                 linout=1,
                                 trace=F)
                
                # Preliminary Models
                model_fold<-list(nullModel,
                                 nullcincModel,# 1
                                 olsModel, # 2
                                 plsModel,  # 3
                                 ridgeModel, # 4
                                 lassoModel, # 5
                                 enetModel, # 6
                                 knnModel,  # 7
                                 cartModel, # 8
                                 rfModel, # 9
                                 bstModel, # 10
                                 cubModel, # 11
                                 svmModel, # 12
                                 nnetModel) # 13
                
        
                # grab predictions
                pred.fold<-foreach(k=1:length(model_fold)) %do%
                        predict.train(model_fold[[k]], x.test)
                
                pred.mat<-as.matrix(do.call(cbind, pred.fold))
                
                # rmse for the candidate learners
                rmse.learners<-foreach(k=1:length(model_fold)) %do%
                        error(pred.mat[,k], y.test)
                
                rmse.mat<-do.call(rbind, rmse.learners)
                
                #
                rmse.mat
                
                
        }
        
        rmse.par<-do.call(cbind, tune.fold)
        
        rmse.avg<-apply(rmse.par, 1, mean)
        
        rmse.sd<-apply(rmse.par, 1, sd)
        
        out<-list("RMSE"=rmse.avg, "SD"=rmse.sd)
        
        out
})

# extract relevant info
rmse.allies<-do.call(cbind, lapply(final.allies, '[[', 'RMSE'))
sd.allies<-do.call(cbind, lapply(final.allies, '[[', 'SD'))


rmse.tune<-rmse.allies[4:14,]
sd.tune<-sd.allies[4:14,]

#

#get the tuning parameters for each model which minimizes mean rmse
par.allies<-foreach(i=1:nrow(rmse.tune)) %do%{
        num<-which(rmse.tune[i,]==min(rmse.tune[i,]))
        unlist(tune.list[[i]][num][1])
}

names(par.allies)<-model.names[-c(1,2, 3)]

# get the min RMSE for each learner
rmse.min<-foreach(i=1:nrow(rmse.tune), .combine=rbind) %do%{
        a<-rmse.tune[i, which(rmse.tune[i,]==min(rmse.tune[i,]))][1]
        b<-sd.tune[i, which(rmse.tune[i,]==min(rmse.tune[i,]))][1]
        
        unlist(c(a, b))
}

table.allies<-rbind(cbind(rmse.allies[1:3,1], sd.allies[1:3,1]), rmse.min)
rownames(table.allies)<-model.names
        
# final tuning and performance parameters for each model
save(par.allies, file="Output/par.allies.Rdata")
save(table.allies, file="Output/table.allies.Rdata")


#
load("Output/table.allies.Rdata")


library(Hmisc)
table.rmse<-round(cbind(table.allies), 3)
table.names<-c("Null",
               "CINC+Year",
               "OLS", 
               "PLS", 
               "Ridge", 
               "Lasso", 
               "Elastic Net", 
               "KNN", 
               "CART", 
               "RF",
               "Boosted Trees",
               "Cubist",
               "SVM Radial",
               "Neural Networks")

rownames(table.rmse)<-table.names
colnames(table.rmse)<-c("RMSE", "SD")

rownames(table.rmse)<-table.names

table.allies<-table.rmse

save(table.allies, file="Output/table.allies.Rdata")
load("Output/table.allies.Rdata")



# compare to null
table.null<-round(cbind((table.rmse[1,c(1,3,5)]-table.rmse[,1][-1])/table.rmse[1,1], (table.rmse[1,c(1,3,5)]-table.rmse[,3][-1])/table.rmse[1,1], (table.rmse[1,c(1,3,5)]-table.rmse[,5][-1])/table.rmse[1,1]),3)


table.rmse
table.null


# spit to latex
latex(table.rmse, file="")
latex(table.null, file="")






