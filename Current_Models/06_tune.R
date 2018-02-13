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
library(Hmisc)

#Send yourself an email - specify your preferred email address, subject, and message. The password is fixed at "rmail".
#send_message(mime(from="phil.henrickson@gmail.com", to="phil.henrickson@gmail.com", subject="Code Finished", "Woo!"))

#setwd("C:/Users/Phil/Dropbox/")
setwd("F:/Dropbox/FSU/FSU_Fall_2017/Paper_1/")

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

# variables to omit
omit<-c("a_BatDeath", "Duration", "icowsal", "dyad_ccode_year", "mariss", "terriss")


# set tuning
ctrlParallel<-trainControl(method="cv", 
                           n=5, 
                           predictionBounds = c(0, NA), 
                           savePredictions="final",
                           allowParallel=T)



# iterating over all imputations
# iterating over all modeling approaches
# iterating over all folds

# register parallel backend
numCore <- detectCores() - 2
registerDoParallel(cores = numCore)


# selecting each fold as the validation set in turn
# tuning, then predicting the evaluation set
tune_imputations<-suppressWarnings(foreach(h=1:length(strong), .packages=c('dplyr', 'foreach', 'caret')) %do% {
         
        #### Strong
        dat<-dplyr::select(strong[[h]], -one_of(omit))
        
        # Split data into five folds, keeping sample proportions
        
        set.seed(1999)
        folds <- createFolds(dat$a_BatDeath_IHS, k = 5, list = TRUE, returnTrain = FALSE)
        iter<-list(c(folds$Fold1),  c(folds$Fold2), c(folds$Fold3), c(folds$Fold4), c(folds$Fold5))
        
        out_strong<-suppressWarnings(foreach(i=1:length(iter), .packages=c('dplyr', 'foreach', 'caret')) %do% {
                
                eval<-dat[iter[[i]],]
                train<-dat[-iter[[i]],]
                
                x.eval<-dplyr::select(eval, -a_BatDeath_IHS)
                y.eval<-dplyr::select(eval, a_BatDeath_IHS)
                
                # nested cross validation
                # Null
                set.seed(1999)
                strong_null<-train(a_BatDeath_IHS~1, 
                                   data=dplyr::select(train, -a_ccode, -b_ccode), 
                                   method="lm",
                                   trControl=ctrlParallel)
                strong_null$method<-"Null"
                
                # null_cinc
                set.seed(1999)
                strong_cinc<-train(a_BatDeath_IHS~a_lcinc+b_lcinc+Year, 
                                   data=dplyr::select(train, -a_ccode, -b_ccode), 
                                   method="lm",
                                   trControl=ctrlParallel)
                strong_cinc$method<-"CINC+Year"
                
                # Linear regression
                set.seed(1999)
                strong_ols<-train(a_BatDeath_IHS~., 
                                  data=dplyr::select(train, -a_ccode, -b_ccode), 
                                  method="lm",
                                  trControl=ctrlParallel)
                strong_ols$method<-"OLS"
                
                # PLS
                set.seed(1999)
                strong_pls<-train(a_BatDeath_IHS~., 
                                  data=dplyr::select(train, -a_ccode, -b_ccode), 
                                  method="pls",
                                  trControl=ctrlParallel,
                                  tuneLength=10,
                                  preProcess=c("center", "scale"))
                strong_pls$method<-"PLS"
                
                # MARs
                set.seed(1999)
                strong_mars<-train(a_BatDeath_IHS~., 
                                  data=dplyr::select(train, -a_ccode, -b_ccode), 
                                  method="earth",
                                  trControl=ctrlParallel,
                                  tuneLength=10,
                                  preProcess=c("center", "scale"))
                strong_mars$method<-"MARS"
                
                # Elastic Net
                set.seed(1999)
                strong_enet<-train(a_BatDeath_IHS~., 
                                   data=dplyr::select(train, -a_ccode, -b_ccode), 
                                   method="enet",
                                   trControl=ctrlParallel,
                                   tuneLength=10,
                                   preProcess=c("center", "scale"))
                strong_enet$method<-"Elastic Net"
                
                # KNN
                set.seed(1999)
                strong_knn<-train(a_BatDeath_IHS~., 
                                  data=dplyr::select(train, -a_ccode, -b_ccode), 
                                  method="knn", 
                                  trControl=ctrlParallel,
                                  tuneLength=10,
                                  preProcess=c("center", "scale"))
                strong_knn$method<-"KNN"
                
                # CART
                set.seed(1999)
                strong_cart<-train(a_BatDeath_IHS~., 
                                   data=dplyr::select(train, -a_ccode, -b_ccode), 
                                   method="rpart",
                                   tuneLength=10,
                                   trControl=ctrlParallel)
                strong_cart$method<-"CART"
                
                # Random Forest
                set.seed(1999)
                strong_rf<-train(a_BatDeath_IHS~., 
                                 data=dplyr::select(train, -a_ccode, -b_ccode), 
                                 method="rf",
                                 tuneLength=10, 
                                 trControl=ctrlParallel)
                strong_rf$method<-"Random Forest"
                
                # Boosted Tree
                set.seed(1999)
                strong_gbm<-train(a_BatDeath_IHS~.,
                                  data=dplyr::select(train, -a_ccode, -b_ccode), 
                                  method="gbm",
                                  tuneLength=10,
                                  trControl=ctrlParallel, 
                                  verbose=F)
                strong_gbm$method<-"Boosted Trees"
                
                # Cubist
                set.seed(1999)
                strong_cub<-train(a_BatDeath_IHS~., 
                                  data=dplyr::select(train, -a_ccode, -b_ccode), 
                                  method="cubist",
                                  tuneLength=10,
                                  trControl=ctrlParallel)
                strong_cub$method<-"Cubist"
                
                # SVM r
                set.seed(1999)
                strong_svm<-train(a_BatDeath_IHS~.,
                                  data=dplyr::select(train, -a_ccode, -b_ccode), 
                                  trControl=ctrlParallel,
                                  method="svmRadial",
                                  tuneLength=10,
                                  preProcess=c("center", "scale"))
                strong_svm$method<-"SVM Radial"
                
                # aNN
                set.seed(1999)
                strong_nnet<-train(a_BatDeath_IHS~.,
                                   data=dplyr::select(train, -a_ccode, -b_ccode), 
                                   trControl=ctrlParallel,
                                   tuneLength=10,
                                   method="avNNet",
                                   preProcess=c("center", "scale"),
                                   linout=1,
                                   trace=F)
                strong_nnet$method<-"Neural Nets"
                
                # Preliminary strong
                models_strong<-lapply(ls(pattern="strong_"), get)
                
                # wipe
                # wipe the individual models to save memory
                rm(list=ls(pattern="strong_"))
                
                trainPred <- as.tbl(
                       foreach(j = 1:length(models_strong), .combine = cbind.data.frame) %do% {
                               
                               foo <- as.tbl(models_strong[[j]]$pred %>% 
                                       arrange(rowIndex) %>% 
                                       dplyr::select(pred))
                               names(foo)<-models_strong[[j]]$method
                               foo
                })
                
                # grab predictions
                evalPred<-as.tbl(
                        foreach(j=1:length(models_strong), .combine = cbind.data.frame) %do% {
                                pred<-data.frame(predict.train(models_strong[[j]], x.eval))
                                colnames(pred)<-models_strong[[j]]$method
                                pred
                        })
                
                # rmse for the candidate learners
                rmseEval<-apply(as.matrix(evalPred), 2, error, y.eval)
                
                # tune
                par<-foreach(j=1:length(models_strong)) %do% {
                        par<-models_strong[[j]]$bestTune
                        par
                }
                
                names(par)<-foreach(j=1:length(models_strong)) %do%
                        models_strong[[j]]$method
                
                # store:
                # rmse
                # predictions
                # tuning paramters
                
                out<-list("rmse"=rmseEval,
                          "tune"=par)
                
                out
        })
        
        #### Average
        dat<-dplyr::select(average[[h]], -one_of(omit))
        
        # Split data into five folds, keeping sample proportions
        
        set.seed(1999)
        folds <- createFolds(dat$a_BatDeath_IHS, k = 5, list = TRUE, returnTrain = FALSE)
        iter<-list(c(folds$Fold1),  c(folds$Fold2), c(folds$Fold3), c(folds$Fold4), c(folds$Fold5))
        
        out_average<-suppressWarnings(foreach(i=1:length(iter), .packages=c('dplyr', 'foreach', 'caret')) %do% {
                
                eval<-dat[iter[[i]],]
                train<-dat[-iter[[i]],]
                
                x.eval<-dplyr::select(eval, -a_BatDeath_IHS)
                y.eval<-dplyr::select(eval, a_BatDeath_IHS)
                
                # nested cross validation
                # Null
                set.seed(1999)
                average_null<-train(a_BatDeath_IHS~1, 
                                   data=dplyr::select(train, -a_ccode, -b_ccode), 
                                   method="lm",
                                   trControl=ctrlParallel)
                average_null$method<-"Null"
                
                # null_cinc
                set.seed(1999)
                average_cinc<-train(a_BatDeath_IHS~a_lcinc+b_lcinc+Year, 
                                   data=dplyr::select(train, -a_ccode, -b_ccode), 
                                   method="lm",
                                   trControl=ctrlParallel)
                average_cinc$method<-"CINC+Year"
                
                # Linear regression
                set.seed(1999)
                average_ols<-train(a_BatDeath_IHS~., 
                                  data=dplyr::select(train, -a_ccode, -b_ccode), 
                                  method="lm",
                                  trControl=ctrlParallel)
                average_ols$method<-"OLS"
                
                # PLS
                set.seed(1999)
                average_pls<-train(a_BatDeath_IHS~., 
                                  data=dplyr::select(train, -a_ccode, -b_ccode), 
                                  method="pls",
                                  trControl=ctrlParallel,
                                  tuneLength=10,
                                  preProcess=c("center", "scale"))
                average_pls$method<-"PLS"
                
                # MARs
                set.seed(1999)
                average_mars<-train(a_BatDeath_IHS~., 
                                   data=dplyr::select(train, -a_ccode, -b_ccode), 
                                   method="earth",
                                   trControl=ctrlParallel,
                                   tuneLength=10,
                                   preProcess=c("center", "scale"))
                average_mars$method<-"MARS"
                
                # Elastic Net
                set.seed(1999)
                average_enet<-train(a_BatDeath_IHS~., 
                                   data=dplyr::select(train, -a_ccode, -b_ccode), 
                                   method="enet",
                                   trControl=ctrlParallel,
                                   tuneLength=10,
                                   preProcess=c("center", "scale"))
                average_enet$method<-"Elastic Net"
                
                # KNN
                set.seed(1999)
                average_knn<-train(a_BatDeath_IHS~., 
                                  data=dplyr::select(train, -a_ccode, -b_ccode), 
                                  method="knn", 
                                  trControl=ctrlParallel,
                                  tuneLength=10,
                                  preProcess=c("center", "scale"))
                average_knn$method<-"KNN"
                
                # CART
                set.seed(1999)
                average_cart<-train(a_BatDeath_IHS~., 
                                   data=dplyr::select(train, -a_ccode, -b_ccode), 
                                   method="rpart",
                                   tuneLength=10,
                                   trControl=ctrlParallel)
                average_cart$method<-"CART"
                
                # Random Forest
                set.seed(1999)
                average_rf<-train(a_BatDeath_IHS~., 
                                 data=dplyr::select(train, -a_ccode, -b_ccode), 
                                 method="rf",
                                 tuneLength=10, 
                                 trControl=ctrlParallel)
                average_rf$method<-"Random Forest"
                
                # Boosted Tree
                set.seed(1999)
                average_gbm<-train(a_BatDeath_IHS~.,
                                  data=dplyr::select(train, -a_ccode, -b_ccode), 
                                  method="gbm",
                                  tuneLength=10,
                                  trControl=ctrlParallel, 
                                  verbose=F)
                average_gbm$method<-"Boosted Trees"
                
                # Cubist
                set.seed(1999)
                average_cub<-train(a_BatDeath_IHS~., 
                                  data=dplyr::select(train, -a_ccode, -b_ccode), 
                                  method="cubist",
                                  tuneLength=10,
                                  trControl=ctrlParallel)
                average_cub$method<-"Cubist"
                
                # SVM r
                set.seed(1999)
                average_svm<-train(a_BatDeath_IHS~.,
                                  data=dplyr::select(train, -a_ccode, -b_ccode), 
                                  trControl=ctrlParallel,
                                  method="svmRadial",
                                  tuneLength=10,
                                  preProcess=c("center", "scale"))
                average_svm$method<-"SVM Radial"
                
                # aNN
                set.seed(1999)
                average_nnet<-train(a_BatDeath_IHS~.,
                                   data=dplyr::select(train, -a_ccode, -b_ccode), 
                                   trControl=ctrlParallel,
                                   tuneLength=10,
                                   method="avNNet",
                                   preProcess=c("center", "scale"),
                                   linout=1,
                                   trace=F)
                average_nnet$method<-"Neural Nets"
                
                # Preliminary average
                models_average<-lapply(ls(pattern="average_"), get)
                
                # wipe
                rm(list=ls(pattern="average_"))
                
                
                trainPred <- as.tbl(
                        foreach(j = 1:length(models_average), .combine = cbind.data.frame) %do% {
                                
                                foo <- as.tbl(models_average[[j]]$pred %>% 
                                                      arrange(rowIndex) %>% 
                                                      dplyr::select(pred))
                                names(foo)<-models_average[[j]]$method
                                foo
                        })
                
                # grab predictions
                evalPred<-as.tbl(
                        foreach(j=1:length(models_average), .combine = cbind.data.frame) %do% {
                                pred<-data.frame(predict.train(models_average[[j]], x.eval))
                                colnames(pred)<-models_average[[j]]$method
                                pred
                        })
                
                # rmse for the candidate learners
                rmseEval<-apply(as.matrix(evalPred), 2, error, y.eval)
                
                # tune
                par<-foreach(j=1:length(models_average)) %do% {
                        par<-models_average[[j]]$bestTune
                        par
                }
                
                names(par)<-foreach(j=1:length(models_average)) %do%
                        models_average[[j]]$method
                
                # store:
                # rmse
                # predictions
                # tuning paramters
                
                out<-list("rmse"=rmseEval,
                          "tune"=par)
                
                out
        })
        
        #### Aggregate
        dat<-dplyr::select(aggregate[[h]], -one_of(omit))
        
        # Split data into five folds, keeping sample proportions
        
        set.seed(1999)
        folds <- createFolds(dat$a_BatDeath_IHS, k = 5, list = TRUE, returnTrain = FALSE)
        iter<-list(c(folds$Fold1),  c(folds$Fold2), c(folds$Fold3), c(folds$Fold4), c(folds$Fold5))
        
        out_aggregate<-suppressWarnings(foreach(i=1:length(iter), .packages=c('dplyr', 'foreach', 'caret')) %do% {
                
                eval<-dat[iter[[i]],]
                train<-dat[-iter[[i]],]
                
                x.eval<-dplyr::select(eval, -a_BatDeath_IHS)
                y.eval<-dplyr::select(eval, a_BatDeath_IHS)
                
                # nested cross validation
                # Null
                set.seed(1999)
                aggregate_null<-train(a_BatDeath_IHS~1, 
                                    data=dplyr::select(train, -a_ccode, -b_ccode), 
                                    method="lm",
                                    trControl=ctrlParallel)
                aggregate_null$method<-"Null"
                
                # null_cinc
                set.seed(1999)
                aggregate_cinc<-train(a_BatDeath_IHS~a_lcinc+b_lcinc+Year, 
                                    data=dplyr::select(train, -a_ccode, -b_ccode), 
                                    method="lm",
                                    trControl=ctrlParallel)
                aggregate_cinc$method<-"CINC+Year"
                
                # Linear regression
                set.seed(1999)
                aggregate_ols<-train(a_BatDeath_IHS~., 
                                   data=dplyr::select(train, -a_ccode, -b_ccode), 
                                   method="lm",
                                   trControl=ctrlParallel)
                aggregate_ols$method<-"OLS"
                
                # PLS
                set.seed(1999)
                aggregate_pls<-train(a_BatDeath_IHS~., 
                                   data=dplyr::select(train, -a_ccode, -b_ccode), 
                                   method="pls",
                                   trControl=ctrlParallel,
                                   tuneLength=10,
                                   preProcess=c("center", "scale"))
                aggregate_pls$method<-"PLS"
                
                # MARs
                set.seed(1999)
                aggregate_mars<-train(a_BatDeath_IHS~., 
                                    data=dplyr::select(train, -a_ccode, -b_ccode), 
                                    method="earth",
                                    trControl=ctrlParallel,
                                    tuneLength=10,
                                    preProcess=c("center", "scale"))
                aggregate_mars$method<-"MARS"
                
                # Elastic Net
                set.seed(1999)
                aggregate_enet<-train(a_BatDeath_IHS~., 
                                    data=dplyr::select(train, -a_ccode, -b_ccode), 
                                    method="enet",
                                    trControl=ctrlParallel,
                                    tuneLength=10,
                                    preProcess=c("center", "scale"))
                aggregate_enet$method<-"Elastic Net"
                
                # KNN
                set.seed(1999)
                aggregate_knn<-train(a_BatDeath_IHS~., 
                                   data=dplyr::select(train, -a_ccode, -b_ccode), 
                                   method="knn", 
                                   trControl=ctrlParallel,
                                   tuneLength=10,
                                   preProcess=c("center", "scale"))
                aggregate_knn$method<-"KNN"
                
                # CART
                set.seed(1999)
                aggregate_cart<-train(a_BatDeath_IHS~., 
                                    data=dplyr::select(train, -a_ccode, -b_ccode), 
                                    method="rpart",
                                    tuneLength=10,
                                    trControl=ctrlParallel)
                aggregate_cart$method<-"CART"
                
                # Random Forest
                set.seed(1999)
                aggregate_rf<-train(a_BatDeath_IHS~., 
                                  data=dplyr::select(train, -a_ccode, -b_ccode), 
                                  method="rf",
                                  tuneLength=10, 
                                  trControl=ctrlParallel)
                aggregate_rf$method<-"Random Forest"
                
                # Boosted Tree
                set.seed(1999)
                aggregate_gbm<-train(a_BatDeath_IHS~.,
                                   data=dplyr::select(train, -a_ccode, -b_ccode), 
                                   method="gbm",
                                   tuneLength=10,
                                   trControl=ctrlParallel, 
                                   verbose=F)
                aggregate_gbm$method<-"Boosted Trees"
                
                # Cubist
                set.seed(1999)
                aggregate_cub<-train(a_BatDeath_IHS~., 
                                   data=dplyr::select(train, -a_ccode, -b_ccode), 
                                   method="cubist",
                                   tuneLength=10,
                                   trControl=ctrlParallel)
                aggregate_cub$method<-"Cubist"
                
                # SVM r
                set.seed(1999)
                aggregate_svm<-train(a_BatDeath_IHS~.,
                                   data=dplyr::select(train, -a_ccode, -b_ccode), 
                                   trControl=ctrlParallel,
                                   method="svmRadial",
                                   tuneLength=10,
                                   preProcess=c("center", "scale"))
                aggregate_svm$method<-"SVM Radial"
                
                # aNN
                set.seed(1999)
                aggregate_nnet<-train(a_BatDeath_IHS~.,
                                    data=dplyr::select(train, -a_ccode, -b_ccode), 
                                    trControl=ctrlParallel,
                                    tuneLength=10,
                                    method="avNNet",
                                    preProcess=c("center", "scale"),
                                    linout=1,
                                    trace=F)
                aggregate_nnet$method<-"Neural Nets"
                
                # Preliminary aggregate
                models_aggregate<-lapply(ls(pattern="aggregate_"), get)
                
                # wipe
                rm(list=ls(pattern="aggregate_"))
                
                trainPred <- as.tbl(
                        foreach(j = 1:length(models_aggregate), .combine = cbind.data.frame) %do% {
                                
                                foo <- as.tbl(models_aggregate[[j]]$pred %>% 
                                                      arrange(rowIndex) %>% 
                                                      dplyr::select(pred))
                                names(foo)<-models_aggregate[[j]]$method
                                foo
                        })
                
                # grab predictions
                evalPred<-as.tbl(
                        foreach(j=1:length(models_aggregate), .combine = cbind.data.frame) %do% {
                                pred<-data.frame(predict.train(models_aggregate[[j]], x.eval))
                                colnames(pred)<-models_aggregate[[j]]$method
                                pred
                        })
                
                # rmse for the candidate learners
                rmseEval<-apply(as.matrix(evalPred), 2, error, y.eval)
                
                # tune
                par<-foreach(j=1:length(models_aggregate)) %do% {
                        par<-models_aggregate[[j]]$bestTune
                        par
                }
                
                names(par)<-foreach(j=1:length(models_aggregate)) %do%
                        models_aggregate[[j]]$method
                
                # store:
                # rmse
                # predictions
                # tuning paramters
                
                out<-list("rmse"=rmseEval,
                          "tune"=par)
                
                out
        })
        
        # Make table storing performance for all three modeling approaches
        # should be a N X 6 matrix of rMSE for all N models
        rmse_strong<-cbind(apply(do.call(cbind, lapply(out_strong, '[[', 'rmse')), 1, mean), apply(do.call(cbind, lapply(out_strong, '[[', 'rmse')), 1, sd))
        rmse_average<-cbind(apply(do.call(cbind, lapply(out_average, '[[', 'rmse')), 1, mean), apply(do.call(cbind, lapply(out_average, '[[', 'rmse')), 1, sd))
        rmse_aggregate<-cbind(apply(do.call(cbind, lapply(out_aggregate, '[[', 'rmse')), 1, mean), apply(do.call(cbind, lapply(out_aggregate, '[[', 'rmse')), 1, sd))
        
        rmse_imputation<-cbind.data.frame(rmse_strong, 
                                          rmse_average,
                                          rmse_aggregate)
        
        colnames(rmse_imputation)<-c("Strong", "SD", "Average","SD", "Aggregate", "SD")
        
        # extract tuning parameters for each model -> this is really inefficient
        par_strong<-lapply(out_strong, '[[', 'tune')
        par_strong<-foreach(i=1:length(rownames(rmse_strong))) %do% {
                do.call(rbind, lapply(par_strong, '[[', rownames(rmse_strong)[i]))
        }
        names(par_strong)<-rownames(rmse_strong)
        
        par_average<-lapply(out_average, '[[', 'tune')
        par_average<-foreach(i=1:length(rownames(rmse_average))) %do% {
                do.call(rbind, lapply(par_average, '[[', rownames(rmse_average)[i]))
        }
        names(par_average)<-rownames(rmse_average)
        
        
        par_aggregate<-lapply(out_aggregate, '[[', 'tune')
        par_aggregate<-foreach(i=1:length(rownames(rmse_aggregate))) %do% {
                do.call(rbind, lapply(par_aggregate, '[[', rownames(rmse_aggregate)[i]))
        }
        names(par_aggregate)<-rownames(rmse_aggregate)
        
        # store that table, and then the tuning parameters in a list
        imputation_out<-list("rmse"=rmse_imputation,
                             "par_strong"=par_strong, 
                             "par_average"=par_average, 
                             "par_aggregate"=par_aggregate)
        
        imputation_out
        
})

save(tune_imputations, file="tune_imputations.Rdata")

load("tune_imputations.Rdata")

# Extract performance
library(abind)
rmse_imputations<-apply(abind(lapply(tune_imputations, '[[', 'rmse'), along=3), 1:2, mean)
save(rmse_imputations, file="rmse_imputations.Rdata")
load("rmse_imputations.Rdata")

# make table and arrange
rmse_table<-as.data.frame(rmse_imputations)

rmse_table<-rmse_results[order(-down_results$LL), , drop = FALSE]

toOut<-rmse_table[order(-rmse_table$Strong), , drop = FALSE]
colnames(toOut)<-c("RMSE", "SD", "RMSE", "SD", "RMSE", "SD")


library(Hmisc)
latex(round(toOut,3), file="")

# make null table
null<-toOut[1,1]

nullTab<-cbind((toOut[,1]-null)/-null, (toOut[,3]-null)/-null, (toOut[,5]-null)/-null)
rownames(nullTab)<-rownames(toOut)

latex(round(nullTab[-1,], 3), file="")





# Extract tuning parameters
# strong
par_strong<-lapply(tune_imputations, '[[', 'par_strong')
par_strong<-foreach(i=1:length(rownames(rmse_table))) %do% {
        do.call(rbind, lapply(par_strong, '[[', rownames(rmse_table)[i]))
}
names(par_strong)<-rownames(rmse_table)
save(par_strong, file="par_strong.Rdata")

# average
par_average<-lapply(tune_imputations, '[[', 'par_average')
par_average<-foreach(i=1:length(rownames(rmse_table))) %do% {
        do.call(rbind, lapply(par_average, '[[', rownames(rmse_table)[i]))
}
names(par_average)<-rownames(rmse_table)
save(par_average, file="par_average.Rdata")

# aggregate
par_aggregate<-lapply(tune_imputations, '[[', 'par_aggregate')
par_aggregate<-foreach(i=1:length(rownames(rmse_table))) %do% {
        do.call(rbind, lapply(par_aggregate, '[[', rownames(rmse_table)[i]))
}
names(par_aggregate)<-rownames(rmse_table)
save(par_aggregate, file="par_aggregate.Rdata")


# Keep the tuning parameters for the eventual ensemble


















# Evaluate stability of tuning parameters
# candidate tuning parameters
par.strong<-lapply(tune.strong, '[', 'tune')

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

for(i in 1: length(par.strong)){
        fold<-par.strong[[i]]
        
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

final.strong<-suppressWarnings(foreach(i=1:length(iter), .packages=c('dplyr', 'foreach', 'caret')) %dopar%  {
        
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
                                 trControl=ctrlParallel)
                
                # Null cinc
                set.seed(1999)
                
                nullcincModel<-train(a_BatDeath_IHS~a_lcinc+b_lcinc+Year, 
                                     data=train, 
                                     method="lm",
                                     trControl=ctrlParallel)
                
                # 2 Linear regression
                set.seed(1999)
                
                olsModel<-train(a_BatDeath_IHS~., 
                                data=train,
                                method="lm",
                                trControl=ctrlParallel)
                
                # 3 PLS
                set.seed(1999)
                
                plsModel<-train(a_BatDeath_IHS~., 
                                data=train,
                                method="pls",
                                trControl=ctrlParallel,
                                preProcess=c("center", "scale"))
                
                # 4 Ridge
                set.seed(1999)
                
                ridgeModel<-train(a_BatDeath_IHS~., 
                                  data=train,
                                  method="ridge",
                                  trControl=ctrlParallel,
                                  preProcess=c("center", "scale"))
                
                # 5 Lasso
                set.seed(1999)
                
                lassoModel<-train(a_BatDeath_IHS~., 
                                  data=train,
                                  method="lasso",
                                  trControl=ctrlParallel,
                                  preProcess=c("center", "scale"))
                
                # 6 Elastic Net
                set.seed(1999)
                
                enetModel<-train(a_BatDeath_IHS~., 
                                 data=train,
                                 method="enet",
                                 trControl=ctrlParallel,
                                 preProcess=c("center", "scale"))
                
                # 7 KNN
                set.seed(1999)
                
                knnModel<-train(a_BatDeath_IHS~., 
                                data=train, 
                                method="knn", 
                                trControl=ctrlParallel, 
                                preProcess=c("center", "scale"))
                
                # 8 CART tuning over max depth
                set.seed(1999)
                
                cartModel<-train(a_BatDeath_IHS~., 
                                 data=train, 
                                 method="rpart2",
                                 tuneGrid=cartTune[[i]])
                
                # 9 Random Forest
                set.seed(1999)
                
                rfModel<-train(a_BatDeath_IHS~., 
                               data=train, 
                               method="rf", 
                               trControl=ctrlParallel, 
                               tuneGrid=rfTune[[i]])
                
                # 9 Boosted Tree
                set.seed(1999)
                
                bstModel<-train(a_BatDeath_IHS~.,
                                data=train,
                                method="gbm",
                                tuneGrid=bstTune[[i]],
                                trControl=ctrlParallel)
                
                # 11 Cubist
                set.seed(1999)
                
                cubModel<-train(a_BatDeath_IHS~., 
                                data=train,  
                                method="cubist", 
                                trControl=ctrlParallel, 
                                tuneGrid = cubTune[[i]])
                
                # 12 SVM r
                set.seed(1999)
                
                svmModel<-train(a_BatDeath_IHS~.,
                                data=train,
                                trControl=ctrlParallel,
                                method="svmRadial",
                                tuneGrid=svmTune[[i]],
                                preProcess=c("center", "scale"))
                
                # 13 aNN
                set.seed(1999)
                
                nnetModel<-train(a_BatDeath_IHS~.,
                                 data=train,
                                 trControl=ctrlParallel,
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
        
        
        rmse_strong<-apply(do.call(cbind, lapply(out_strong, '[[', 'rmse')), 1, mean)
        rmse_average<-apply(do.call(cbind, lapply(out_strong, '[[', 'rmse')), 1, mean)
        rmse_aggregate<-apply(do.call(cbind, lapply(out_strong, '[[', 'rmse')), 1, mean)
        
        rmse_out<-cbind.data.frame(rmse_strong,
                                   rmse_average,
                                   rmse_aggregate)
        
        colnames(rmse_out)<-c("Strong", "Average", "Aggregate")
        
        
        
        rmse.avg<-apply(rmse.par, 1, mean)
        
        rmse.sd<-apply(rmse.par, 1, sd)
        
        out<-list("RMSE"=rmse.avg, "SD"=rmse.sd)
        
        out
})

# extract relevant info
rmse.strong<-do.call(cbind, lapply(final.strong, '[[', 'RMSE'))
sd.strong<-do.call(cbind, lapply(final.strong, '[[', 'SD'))


rmse.tune<-rmse.strong[4:14,]
sd.tune<-sd.strong[4:14,]

#

#get the tuning parameters for each model which minimizes mean rmse
par.strong<-foreach(i=1:nrow(rmse.tune)) %do%{
        num<-which(rmse.tune[i,]==min(rmse.tune[i,]))
        unlist(tune.list[[i]][num][1])
}

names(par.strong)<-model.names[-c(1,2, 3)]

# get the min RMSE for each learner
rmse.min<-foreach(i=1:nrow(rmse.tune), .combine=rbind) %do%{
        a<-rmse.tune[i, which(rmse.tune[i,]==min(rmse.tune[i,]))][1]
        b<-sd.tune[i, which(rmse.tune[i,]==min(rmse.tune[i,]))][1]
        
        unlist(c(a, b))
}

table.strong<-rbind(cbind(rmse.strong[1:3,1], sd.strong[1:3,1]), rmse.min)
rownames(table.strong)<-model.names

# final tuning and performance parameters for each model
save(par.strong, file="Output/par.strong.Rdata")
save(table.strong, file="Output/table.strong.Rdata")


# average opponent criterion
dat<-average.trim

# Split data into five folds
set.seed(1999)
folds <- createFolds(dat$a_BatDeath_IHS, k = 5, list = TRUE, returnTrain = FALSE)
iter<-list(c(folds$Fold1),  c(folds$Fold2), c(folds$Fold3), c(folds$Fold4), c(folds$Fold5))

# set tuning
ctrlParallel<-trainControl(method="cv", n=5, predictionBounds = c(0, NA), savePredictions="final", allowParallel=T)

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
tune.average<-suppressWarnings(foreach(i=1:length(iter), .packages=c('dplyr', 'foreach', 'caret')) %dopar% {
        
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
                         trControl=ctrlParallel)
        
        # null_cinc
        set.seed(1999)
        
        nullcincModel<-train(a_BatDeath_IHS~a_lcinc+b_lcinc+Year, 
                             data=train, 
                             method="lm",
                             trControl=ctrlParallel)
        
        # 2 Linear regression
        set.seed(1999)
        
        olsModel<-train(a_BatDeath_IHS~., 
                        data=train,
                        method="lm",
                        trControl=ctrlParallel)
        
        # 3 PLS
        set.seed(1999)
        
        plsModel<-train(a_BatDeath_IHS~., 
                        data=train,
                        method="pls",
                        trControl=ctrlParallel,
                        tuneGrid=plsGrid,
                        preProcess=c("center", "scale"))
        
        # 4 Ridge
        set.seed(1999)
        
        ridgeModel<-train(a_BatDeath_IHS~., 
                          data=train,
                          method="ridge",
                          trControl=ctrlParallel,
                          tuneGrid=ridgeGrid,
                          preProcess=c("center", "scale"))
        
        # 5 Lasso
        set.seed(1999)
        
        lassoModel<-train(a_BatDeath_IHS~., 
                          data=train,
                          method="lasso",
                          trControl=ctrlParallel,
                          tuneGrid=lassoGrid,
                          preProcess=c("center", "scale"))
        
        # 6 Elastic Net
        set.seed(1999)
        
        enetModel<-train(a_BatDeath_IHS~., 
                         data=train,
                         method="enet",
                         trControl=ctrlParallel,
                         tuneGrid=enetGrid,
                         preProcess=c("center", "scale"))
        
        # 7 KNN
        set.seed(1999)
        
        knnModel<-train(a_BatDeath_IHS~., 
                        data=train, 
                        method="knn", 
                        tuneGrid=knnGrid, 
                        trControl=ctrlParallel,
                        preProcess=c("center", "scale"))
        
        # 8 CART tuning over max depth
        set.seed(1999)
        
        cartModel<-train(a_BatDeath_IHS~., 
                         data=train, 
                         method="rpart2",
                         trControl=ctrlParallel, 
                         tuneGrid=cartGrid)
        
        # 9 Random Forest
        set.seed(1999)
        
        rfModel<-train(a_BatDeath_IHS~., 
                       data=train, 
                       method="rf", 
                       trControl=ctrlParallel, 
                       tuneGrid=rfGrid,
                       importance=T)
        
        # 10 Boosted Tree
        set.seed(1999)
        
        bstModel<-train(a_BatDeath_IHS~.,
                        data=train,
                        method="gbm",
                        tuneGrid=bstGrid,
                        trControl=ctrlParallel, 
                        verbose=F)
        
        # 11 Cubist
        set.seed(1999)
        
        cubModel<-train(a_BatDeath_IHS~., 
                        data=train,  
                        method="cubist", 
                        trControl=ctrlParallel, 
                        tuneGrid = cubGrid)
        
        # 12 SVM r
        set.seed(1999)
        
        svmModel<-train(a_BatDeath_IHS~.,
                        data=train,
                        trControl=ctrlParallel,
                        method="svmRadial",
                        tuneGrid=svmGrid,
                        preProcess=c("center", "scale"))
        
        # 13 aNN
        set.seed(1999)
        
        nnetModel<-train(a_BatDeath_IHS~.,
                         data=train,
                         trControl=ctrlParallel,
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

save(tune.average, file="Output/tune.average.Rdata")

load("Output/tune.average.Rdata")


# Evaluate performance



# Evaluate stability of tuning parameters
# candidate tuning parameters
par.average<-lapply(tune.average, '[', 'tune')

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

for(i in 1: length(par.average)){
        fold<-par.average[[i]]
        
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

final.average<-suppressWarnings(foreach(i=1:length(iter), .packages=c('dplyr', 'foreach', 'caret')) %dopar%  {
        
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
                                 trControl=ctrlParallel)
                
                # Null cinc
                set.seed(1999)
                
                nullcincModel<-train(a_BatDeath_IHS~a_lcinc+b_lcinc+Year, 
                                     data=train, 
                                     method="lm",
                                     trControl=ctrlParallel)
                
                # 2 Linear regression
                set.seed(1999)
                
                olsModel<-train(a_BatDeath_IHS~., 
                                data=train,
                                method="lm",
                                trControl=ctrlParallel)
                
                # 3 PLS
                set.seed(1999)
                
                plsModel<-train(a_BatDeath_IHS~., 
                                data=train,
                                method="pls",
                                trControl=ctrlParallel,
                                tuneGrid=plsTune[[i]],
                                preProcess=c("center", "scale"))
                
                # 4 Ridge
                set.seed(1999)
                
                ridgeModel<-train(a_BatDeath_IHS~., 
                                  data=train,
                                  method="ridge",
                                  trControl=ctrlParallel,
                                  tuneGrid=ridgeTune[[i]],
                                  preProcess=c("center", "scale"))
                
                # 5 Lasso
                set.seed(1999)
                
                lassoModel<-train(a_BatDeath_IHS~., 
                                  data=train,
                                  method="lasso",
                                  trControl=ctrlParallel,
                                  tuneGrid=lassoTune[[i]],
                                  preProcess=c("center", "scale"))
                
                # 6 Elastic Net
                set.seed(1999)
                
                enetModel<-train(a_BatDeath_IHS~., 
                                 data=train,
                                 method="enet",
                                 trControl=ctrlParallel,
                                 tuneGrid=enetTune[[i]],
                                 preProcess=c("center", "scale"))
                
                # 7 KNN
                set.seed(1999)
                
                knnModel<-train(a_BatDeath_IHS~., 
                                data=train, 
                                method="knn", 
                                tuneGrid=knnTune[[i]], 
                                trControl=ctrlParallel, 
                                preProcess=c("center", "scale"))
                
                # 8 CART tuning over max depth
                set.seed(1999)
                
                cartModel<-train(a_BatDeath_IHS~., 
                                 data=train, 
                                 method="rpart2",
                                 trControl=ctrlParallel, 
                                 tuneGrid=cartTune[[i]])
                
                # 9 Random Forest
                set.seed(1999)
                
                rfModel<-train(a_BatDeath_IHS~., 
                               data=train, 
                               method="rf", 
                               trControl=ctrlParallel, 
                               tuneGrid=rfTune[[i]])
                
                # 9 Boosted Tree
                set.seed(1999)
                
                bstModel<-train(a_BatDeath_IHS~.,
                                data=train,
                                method="gbm",
                                tuneGrid=bstTune[[i]],
                                trControl=ctrlParallel)
                
                # 11 Cubist
                set.seed(1999)
                
                cubModel<-train(a_BatDeath_IHS~., 
                                data=train,  
                                method="cubist", 
                                trControl=ctrlParallel, 
                                tuneGrid = cubTune[[i]])
                
                # 12 SVM r
                set.seed(1999)
                
                svmModel<-train(a_BatDeath_IHS~.,
                                data=train,
                                trControl=ctrlParallel,
                                method="svmRadial",
                                tuneGrid=svmTune[[i]],
                                preProcess=c("center", "scale"))
                
                # 13 aNN
                set.seed(1999)
                
                nnetModel<-train(a_BatDeath_IHS~.,
                                 data=train,
                                 trControl=ctrlParallel,
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
rmse.average<-do.call(cbind, lapply(final.average, '[[', 'RMSE'))
sd.average<-do.call(cbind, lapply(final.average, '[[', 'SD'))


rmse.tune<-rmse.average[4:14,]
sd.tune<-sd.average[4:14,]

#

#get the tuning parameters for each model which minimizes mean rmse
par.average<-foreach(i=1:nrow(rmse.tune)) %do%{
        num<-which(rmse.tune[i,]==min(rmse.tune[i,]))
        unlist(tune.list[[i]][num][1])
}

names(par.average)<-model.names[-c(1,2, 3)]

# get the min RMSE for each learner
rmse.min<-foreach(i=1:nrow(rmse.tune), .combine=rbind) %do%{
        a<-rmse.tune[i, which(rmse.tune[i,]==min(rmse.tune[i,]))][1]
        b<-sd.tune[i, which(rmse.tune[i,]==min(rmse.tune[i,]))][1]
        
        unlist(c(a, b))
}

table.average<-rbind(cbind(rmse.average[1:3,1], sd.average[1:3,1]), rmse.min)
rownames(table.average)<-model.names

# final tuning and performance parameters for each model
save(par.average, file="Output/par.average.Rdata")
save(table.average, file="Output/table.average.Rdata")






# aggregate opponent criterion
dat<-aggregate.trim

# Split data into five folds
set.seed(1999)
folds <- createFolds(dat$a_BatDeath_IHS, k = 5, list = TRUE, returnTrain = FALSE)
iter<-list(c(folds$Fold1),  c(folds$Fold2), c(folds$Fold3), c(folds$Fold4), c(folds$Fold5))

# set tuning
ctrlParallel<-trainControl(method="cv", n=5, predictionBounds = c(0, NA), savePredictions="final", allowParallel=T)

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
tune.aggregate<-suppressWarnings(foreach(i=1:length(iter), .packages=c('dplyr', 'foreach', 'caret')) %dopar% {
        
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
                         trControl=ctrlParallel)
        
        # null_cinc
        set.seed(1999)
        
        nullcincModel<-train(a_BatDeath_IHS~a_lcinc+b_lcinc+Year, 
                             data=train, 
                             method="lm",
                             trControl=ctrlParallel)
        
        # 2 Linear regression
        set.seed(1999)
        
        olsModel<-train(a_BatDeath_IHS~., 
                        data=train,
                        method="lm",
                        trControl=ctrlParallel)
        
        # 3 PLS
        set.seed(1999)
        
        plsModel<-train(a_BatDeath_IHS~., 
                        data=train,
                        method="pls",
                        trControl=ctrlParallel,
                        tuneGrid=plsGrid,
                        preProcess=c("center", "scale"))
        
        # 4 Ridge
        set.seed(1999)
        
        ridgeModel<-train(a_BatDeath_IHS~., 
                          data=train,
                          method="ridge",
                          trControl=ctrlParallel,
                          tuneGrid=ridgeGrid,
                          preProcess=c("center", "scale"))
        
        # 5 Lasso
        set.seed(1999)
        
        lassoModel<-train(a_BatDeath_IHS~., 
                          data=train,
                          method="lasso",
                          trControl=ctrlParallel,
                          tuneGrid=lassoGrid,
                          preProcess=c("center", "scale"))
        
        # 6 Elastic Net
        set.seed(1999)
        
        enetModel<-train(a_BatDeath_IHS~., 
                         data=train,
                         method="enet",
                         trControl=ctrlParallel,
                         tuneGrid=enetGrid,
                         preProcess=c("center", "scale"))
        
        # 7 KNN
        set.seed(1999)
        
        knnModel<-train(a_BatDeath_IHS~., 
                        data=train, 
                        method="knn", 
                        tuneGrid=knnGrid, 
                        trControl=ctrlParallel,
                        preProcess=c("center", "scale"))
        
        # 8 CART tuning over max depth
        set.seed(1999)
        
        cartModel<-train(a_BatDeath_IHS~., 
                         data=train, 
                         method="rpart2",
                         trControl=ctrlParallel, 
                         tuneGrid=cartGrid)
        
        # 9 Random Forest
        set.seed(1999)
        
        rfModel<-train(a_BatDeath_IHS~., 
                       data=train, 
                       method="rf", 
                       trControl=ctrlParallel, 
                       tuneGrid=rfGrid,
                       importance=T)
        
        # 10 Boosted Tree
        set.seed(1999)
        
        bstModel<-train(a_BatDeath_IHS~.,
                        data=train,
                        method="gbm",
                        tuneGrid=bstGrid,
                        trControl=ctrlParallel, 
                        verbose=F)
        
        # 11 Cubist
        set.seed(1999)
        
        cubModel<-train(a_BatDeath_IHS~., 
                        data=train,  
                        method="cubist", 
                        trControl=ctrlParallel, 
                        tuneGrid = cubGrid)
        
        # 12 SVM r
        set.seed(1999)
        
        svmModel<-train(a_BatDeath_IHS~.,
                        data=train,
                        trControl=ctrlParallel,
                        method="svmRadial",
                        tuneGrid=svmGrid,
                        preProcess=c("center", "scale"))
        
        # 13 aNN
        set.seed(1999)
        
        nnetModel<-train(a_BatDeath_IHS~.,
                         data=train,
                         trControl=ctrlParallel,
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

save(tune.aggregate, file="Output/tune.aggregate.Rdata")

load("Output/tune.aggregate.Rdata")


# Evaluate performance



# Evaluate stability of tuning parameters
# candidate tuning parameters
par.aggregate<-lapply(tune.aggregate, '[', 'tune')

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

for(i in 1: length(par.aggregate)){
        fold<-par.aggregate[[i]]
        
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

final.aggregate<-suppressWarnings(foreach(i=1:length(iter), .packages=c('dplyr', 'foreach', 'caret')) %dopar%  {
        
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
                                 trControl=ctrlParallel)
                
                # Null cinc
                set.seed(1999)
                
                nullcincModel<-train(a_BatDeath_IHS~a_lcinc+b_lcinc+Year, 
                                     data=train, 
                                     method="lm",
                                     trControl=ctrlParallel)
                
                # 2 Linear regression
                set.seed(1999)
                
                olsModel<-train(a_BatDeath_IHS~., 
                                data=train,
                                method="lm",
                                trControl=ctrlParallel)
                
                # 3 PLS
                set.seed(1999)
                
                plsModel<-train(a_BatDeath_IHS~., 
                                data=train,
                                method="pls",
                                trControl=ctrlParallel,
                                tuneGrid=plsTune[[i]],
                                preProcess=c("center", "scale"))
                
                # 4 Ridge
                set.seed(1999)
                
                ridgeModel<-train(a_BatDeath_IHS~., 
                                  data=train,
                                  method="ridge",
                                  trControl=ctrlParallel,
                                  tuneGrid=ridgeTune[[i]],
                                  preProcess=c("center", "scale"))
                
                # 5 Lasso
                set.seed(1999)
                
                lassoModel<-train(a_BatDeath_IHS~., 
                                  data=train,
                                  method="lasso",
                                  trControl=ctrlParallel,
                                  tuneGrid=lassoTune[[i]],
                                  preProcess=c("center", "scale"))
                
                # 6 Elastic Net
                set.seed(1999)
                
                enetModel<-train(a_BatDeath_IHS~., 
                                 data=train,
                                 method="enet",
                                 trControl=ctrlParallel,
                                 tuneGrid=enetTune[[i]],
                                 preProcess=c("center", "scale"))
                
                # 7 KNN
                set.seed(1999)
                
                knnModel<-train(a_BatDeath_IHS~., 
                                data=train, 
                                method="knn", 
                                tuneGrid=knnTune[[i]], 
                                trControl=ctrlParallel, 
                                preProcess=c("center", "scale"))
                
                # 8 CART tuning over max depth
                set.seed(1999)
                
                cartModel<-train(a_BatDeath_IHS~., 
                                 data=train, 
                                 method="rpart2",
                                 trControl=ctrlParallel, 
                                 tuneGrid=cartTune[[i]])
                
                # 9 Random Forest
                set.seed(1999)
                
                rfModel<-train(a_BatDeath_IHS~., 
                               data=train, 
                               method="rf", 
                               trControl=ctrlParallel, 
                               tuneGrid=rfTune[[i]])
                
                # 9 Boosted Tree
                set.seed(1999)
                
                bstModel<-train(a_BatDeath_IHS~.,
                                data=train,
                                method="gbm",
                                tuneGrid=bstTune[[i]],
                                trControl=ctrlParallel)
                
                # 11 Cubist
                set.seed(1999)
                
                cubModel<-train(a_BatDeath_IHS~., 
                                data=train,  
                                method="cubist", 
                                trControl=ctrlParallel, 
                                tuneGrid = cubTune[[i]])
                
                # 12 SVM r
                set.seed(1999)
                
                svmModel<-train(a_BatDeath_IHS~.,
                                data=train,
                                trControl=ctrlParallel,
                                method="svmRadial",
                                tuneGrid=svmTune[[i]],
                                preProcess=c("center", "scale"))
                
                # 13 aNN
                set.seed(1999)
                
                nnetModel<-train(a_BatDeath_IHS~.,
                                 data=train,
                                 trControl=ctrlParallel,
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
rmse.aggregate<-do.call(cbind, lapply(final.aggregate, '[[', 'RMSE'))
sd.aggregate<-do.call(cbind, lapply(final.aggregate, '[[', 'SD'))


rmse.tune<-rmse.aggregate[4:14,]
sd.tune<-sd.aggregate[4:14,]

#

#get the tuning parameters for each model which minimizes mean rmse
par.aggregate<-foreach(i=1:nrow(rmse.tune)) %do%{
        num<-which(rmse.tune[i,]==min(rmse.tune[i,]))
        unlist(tune.list[[i]][num][1])
}

names(par.aggregate)<-model.names[-c(1,2, 3)]

# get the min RMSE for each learner
rmse.min<-foreach(i=1:nrow(rmse.tune), .combine=rbind) %do%{
        a<-rmse.tune[i, which(rmse.tune[i,]==min(rmse.tune[i,]))][1]
        b<-sd.tune[i, which(rmse.tune[i,]==min(rmse.tune[i,]))][1]
        
        unlist(c(a, b))
}

table.aggregate<-rbind(cbind(rmse.aggregate[1:3,1], sd.aggregate[1:3,1]), rmse.min)
rownames(table.aggregate)<-model.names

# final tuning and performance parameters for each model
save(par.aggregate, file="Output/par.aggregate.Rdata")
save(table.aggregate, file="Output/table.aggregate.Rdata")



#
load("Output/table.strong.Rdata")
load("Output/table.average.Rdata")
load("Output/table.aggregate.Rdata")

library(Hmisc)
table.rmse<-round(cbind(table.strong, table.average, table.aggregate), 3)
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
colnames(table.rmse)<-c(rep(c("RMSE", "SD"), 3))

rownames(table.rmse)<-table.names
save(table.rmse, file="Output/table.rmse.Rdata")

load("Output/table.rmse.Rdata")

table.rmse

# compare to null
table.null<-round(cbind((table.rmse[1,1]-table.rmse[,1][-1])/table.rmse[1,1], (table.rmse[1,1]-table.rmse[,3][-1])/table.rmse[1,1], (table.rmse[1,1]-table.rmse[,5][-1])/table.rmse[1,1]),3)
save(table.null, file="Output/table.null.Rdata")

table.rmse
table.null


# spit to latex
latex(table.rmse, file="")
latex(table.null, file="")






