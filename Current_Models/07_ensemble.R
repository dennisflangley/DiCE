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
library(abind)

#Send yourself an email - specify your preferred email address, subject, and message. The password is fixed at "rmail".
#send_message(mime(from="phil.henrickson@gmail.com", to="phil.henrickson@gmail.com", subject="Code Finished", "Woo!"))

setwd("/Users/philhenrickson/Dropbox/FSU/FSU_Fall_2017/Paper_1/")
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


# load in previously selected tuning parameters
load("par_strong.Rdata")
load("par_average.RData")
load("par_aggregate.RData")


# create tuning grids
plsGrid<-expand.grid(.ncomp=seq(6, 10, 2))
marsGrid<-expand.grid(.nprune=seq(10, 26, 2), .degree=1)
knnGrid<-expand.grid(.k=seq(2, 12, 2))
cartGrid<-expand.grid(.maxdepth=seq(1, 14, 2))
enetGrid<-expand.grid(.fraction = seq(0.5, 1, 0.05), .lambda=seq(0, 0.05, 0.01))
rfGrid<-expand.grid(.mtry=seq(1, 16, 2))
cubGrid<-expand.grid(.committees=c(1, 10, 20), .neighbors=c(1, 5, 9))
gbmGrid<-expand.grid(.n.trees=c(50, 100, 150, 200),
                     .interaction.depth=seq(1, 10, 2),
                     .shrinkage=0.1,
                     .n.minobsinnode=10)
svmGrid<-expand.grid(.sigma=seq(0.01, 0.045, 0.005),
                     .C=seq(1, 5, 2))
nnetGrid<-expand.grid(.size=seq(1,13, 2),
                      .decay=seq(0, 0.1, 0.01),
                      .bag=F)


# set tuning
ctrlParallel<-trainControl(method="cv", 
                           n=5, 
                           predictionBounds = c(0, NA), 
                           savePredictions="all",
                           allowParallel=T)

# Function to train a model with a specific tuning parameter
# store the training predictions
# store the validation set predictions
# store the model
# then, iterate over this function for each validation set in an outer loop
# then, iterate over this function for all imputations in another outer loop

ModelsGrid<-function(modName, tune, trainData, validData, omit, preProcess) {
        
        modelsList<-foreach(i=1:nrow(tune)) %do% {
                
                if (modName=="avNNet"){
                        set.seed(1999)
                        model<-train(a_BatDeath_IHS~., 
                                     data=dplyr::select(trainData, -one_of(omit)), 
                                     method=modName,
                                     trControl=ctrlParallel,
                                     tuneGrid=as.data.frame(tune[i,]),
                                     preProcess=c("center", "scale"),
                                     linout=1,
                                     trace=F)
                        
                }else if(preProcess==T) {
                        set.seed(1999)
                        model<-train(a_BatDeath_IHS~., 
                                     data=dplyr::select(trainData, -one_of(omit)), 
                                     method=modName,
                                     trControl=ctrlParallel,
                                     tuneGrid=as.data.frame(tune[i,]),
                                     preProcess=c("center", "scale"))
                        
                }else{
                        set.seed(1999)
                        model<-train(a_BatDeath_IHS~., 
                                     data=dplyr::select(trainData, -one_of(omit)), 
                                     method=modName,
                                     trControl=ctrlParallel,
                                     tuneGrid=as.data.frame(tune[i,]))
                        
                }
                
                # grabbing out of sample predictions
                predOOS <- as.tbl(model$pred) %>% 
                        arrange(rowIndex) %>% 
                        dplyr::select(pred)
                
                tuneName<-unlist(Map(paste, names(tune[i,]), tune[i,], sep=""))
                names(predOOS)<-paste(modName, paste(tuneName, collapse=" "))
                
                
                # grabbing validation set predictions
                predEval<-as.data.frame(predict.train(model,
                                                      newdata=validData))
                
                names(predEval)<-paste(modName, paste(tuneName, collapse=" "))
                
                out<-list("predOOS"=predOOS,
                          "predEval"=predEval)
                # "tabEval"=tabEval,
                # "model"=model)
                
                out
                
        }
        
        predOOS<-abind(lapply(modelsList, '[[', 'predOOS'))
        predEval<-abind(lapply(modelsList, '[[', 'predEval'))
        tabEval<-tabEval<-data.frame(actual=validData$a_BatDeath_IHS,
                                     dyad_ccode_year=as.factor(paste(paste(validData$a_ccode, validData$b_ccode, sep=""), validData$Year, sep="_")))
        #models<-lapply(modelsList, '[[', 'model')
        
        final<-list("predOOS"=predOOS,
                    "predEval"=predEval,
                    "tabEval"=tabEval)
        
}



# Loop over the modeling approaches
dat.approaches<-foreach(i=1:length(strong)) %do% {
        list(strong[[i]], average[[i]], aggregate[[i]])
}
dat.names<-c("Strong", "Average", "Aggregate")

modelsOut<-suppressWarnings(foreach(h=1:length(dat.names)) %do% {
        
        ### variables to omit
        omit<-c("a_BatDeath", "Duration", "icowsal", "dyad_ccode_year", "mariss", "terriss")
        dat<-dplyr::select(dat.approaches[[1]][[h]], -one_of(omit))
        
        set.seed(1999)
        folds <- createFolds(dat$a_BatDeath_IHS, k = 5, list = TRUE, returnTrain = FALSE)
        iter<-list(c(folds$Fold1),  c(folds$Fold2), c(folds$Fold3), c(folds$Fold4), c(folds$Fold5))
        
        #
        eval<-foreach(i=1:length(iter), .combine=rbind.data.frame) %do% {
                hold<-cbind.data.frame(dat[iter[[i]]]$a_BatDeath_IHS, 
                                       dat[iter[[i]]]$a_ccode, 
                                       dat[iter[[i]]]$b_ccode, 
                                       dat[iter[[i]]]$Year)
                
                names(hold)<-c("actual", "a_ccode", "b_ccode", "Year")
                hold
        }
        
        # Null
        best_null<-out_null<-foreach(i=1:length(iter), .combine=rbind.data.frame) %do% {
                model<-train(a_BatDeath_IHS~1, 
                             data=dplyr::select(dat[-iter[[i]]], -a_ccode, -b_ccode), 
                             method="lm",
                             trControl=ctrlParallel)
                
                as.matrix(predict.train(model,
                                        newdata=dat[iter[[i]]]))
        }
        rmse_null<-apply(out_null, 2, function(x) {
                sqrt(sum((x-eval$actual)^2)/length(eval$actual))
        })
        names(rmse_null)<-"null"
        names(out_null)<-"null"
        
        # OLS - CINC + Year
        best_cincyear<-out_cincyear<-foreach(i=1:length(iter), .combine=rbind.data.frame) %do% {
                model<-train(a_BatDeath_IHS~a_lcinc+b_lcinc+Year, 
                             data=dplyr::select(dat[-iter[[i]]], -a_ccode, -b_ccode), 
                             method="lm",
                             trControl=ctrlParallel)
                
                as.matrix(predict.train(model,
                                        newdata=dat[iter[[i]]]))
        }
        
        rmse_cincyear<-apply(out_cincyear, 2, function(x) {
                sqrt(sum((x-eval$actual)^2)/length(eval$actual))
        })
        names(rmse_cincyear)<-"cincyear"
        names(out_cincyear)<-"cincyear"
        
        
        # OLS all predictors
        best_ols<-out_ols<-foreach(i=1:length(iter), .combine=rbind.data.frame) %do% {
                model<-train(a_BatDeath_IHS~., 
                             data=dplyr::select(dat[-iter[[i]]], -a_ccode, -b_ccode), 
                             method="lm",
                             trControl=ctrlParallel)
                
                as.matrix(predict.train(model,
                                        newdata=dat[iter[[i]]]))
        }
        rmse_ols<-apply(out_ols, 2, function(x) {
                sqrt(sum((x-eval$actual)^2)/length(eval$actual))
        })
        names(rmse_ols)<-"ols"
        names(out_ols)<-"ols"
        
        # For models with tuning parameters, use the function
        # PLS
        pred_pls<-foreach(i=1:length(iter)) %do% {
                ModelsGrid(modName=c("pls"),
                           tune=as.tbl(plsGrid),
                           trainData=dat[-iter[[i]]],
                           validData=dat[iter[[i]]],
                           omit<-c("a_ccode", "b_ccode"),
                           preProcess=T)
        }
        # extract
        out_pls<-as.tbl(do.call(rbind.data.frame, lapply(pred_pls, '[[', 'predEval')))
        # grab error
        rmseGrid_pls<-apply(out_pls, 2, function(x) {
                sqrt(sum((x-eval$actual)^2)/length(eval$actual))
        })
        # which minimizes?
        # grab the rmse
        rmse_pls<-rmseGrid_pls[which(rmseGrid_pls==min(rmseGrid_pls))][1]
        # and predictions
        best_pls<-out_pls[which(rmseGrid_pls==min(rmseGrid_pls))][1]
        

        # MARS
        pred_mars<-foreach(i=1:length(iter)) %do% {
                out<-ModelsGrid(modName=c("earth"),
                                tune=as.tbl(marsGrid),
                                trainData<-dat[-iter[[i]]],
                                validData<-dat[iter[[i]]],
                                omit<-c("a_ccode", "b_ccode"),
                                preProcess=T)
        }
        out_mars<-as.tbl(do.call(rbind.data.frame, lapply(pred_mars, '[[', 'predEval')))
        # grab error
        rmseGrid_mars<-apply(out_mars, 2, function(x) {
                sqrt(sum((x-eval$actual)^2)/length(eval$actual))
        })
        # which minimizes?
        rmse_mars<-rmseGrid_mars[which(rmseGrid_mars==min(rmseGrid_mars))][1]
        best_mars<-out_mars[which(rmseGrid_mars==min(rmseGrid_mars))][1]
        
        
        # KNN
        pred_knn<-foreach(i=1:length(iter)) %do% {
                ModelsGrid(modName=c("knn"),
                           tune=as.tbl(knnGrid),
                           trainData<-dat[-iter[[i]]],
                           validData<-dat[iter[[i]]],
                           omit<-c("a_ccode", "b_ccode"),
                           preProcess=T)
        }
        out_knn<-as.tbl(do.call(rbind.data.frame, lapply(pred_knn, '[[', 'predEval')))
        # grab error
        rmseGrid_knn<-apply(out_knn, 2, function(x) {
                sqrt(sum((x-eval$actual)^2)/length(eval$actual))
        })
        # which minimizes?
        rmse_knn<-rmseGrid_knn[which(rmseGrid_knn==min(rmseGrid_knn))][1]
        best_knn<-out_knn[which(rmseGrid_knn==min(rmseGrid_knn))][1]
        
        
        # Elastic Net
        pred_enet<-foreach(i=1:length(iter)) %do% {
                ModelsGrid(modName=c("enet"),
                           tune=as.tbl(enetGrid),
                           trainData<-dat[-iter[[i]]],
                           validData<-dat[iter[[i]]],
                           omit<-c("a_ccode", "b_ccode"),
                           preProcess=T)
        }
        out_enet<-as.tbl(do.call(rbind.data.frame, lapply(pred_enet, '[[', 'predEval')))
        # grab error
        rmseGrid_enet<-apply(out_enet, 2, function(x) {
                sqrt(sum((x-eval$actual)^2)/length(eval$actual))
        })
        # which minimizes?
        rmse_enet<-rmseGrid_enet[which(rmseGrid_enet==min(rmseGrid_enet))][1]
        best_enet<-out_enet[which(rmseGrid_enet==min(rmseGrid_enet))][1]
        

        # CART
        pred_cart<-foreach(i=1:length(iter)) %do% {
                ModelsGrid(modName=c("rpart2"),
                           tune=as.tbl(cartGrid),
                           trainData<-dat[-iter[[i]]],
                           validData<-dat[iter[[i]]],
                           omit<-c("a_ccode", "b_ccode"),
                           preProcess=F)
        }
        out_cart<-as.tbl(do.call(rbind.data.frame, lapply(pred_cart, '[[', 'predEval')))
        # grab error
        rmseGrid_cart<-apply(out_cart, 2, function(x) {
                sqrt(sum((x-eval$actual)^2)/length(eval$actual))
        })
        # which minimizes?
        rmse_cart<-rmseGrid_cart[which(rmseGrid_cart==min(rmseGrid_cart))][1]
        best_cart<-out_cart[which(rmseGrid_cart==min(rmseGrid_cart))][1]
        
        
        ### set aside
        # Random forest
        pred_rf<-foreach(i=1:length(iter)) %do% {
                ModelsGrid(modName=c("rf"),
                           tune=as.tbl(rfGrid),
                           trainData<-dat[-iter[[i]]],
                           validData<-dat[iter[[i]]],
                           omit<-c("a_ccode", "b_ccode"),
                           preProcess=F)
        }
        out_rf<-as.tbl(do.call(rbind.data.frame, lapply(pred_rf, '[[', 'predEval')))
        # grab error
        rmseGrid_rf<-apply(out_rf, 2, function(x) {
                sqrt(sum((x-eval$actual)^2)/length(eval$actual))
        })
        # which minimizes?
        rmse_rf<-rmseGrid_rf[which(rmseGrid_rf==min(rmseGrid_rf))][1]
        best_rf<-out_rf[which(rmseGrid_rf==min(rmseGrid_rf))][1]
        
        
        # Boosted Trees
        pred_gbm<-foreach(i=1:length(iter)) %do% {
                ModelsGrid(modName=c("gbm"),
                           tune=as.tbl(gbmGrid),
                           trainData<-dat[-iter[[i]]],
                           validData<-dat[iter[[i]]],
                           omit<-c("a_ccode", "b_ccode"),
                           preProcess=F)
        }
        out_gbm<-as.tbl(do.call(rbind.data.frame, lapply(pred_gbm, '[[', 'predEval')))
        # grab error
        rmseGrid_gbm<-apply(out_gbm, 2, function(x) {
                sqrt(sum((x-eval$actual)^2)/length(eval$actual))
        })
        # which minimizes?
        rmse_gbm<-rmseGrid_gbm[which(rmseGrid_gbm==min(rmseGrid_gbm))][1]
        best_gbm<-out_gbm[which(rmseGrid_gbm==min(rmseGrid_gbm))][1]
        
        
        # cubist
        pred_cub<-foreach(i=1:length(iter)) %do% {
                ModelsGrid(modName=c("cubist"),
                           tune=as.tbl(cubGrid),
                           trainData<-dat[-iter[[i]]],
                           validData<-dat[iter[[i]]],
                           omit<-c("a_ccode", "b_ccode"),
                           preProcess=F)
        }
        out_cub<-as.tbl(do.call(rbind.data.frame, lapply(pred_cub, '[[', 'predEval')))
        # grab error
        rmseGrid_cub<-apply(out_cub, 2, function(x) {
                sqrt(sum((x-eval$actual)^2)/length(eval$actual))
        })
        # which minimizes?
        rmse_cub<-rmseGrid_cub[which(rmseGrid_cub==min(rmseGrid_cub))][1]
        best_cub<-out_cub[which(rmseGrid_cub==min(rmseGrid_cub))][1]
        
        
        # svm
        pred_svm<-foreach(i=1:length(iter)) %do% {
                ModelsGrid(modName=c("svmRadial"),
                           tune=as.tbl(svmGrid),
                           trainData<-dat[-iter[[i]]],
                           validData<-dat[iter[[i]]],
                           omit<-c("a_ccode", "b_ccode"),
                           preProcess=T)
        }
        out_svm<-as.tbl(do.call(rbind.data.frame, lapply(pred_svm, '[[', 'predEval')))
        # grab error
        rmseGrid_svm<-apply(out_svm, 2, function(x) {
                sqrt(sum((x-eval$actual)^2)/length(eval$actual))
        })
        # which minimizes?
        rmse_svm<-rmseGrid_svm[which(rmseGrid_svm==min(rmseGrid_svm))][1]
        best_svm<-out_svm[which(rmseGrid_svm==min(rmseGrid_svm))][1]
        
        
        # neural net
        pred_nnet<-foreach(i=1:length(iter)) %do% {
                ModelsGrid(modName=c("avNNet"),
                           tune=as.tbl(nnetGrid),
                           trainData<-dat[-iter[[i]]],
                           validData<-dat[iter[[i]]],
                           omit<-c("a_ccode", "b_ccode"),
                           preProcess=T)
        }
        out_nnet<-as.tbl(do.call(rbind.data.frame, lapply(pred_nnet, '[[', 'predEval')))
        # grab error
        rmseGrid_nnet<-apply(out_nnet, 2, function(x) {
                sqrt(sum((x-eval$actual)^2)/length(eval$actual))
        })
        # which minimizes?
        rmse_nnet<-rmseGrid_nnet[which(rmseGrid_nnet==min(rmseGrid_nnet))][1]
        best_nnet<-out_nnet[which(rmseGrid_nnet==min(rmseGrid_nnet))][1]
        
        
        # store output
        list<-lapply(ls(pattern="rmse_"), get)
        #wipe
        rm(list=ls(pattern="rmse_"))
        
        # store
        error_out<-data.frame(model=unlist(lapply(list, names)),
                             rmse=do.call(rbind, list))
        
        # all predictions
        pred_out<-do.call(cbind, lapply(ls(pattern="out_"), get))
        colnames(pred_out)<-paste(dat.names[h], colnames(pred_out), sep=":")
        
        rm(list=ls(pattern="out_"))
        
        # best predictions
        final_out<-do.call(cbind, lapply(ls(pattern="best_"), get))
        colnames(final_out)<-paste(dat.names[h], colnames(final_out), sep=":")
        
        
        out<-list("rmse"=error_out,
                  "pred"=pred_out,
                  "best_pred"=final_out)
        
        out

})

save(modelsOut, file="modelsOut.Rdata")

load("modelsOut.Rdata")


# extract rmse
rmse_strong<-modelsOut[[1]]$rmse
rmse_average<-modelsOut[[2]]$rmse
rmse_aggregate<-modelsOut[[3]]$rmse

save(rmse_aggregate, file="Output/rmse_aggregate.Rdata")

# extract best predictions
bestPred_strong<-modelsOut[[1]]$best_pred
bestPred_average<-modelsOut[[2]]$best_pred
bestPred_aggregate<-modelsOut[[3]]$best_pred

save(bestPred_strong, file="Output/bestPred_strong.Rdata")
save(bestPred_average, file="Output/bestPred_average.Rdata")
save(bestPred_aggregate, file="Output/bestPred_aggregate.Rdata")

# extract all predictions
allPred_strong<-modelsOut[[1]]$pred
allPred_average<-modelsOut[[2]]$pred
allPred_aggregate<-modelsOut[[3]]$pred


# extract actual validation set
### variables to omit
omit<-c("a_BatDeath", "Duration", "icowsal", "dyad_ccode_year", "mariss", "terriss")
dat<-dplyr::select(dat.approaches[[1]][[1]], -one_of(omit))

set.seed(1999)
folds <- createFolds(dat$a_BatDeath_IHS, k = 5, list = TRUE, returnTrain = FALSE)
iter<-list(c(folds$Fold1),  c(folds$Fold2), c(folds$Fold3), c(folds$Fold4), c(folds$Fold5))

#
eval<-foreach(i=1:length(iter), .combine=rbind.data.frame) %do% {
        hold<-cbind.data.frame(dat[iter[[i]]]$a_BatDeath_IHS, 
                               dat[iter[[i]]]$a_ccode, 
                               dat[iter[[i]]]$b_ccode, 
                               dat[iter[[i]]]$Year)
        
        names(hold)<-c("actual", "a_ccode", "b_ccode", "Year")
        hold
}

save(eval, file="Output/eval.Rdata")

## plot predictions vs actual on the validation set



# Ensemble all predictions 
# run functions for the ensemble
frontier <- function(weights,
                     pred.Y,
                     Y)
{
        sqrt(sum((pred.Y%*%weights-Y)^2)/nrow(Y))
}

constraint <- function(weights,
                       pred.Y, 
                       Y) {
        sum(weights)
}

### Get predictions for ensemble
pred.ensemble<-as.matrix(cbind(bestPred_aggregate,
                     bestPred_strong,
                     bestPred_average))

y.out<-as.matrix(eval$actual)

starts <- as.matrix(rep(1 / ncol(pred.ensemble), ncol(pred.ensemble)))

library(Rsolnp)

solver <- suppressWarnings( 
        solnp(pars = starts,
              fun = frontier,
              Y = as.matrix(y.out),
              pred.Y = as.matrix(pred.ensemble),
              eqfun = constraint,
              eqB = 1,
              LB = rep(0, ncol(pred.ensemble)),
              UB = rep(1, ncol(pred.ensemble)),
              control = list(trace = 0)))

weights<-solver$pars
names(weights)<-colnames(pred.ensemble)

round(weights[which(weights > 0.001)], 3)


# compute ensemble error
rmse.ensemble<-error(pred.ensemble%*%weights, y.out)
rmse.ensemble


# Run ensemble over all predictions
### Get predictions for ensemble
pred.ensemble.all<-as.matrix(cbind(allPred_aggregate,
                               allPred_strong,
                               allPred_average))

y.out<-as.matrix(eval$actual)

starts <- as.matrix(rep(1 / ncol(pred.ensemble.all), ncol(pred.ensemble.all)))

library(Rsolnp)

solver <- suppressWarnings( 
        solnp(pars = starts,
              fun = frontier,
              Y = as.matrix(y.out),
              pred.Y = as.matrix(pred.ensemble.all),
              eqfun = constraint,
              eqB = 1,
              LB = rep(0, ncol(pred.ensemble.all)),
              UB = rep(1, ncol(pred.ensemble.all)),
              control = list(trace = 0)))

weights.all<-solver$pars
names(weights.all)<-colnames(pred.ensemble.all)

round(weights.all[which(weights.all > 0.001)], 3)

rmse.ensemble.all<-error(pred.ensemble.all%*%weights.all, y.out)
rmse.ensemble.all

# okay we will go with this ensemble
ensemble.weights<-weights.all
save(ensemble.weights, file="ensemble.weights.Rdata")


### Predict the set using the ensemble
pred.final<-pred.ensemble.all%*%weights.all


# quick ggplot
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

df<-data.table(pred.final, y.out)
colnames(df)<-c("Predicted_Ensemble", "Observed")

scatter.ensemble<-ggplot(df, aes(x=Predicted_Ensemble, y=Observed))+
        geom_point(alpha=0.25)+
        geom_smooth()+
        coord_cartesian(xlim = c(0,18), ylim=c(0,18))+
        ggtitle("Battle Deaths (IHS) - Ensemble")+
        labs(x="Predicted")+
        labs(y="Observed")+
        theme_update(plot.title = element_text(hjust=0.5)) +
        theme_bw()
scatter.ensemble
ggsave("Figures/ensemble_scatter.pdf")



# Make sorted graph frm ensemble
sorted<-data.table(arrange(df, Observed), seq(1, nrow(df)))

colnames(sorted)<-c("Predicted", "Observed", "ID")

g<-gather(sorted, 
          value="Battle_Deaths",
          key="Type",
          Predicted, Observed)

h<-gather(sorted, value="Battle_Deaths", key="Type", Observed)

k<-gather(sorted, value="Battle_Deaths", key="Yype", Predicted)

ggplot(sorted, aes(x=ID, y=Predicted))+
        geom_point(data=h,aes(x=ID, y=Battle_Deaths, color=Type), alpha=0.25)+
        labs(x="Dispute")+
        labs(y="Battle Deaths (IHS)")+
        ggtitle("Ensemble")+
        theme(axis.ticks.x=element_blank())+
        theme_bw()
ggsave("Figures/ensemble_sorted_1.pdf")

ggplot(sorted, aes(x=ID, y=Predicted))+
        geom_point(data=g,aes(x=ID, y=Battle_Deaths, color=Type), alpha=0.75)+
        scale_colour_manual(values=c("black", "grey"))+
        geom_smooth()+
        labs(x="Dispute")+
        labs(y="Battle Deaths (IHS)")+
        ggtitle("Ensemble")+
        theme(axis.ticks.x=element_blank())+
        theme_bw()
ggsave("Figures/ensemble_sorted_2.pdf")

### Make weights table
# The data, mode with the tuning parameter,  PRL, and weight
weights.table<-weights.all[which(weights.all > 0.0001)]

# need the null prediction, what was it?
null<-rmse_strong[which(rmse_strong$model=='null'),2]

# model
foo<-do.call(rbind, strsplit(names(weights.table), ":"))
data<-foo[,1]
model<-foo[,2]

# rmse and PRL for each
bar<-pred.ensemble.all[,which(colnames(pred.ensemble.all) %in% names(weights.table) == T)]
rmse<-apply(bar, 2, function(x) {
        sqrt(sum((x-eval$actual)^2)/length(eval$actual))
})
prl<- -(rmse-null)/null

# make table
cleanMod <- function(x){
        x <- gsub(pattern = "cubist", replacement = "Cubist", x)
        x <- gsub(pattern = "earth", replacement = "MARS", x)
        x <- gsub(pattern = "avNNet", replacement = "Neural Nets", x)
        x <- gsub(pattern = "rf", replacement = "Random Forest", x)
        x <- gsub(pattern = "gbm", replacement = "Boosted Trees", x)
}

toOut<-data.frame(Data = data,
           Model = cleanMod(model),
           Weight = as.vector(round(weights.table,3)),
           RMSE = as.vector(round(rmse,3)),
           PRL = as.vector(round(prl,3)))

hold<-data.frame(Data="All",
                          Model="Ensemble",
                          Weight="-",
                          RMSE=round(rmse.ensemble.all,3),
                          PRL= round(-(rmse.ensemble.all-null)/null,3))
# weight
ensembleTable<-rbind(toOut, hold)
temp<-ensembleTable[order(ensembleTable$PRL), , drop=F]

temp
ensembleOut<-temp
save(ensembleOut, file="Output/ensembleOut.Rdata")


#
ensembleOut <- ensembleOut %>%
        remove_rownames %>%
        column_to_rownames(var="Data")

library(Hmisc)
latex(ensembleOut, file="")

evalActual<-lapply(pred_pls, '[[', 'tabEval')
evalPreds_strong<-as.tbl(do.call(cbind.data.frame, lapply(ls(pattern="out_"), get)))
colnames(evalPreds_strong)<-paste("strong", colnames(evalPreds_strong), sep=":")

# wipe
rm(list=ls(pattern="out_"))
rm(list=ls(pattern="pred_"))


















        