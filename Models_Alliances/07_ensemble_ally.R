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

#Send yourself an email - specify your preferred email address, subject, and message. The password is fixed at "rmail".
#send_message(mime(from="phil.henrickson@gmail.com", to="phil.henrickson@gmail.com", subject="Code Finished", "Woo!"))

#setwd("C:/Users/Phil/Dropbox/")
setwd("F:/Dropbox/FSU/FSU_Fall_2017/Paper_1/")



error<-function(pred.Y, Y) {
        sqrt(sum((pred.Y-Y)^2)/nrow(Y))
}

load("Final_Data/allies1.RData")

# select features needed for modelling
allies.trim<-dplyr::select(allies1,a_BatDeath_IHS, Year, a_irst, a_milex, a_milper, a_pec, a_tpop, a_upop, a_lcinc, a_polity2,
                           a_ally_irst, a_ally_milex, a_ally_milper , a_ally_pec, a_ally_tpop, a_ally_upop, a_ally_lcinc, a_ally_polity2,
                           b_irst, b_milex, b_milper, b_pec, b_tpop, b_upop, b_lcinc, b_polity2, conttype, participants, terriss, riveriss, mariss, icowsalc)

# load in previously selected tuning parameters
load("Output/par.allies.Rdata")


# allies
tune<-par.allies

# set tuning parameters
pls.tune<-expand.grid(.ncomp=tune$pls)
ridge.tune<-expand.grid(.lambda=tune$ridge)
lasso.tune<-expand.grid(.fraction=tune$lasso)
enet.tune<-expand.grid(.fraction=tune$enet[1], .lambda=tune$enet[2])
knn.tune<-expand.grid(.k=tune$knn)
cart.tune<-expand.grid(.maxdepth=tune$cart)
rf.tune<-expand.grid(.mtry=tune$rf)
bst.tune<-expand.grid(interaction.depth=tune$bsttree[2], n.trees = tune$bsttree[1], shrinkage=tune$bsttree[3], n.minobsinnode=tune$bsttree[4])
cub.tune<-expand.grid(.committees = tune$cub[1], .neighbors = tune$cub[2])
svm.tune<-expand.grid(.sigma=tune$svm[1], .C=tune$svm[2])
nnet.tune<-expand.grid(.size = tune$nnet[1], .decay = tune$nnet[2], .bag=T)

model.names<-c("null", "nullcinc", "ols","pls", "ridge", "lasso", "enet", "knn", "cart", "rf", "gbm", "cub", "svm", "nnet")

# run over all datasets using caretList
train<-allies.trim

tune_control<-trainControl(method="cv", n=5, predictionBounds = c(0, NA), savePredictions="final", allowParallel=T)

#x.test<-dplyr::select(test, -a_BatDeath_IHS)
#y.test<-dplyr::select(test, a_BatDeath_IHS)

registerDoSEQ()

# 1 Null
set.seed(1999)
nullModel<-train(a_BatDeath_IHS~1, 
                 data=train, 
                 method="lm", 
                 trControl=tune_control)

# null cinc
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
                tuneGrid=pls.tune,
                preProcess=c("center", "scale"))

# 4 Ridge
set.seed(1999)
ridgeModel<-train(a_BatDeath_IHS~., 
                  data=train,
                  method="ridge",
                  trControl=tune_control,
                  tuneGrid=ridge.tune,
                  preProcess=c("center", "scale"))

# 5 Lasso
set.seed(1999)
lassoModel<-train(a_BatDeath_IHS~., 
                  data=train,
                  method="lasso",
                  trControl=tune_control,
                  tuneGrid=lasso.tune,
                  preProcess=c("center", "scale"))

# 6 Elastic Net
set.seed(1999)
enetModel<-train(a_BatDeath_IHS~., 
                 data=train,
                 method="enet",
                 trControl=tune_control,
                 tuneGrid=enet.tune,
                 preProcess=c("center", "scale"))

# 7 KNN
set.seed(1999)
knnModel<-train(a_BatDeath_IHS~., 
                data=train, 
                method="knn", 
                tuneGrid=knn.tune, 
                trControl=tune_control,
                preProcess=c("center", "scale"))

# 8 CART tuning over max depth
set.seed(1999)
cartModel<-train(a_BatDeath_IHS~., 
                 data=train, 
                 method="rpart2",
                 trControl=tune_control, 
                 tuneGrid=cart.tune)

# 9 Random Forest
set.seed(1999)
rfModel<-train(a_BatDeath_IHS~., 
               data=train, 
               method="rf", 
               trControl=tune_control, 
               tuneGrid=rf.tune,
               importance=T)

# 10 Boosted Tree
set.seed(1999)
bstModel<-train(a_BatDeath_IHS~.,
                data=train,
                method="gbm",
                tuneGrid=bst.tune,
                trControl=tune_control,
                verbose=F)

# 11 Cubist
set.seed(1999)
cubModel<-train(a_BatDeath_IHS~., 
                data=train,  
                method="cubist", 
                trControl=tune_control, 
                tuneGrid = cub.tune)

# 12 SVM r
set.seed(1999)
svmModel<-train(a_BatDeath_IHS~.,
                data=train,
                trControl=tune_control,
                method="svmRadial",
                tuneGrid=svm.tune,
                preProcess=c("center", "scale"))

# 13 aNN
set.seed(1999)
nnetModel<-train(a_BatDeath_IHS~.,
                 data=train,
                 trControl=tune_control,
                 tuneGrid=nnet.tune,
                 method="avNNet",
                 preProcess=c("center", "scale"),
                 linout=1,
                 trace=F)

# Preliminary Models
model_list<-list(nullModel, 
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

rmse.out<-foreach(i=1:length(model_list), .combine=rbind) %do% {
        rmse<-model_list[[i]]$results$RMSE
        sd<-model_list[[i]]$results$RMSESD
        c(rmse, sd)
}

model.names<-c("Null", "CINC+Year", "OLS","PLS", "Ridge", "Lasso", "Elastic Net", "KNN", "Cart", "Random Forests", "Boosted Trees", "Cubist", "SVM - Radial", "Neural Nets")

rownames(rmse.out)<-model.names
rmse.out

library(Hmisc)
latex(round(rmse.out, 3), file="")

# save models
save(model_list, file="Output/model_list_allies.Rdata")


# extract predictions
pred.out<-foreach(i=1:length(model_list)) %do% {
        pred<-model_list[[i]]$pred$pred
}

pred.mat<-do.call(cbind, pred.out)
colnames(pred.mat)<-model.names

obs.allies<-nullModel$pred$obs

# 
pred.allies<-pred.mat


pred.allies<-as.data.frame(pred.allies)

# save predictions
save(pred.allies, file="Output/pred.allies.RData")



pred.ensemble<-cbind(pred.allies)
y.out.allies<-cbind(obs.allies)

save(y.out.allies, file="Output/y.out.allies.Rdata")


## Ensembling

load("Output/pred.allies.Rdata")


load("Output/y.out.allies.Rdata")


pred.ensemble<-as.matrix(cbind(as.matrix(pred.allies)))

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


starts <- as.matrix(rep(1 / ncol(pred.ensemble), ncol(pred.ensemble)))
solver <- suppressWarnings( 
        solnp(pars = starts,
              fun = frontier,
              Y = y.out.allies,
              pred.Y = pred.ensemble,
              eqfun = constraint,
              eqB = 1,
              LB = rep(0, ncol(pred.ensemble)),
              UB = rep(1, ncol(pred.ensemble)),
              control = list(trace = 0)))

weights.allies<-solver$pars
names(weights.allies)<-colnames(pred.ensemble)

# Make table of weights
weights.mat<-matrix(NA, 14, 1)
rownames(weights.mat)<-model.names

weights.allies.out<-round(weights.allies, 3)
save(weights.allies.out, file="Output/weights.allies.out.RData")

# latex
latex(round(weights.allies.out,3), file="")

# final ensembled rmse
rmse.ensemble<-error(pred.ensemble%*%weights.allies, y.out.allies)
rmse.ensemble

pred.final<-pred.ensemble%*%weights.allies


table.allies<-cbind(rmse.out, weights.allies.out)
colnames(table.allies)<-c("RMSE", "SD", "Weight")
latex(round(table.allies, 3), file="")

# quick ggplot
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

df<-data.table(pred.final, y.out.allies)
colnames(df)<-c("Predicted_Ensemble", "Observed")

scatter.ensemble<-ggplot(df, aes(x=Predicted_Ensemble, y=Observed))+
        geom_point(color="blue", alpha=0.25)+
        geom_smooth()+
        coord_cartesian(xlim = c(0,17), ylim=c(0,17))+
        ggtitle("Battle Deaths (IHS) - Ensemble")+
        labs(x="Predicted")+
        labs(y="Observed")+
        theme_update(plot.title = element_text(hjust=0.5)) +
        theme_update()
scatter.ensemble
ggsave("Figures/ensemble_scatter_allies.pdf")



# Make sorted graph frm ensemble
sorted<-data.table(arrange(df, Observed), seq(1, nrow(df)))

colnames(sorted)<-c("Predicted", "Observed", "ID")

g<-gather(sorted, 
          value="Battle_Deaths",
          key="type",
          Predicted, Observed)

h<-gather(sorted, value="Battle_Deaths", key="type", Observed)

k<-gather(sorted, value="Battle_Deaths", key="type", Predicted)

ggplot(sorted, aes(x=ID, y=Predicted))+
      geom_point(data=h,aes(x=ID, y=Battle_Deaths, color=type), alpha=0.25)+
        labs(x="Dispute")+
        labs(y="Battle Deaths (IHS)")+
        ggtitle("Ensemble")+
        theme(axis.ticks.x=element_blank())+
        theme_update()
ggsave("Figures/ensemble_sorted_1.pdf")

ggplot(sorted, aes(x=ID, y=Predicted))+
        geom_point(data=g,aes(x=ID, y=Battle_Deaths, color=type), alpha=0.25)+
        geom_smooth()+
        labs(x="Dispute")+
        labs(y="Battle Deaths (IHS)")+
        ggtitle("Ensemble")+
        theme(axis.ticks.x=element_blank())+
        theme_update()
ggsave("Figures/ensemble_sorted_2.pdf")





# ensemble boots
dat<-allies.trim

tune_control<-trainControl(method="cv", n=5, predictionBounds = c(0, NA), savePredictions="final", allowParallel=T)

cl <- makeCluster(7)
registerDoParallel(cl)

ensemble_boots<-foreach(i=1:10, .packages=c('dplyr', 'foreach', 'caret')) %dopar% {
      
        # bootstrap
        # dont use RMSE estimates, as they are off
        boot<-sample(1:nrow(dat), replace=T)
        train<-dat[boot,]
        
        # 1 Null
        set.seed(1999)
        nullModel<-train(a_BatDeath_IHS~1, 
                         data=train, 
                         method="lm", 
                         trControl=tune_control)
        
        # null cinc
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
                        tuneGrid=pls.tune,
                        preProcess=c("center", "scale"))
        
        # 4 Ridge
        set.seed(1999)
        ridgeModel<-train(a_BatDeath_IHS~., 
                          data=train,
                          method="ridge",
                          trControl=tune_control,
                          tuneGrid=ridge.tune,
                          preProcess=c("center", "scale"))
        
        # 5 Lasso
        set.seed(1999)
        lassoModel<-train(a_BatDeath_IHS~., 
                          data=train,
                          method="lasso",
                          trControl=tune_control,
                          tuneGrid=lasso.tune,
                          preProcess=c("center", "scale"))
        
        # 6 Elastic Net
        set.seed(1999)
        enetModel<-train(a_BatDeath_IHS~., 
                         data=train,
                         method="enet",
                         trControl=tune_control,
                         tuneGrid=enet.tune,
                         preProcess=c("center", "scale"))
        
        # 7 KNN
        set.seed(1999)
        knnModel<-train(a_BatDeath_IHS~., 
                        data=train, 
                        method="knn", 
                        tuneGrid=knn.tune, 
                        trControl=tune_control,
                        preProcess=c("center", "scale"))
        
        # 8 CART tuning over max depth
        set.seed(1999)
        cartModel<-train(a_BatDeath_IHS~., 
                         data=train, 
                         method="rpart2",
                         trControl=tune_control, 
                         tuneGrid=cart.tune)
        
        # 9 Random Forest
        set.seed(1999)
        rfModel<-train(a_BatDeath_IHS~., 
                       data=train, 
                       method="rf", 
                       trControl=tune_control, 
                       tuneGrid=rf.tune,
                       importance=T)
        
        # 10 Boosted Tree
        set.seed(1999)
        bstModel<-train(a_BatDeath_IHS~.,
                        data=train,
                        method="gbm",
                        tuneGrid=bst.tune,
                        trControl=tune_control,
                        verbose=F)
        
        # 11 Cubist
        set.seed(1999)
        cubModel<-train(a_BatDeath_IHS~., 
                        data=train,  
                        method="cubist", 
                        trControl=tune_control, 
                        tuneGrid = cub.tune)
        
        # 12 SVM r
        set.seed(1999)
        svmModel<-train(a_BatDeath_IHS~.,
                        data=train,
                        trControl=tune_control,
                        method="svmRadial",
                        tuneGrid=svm.tune,
                        preProcess=c("center", "scale"))
        
        # 13 aNN
        set.seed(1999)
        nnetModel<-train(a_BatDeath_IHS~.,
                         data=train,
                         trControl=tune_control,
                         tuneGrid=nnet.tune,
                         method="avNNet",
                         preProcess=c("center", "scale"),
                         linout=1,
                         trace=F)
        
        # Preliminary Models
        model_list<-list(nullModel, 
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
        
        model_list
}

dat<-allies.trim
boot<-sample(1:nrow(dat), replace=T)
dat.boot<-dat[boot,]


# 1 Null
set.seed(1999)
nullModel<-train(a_BatDeath_IHS~1, 
                 data=train, 
                 method="lm", 
                 trControl=tune_control)

# null cinc
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
                tuneGrid=pls.tune,
                preProcess=c("center", "scale"))

# 4 Ridge
set.seed(1999)
ridgeModel<-train(a_BatDeath_IHS~., 
                  data=train,
                  method="ridge",
                  trControl=tune_control,
                  tuneGrid=ridge.tune,
                  preProcess=c("center", "scale"))

# 5 Lasso
set.seed(1999)
lassoModel<-train(a_BatDeath_IHS~., 
                  data=train,
                  method="lasso",
                  trControl=tune_control,
                  tuneGrid=lasso.tune,
                  preProcess=c("center", "scale"))

# 6 Elastic Net
set.seed(1999)
enetModel<-train(a_BatDeath_IHS~., 
                 data=train,
                 method="enet",
                 trControl=tune_control,
                 tuneGrid=enet.tune,
                 preProcess=c("center", "scale"))

# 7 KNN
set.seed(1999)
knnModel<-train(a_BatDeath_IHS~., 
                data=train, 
                method="knn", 
                tuneGrid=knn.tune, 
                trControl=tune_control,
                preProcess=c("center", "scale"))

# 8 CART tuning over max depth
set.seed(1999)
cartModel<-train(a_BatDeath_IHS~., 
                 data=train, 
                 method="rpart2",
                 trControl=tune_control, 
                 tuneGrid=cart.tune)

# 9 Random Forest
set.seed(1999)
rfModel<-train(a_BatDeath_IHS~., 
               data=train, 
               method="rf", 
               trControl=tune_control, 
               tuneGrid=rf.tune,
               importance=T)

# 10 Boosted Tree
set.seed(1999)
bstModel<-train(a_BatDeath_IHS~.,
                data=train,
                method="gbm",
                tuneGrid=bst.tune,
                trControl=tune_control,
                verbose=F)

# 11 Cubist
set.seed(1999)
cubModel<-train(a_BatDeath_IHS~., 
                data=train,  
                method="cubist", 
                trControl=tune_control, 
                tuneGrid = cub.tune)

# 12 SVM r
set.seed(1999)
svmModel<-train(a_BatDeath_IHS~.,
                data=train,
                trControl=tune_control,
                method="svmRadial",
                tuneGrid=svm.tune,
                preProcess=c("center", "scale"))

# 13 aNN
set.seed(1999)
nnetModel<-train(a_BatDeath_IHS~.,
                 data=train,
                 trControl=tune_control,
                 tuneGrid=nnet.tune,
                 method="avNNet",
                 preProcess=c("center", "scale"),
                 linout=1,
                 trace=F)

# Preliminary Models
model_list<-list(nullModel, 
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








        