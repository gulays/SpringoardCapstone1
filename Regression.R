###ATTReg has 31 variables (Open, High, Low are skipped)
ATT <- read_csv("C:/Users/gulay/Anaconda3/pkgs/ATT.csv", 
                col_types = cols(High = col_skip(), Low = col_skip(), 
                                 Open = col_skip(),Date = col_skip(), X1 = col_skip()))
##Remove NA values
ATT <- ATT[272:1840,]

## Scaling
ATT.scaled <- as.data.frame(scale(ATT, center = TRUE, scale = TRUE))
ATT.scaled$Action <- ATT$Action
ATT.scaled$PrDir<-ATT$PrDir
ATT.scaled$Day<-ATT$Day

train <- ATT.scaled[1:1316,]
test <- ATT.scaled[1317:1559,]

##Lasso
library(glmnet)
x = subset(ATT[1:1316,], select= - c(PrDir,Close) )
x<-data.matrix(x)

y=ATT[1:1316,]$PrDir
Lasso1=cv.glmnet(x, y, family='binomial', 
                 alpha=1, parallel=FALSE, 
                 standardize=TRUE, type.measure='auc',nfolds = 5)

coef(Lasso1)
plot(Lasso1)

predict(Lasso1, type="coefficients")

summary(Lasso1)

set.seed(1)
cv.out=cv.glmnet(x,y,alpha=1)
plot(cv.out)
bestlam =cv.out$lambda.min
lasso.pred=predict(Lasso1 ,s=bestlam ,
                   newx=data.matrix(subset(ATT[1317:1559,], select= - c(PrDir,Close) )),
                   type = 'response')
mean(lasso.pred)

fitted.lasso1 <- ifelse(lasso.pred > 0.43,1,0)
misClasificErrorLasso <- mean(fitted.lasso1 != ATT[1317:1559,]$PrDir)
print(paste('Accuracy for Lasso',1-misClasificErrorLasso))

mean((fitted.lasso1 -ATT[1317:1559,]$PrDir)^2)


####Logistic with all features
fit.log1 <- glm(PrDir ~ ., family = binomial,control = list(maxit = 50),
                data= subset(train,
                             select=-c(Close)))
summary(fit.log1)
library(pscl)
pR2(fit.log1)

log.predict1 <- predict(fit.log1,
                          newdata=subset(test,
                                       select=-c(Close,PrDir)),type='response')
mean(log.predict1)
mean(test$PrDir)

mean((log.predict1 -test$PrDir)^2)
fitted.log1 <- ifelse(log.predict1 > 0.42,1,0)
misClasificError <- mean(fitted.log1 != test$PrDir)
print(paste('Accuracy for LogReg',1-misClasificError))
mean((fitted.log1 -test$PrDir)^2)


####Logistic with reduced model
fit.log2 <- glm(PrDir ~ ., family = binomial,control = list(maxit = 50),
                data= subset(train,
                             select=-c(Close, Volume , Day, Action, MA20, MA270, ROC1, ROC10, 
                                       ROC270, PCh10, PCh270,EffR10, EffR20,
                                       EffR270, Closelag1, Closelag20, Closelag270, Volumelag1,
                                       Volumelag5,Volumelag10, Volumelag20 , Volumelag270 )))
summary(fit.log2)

log.predict2 <- predict(fit.log2,
                        newdata=subset(test,
                                       select=-c(Close,PrDir)),type='response')
mean(log.predict2)


mean((log.predict2 -test$PrDir)^2)
fitted.log2 <- ifelse(log.predict2 > 0.42,1,0)
misClasificError2 <- mean(fitted.log2 != test$PrDir)
print(paste('Accuracy for LogReg',1-misClasificError2))
mean((fitted.log2 -test$PrDir)^2)





###Linear REgression for Close with train data
fit.lin1 <- lm(Close ~ ., data = train)
summary(fit.lin1)

fit.lin2 <- lm(Close ~ ., data = subset(train,
                                        select=-c(PrDir)))
summary(fit.lin2)


fit.lin3 <- lm (Close ~ Volume + Action + MA10 + PCh1 + 
                  EffR20 + Closelag1 + Closelag2 + 
                  Closelag5 + Closelag10 + Volumelag2 , data = subset(train,
                                                                      select=-c(PrDir)))
summary(fit.lin3)


####Random Forest

library(randomForest)
set.seed(1)
fit.RF1 <- randomForest(as.factor(PrDir) ~ . , 
                        data=subset(train,select= - Close), importance=TRUE, ntree=2000)
#Variable importance plot
varImpPlot(fit.RF1,sort = T,
           main="Variable Importance",
           n.var=10)
# Variable Importance Table for Gini
var.imp.gini <- data.frame(importance(fit.RF1, type=2))
# make row names as columns
var.imp.gini$Variables <- row.names(var.imp.gini)
var.imp.gini[order(var.imp.gini$MeanDecreaseGini,decreasing = T),]

# Variable Importance Table for Mean decrease accuracy
var.imp.acc<- data.frame(importance(fit.RF1, type=1))
# make row names as columns
var.imp.acc$Variables <- row.names(var.imp.acc)
var.imp.acc[order(var.imp.acc$MeanDecreaseAccuracy,decreasing = T),]




# Load Library or packages
library(e1071)
library(caret)



# Predicting response variable for test data
test1<-test
test1$predicted.response <- predict(fit.RF1 ,subset(test1,select= - Close))
summary(test1$predicted.response)

# Create Confusion Matrix
confusionMatrix(data=test1$predicted.response,
                reference=test1$PrDir)

randomForest

####Random forest ntree = 100

fit.RF2 <- randomForest(as.factor(PrDir) ~ . , 
                        data=subset(train,select= - Close), importance=TRUE, ntree=100)
#Variable importance plot
varImpPlot(fit.RF2,sort = T,
           main="Variable Importance",
           n.var=10)
# Variable Importance Table for Gini
var.imp.gini2 <- data.frame(importance(fit.RF2, type=2))
# make row names as columns
var.imp.gini2$Variables <- row.names(var.imp.gini2)
var.imp.gini2[order(var.imp.gini2$MeanDecreaseGini,decreasing = T),]

# Variable Importance Table for Mean decrease accuracy
var.imp.acc2<- data.frame(importance(fit.RF2, type=1))
# make row names as columns
var.imp.acc2$Variables <- row.names(var.imp.acc2)
var.imp.acc2[order(var.imp.acc2$MeanDecreaseAccuracy,decreasing = T),]



# Predicting response variable for test data
test2<-test
test2$predicted.response <- predict(fit.RF2 ,subset(test2,select= - Close))

# Create Confusion Matrix
confusionMatrix(data=test2$predicted.response,
                reference=test2$PrDir)

#Random Forest practice
####Random forest ntree = 500

fit.RF3 <- randomForest(as.factor(PrDir) ~ . , 
                        data=subset(train,select= - Close), 
                        mtry = 14,
                        importance=TRUE, ntree=500)
# Predicting response variable for test data
test3<-test
test3$predicted.response <- predict(fit.RF3 ,subset(test3,select= - Close))

# Create Confusion Matrix
confusionMatrix(data=test3$predicted.response,
                reference=test3$PrDir)



#Boosting
library (gbm)
set.seed(1)

fit.gbm1 <- gbm(PrDir ~ . , 
                        data=subset(train,select= - Close), 
                        distribution="bernoulli",n.trees=500, interaction.depth=4)
summary(fit.gbm1)

predict.gbm1 <- predict(fit.gbm1 ,subset(test,select= - Close),n.trees=500)
mean(predict.gbm1)

fitted.gbm1 <- ifelse(predict.gbm1 > 0.002405568,1,0)
misClasificError.gbm1 <- mean(fitted.gbm1 != test$PrDir)
print(paste('Accuracy for GBM',1-misClasificError.gbm1))

confusionMatrix(fitted.gbm1,test$PrDir)

##Boosting with n.trees = 200
fit.gbm2 <- gbm(PrDir ~ . , 
                data=subset(train,select= - Close), 
                distribution="bernoulli",n.trees=200, interaction.depth=6)
summary(fit.gbm2)

predict.gbm2 <- predict(fit.gbm2 ,subset(test,select= - Close),n.trees=200)
mean(predict.gbm2)
fitted.gbm2 <- ifelse(predict.gbm2 > 0,1,0)
misClasificError.gbm2 <- mean(fitted.gbm2 != test$PrDir)
print(paste('Accuracy for GBM',1-misClasificError.gbm2))

confusionMatrix(fitted.gbm2,test$PrDir)




##XGboost
library(xgboost)
fit.xgboost1<-xgboost(data = data.matrix(subset(train,select= - c(Close,PrDir))), 
        label = train$PrDir,nrounds = 100, objective = "binary:logistic",
        eval_metric = "error", verbose = 1)
#verbose = 1, print evaluation metric
summary(fit.xgboost1)


##Prediction
predict.xgboost1 <- predict(fit.xgboost1, data.matrix(subset(test,select= - Close)))
mean(predict.xgboost1)
fitted.xgboost1 <- ifelse(predict.xgboost1 > 0.5,1,0)
misClasificError.xgboost1 <- mean(fitted.xgboost1 != test$PrDir)
print(paste('Accuracy for XGBoost',1-misClasificError.xgboost1))

confusionMatrix(fitted.xgboost1,test$PrDir)


#XGBOOST nrounds = 2000
fit.xgboost2<-xgboost(data = data.matrix(subset(train,select= - c(Close,PrDir))), 
                      label = train$PrDir,nrounds = 2000, objective = "binary:logistic",
                      eval_metric = "error", verbose = 1)
#verbose = 1, print evaluation metric



##Prediction
predict.xgboost2 <- predict(fit.xgboost2, data.matrix(subset(test,select= - Close)))

fitted.xgboost2 <- ifelse(predict.xgboost2 > 0.5,1,0)
#misClasificError.xgboost2 <- mean(fitted.xgboost2 != test$PrDir)
#print(paste('Accuracy for XGBoost',1-misClasificError.xgboost2))

confusionMatrix(fitted.xgboost2,test$PrDir)
