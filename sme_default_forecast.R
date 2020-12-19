library(ISLR)
library(vip)
library(randomForest)
library(iml)
library(caret)
library(randomForestExplainer)
library(Metrics)
library(caTools)
library(MASS)
library(ggplot2)
library(MASS)
library(pdp)
library(plotly)
library(plyr)

load('data/sme_train.RData')
load('data/sme_test.RData')
colnames(SMETrain)[1] <- 'LegalStatus'
colnames(SMETest)[1] <- 'LegalStatus'

colnames(SMETrain)[88] <- 'EBITDA_lag_1yrs'
colnames(SMETest)[88] <- 'EBITDA_lag_1yrs'

SMETrain = rfImpute(LegalStatus ~ ., data = SMETrain, iter = 5, ntree = 50)
SMETest = rfImpute(LegalStatus ~ ., data = SMETest, iter = 5, ntree = 50)

class_randomForest <- randomForest(Stato.giuridico ~ ., data = SMETrain,
                                   replace = TRUE, nPerm = 4, importance = TRUE,
                                   proximity = TRUE, oob.prox = TRUE, 
                                   keep.inbag = TRUE)
class_randomForest

SMETrain$RF_pred <- predict(class_randomForest, newdata = SMETrain)
SMETest$RF_pred <- predict(class_randomForest, newdata = SMETest)

# Test set's Confusion Matrix
conf_matrix <- table(SMETest$Stato.giuridico, SMETest$RF_pred)
err_rate <- c(conf_matrix[1,2]/sum(conf_matrix[1,]), 
              conf_matrix[2,1]/sum(conf_matrix[2,]))
conf_matrix <- cbind(conf_matrix, err_rate)
colnames(conf_matrix)[3] <- 'Class Error'
conf_matrix

# Variance Importance
imp_MDA <- vi_model(class_randomForest, type = 1, scale = TRUE)
imp_MDA <- imp_MDA[order(-imp_MDA$Importance), ]

imp_MDI <- vi_model(class_randomForest, type = 2, scale = TRUE)
imp_MDI <- imp_MDI[order(-imp_MDI$Importance), ]

MDA <- vip(imp_MDA, num_features = 16, horizontal = TRUE,
           aesthetics = list(size = 1), main = 'MDA')
MDI <- vip(imp_MDI, num_features = 16, horizontal = TRUE,
           aesthetics = list(size = 1), main = 'MDI')

