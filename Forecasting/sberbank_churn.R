#### CHURN MODEL ####

# Author: Aleksandr Akimenko
# Date: 28-Sep-16



#### REQUIRED PACKAGES ####

list.of.packages <- c( "data.table" , "glmnet" ,
                      "gbm","randomForest", "xgboost")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(data.table)
library(gbm)
library(glmnet)	 
library(data.table)
library(randomForest)
library(xgboost)


#### PARAMETERS ####

path<-"/Users/AlexAkimenko/Documents/WORK/sber/"
options(scipen=999)

####### FUNCTIONS ###########

normalizedGini <- function(aa, pp) {
  Gini <- function(a, p) {
    if (length(a) !=  length(p)) stop("Actual and Predicted need to be equal lengths!")
    temp.df <- data.frame(a, p, c(1:length(a)))
    colnames(temp.df)<-c("actual","pred","range")
    temp.df <- temp.df[order(-temp.df$pred, temp.df$range),]
    population.delta <- 1 / length(a)
    total.losses <- sum(a)
    null.losses <- rep(population.delta, length(a)) # Hopefully is similar to accumulatedPopulationPercentageSum
    accum.losses <- temp.df$actual / total.losses # Hopefully is similar to accumulatedLossPercentageSum
    gini.sum <- cumsum(accum.losses - null.losses) # Not sure if this is having the same effect or not
    sum(gini.sum) / length(a)
  }
  Gini(aa,pp) / Gini(aa,aa)
}

get_matrix<-function(data_frame){
  for (i in 1:ncol(data_frame)){
    if(is.factor(data_frame[,i])){
      data_frame[,i]<-as.numeric(as.character(data_frame[,i]))
    } else if (is.character(data_frame[,i])) {
      data_frame[,i]<-as.numeric(data_frame[,i])
    } 
  }
  return(as.matrix(data_frame))
}


#### DATA LOAD ####

train <- fread(paste0(path,"train_data.tsv"))
y <- fread("/Users/AlexAkimenko/Documents/WORK/sber/train_churn.tsv")
test <- fread("/Users/AlexAkimenko/Documents/WORK/sber/test_data.tsv")


#### SAMPLING ####

is_train<-sample(1:nrow(train), nrow(train)*0.7,replace=FALSE)
X_train<-head(train[is_train,],10000)
X_test<-head(train[-is_train,],10000)
y_train<-head(y[is_train]==1,10000)
y_test<-head(y[-is_train]==1,10000)


#### GLM ####

fit_glm<-glm(y_train~.,X_train,family = "binomial")
summary(fit_glm)
y_train_pred<-predict(fit_glm,X_train)
y_test_pred<-predict(fit_glm,X_test)
Gini(y_train,y_train_pred)
Gini(y_test,y_test_pred)
Gini2(y_train,y_train_pred)
Gini2(y_test,y_test_pred)

#### GLMNET ####

fit_glmnet <- glmnet(get_matrix(X_train[,-ncol(X_train),with=F]), y_train)
y_train_pred<-predict(fit_glmnet,get_matrix(X_train[,-ncol(X_train),with=F]),s=0)
y_test_pred<-predict(fit_glmnet,get_matrix(X_test[,-ncol(X_test),with=F]),s=0)
normalizedGini(y_train,y_train_pred)
normalizedGini(y_test,y_test_pred)


#### RANDOM FOREST ####


#### KNN ####


#### XGBOOST ####


