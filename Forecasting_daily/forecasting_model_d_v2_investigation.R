#### DAILY FORECAST ####

# Author: Aleksandr Akimenko
# Date: Sep'16
# Desription: linear regression with lagged regressors and regressors based on dates
# Version: 1.0 "Hardcore way"
# Dependencies: 
  # Batches in *.sas7bdat format
# Packages:  "openxlsx", "haven", "data.table" , "glmnet" ,"lubridate"





#### REQUIRED PACKAGES ####

list.of.packages <- c("openxlsx", "haven", "data.table" , "glmnet" ,"lubridate",
                      "gbm","randomForest","data.table", "FNN", "xgboost","Ckmeans.1d.dp",
                      "stats")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library(openxlsx)
library(haven)
library(data.table)
library(gbm)
library(glmnet)	 
library(lubridate)	
library(data.table)
library(randomForest)
library(FNN)
library(xgboost)
library(Ckmeans.1d.dp)
library(stats)

# data : pca
# models:
# lm,ridge,lasso,elastic net, random forest, 
# knn, neural net, deep neural nets, gbm, xgboost


#### PARAMETERS ####

# path<-"//vkovnazcti0023/COL/0-Collections_Strategy_and_System_Support/STRATEGY/Akimenko/pilots/Forecasting_daily/" # current working directory
path<-"/Users/AlexAkimenko/Documents/работа/Citi/Forecasting_daily/"
# path_batches<-"//vkovnazcti0025/Collection_Strategy/Ad_Hocs/Akimenko/"
path_batches<-"/Users/AlexAkimenko/Documents/работа/Citi/Forecasting_daily/"
n.ahead<-30   # number of periods to forecast
options(scipen=999)



#### DATA LOAD & CLEANING ####

source(paste0(path,"Functions.R"))
actuals<-read.xlsx(paste0(path,"input.xlsx"))
actuals$Date<-as.Date(actuals$Date, origin="1899-12-30")
Y<-actuals[,1:15]
batches<-read_sas(paste0(path_batches,"par_643_date.sas7bdat"))
buckets<-data.frame(this_bk=colnames(Y)[2:15],
                    prev_bk=c(NA,colnames(Y)[2:8],NA,colnames(Y)[10:14]),
                    next_bk=c(colnames(Y)[3:9],NA,colnames(Y)[11:15],NA))
if(file.exists(paste0(path,"1_data/lag_dates.rds"))==F){
  lag_dates<-get_lagged_dates(35)
  saveRDS(lag_dates,paste0(path,"1_data/lag_dates.rds"))
} else {
  lag_dates<-readRDS(paste0(path,"1_data/lag_dates.rds"))
}



#### FEATURE ENGINEERING ####


X<-batches[,c("ORACLE_DATE_STAMP","DAY_OF_MONTH", "N_MONTH","N_YEAR","WORKING_DAY_OF_MONTH",
                            "LAST_DAY_OF_A_MONTH_IND","CURRENT_LAST_WORKING_DAY","LAST_WORKING_DAY_OF_MTH_IND",
                            "DAYS_IN_MONTH","WORK_DAYS_IN_MONTH", "LAG_1M_DATE","DAY_OF_WEEK")]
X$days_since_l_batch<-as.factor(X$ORACLE_DATE_STAMP-X$CURRENT_LAST_WORKING_DAY)
X$DAY_OF_MONTH<-as.factor(X$DAY_OF_MONTH)
X$DAY_OF_WEEK<-as.factor(X$DAY_OF_WEEK)
X$LAST_WORKING_DAY_OF_MTH_IND<-as.factor(X$LAST_WORKING_DAY_OF_MTH_IND)
X$cycle_move_1<-ifelse(X$LAST_WORKING_DAY_OF_MTH_IND=="1" & X$DAYS_IN_MONTH<31,31-day(X$ORACLE_DATE_STAMP),0)
X$cycle_move_temp<-ifelse(X$LAST_WORKING_DAY_OF_MTH_IND=="1",X$DAYS_IN_MONTH-day(X$ORACLE_DATE_STAMP),0)
X$cycle_move_2<-ifelse(X$WORKING_DAY_OF_MONTH=="1",X$cycle_move_temp[match(X$LAG_1M_DATE,X[,1])],0)
X$cycle_move_temp<-NULL
X$LAG_1M_DATE<-NULL
X$CURRENT_LAST_WORKING_DAY<-NULL
X<-X[is.na(X$DAY_OF_MONTH)==F,]
X<-X[is.na(X$days_since_l_batch)==F,]

# one-hot encoding for glmnet#
X<-with(X,
     data.frame(X[,!(colnames(X) %in% c("DAY_OF_MONTH","days_since_l_batch","DAY_OF_WEEK"))],
                model.matrix(~DAY_OF_MONTH-1,X),model.matrix(~days_since_l_batch-1,X), model.matrix(~DAY_OF_WEEK-1,X)))


#### INPUT DATA & CLEANING ####

## normalization ##
for (i in 2:ncol(Y)){
  Y[,i]<-(Y[,i]-mean(Y[,i]))/sd(Y[,i])
}

## cleaning ##
# 
dates_cleaned<-Y[Y$PILB1<=3,1]
dates_train<-dates_cleaned[!(dates_cleaned<as.Date("2014/6/27") | 
                   (dates_cleaned<as.Date("2016/1/15") & dates_cleaned>=as.Date("2015/12/30")) |
                   (dates_cleaned<as.Date("2015/9/11") & dates_cleaned>=as.Date("2015/9/7")) |
                     dates_cleaned>=as.Date("2016/6/27"))] 
dates_test<-dates_cleaned[dates_cleaned>=as.Date("2016/6/27")]

## input data ##
X_train_list<-NULL
X_test_list<-NULL
y_train_list<-NULL
y_test_list<-NULL
bucket_name_list<-NULL

i=1
for (bucket_name in colnames(Y)[-1]){
  #bucket_name<-colnames(Y)[3]

  X_train_lagged<-X_lagged_create(bucket_name,dates_train,Y)
  X_test_lagged<-X_lagged_create(bucket_name,dates_test,Y)
  X_train<-X[match(dates_train,X[,1]),-1]
  X_test<-X[match(dates_test,X[,1]),-1]
  X_train<-cbind(X_train,X_train_lagged)
  X_test<-cbind(X_test,X_test_lagged)
  y_train<-Y[match(dates_train,Y[,1]),bucket_name]
  y_test<-Y[match(dates_test,Y[,1]),bucket_name]
  bucket_name_list[[i]]<-bucket_name
  X_train_list[[i]]<-X_train
  X_test_list[[i]]<-X_test
  y_train_list[[i]]<-y_train
  y_test_list[[i]]<-y_test
  i=i+1
} 
  

#### LM MODELS ####

# out<-data.table()

if(file.exists(paste0(path,"out.rds"))==F){
  out<-data.table()
} else {
  out<-readRDS(paste0(path,"out.rds"))
}

weights_lm<-(1:nrow(X_train))*(1/nrow(X_train))+1

for (i in 1:length(bucket_name_list)){
  X_train<-X_train_list[[i]]
  X_test<-X_test_list[[i]]
  y_train<-y_train_list[[i]]
  y_test<-y_test_list[[i]]
  fit_lm <- lm(y_train~.,X_train,weights = weights_lm)
  summary(fit_lm)
  y_train_pred<-predict(fit_lm,X_train)
  y_test_pred<-predict(fit_lm,X_test)
  mse(y_train_pred,y_train)
  mse(y_test_pred,y_test)
  plot(x=dates_test, y=y_test)
  lines(x=dates_test, y=y_test_pred)
  out_i<-data.frame(model="lm weighted - v6 (last 6 M, no pca)",bucket=bucket_name_list[[i]],
                    mse_train=mse(y_train_pred,y_train),mse_test=mse(y_test_pred,y_test))
  out<-rbind(out,out_i)
}
  

#### STEPWISE REGRESSION ####

for (i in 1:length(bucket_name_list)){
  X_train<-X_train_list[[i]]
  X_test<-X_test_list[[i]]
  y_train<-y_train_list[[i]]
  y_test<-y_test_list[[i]]
  fit_lm <- lm(y_train~.,X_train,weights = weights_lm)
  fit_lm_clean<-step(fit_lm,direction="backward",test="F",trace=F,weights = weights_lm)
  y_train_pred<-predict(fit_lm_clean,X_train)
  y_test_pred<-predict(fit_lm_clean,X_test)
  plot(x=dates_test, y=y_test)
  lines(x=dates_test, y=y_test_pred)
  out_i<-data.frame(model="stepwise regresson weighted - v1 (no outliers, no pca)",bucket=bucket_name_list[[i]],
                    mse_train=mse(y_train_pred,y_train),mse_test=mse(y_test_pred,y_test))
  out<-rbind(out,out_i)
}


#### GLMNET ####

for (i in 1:length(bucket_name_list)){
  X_train<-X_train_list[[i]]
  X_test<-X_test_list[[i]]
  y_train<-y_train_list[[i]]
  y_test<-y_test_list[[i]]
  fit_glmnet <- glmnet(get_matrix(X_train), y_train,weights = weights_lm)
  plot(fit_glmnet)
  print(fit_glmnet)  
  #coef(fit_glmnet,s=0.005)
  y_train_pred<-predict(fit_glmnet,get_matrix(X_train),s=0.005)
  y_test_pred<-predict(fit_glmnet,get_matrix(X_test),s=0.005)
  plot(x=dates_test, y=y_test)
  lines(x=dates_test, y=y_test_pred)
  out_i<-data.frame(model="penalized regresson weighted - v6 (last 6 M, no pca)",bucket=bucket_name_list[[i]],
                    mse_train=mse(y_train_pred,y_train),mse_test=mse(y_test_pred,y_test))
  out<-rbind(out,out_i)
}


#### RANDOM FOREST ####

for (i in 1:length(bucket_name_list)){
  X_train<-X_train_list[[i]]
  X_test<-X_test_list[[i]]
  y_train<-y_train_list[[i]]
  y_test<-y_test_list[[i]]
  fit_rf <- randomForest(X_train, y_train, ntree=1000, importance=TRUE)
  #imp_rf<-data.frame(importance(fit_rf))
  #imp_rf<-imp_rf[order(-imp_rf$IncNodePurity),]
  #top_15<-rownames(imp_rf)[1:15]
  #fit_rf <- randomForest(X_train[,top_15], y_train, ntree=600, importance=TRUE)
  #varImpPlot(fit_rf)
  y_train_pred<-predict(fit_rf,X_train)
  y_test_pred<-predict(fit_rf,X_test)
  mse(y_train_pred,y_train)
  mse(y_test_pred,y_test)
  plot(x=dates_test, y=y_test)
  lines(x=dates_test, y=y_test_pred)
  out_i<-data.frame(model="random forest - v2 (clened, no pca)",bucket=bucket_name_list[[i]],
                    mse_train=mse(y_train_pred,y_train),mse_test=mse(y_test_pred,y_test))
  out<-rbind(out,out_i)
}
saveRDS(out,paste0(path,"out.rds"))
# no weightening for regression


#### KNN ####

for (i in 1:length(bucket_name_list)){
  X_train<-get_matrix(X_train_list[[i]])
  X_test<-get_matrix(X_test_list[[i]])
  y_train<-y_train_list[[i]]
  y_test<-y_test_list[[i]]
  
  knn_perf<-NULL
  for (k_i in 1:40) {
    y_test_pred<-knn.reg(X_train,X_test,y_train,k=k_i)[[4]]
    knn_perf_i<-data.table(k=k_i,mse=mse(y_test_pred,y_test))
    knn_perf<-rbind(knn_perf,knn_perf_i)
  }
  k_final<-which.min( knn_perf$mse ) 
  y_train_pred<-knn.reg(X_train,X_train,y_train,k=k_final)[[4]]
  y_test_pred<-knn.reg(X_train,X_test,y_train,k=k_final)[[4]]
  plot(x=dates_test, y=y_test)
  lines(x=dates_test, y=y_test_pred)
  out_i<-data.frame(model="knn - v1 (clened, no pca)",bucket=bucket_name_list[[i]],
                    mse_train=mse(y_train_pred,y_train),mse_test=mse(y_test_pred,y_test))
  out<-rbind(out,out_i)
}
saveRDS(out,paste0(path,"out.rds"))


#### PCA ####

i=13
X_train<-get_matrix(X_train_list[[i]])
X_train_pca <- prcomp(X_train[,!(colnames(X_train) %in% c("days_since_l_batch6","DAY_OF_WEEK1"))],
                 center = TRUE) 
plot(X_train_pca, type = "l")
X_test<-get_matrix(X_test_list[[i]])
y_train<-y_train_list[[i]]
y_test<-y_test_list[[i]]


#### XGBOOST ####

for (i in 1:length(bucket_name_list)){
  X_train<-get_matrix(X_train_list[[i]])
  X_test<-get_matrix(X_test_list[[i]])
  y_train<-y_train_list[[i]]
  y_test<-y_test_list[[i]]
  xgbMatrix <- xgb.DMatrix(data=X_train, 
                           label = y_train, 
                           weight = weights_lm)
  cv_xgboost <- xgb.cv(data=xgbMatrix,
                         nfold = 5, maximize = FALSE,  early.stop.round = 8,
                          nrounds = 1200, objective ="reg:linear", max.depth = 4,
                          eta = 0.1, min_child_weight = 20,colsample_bytree = 0.1)
  fit_xgboost <- xgboost(data=xgbMatrix,
                       nrounds = nrow(cv_xgboost), objective ="reg:linear", max.depth = 4,
                       eta = 0.1, min_child_weight = 20,colsample_bytree = 0.1)
  #importance_matrix <- xgb.importance(feature_names =colnames(X_train), model = fit_xgboost)
  #xgb.plot.importance(head(importance_matrix,10))
  y_train_pred<-predict(fit_xgboost,X_train)
  y_test_pred<-predict(fit_xgboost,X_test)
  plot(x=dates_test, y=y_test)
  lines(x=dates_test, y=y_test_pred)
  plot(x=dates_train, y=y_train)
  lines(x=dates_train, y=y_train_pred)
  mse(y_test_pred,y_test)
  mse(y_train_pred,y_train)
  out_i<-data.frame(model="XGBoost weighted - v6 (last 6 M, no pca)",bucket=bucket_name_list[[i]],
                    mse_train=mse(y_train_pred,y_train),mse_test=mse(y_test_pred,y_test))
  out<-rbind(out,out_i)
}
#out<-out[-c((nrow(out)-13):nrow(out)),]




### SUMMARY ### 

out[bucket!="PILB7" | bucket!="CCB7",.(mse_train=mean(mse_train),mse_test=mean(mse_test)),by=model]
saveRDS(out,paste0(path,"out.rds"))
write.csv(out,paste0(path,"out.csv"))


#### VISUALIZATION ####

X_train<-cbind(X_train,Y[match(dates_train,Y[,1]),4])
X_train<-X_train[X_train$N_YEAR==2016,]
X_train_cast<-dcast(X_train, N_MONTH ~ DAY_OF_MONTH , fun=mean)
write.csv(X_train_cast,paste0(path,"X_train_cast.csv"))
