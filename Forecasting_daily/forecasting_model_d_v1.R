library(forecast)
# install.packages("openxlsx")
# install.packages("glmnet")
library(openxlsx)
library(haven)
library(reshape)
library(data.table)
library(gbm)
library(glmnet)	 




#### PARAMETERS ####

# path<-"//vkovnazcti0023/COL/0-Collections_Strategy_and_System_Support/STRATEGY/Akimenko/pilots/Forecasting_daily/" # current working directory
path<-"/Users/AlexAkimenko/Documents/работа/Citi/Forecasting_daily/"
path_batches<-"/Users/AlexAkimenko/Documents/работа/Citi/Forecasting_daily/"
n.ahead<-30   # number of periods to forecast


####### FUNCTIONS ###########

MAPE<-function(y_pred,y_test){
  return(mean(abs(y_test-y_pred)/y_test))
}

performance<-function(y_pred,y_test){
  r2<-round(cor(y_pred,y_test)^2*100,1)
  MAPE=round(mean(abs(y_test-y_pred)/y_test)*100,1)
  MaxAPE<-round(max(abs(y_test-y_pred)/y_test)*100,1)
  return(paste("MAPE=",MAPE,"%; MaxAPE=",MaxAPE,"%; R2=",r2,"%"))
}

forecast_lm<-function(dates_train,dates_test,X,Y){ #Y and X need to have dates as 1st var
  X_train<-X[match(dates_train,X[,1]),-1]
  X_test<-X[match(dates_test,X[,1]),-1]
  Y_train<-Y[match(dates_train,Y[,1]),]
  bucket_name<-"CCB1" # another loop should go here
  X_train_lagged<-X_lagged_create(bucket_name,dates_train,Y)
  X_train<-rbind(X_train,X_train_lagged)
  y_train<-Y_train[,bucket_name]
  lm_fit<-lm(y_train~.,X_train)
  print(summary(lm_fit))
  for (n.ahead in 1:length(dates_test)){
    X_test_lagged<-X_lagged_create(bucket_name,dates_test[n.ahead],Y)
    
  }
  return(predict(lm_fit,X_test))
}

forecast_gbm<-function(X_train,X_test,y_train){
  gbm_fit= gbm(y_train~., X_train,
              n.trees=2000,
              shrinkage=0.1,
              distribution="gaussian",
              interaction.depth=10,
              bag.fraction=0.9,
              train.fraction = 0.8,
              #cv.fold=3,
              n.minobsinnode = 50
  )
  print(summary.gbm(gbm_fit))
  return(predict(gbm_fit,X_test,n.trees=gbm.perf(gbm_fit),single.tree=FALSE))
}

X_lagged_create<-function(bucket_name,actual_date,actuals){
  actuals_sampled<-actuals[match(actual_date,actuals[,1]),]
  X_lagged<-NULL
  for (i in 1:3){ 
    bucket_name_i<-as.character(buckets[buckets$this_bk==bucket_name,i])
    if(is.na(bucket_name_i)==F){
      lag_values<-data.frame(actual_date,value=actuals_sampled[,bucket_name_i])
      lag_values<-merge(lag_dates,lag_values,by.x="lag_date",by.y="actual_date")
      lag_values$var_name<-paste0(colnames(buckets)[i],"_",lag_values$variable)
      X_lagged<-rbind(X_lagged,lag_values)
    }
  }
  X_lagged<-dcast(X_lagged,actual_date~var_name,mean)
  X_lagged<-X_lagged[match(actual_date,X_lagged[,1]),]
  X_lagged[,1]<-NULL
  return(X_lagged)
}
# system.time(X_lagged_create("CCB1",actual_date,actuals))

get_lagged_dates<-function(max_lag=35){
  lag_dates<-data.frame(actual_date=batches$ORACLE_DATE_STAMP[batches$HOLIDAY_IND=="0"])
  for (i in 1:max_lag){
    lag_dates$lag_date_calendar<-lag_dates$actual_date-i
    lag_dates<-merge(lag_dates,batches[,c("ORACLE_DATE_STAMP","NEXT_WRKNG_DT","HOLIDAY_IND")],by.x="lag_date_calendar",by.y="ORACLE_DATE_STAMP")
    lag_dates$lag_date<-as.Date(ifelse(lag_dates$HOLIDAY_IND=="1",lag_dates$NEXT_WRKNG_DT,lag_dates$lag_date_calendar),origin = "1970-01-01")
    lag_dates<-lag_dates[,-c(1,ncol(lag_dates)-2,ncol(lag_dates)-1)]
    colnames(lag_dates)[ncol(lag_dates)]<-paste0("lag_",i,"d")
  }
  lag_dates_melted<-melt(lag_dates,id="actual_date")
  colnames(lag_dates_melted)[3]<-"lag_date"
  return(lag_dates_melted)
}
  

#### DATA LOAD ####

actuals<-read.xlsx(paste0(path,"input.xlsx"))
actuals$Date<-as.Date(actuals$Date, origin="1899-12-30")
batches<-read_sas(paste0(path_batches,"par_643_date.sas7bdat"))
buckets<-data.frame(this_bk=colnames(actuals)[2:15],
                    prev_bk=c(NA,colnames(actuals)[2:8],NA,colnames(actuals)[10:14]),
                    next_bk=c(colnames(actuals)[3:9],NA,colnames(actuals)[11:15],NA))
if(file.exists(paste0(path,"lag_dates.rds"))==F){
  lag_dates<-get_lagged_dates(35)
  saveRDS(lag_dates,paste0(path,"lag_dates.rds"))
} else {
  lag_dates<-readRDS(paste0(path,"lag_dates.rds"))
}


#### FEATURE ENGINEERING ####

X<-batches[,c("ORACLE_DATE_STAMP","DAY_OF_MONTH", "N_MONTH","N_YEAR","WORKING_DAY_OF_MONTH",
                            "LAST_DAY_OF_A_MONTH_IND","CURRENT_LAST_WORKING_DAY","LAST_WORKING_DAY_OF_MTH_IND",
                            "DAYS_IN_MONTH","WORK_DAYS_IN_MONTH")]
X$days_since_l_batch<-as.factor(X$ORACLE_DATE_STAMP-X$CURRENT_LAST_WORKING_DAY)
X$DAY_OF_MONTH<-as.factor(X$DAY_OF_MONTH)
X$LAST_DAY_OF_A_MONTH_IND<-as.factor(X$LAST_DAY_OF_A_MONTH_IND)
X$LAST_WORKING_DAY_OF_MTH_IND<-as.factor(X$LAST_WORKING_DAY_OF_MTH_IND)
X$CURRENT_LAST_WORKING_DAY<-NULL







#### LM MODEL ####

bucket_name<-"PILB3"
actuals_i<-actuals[,bucket_name] 


x_lagged<-x_lagged_create(bucket_name)
X<-cbind(regr,x_lagged)
end<-length(actuals_i)-n.ahead
start<-36
dates<-actuals[end:length(actuals_i),1]

X_train<-X[start:end,]
X_test<-X[end:nrow(X),]
y_train<-actuals_i[start:end]
y_test<-actuals_i[end:length(actuals_i)]

y_pred<-forecast_lm(X_train,X_test,y_train,bucket_name)
# y_pred<-forecast_gbm(X_train,X_test,y_train)
performance(y_pred,y_test)

plot(y=y_pred,x=dates)
lines(y=y_test,x=dates)
  



#### INVESTIGATION ####
# actuals_ts<-ts(actuals[,-1], start=actuals[1,1], end=actuals[nrow(actuals),1],frequency = 365)

# ts_i<-actuals_ts[,1]
# 
# x2 <- msts(actuals[,2], seasonal.periods=c(7,365.25))
# fit <- tbats(x2)
# x.sa <- seasadj(fit)
# 
# v[,17]<-as.factor(v[,17])
# v[,18]<-as.factor(v[,18])
# v[,19]<-as.factor(v[,19])
# v[,20]<-as.factor(v[,20])
# v[,21]<-as.factor(v[,21])
# v[,22]<-as.factor(v[,22])
