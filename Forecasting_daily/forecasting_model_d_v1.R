#### DAILY FORECAST ####

# Author: Aleksandr Akimenko
# Date: Sep'16
# Desription: linear regression with lagged regressors and regressors based on dates
# Version: 1.0 "Hardcore way"
# Dependencies: 
  # Batches in *.sas7bdat format
# Packages:  
  # install.packages("openxlsx")
  # install.packages("haven")
  # install.packages("data.table")


#### REQUIRED PACKAGES ####

# library(forecast)
# install.packages("openxlsx")
# install.packages("glmnet")
library(openxlsx)
library(haven)
# library(reshape)
library(data.table)
# library(gbm)
library(glmnet)	 
library(lubridate)	 


#### PARAMETERS ####

path<-"//vkovnazcti0023/COL/0-Collections_Strategy_and_System_Support/STRATEGY/Akimenko/pilots/Forecasting_daily/" # current working directory
# path<-"/Users/AlexAkimenko/Documents/работа/Citi/Forecasting_daily/"
path_batches<-"//vkovnazcti0025/Collection_Strategy/Ad_Hocs/Akimenko/"
# path_batches<-"/Users/AlexAkimenko/Documents/работа/Citi/Forecasting_daily/"
n.ahead<-30   # number of periods to forecast


####### FUNCTIONS ###########

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

get_lagged_dates<-function(max_lag=35){
  lag_dates<-data.frame(actual_date=batches$ORACLE_DATE_STAMP[batches$HOLIDAY_IND=="0"])
  for (i in 1:max_lag){
    lag_dates$lag_date_calendar<-lag_dates$actual_date-i
    lag_dates<-merge(lag_dates,batches[,c("ORACLE_DATE_STAMP","CURRENT_LAST_WORKING_DAY","HOLIDAY_IND")],by.x="lag_date_calendar",by.y="ORACLE_DATE_STAMP")
    lag_dates$lag_date<-as.Date(ifelse(lag_dates$HOLIDAY_IND=="1",lag_dates$CURRENT_LAST_WORKING_DAY,lag_dates$lag_date_calendar),origin = "1970-01-01")
    lag_dates<-lag_dates[,-c(1,ncol(lag_dates)-2,ncol(lag_dates)-1)]
    colnames(lag_dates)[ncol(lag_dates)]<-paste0("lag_",i,"d")
  }
  lag_dates_melted<-melt(lag_dates,id="actual_date")
  colnames(lag_dates_melted)[3]<-"lag_date"
  return(lag_dates_melted)
}

X_lagged_create<-function(bucket_name,actual_date,Y){
  X_lagged<-NULL
  for (i in 1:3){ 
    bucket_name_i<-as.character(buckets[buckets$this_bk==bucket_name,i])
    if(is.na(bucket_name_i)==F){
      lag_values<-data.frame(actual_date=Y[,1],value=Y[,bucket_name_i])
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

create_lm<-function(dates_train,dates_test,bucket_name,X,Y){
  X_train_lagged<-X_lagged_create(bucket_name,dates_train,Y)
  X_test_lagged<-X_lagged_create(bucket_name,dates_test,Y)
  X_train<-X[match(dates_train,X[,1]),-1]
  X_test<-X[match(dates_test,X[,1]),-1]
  X_train<-cbind(X_train,X_train_lagged)
  X_test<-cbind(X_test,X_test_lagged)
  y_train<-Y[match(dates_train,Y[,1]),bucket_name]
  y_test<-Y[match(dates_test,Y[,1]),bucket_name]
  fit_lm <- lm(y_train~.,X_train)
  print(summary(fit_lm))
  y_pred<-predict(fit_lm,X_test)
  print(performance(y_pred,y_test))
  fit_lm_clean<-step(fit_lm,direction="backward",test="F",trace=F)
  print(summary(fit_lm_clean))
  y_pred<-predict(fit_lm_clean,X_test)
  print(performance(y_pred,y_test))
  plot(y=y_test,x=dates_test)
  lines(y=y_pred,x=dates_test)
  return(fit_lm_clean)
}

forecast_lm<-function(dates_pred,X,Y){ #Y and X need to have dates as 1st var
  new_rows<-data.frame(dates_pred,matrix(c(rep.int(NA,length(dates_pred)*(ncol(Y)-1))),nrow=length(dates_pred),ncol=ncol(Y)-1))
  colnames(new_rows)<-colnames(Y)
  Y_actual<-Y[!(Y$Date %in% dates_pred), ]
  Y_pred<-rbind(Y_actual,new_rows)
  for (n in 1:length(dates_pred)){
    for (bucket_name in colnames(Y_pred)[-1]){
      dates_test_i<-dates_pred[n]
      fit_lm<-get(paste0(bucket_name,"_lm"))
      X_test<-X[match(dates_test_i,X[,1]),-1]
      X_test_lagged<-X_lagged_create(bucket_name,dates_test_i,Y_pred)
      X_test<-cbind(X_test,X_test_lagged)
      y_pred<-predict(fit_lm,X_test)
      Y_pred[Y_pred$Date==dates_test_i,bucket_name]<-y_pred
    }
  }
  return(tail(Y_pred,length(dates_pred)))
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

performance<-function(y_pred,y_test){
  r2<-round(cor(y_pred,y_test)^2*100,1)
  MAPE=round(mean(abs(y_test-y_pred)/y_test)*100,1)
  MaxAPE<-round(max(abs(y_test-y_pred)/y_test)*100,1)
  return(paste("MAPE=",MAPE,"%; MaxAPE=",MaxAPE,"%; R2=",r2,"%"))
}


#### DATA LOAD ####

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


#### LM MODELS ####

for (bucket_name in colnames(Y)[-1]){
  dates_train<-Y[35:(nrow(Y)-35),1]
  dates_test<-Y[(nrow(Y)-34):nrow(Y),1]
  fit_lm<-create_lm(dates_train,dates_test,bucket_name,X,Y)
  #saveRDS(fit_lm,paste0(path,"2_models/",bucket_name,"_lm.rds"))
  assign(paste0(bucket_name,"_lm"),fit_lm)
}


#### GET FORECAST ####

for (bucket_name in colnames(Y)[-1]){
  fit_lm<-readRDS(paste0(path,"2_models/",bucket_name,"_lm.rds"))
    assign(paste0(bucket_name,"_lm"),fit_lm)
}

for (bucket_name in colnames(Y)[-1]){
  fit_lm<-get(paste0(bucket_name,"_lm"))
  print(paste0(paste0(bucket_name,"_lm")," - R^2: ",summary(fit_lm)$r.squared))
}


dates_pred<-lag_dates[lag_dates$actual_date > max(Y[,1]),1][1:n.ahead]

Y_pred<-forecast_lm(dates_pred,X,Y)

for (i in (1:12)){
  print(paste0(colnames(Y)[i+1]," - ",
               performance(Y_pred[,i+1],tail(Y[,i+1],nrow(Y_pred)))))
} 








#### INVESTIGATION ####

bucket_name<-colnames(Y)[11]
dates_train<-Y[35:(nrow(Y)-70),1] # dates_train<-Y[Y$Date<as.Date("2015/12/15"),1] 
dates_test<-Y[(nrow(Y)-69):nrow(Y)-35,1] # dates_test<-Y[Y$Date>=as.Date("2015/12/15"),1] 
X_train_lagged<-X_lagged_create(bucket_name,dates_train,Y)
X_test_lagged<-X_lagged_create(bucket_name,dates_test,Y)
X_train<-X[match(dates_train,X[,1]),-1]
X_test<-X[match(dates_test,X[,1]),-1]
X_train<-cbind(X_train,X_train_lagged)
X_test<-cbind(X_test,X_test_lagged)
y_train<-Y[match(dates_train,Y[,1]),bucket_name]
y_test<-Y[match(dates_test,Y[,1]),bucket_name]


fit_lm <- lm(y_train~.,X_train)
summary(fit_lm)
y_pred<-predict(fit_lm,X_test)
performance(y_pred,y_test)
plot(x=dates_test, y=y_test)
lines(x=dates_test, y=y_pred)

fit_lm_clean<-step(fit_lm,direction="backward",test="F",trace=F)
print(summary(fit_lm_clean))
y_pred<-predict(fit_lm_clean,X_test)
print(performance(y_pred,y_test))


#### GLMNET ####

fit_glmnet <- glmnet(get_matrix(X_train), y_train)
plot(fit_glmnet)
coef(fit_glmnet,s=1)
y_pred<-predict(fit_glmnet,get_matrix(X_test),s=c(1))
performance(y_pred,y_test)


write.csv(batches,paste0(path,"batches.csv"))
