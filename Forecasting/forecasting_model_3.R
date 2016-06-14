#install.packages("forecast")
#install.packages("reshape")
#install.packages("xts")
#install.packages("plyr")
library(forecast)
library(reshape)
library(xts)
library(plyr)

forecastStl <- function(myTs,var_name, n.ahead = 6) {
  fit.stl <- stl(myTs,t.window=8, s.window = 'periodic')
  sts <- fit.stl$time.series
  fore <- forecast(fit.stl, h = n.ahead, level = 95)
  fore.seasonal<-lag(sts[,1],-12)*(fore$mean/fore$mean)
  fore.trend<-fore$mean-fore.seasonal
  plot(sts,main=paste0(var_name," - STL Decompose"))
  pdf(paste0(path,"output/",var_name,"_stl_decompose.pdf"))
  plot(sts)
  dev.off( )
  return(cbind(forecast=fore$mean,
               trend=fore.trend,seasonal=fore.seasonal))
}


avg_l3_ns_months<-function(myTs,n.ahead = 6){
  myTs_cleared<-subset(myTs,season=c(2,3,4,6,7,8,9,10,11))
  pred<-rep(mean(tail(myTs_cleared,3)),n.ahead )
  start<-tsp(myTs)[2]+2/12
  start<-c(trunc(start),start%%1*12)     # incorrect for Dec!
  pred<-ts(pred,start=start, freuency=12) 
  return(pred)
}




#path<-"/Users/AlexAkimenko/Documents/работа/Citi/Forecasting/"
path<-"//vkovnazcti0023/COL/0-Collections_Strategy_and_System_Support/STRATEGY/Akimenko/pilots/forecasting/"
v<-read.csv(paste0(path,"input.csv"),header=T,sep=";")
variables<-colnames(v)[2:ncol(v)]
start_month<-c(2012,12)  # correct - needs to be selected from input data (min month+1 due to net flow logic)
end_month<-c(2016, 4) # correct - needs to be selected from input data
forecast_month<-forecast_month<-format(Sys.time(), "%Y-%m-01")
n.ahead<-6
v<-ts(v[,-1], start=start_month, end=end_month, frequency=12)




# net flow model works on product level only. Thus it requires specific input- time series data ("v") with column names containing product name ("prod")

net_flow_model<-function(prod,v=v, n.ahead=6){
  #output<-ts()
  ts<-v[,grep(prod, colnames(v))]
  for (i in 2:ncol(ts)){ 
    y<-ts[,i]/lag(ts[,i-1],-1) ### net flows calculation
    var_name<-paste0(colnames(ts)[i],"NF")
    fore<-forecastStl(y,var_name,n.ahead)
    y_pred<-fore[,1]
    y_decom<-fore[,-1]
    name<-paste0(var_name,"_",colnames(y_decom))
    nf<-c(as.xts(y),as.xts(y_pred))
    colnames(nf)<-var_name
    if (grepl("0", colnames(ts)[i-1])){
      fore<-forecastStl(ts[,i-1],colnames(ts)[i-1],n.ahead)
      bucket0_pred<-fore[,1]
      bucket0_decom<-fore[,-1]
      bucket0<-c(as.xts(ts[,i-1]),as.xts(bucket0_pred))
      bucket1<-c(as.xts(ts[,i]),lag(bucket0,1)*as.xts(y_pred))
      output<-cbind(bucket0,bucket1)
      output2<-nf
      output3<-cbind(bucket0_decom,y_decom)
      names<-c(paste0(colnames(ts)[i-1],"_",colnames(bucket0_decom)),name)
    } else {
      bucket<-c(as.xts(ts[,i]),lag(output[,i-1],1)*as.xts(y_pred)) # здесь берется из таблицы с actuals+forecast
      output<-cbind(output,bucket)
      output2<-cbind(output2,nf)
      output3<-cbind(output3,y_decom)
      names<-c(names,name)
    }
  }
  colnames(output3)<-names
  colnames(output)<-colnames(v)[grep(prod, colnames(v))]
  return (list(tail(output,n.ahead+1),output2,as.xts(output3)))
}





MAPE<-function(prod,v=v, n.ahead=6){
  test_df<-data.frame()
  for (i in 26:nrow(v)){
    end<-tsp(v)[1]+i/12
    end<-c(trunc(end),end%%1*12)
    ts<-v[,grep(prod, colnames(v))]
    ts<-window(ts,end=end)
    ts_pred<-net_flow_model(prod,ts,n.ahead)[[1]]
    ts_actual<-as.xts(v[,grep(prod, colnames(v))])*(ts_pred/ts_pred)
    MAPE<-abs(ts_pred-ts_actual)/ts_actual
    colnames(MAPE)<-paste0(colnames(MAPE),"_MAPE")
    colnames(ts_pred)<-paste0(colnames(ts_pred),"_Forecast")
    colnames(ts_actual)<-paste0(colnames(ts_actual),"_Actual")
    n_ahead<-0:n.ahead
    df<-cbind(ts_actual,ts_pred,MAPE)
    df<-data.frame(YearMonth=as.Date(time(df)),as.matrix(df), n_ahead)
    df<-melt(df,id=c("YearMonth","n_ahead"))
    test_df<-rbind(test_df,df)
  }
  return(test_df)
}




###  validation summary ####
MAPE_CC<-MAPE("CC",v,n.ahead)
MAPE_CC<-MAPE_CC[MAPE_CC$YearMonth>=seq(max(MAPE_CC$YearMonth), length=2, by="-2 years")[2],] # this to have foreast and actuals for last 2 years always
t1<-MAPE_CC[grep("MAPE",MAPE_CC$variable),]
t1<-aggregate(t1$value,by=list(t1$variable,t1$n_ahead),FUN=mean, na.rm=TRUE)

MAPE_PIL<-MAPE("PIL",v,n.ahead)
t2<-MAPE_PIL[grep("MAPE",MAPE_PIL$variable),]

output2<-rbind(t1,t2)
colnames(output2)<-c("variable","n_ahead","MAPE")
output2<-output2[output2$n_ahead>0,]
t3<-aggregate(output2$MAPE,by=list(output2$variable),FUN=mean, na.rm=TRUE)

colnames(t3)<-c("variable","MAPE")
t3$n_ahead<-"Cumulative"
output2<-rbind(output2,t3)
output2$forecast_month<-forecast_month
write.table(output2, paste0(path,"/output/validation_summary.txt"), sep="\t",row.names=F)	

# CC
# last 3 months average model - 14.4%, 12.5%, 13.6%, 13.4%, 12.2%, 10.6% (12.8%)
# bucket stl model - 4.7%, 6.5%, 6.8%, 10.5%, 12.9%, 10.8%, 9.4%  (8.8%)
# accuracy increase is 31%
# net flow stl model - 4.7%, 8.0%, 5.6%, 4.1%, 4.0%, 2.8%, 2.7% (4.6%)
# accuracy increase is 48% (64% total!)

# PIL
# last 3 months average model - 6.3%, 8.6%, 9.5%, 11.0%, 11.7% (9.4%)
# bucket stl model 4.8%, 13.3%, 8.4%, 11.4%, 15.0% (10.6%)
# net flow stl model - 4.8%, 11.6%, 10.1%, 9.8%, 13.4% (9.9%)
# accuracy increase is 6% (-5% total)


###  validation results
output<-rbind(MAPE_CC,MAPE_PIL)
output_c<-cast(output, n_ahead + variable ~ YearMonth)
output_c[is.na(output_c)] <- ""
output_c<-output_c[!(grepl("Actual",output_c$variable) & output_c$n_ahead!=0),]
output_c<-output_c[!(!grepl("Actual",output_c$variable) & output_c$n_ahead==0),]
write.table(output_c, paste0(path,"/output/validation_results.txt"), sep="\t",row.names=F)	

###  forecast
pred_CC<-net_flow_model("CC",v,n.ahead)
pred_PIL<-net_flow_model("PIL",v,n.ahead)

CC_PIL<-cbind(pred_CC[[1]],pred_PIL[[1]])
CC_PIL_df<-data.frame(t(coredata(CC_PIL)))
colnames(CC_PIL_df)<-as.Date(time(CC_PIL))
CC_PIL_df<-cbind(variable=rownames(CC_PIL_df),CC_PIL_df)

CC_PIL_nf<-tail(cbind(pred_CC[[2]],pred_PIL[[2]]),25)
CC_PIL_nf_df<-data.frame(t(coredata(CC_PIL_nf)))
colnames(CC_PIL_nf_df)<-as.Date(time(CC_PIL_nf))
CC_PIL_nf_df<-cbind(variable=rownames(CC_PIL_nf_df),CC_PIL_nf_df)

CC_PIL_decom<-cbind(pred_CC[[3]],pred_PIL[[3]])
CC_PIL_decom_df<-data.frame(t(coredata(CC_PIL_decom)))
colnames(CC_PIL_decom_df)<-as.Date(time(CC_PIL_decom))
CC_PIL_decom_df<-cbind(variable=rownames(CC_PIL_decom_df),CC_PIL_decom_df)

output3<-rbind.fill(CC_PIL_nf_df,CC_PIL_df,CC_PIL_decom_df)
output3[is.na(output3)] <- ""
write.table(output3, paste0(path,"/output/forecast.txt"), sep="\t",row.names=F)	

#### deep dive into variables


for (i in 26:38){
  end<-tsp(v)[1]+i/12
  end<-c(trunc(end),end%%1*12)
  ts<-window(v[,8],end=end)
  fit.stl <- stl(ts,t.window=100, s.window = 'periodic')
  fore <- forecast(fit.stl, h = n.ahead, level = 95)
  if (i==26){
    plot(v[,8])
    lines(fore$mean,col="red")
  } else {
    lines(fore$mean,col="red")
  }
}
for (i in 26:38){
  end<-tsp(v)[1]+i/12
  end<-c(trunc(end),end%%1*12)
  ts<-window(v,end=end)
  ts_pred<-net_flow_model(prod,ts,n.ahead)
  if (i==26){
    plot(as.xts(v[,9]))
    lines(ts_pred[,2],col="red")
  } else {
    lines(ts_pred[,2],col="red")
  }
}

forecastStl <- function(myTs,var_name, n.ahead = 6) {
  fit.stl <- stl(myTs,t.window=8, s.window = 'periodic')
  sts <- fit.stl$time.series
  fore <- forecast(fit.stl, h = n.ahead, level = 95)
  #plot(sts,main=paste0(var_name," - STL Decompose"))
  #pdf(paste0(path,"output/",var_name,"_stl_decompose.pdf"))
  #plot(sts)
  #dev.off( )
  return(fore$mean)
}



#### deep dive into variables (avg_l3_ns_months)

for (i in 26:38){
  end<-tsp(v)[1]+i/12
  end<-c(trunc(end),end%%1*12)
  ts<-window(v[,8],end=end)
  fore <- avg_l3_ns_months(ts,6)
  if (i==26){
    plot(v[,8])
    lines(fore,col="red")
  } else {
    lines(fore,col="red")
  }
}


#install.packages("stsm")
#install.packages("stsm.class")
library(stsm.class)
library(stsm)
t<-window(v[,1],start=c(2013,1),end=c(2015,12))
m <- stsm.model(model = "BSM", y = t, transPars = "StructTS")
fit2 <- stsmFit(m, stsm.method = "maxlik.td.optim", method = "L-BFGS-B", 
                KF.args = list(P0cov = TRUE))
