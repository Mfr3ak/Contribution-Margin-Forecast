# Need no user input every thing is based on mtd_bind.csv

setwd("C:/Users/MADIMUL/Downloads/Work/CM Forecast/Processed Files/")  
data<-read.csv("mtd_bind.csv")
# This line must be run at this point to get dates in consistent format
data$datestamp<-as.Date(paste(data$Invoicing.Date_2,data$Invoicing.Date_1,"28",sep = "-")) 
# data_a<-read.csv("m_acc_bind.csv")
data_op1<-read.csv("Op File 1.csv")
data_op2<-read.csv("Op File 2.csv")
data_op3<-read.csv("OP_CM_Service_2017.csv")
Code_Map<-read.csv("Branch Codes.csv")

library(zoo)
library(lmtest)
library(urca)
library(forecast)
library(tseries)
library(plyr)
library(dplyr)

# Combining accounting data with mtd_bind
# data<-rbind(data,data_a)
data<-ddply(data,c("datestamp","SC.5","Maintenance.Code"),summarize,Contribution.Margin=sum(as.numeric(Contribution.Margin)))
number_forecast_month<-3 # Number of months of forecast required
forecast_gathered<-1 # Month for which the forecast is gathered
# View(data)

# Combining month and year
data_final<-NULL
data_forecast_month<-NULL

##########################################################################################################
# For Total Japan CM

data_1<-ddply(data,c("datestamp"),summarize,Contribution.Margin=sum(as.numeric(Contribution.Margin)))
# Removing unnecessary data entries
data_timestamps<-data_1$datestamp
data_1<-data_1[,2]

# Time series
data_1=ts(data_1,frequency = 12,start = c(2015,1))
plot(data_1)

# Model fitting with seasonality
fit_seasonal<-auto.arima(data_1,seasonal = TRUE)
fit_seasonal
seas_forecast<-forecast(fit_seasonal,h=number_forecast_month)

# par(mfrow=c(1,1))
plot(seas_forecast,type="b",lwd = 3,xlab = "Time", ylab = "CM")

data_forecast_month<-as.data.frame((seas_forecast$mean[forecast_gathered]))
colnames(data_forecast_month)<-c("Forecast")
data_final<-rbind(data_final,data_forecast_month)

##########################################################################################################
# For Total Service level Forecast
# Initiating data sets

data_Service_level<-ddply(data,c("datestamp","Maintenance.Code"),summarize,Contribution.Margin=sum(as.numeric(Contribution.Margin)))
for(i in 1:length(unique(data_Service_level$Maintenance.Code))){
  current_MC<-NULL
  current_MC <- levels(data_Service_level$Maintenance.Code)[i]
  if (current_MC %in% c("Ippan","Shaken","Tenken")) {
    print(current_MC)
    data_1<-subset(data_Service_level,data_Service_level$Maintenance.Code==current_MC)
    
    # Removing unnecessary data entries
    data_timestamps<-data_1$datestamp
    data_1<-data_1[,3]
    
    # Time series
    data_1=ts(data_1,frequency = 12,start = c(2015,1))
    plot(data_1)
    
    # Model fitting with seasonality
    fit_seasonal<-auto.arima(data_1,seasonal = TRUE)
    fit_seasonal
    seas_forecast<-forecast(fit_seasonal,h=number_forecast_month)
    
    # par(mfrow=c(1,1))
    plot(seas_forecast,type="b",lwd = 3,xlab = "Time", ylab = "CM")
    
    # For SC level Forecast
    data_forecast_month<-as.data.frame(seas_forecast$mean[forecast_gathered])
    colnames(data_forecast_month)<-c("Forecast")
    data_final<-rbind(data_final,data_forecast_month)
  }# for "if" condition
  
}# for "for" loop
##########################################################################################################
# For SC level Forecast
data_SC_level_forecast<-NULL

for(i in 1:length(unique(data$SC.5))){
  current_SC<-NULL
  current_SC <- levels(data$SC.5)[i]
  data_1<-subset(data,data$SC.5==current_SC)
  
  # Removing unnecessary data entries
  data_1<-ddply(data_1,c("datestamp"),summarize,Contribution.Margin=sum(as.numeric(Contribution.Margin)))
  data_1_timestamps<-data_1$datestamp # Binding month dates to timestamp
  data_1<-data_1[,2]
  
  ################# Time series #########################
  data_1=ts(data_1,frequency = 12,start = c(2015,1))
  plot(data_1)
  
  # Model fitting with seasonality
  fit_seasonal<-auto.arima(data_1,seasonal = TRUE)
  fit_seasonal
  seas_forecast<-forecast(fit_seasonal,h=number_forecast_month)
  
  # For SC level Forecast
  data_forecast_month<-as.data.frame((seas_forecast$mean[forecast_gathered]))
  colnames(data_forecast_month)<-c("Forecast")
  data_final<-rbind(data_final,data_forecast_month)
} # End of for loop
##########################################################################################################
# For SC-Service Level Forecast 
# Initiating data sets
for(i in 1:length(unique(data$Maintenance.Code))){
  current_MC<-NULL
  current_MC <- levels(data$Maintenance.Code)[i]
  if (current_MC %in% c("Ippan","Shaken","Tenken")) {
    print(current_MC)
    data_2<-subset(data,data$Maintenance.Code==current_MC)
    for (j in 1:length(unique(data$SC.5))) {
      current_SC<-NULL
      current_SC<-levels(data$SC.5)[j]
      data_1<-subset(data_2,data_2$SC.5==current_SC)
      
      # Removing unnecessary data entries
      data_timestamps<-data_1$datestamp
      data_1<-data_1[,4]
      
      # Time series
      data_1=ts(data_1,frequency = 12,start = c(2015,1))
      plot(data_1)
      
      # Model fitting with seasonality
      fit_seasonal<-auto.arima(data_1,seasonal = TRUE)
      fit_seasonal
      seas_forecast<-forecast(fit_seasonal,h=number_forecast_month)
      
      # par(mfrow=c(1,1))
      plot(seas_forecast,type="b",lwd = 3,xlab = "Time", ylab = "CM")
      
      # For SC level Forecast
      data_forecast_month<-as.data.frame((seas_forecast$mean[forecast_gathered]))
      colnames(data_forecast_month)<-c("Forecast")
      data_final<-rbind(data_final,data_forecast_month)
    } # for "if" condition
    
  }# for "for" loop
  
}# for "for - 2" loop
# write.csv(data_final,"data_time_series.csv",row.names = FALSE)
data_final<-cbind(data_final,pred_final)
colnames(data_final)<-c("Forecast_Timeseries","Forecast_Regression","Service","SC")
# write.csv(data_final,"Combined_Pred.csv",row.names = FALSE)
final_prediction<-as.data.frame(data_final[,1]*0.37+data_final[,2]*0.63)
final_prediction$Service<-data_final$Service
final_prediction$SC<-data_final$SC
colnames(final_prediction)<-c("Forecast","Service","SC")
# write.csv(final_prediction,"Final_Prediction_temp.csv",row.names=FALSE)


