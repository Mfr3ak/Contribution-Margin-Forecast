# Assumption
# There is CM Contributed on some holidays. I have completely ignored those CM Contributions.
# Updating the Holiday list will delete these data points from the final data.
# Update the forecast_month on row 19

setwd("C:/Users/MADIMUL/Downloads/Work/CM Forecast/Processed Files/")
data<-read.csv("mtd_2017.csv")
Code_Map<-read.csv("Branch Codes.csv")

library(reshape2)
library(plyr)
library(dplyr)
library(lubridate)
library(tidyr)
library(corrplot)
library(pls)

data<-merge(data,Code_Map,by.x = "Maintenance.Base.Code",by.y = "Branch.code",all.x = TRUE)
forecast_month<-2 # Update the Month of Forecast.

# Creating Calendar with holidays to exclude those numbers from the total dates list
# These holidays correspond to the 2017 holidays

Holiday_1<-(paste0("2017-01-",c("01","02","03","04","08","09",15,22,29)))
Holiday_2<-(paste0("2017-02-",c("05",11,12,19,26)))
Holiday_3<-(paste0("2017-03-",c("05",12,19,20,26)))
Holiday_4<-(paste0("2017-04-",c("02","09",16,23,30)))
Holiday_5<-(paste0("2017-05-",c("03","04","05","07","14","21","28")))
Holiday_6<-(paste0("2017-06-",c("04",11,18,25)))
Holiday_7<-(paste0("2017-07-",c("02","09",16,17,23,30)))
Holiday_8<-(paste0("2017-08-",c("06",11,12,13,14,15,16,20,27)))
Holiday_9<-(paste0("2017-09-",c("03",10,17,18,23,24)))
Holiday_10<-(paste0("2017-10-",c("01","08","09",15,22,29)))
Holiday_11<-(paste0("2017-11-",c("03","05",12,19,23,26)))
Holiday_12<-(paste0("2017-12-",c("03",10,17,23,24,29,30,31)))

Holidays<-c(Holiday_1,Holiday_2,Holiday_3,Holiday_4,Holiday_5,Holiday_6,Holiday_7,Holiday_8,Holiday_9,
                Holiday_10,Holiday_11,Holiday_12)

rm(Holiday_1,
   Holiday_2,
   Holiday_3,
   Holiday_4,
   Holiday_5,
   Holiday_6,
   Holiday_7,
   Holiday_8,
   Holiday_9,
   Holiday_10,
   Holiday_11,
   Holiday_12)

###########################################################################################################
# Total CM
data_1_Total<-ddply(data,c("Invoicing.Date"),summarize,
                    Contribution.Margin=sum(as.numeric(Contribution.Margin)))
data_1_Total$Month<-month(data_1_Total$Invoicing.Date)
data_1_Total<-subset(data_1_Total,data_1_Total$Month==forecast_month) # Total CM per day
data_1_Total<-subset(data_1_Total,!(data_1_Total$Invoicing.Date %in% Holidays))
data_1_Total_CM<-sum(data_1_Total$Contribution.Margin)
data_1_Total$Percentage_share<-data_1_Total$Contribution.Margin/data_1_Total_CM

###########################################################################################################
# Service CM
data_1_Service<-ddply(data,c("Invoicing.Date","Maintenance.Code"),summarize,
                      Contribution.Margin=sum(as.numeric(Contribution.Margin)))
data_1_Service$Month<-month(data_1_Service$Invoicing.Date)
data_1_Service<-subset(data_1_Service,data_1_Service$Month==forecast_month)
data_1_Service_final<-NULL
for (i in c("Ippan","Shaken","Tenken")) {
  data_1_Service_temp<-subset(data_1_Service,
                         data_1_Service$Maintenance.Code==i)
  data_1_Service_temp<-subset(data_1_Service_temp,!(data_1_Service_temp$Invoicing.Date %in% Holidays))
  data_1_Service_CM<-sum(data_1_Service_temp$Contribution.Margin)
  data_1_Service_temp$Percentage_share<-data_1_Service_temp$Contribution.Margin/data_1_Service_CM
  data_1_Service_final<-rbind(data_1_Service_final,data_1_Service_temp)
  rm(data_1_Service_temp)
}
###########################################################################################################
# Total SC
data_1_SC<-ddply(data,c("Invoicing.Date","SC.5"),summarize,
                 Contribution.Margin=sum(as.numeric(Contribution.Margin)))
data_1_SC$Month<-month(data_1_SC$Invoicing.Date)
data_1_SC<-subset(data_1_SC,data_1_SC$Month==forecast_month)
data_1_SC_final<-NULL
for (i in unique(data_1_SC$SC.5)) {
  data_1_SC_temp<-subset(data_1_SC,data_1_SC$SC.5==i)
  data_1_SC_temp<-subset(data_1_SC_temp,!(data_1_SC_temp$Invoicing.Date %in% Holidays))
  data_1_SC_CM<-sum(data_1_SC_temp$Contribution.Margin)
  data_1_SC_temp$Percentage_share<-data_1_SC_temp$Contribution.Margin/data_1_SC_CM
  data_1_SC_final<-rbind(data_1_SC_final,data_1_SC_temp)
  rm(data_1_SC_temp)
}

###########################################################################################################
# SC - Service CM
data_1_SC_Service<-ddply(data,c("Invoicing.Date","Maintenance.Code","SC.5"),
                         summarize,Contribution.Margin=sum(as.numeric(Contribution.Margin)))
data_1_SC_Service$Month<-month(data_1_SC_Service$Invoicing.Date)
data_1_SC_Service<-subset(data_1_SC_Service,data_1_SC_Service$Month==forecast_month)
data_1_SC_Service<-subset(data_1_SC_Service,
                          data_1_SC_Service$Maintenance.Code=="Ippan"|data_1_SC_Service$Maintenance.Code=="Shaken"|data_1_SC_Service$Maintenance.Code=="Tenken")
data_1_SC_Service_final<-NULL
for (i in unique(data_1_SC_Service$Maintenance.Code)) {
  for (j in unique(data_1_SC_Service$SC.5)) {
    data_1_SC_Service_temp<-subset(data_1_SC_Service,data_1_SC_Service$Maintenance.Code==i & data_1_SC_Service$SC.5==j)
    data_1_SC_Service_temp<-subset(data_1_SC_Service_temp,!(data_1_SC_Service_temp$Invoicing.Date %in% Holidays))
    data_1_SC_Service_CM<-sum(data_1_SC_Service_temp$Contribution.Margin)
    data_1_SC_Service_temp$Percentage_share<-data_1_SC_Service_temp$Contribution.Margin/data_1_SC_Service_CM
    data_1_SC_Service_final<-rbind(data_1_SC_Service_final,data_1_SC_Service_temp)
    rm(data_1_SC_Service_temp)
  } # for loop ending for Maintenance.Code
} # for loop ending fror SC.5

###########################################################################################################

rm(Dates)
Dates<-as.data.frame(data_1_Total$Invoicing.Date)
for (i in 1:nrow(Dates)) {
  Dates$Day[i]<-i
}
colnames(Dates)<-c("Invoicing.Date","Day")

# Attaching the Day Column to final datasets
data_1_Total<-merge(data_1_Total,Dates,by.x = "Invoicing.Date",by.y = "Invoicing.Date",all.x = TRUE)
data_1_Service_final<-merge(data_1_Service_final,Dates,by.x = "Invoicing.Date",by.y = "Invoicing.Date",all.x = TRUE)
data_1_SC_final<-merge(data_1_SC_final,Dates,by.x = "Invoicing.Date",by.y = "Invoicing.Date",all.x = TRUE)
data_1_SC_Service_final<-merge(data_1_SC_Service_final,Dates,by.x = "Invoicing.Date",by.y = "Invoicing.Date",all.x = TRUE)

# Splitting the forecast to daily data
###########################################################################################################
# Total CM
data_1_Total_Complete<-data_1_Total
for (i in c(1:nrow(data_1_Total))) {
  data_1_Total_Complete$Monthly_Forecast[i]<-data_1_Total$Percentage_share[i]*final_prediction[1,1]
}

###########################################################################################################
# For Service level CM
data_1_Service_final_Complete<-NULL
for (i in unique(data_1_Service_final$Maintenance.Code)) {
  data_1_Service_temp<-subset(data_1_Service_final,data_1_Service_final$Maintenance.Code==i)
  final_prediction_temp<-subset(final_prediction,final_prediction$Service==i & final_prediction$SC=="ALL")
  for (j in c(1:nrow(data_1_Service_temp))) {
    data_1_Service_temp$Monthly_Forecast[j]<-data_1_Service_temp$Percentage_share[j]*final_prediction_temp[1,1]
  }
  data_1_Service_final_Complete<-rbind(data_1_Service_final_Complete,data_1_Service_temp)
}

###########################################################################################################
# For SC Level CM
data_1_SC_final_Complete<-NULL
for (i in unique(data_1_SC_final$SC.5)) {
  data_1_SC_temp<-subset(data_1_SC_final,data_1_SC_final$SC.5==i)
  final_prediction_temp<-subset(final_prediction,final_prediction$SC==i & final_prediction$Service=="ALL")
  for (j in c(1:nrow(data_1_SC_temp))) {
    data_1_SC_temp$Monthly_Forecast[j]<-data_1_SC_temp$Percentage_share[j]*final_prediction_temp[1,1]
  }
  data_1_SC_final_Complete<-rbind(data_1_SC_final_Complete,data_1_SC_temp)
}

###########################################################################################################
# For SC-Service Level CM
data_1_SC_Service_final_Complete<-NULL
for (i in unique(data_1_SC_Service_final$Maintenance.Code)) {
  for (j in unique(data_1_SC_Service_final$SC.5)) {
    data_1_SC_Service_temp<-subset(data_1_SC_Service_final,data_1_SC_Service_final$Maintenance.Code==i &
                                     data_1_SC_Service_final$SC.5==j)
    final_prediction_temp<-subset(final_prediction,final_prediction$Service==i & final_prediction$SC==j)
    for (k in c(1:nrow(data_1_SC_Service_temp))) {
      data_1_SC_Service_temp$Monthly_Forecast[k]<-data_1_SC_Service_temp$Percentage_share[k]*final_prediction_temp[1,1]
    }
    data_1_SC_Service_final_Complete<-rbind(data_1_SC_Service_final_Complete,data_1_SC_Service_temp)
  }
}

data_1_Total_Complete$Maintenance.Code<-"ALL"
data_1_Total_Complete$SC.5<-"ALL"
data_1_SC_final_Complete$Maintenance.Code<-"ALL"
data_1_Service_final_Complete$SC.5<-"ALL"

data_1_Total_Complete<-data_1_Total_Complete[,c(1,5,7,8,6,2,4,3)]
data_1_SC_final_Complete<-data_1_SC_final_Complete[,c(1,6,8,2,7,3,5,4)]
data_1_Service_final_Complete<-data_1_Service_final_Complete[,c(1,6,2,8,7,3,5,4)]
data_1_SC_Service_final_Complete<-data_1_SC_Service_final_Complete[,c(1,7,2,3,8,4,6,5)]

final_bind<-rbind(data_1_Total_Complete,
                  data_1_Service_final_Complete,
                  data_1_SC_final_Complete,
                  data_1_SC_Service_final_Complete)

# Weekly Split
for (i in 1:nrow(final_bind)) {
  if (final_bind$Day[i]<=5) {
    final_bind$Week[i]<-1
  } else if (final_bind$Day[i]<=10) {
    final_bind$Week[i]<-2
  } else if (final_bind$Day[i]<=15) {
    final_bind$Week[i]<-3
  } else {
    final_bind$Week[i]<-4
  }
}

write.csv(final_bind,"final_forecast.csv",row.names = FALSE)