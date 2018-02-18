# Update forecast_month in row 14

setwd("C:/Users/MADIMUL/Downloads/Work/CM Forecast/Data/Current Month 1_10/")
data<-read.csv("1-10_2018_02_15.csv")
setwd("C:/Users/MADIMUL/Downloads/Work/CM Forecast/Processed Files/")
data_OP_final_bind_merged<-read.csv("data_OP_final_bind_merged.csv")
Code_Map<-read.csv("Branch Codes.csv",na.strings = c(""," "))

library(plyr)
library(dplyr)
library(lubridate)
library(tidyr)

forecast_month<-2 # Change this month in order to collect data about current month.
colnames(data)<- c("Date","SC","RSC","Small_Date","Order#",
                   "Vehicle Class","Owner","Service","User#",
                   "User name","Selling Price","CM","Rate","No Sale",
                   "","","","","","","","")

# Creating 2018 Calendar
Holiday_1<-(paste0("201801",c("01","02","03","04","07","08",14,21,28))) # January has a fucked up number of working days
Holiday_2<-(paste0("201802",c("04",11,12,18,25)))
Holiday_3<-(paste0("201803",c("04",11,18,21,25)))
Holiday_4<-(paste0("201804",c("01","08",15,22,29,30))) # April Daily Forecast will be fucked up due to on extra working day
Holiday_5<-(paste0("201805",c("03","04","05","06",13,20,27)))
Holiday_6<-(paste0("201806",c("03",10,17,24)))
Holiday_7<-(paste0("201807",c("01","08",15,16,22,29)))
Holiday_8<-(paste0("201808",c("05",11,12,13,14,15,16,19,26)))
Holiday_9<-(paste0("201809",c("02","09",16,17,23,24,30)))
Holiday_10<-(paste0("201810",c("07","08",14,21,28)))
Holiday_11<-(paste0("201811",c("03","04",11,18,23,25)))
Holiday_12<-(paste0("201812",c("02","09",16,23,24,29,30,31)))

Holidays_2018<-c(Holiday_1,Holiday_2,Holiday_3,Holiday_4,Holiday_5,Holiday_6,Holiday_7,Holiday_8,Holiday_9,
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

data_1<-subset(data,(data$Service==11 | data$Service==12 | data$Service==13))
data_1$Month<-as.numeric(as.character(substring(data_1$Date,5,6)))
data_1<-subset(data_1,data_1$Month==forecast_month)

for (i in 1:nrow(data_1)) {
  if (data_1$Service[i]==11) {
    data_1$Service[i]<-"Ippan"
  } else if (data_1$Service[i]==12) {
    data_1$Service[i]<-"Shaken"
  } else {
    data_1$Service[i]<-"Tenken"
  }
}

data_1$Branch<-substring(data_1$`Order#`,1,2)
data_1$Branch<-as.character(data_1$Branch)
data_1<-merge(data_1,Code_Map,by.x = "Branch",by.y = "Branch.code",all.x = TRUE)
data_1<-data_1[,c(28,9,2,13)]

Dates<-as.data.frame(unique(data_1$Date))
colnames(Dates)<-c("Date")
Dates$Day<-as.numeric(as.character(substr(Dates$Date,7,8)))
Dates<-Dates[order(Dates$Day),]
Dates<-as.data.frame(Dates[,1])
colnames(Dates)<-c("Date")
Dates<-subset(Dates,!(Dates[,1] %in% Holidays_2018))

for (i in 1:nrow(Dates)) {
  Dates$Day[i]<-i
}
colnames(Dates)<-c("Date","Day")

for (i in 1:nrow(data_1)) {
  if (data_1$Date[i]=="20180211") {
    data_1$Date[i]<-"20180213"
  # } else if (data_1$Date[i] =="20180121") {
  #   data_1$Date[i]<-"20180122"
  # } else if (data_1$Date[i] =="20180128") {
  #   data_1$Date[i]<-"20180129"
  }
}

data_1<-ddply(data_1,c("SC.5","Service","Date"),summarize,Actual_CM=sum(as.numeric(CM)))
data_1<-merge(data_1,Dates,by.x = "Date",by.y = "Date",all.x = TRUE)
# data_1$Day<-as.numeric(as.character(substring(data_1$Date,7,8)))
for (i in 1:nrow(data_1)) {
  if (data_1$Day[i]<=5) {
    data_1$Week[i]<-1
  } else if (data_1$Day[i]<=10) {
    data_1$Week[i]<-2
  } else if (data_1$Day[i]<=15){
    data_1$Week[i]<-3
  } else data_1$Week[i]<-4
}

data_1_Total_Actual<-ddply(data_1,c("Date","Day","Week"),summarize,Actual_CM=sum(as.numeric(Actual_CM)))
data_1_Total_Actual$SC<-"ALL"
data_1_Total_Actual$Service<-"ALL"
data_1_Total_Actual<-data_1_Total_Actual[,c(5,6,1,3,2,4)]
colnames(data_1_Total_Actual)<-c("SC","Service","Date","Week","Day","Actual_CM")

data_1_SC_Actual<-ddply(data_1,c("SC.5","Date","Day","Week"),summarize,Actual_CM=sum(as.numeric(Actual_CM)))
data_1_SC_Actual$Service<-"ALL"
data_1_SC_Actual<-data_1_SC_Actual[,c(1,6,2,4,3,5)]
colnames(data_1_SC_Actual)<-c("SC","Service","Date","Week","Day","Actual_CM")

data_1_Service_Actual<-ddply(data_1,c("Service","Date","Day","Week"),summarize,Actual_CM=sum(as.numeric(Actual_CM)))
data_1_Service_Actual$SC<-"ALL"
data_1_Service_Actual<-data_1_Service_Actual[,c(6,1,2,4,3,5)]
colnames(data_1_Service_Actual)<-c("SC","Service","Date","Week","Day","Actual_CM")

data_1_SC_Service_Actual<-ddply(data_1,c("Service","SC.5","Date","Day","Week"),summarize,Actual_CM=sum(as.numeric(Actual_CM)))
data_1_SC_Service_Actual<-data_1_SC_Service_Actual[,c(2,1,3,5,4,6)]
colnames(data_1_SC_Service_Actual)<-c("SC","Service","Date","Week","Day","Actual_CM")

data_1_Actual_final<-rbind(data_1_Total_Actual,data_1_Service_Actual,data_1_SC_Actual,data_1_SC_Service_Actual)
names(data_OP_final_bind_merged)
names(data_1_Actual_final)

data_OP_final_bind_Actual_merge<-merge(data_OP_final_bind_merged,data_1_Actual_final,by.x = c("SC.5","Maintenance.Code","Work_Day"),
                                       by.y =c("SC","Service","Day"),all.x = TRUE )

write.csv(data_OP_final_bind_Actual_merge,"All_bind.csv",row.names = FALSE)

data_OP_final_bind_Actual_merge_long<-gather(data_OP_final_bind_Actual_merge,CM_Factor,CM_Value,
                                             Monthly_Forecast,Contribution.Margin,OP,Actual_CM,factor_key = TRUE)

write.csv(data_OP_final_bind_Actual_merge_long,"All_bind_long.csv",row.names = FALSE)
