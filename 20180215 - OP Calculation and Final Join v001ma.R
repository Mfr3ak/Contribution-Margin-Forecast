# Update forecast_month on row 7

setwd("C:/Users/MADIMUL/Downloads/Work/CM Forecast/Processed Files/")
data_OP<-read.csv("Initiative target All SC_Feb_20180131.csv")
data_OP1<-read.csv("18OP_final.csv")
Code_Map<-read.csv("Branch Codes.csv")
forecast_month<-2

library(plyr)
library(dplyr)
library(tidyr)
library(bizdays)
library(lubridate)

data<-data_OP[,c(2,3,69)]
data<-merge(data,Code_Map,by.x = "SO",by.y = "Branch.code",all.x = TRUE)
data<-data[,c(7,3)]
colnames(data)<-c("SC.5","OP")
# data$OP<-sapply(data$OP,as.numeric)
# data$OP<-as.numeric(as.character(data$OP))
# data$OP<-as.numeric(levels(data$OP))[data$OP]
str(data)
data<-ddply(data,c("SC.5"),summarize,OP=sum(as.numeric(OP)))
data$OP<-data$OP*1000
sum(data$OP)
colnames(data)<-c("SC","OP")
data$Service<-"ALL"
data_total_OP<-as.data.frame(sum(data$OP))
data_total_OP$Service<-"ALL"
data_total_OP$SC<-"ALL"

data_1<-data_OP1[,c(4,6,25,26,27,28,29,63,64,65,69,70,84,85,86,87,88)]
data_1<-subset(data_1,data_1$MM==forecast_month)
data_1<-data_1[,-c(1)]

str(data_1)

names(data_1)

data_1$Shaken_CM<-rowSums(data_1[,c(2,3,4,5,6)])

data_1$Tenken_CM<-rowSums(data_1[,c(7,8,9,10,11)])

data_1$Ippan_CM<-rowSums(data_1[,c(12,13,14,15,16)])

data_1<-data_1[,c(1,17,18,19)]

data_1<-merge(data_1,Code_Map,by.x = "Code",by.y = "Branch.code",all.x = TRUE)

data_1<-data_1[,c(1,2,3,4,8)]
colnames(data_1)<-c("Code","Shaken","Tenken","Ippan","SC")

data_1<-gather(data_1,Service,CM,Shaken,Tenken,Ippan)
data_1<-data_1[,c(2,3,4)]
data_1<-ddply(data_1,c("SC","Service"),summarize,CM=sum(as.numeric(CM)))
data_1$CM<-data_1$CM*1000
colnames(data_1)<-c("SC","Service","OP")
data_1_Service_OP<-ddply(data_1,"Service",summarize,OP=sum(as.numeric(OP)))
data_1_Service_OP$SC<-"ALL"

# Rearraging columns for row binding
colnames(data_total_OP)<-c("OP","Service","SC")
data_total_OP<-data_total_OP[,c(3,2,1)]
data_1_Service_OP<-data_1_Service_OP[,c(3,1,2)]
data_SC_OP<-data[,c(1,3,2)]
data_SC_Service_OP<-data_1

data_OP_final<-rbind(data_total_OP,data_1_Service_OP,data_SC_OP,data_SC_Service_OP)

##########################################################################################################
# Final Calendar Bind

# Creating 2018 Calendar
Holiday_1<-(paste0("2018-01-",c("01","02","03","04","07","08",14,21,28))) # January has a fucked up number of working days
Holiday_2<-(paste0("2018-02-",c("04",11,12,18,25)))
Holiday_3<-(paste0("2018-03-",c("04",11,18,21,25)))
Holiday_4<-(paste0("2018-04-",c("01","08",15,22,29,30))) # April Daily Forecast will be fucked up due to on extra working day
Holiday_5<-(paste0("2018-05-",c("03","04","05","06",13,20,27)))
Holiday_6<-(paste0("2018-06-",c("03",10,17,24)))
Holiday_7<-(paste0("2018-07-",c("01","08",15,16,22,29)))
Holiday_8<-(paste0("2018-08-",c("05",11,12,13,14,15,16,19,26)))
Holiday_9<-(paste0("2018-09-",c("02","09",16,17,23,24,30)))
Holiday_10<-(paste0("2018-10-",c("07","08",14,21,28)))
Holiday_11<-(paste0("2018-11-",c("03","04",11,18,23,25)))
Holiday_12<-(paste0("2018-12-",c("02","09",16,23,24,29,30,31)))

Holidays_2018<-as.data.frame(c(Holiday_1,Holiday_2,Holiday_3,Holiday_4,Holiday_5,Holiday_6,Holiday_7,Holiday_8,Holiday_9,
                 Holiday_10,Holiday_11,Holiday_12))
colnames(Holidays_2018)<-c("Date")

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

create.calendar(name='MyCalendar_Actual')
Calendar_Actual<- as.data.frame(bizseq("2018-01-01","2018-12-31",'MyCalendar_Actual'))
names(Calendar_Actual)<-c("Date")
Calendar_Actual$Day<-as.numeric(as.character(substring(Calendar_Actual$Date,9,10)))
Calendar_Actual$Month<-month(Calendar_Actual$Date)
Calendar_Actual<-subset(Calendar_Actual,Calendar_Actual$Month==forecast_month)

for (i in 1:nrow(Calendar_Actual)) {
  if (Calendar_Actual$Day[i]<=7) {
    Calendar_Actual$Week_Day[i]<-1
  }else if (Calendar_Actual$Day[i]<=14) {
    Calendar_Actual$Week_Day[i]<-2
  }else if (Calendar_Actual$Day[i]<=21) {
    Calendar_Actual$Week_Day[i]<-3
  }else Calendar_Actual$Week_Day[i]<-4
}

Calendar_Actual$Date<-as.factor(Calendar_Actual$Date)
Calendar_Actual<-subset(Calendar_Actual,!(Calendar_Actual$Date %in% Holidays_2018$Date))

for (i in 1:nrow(Calendar_Actual)) {
  Calendar_Actual$Work_Day_Number[i]<-i
  if (i<=5) {
    Calendar_Actual$Work_Week[i]<-1
  } else if (i<=10) {
    Calendar_Actual$Work_Week[i]<-2
  } else if (i<=15) {
    Calendar_Actual$Work_Week[i]<-3
  } else Calendar_Actual$Work_Week[i]<-4
}

# Conversion to weekly data
Weekly_share<-as.data.frame(c(0.05,0.25,0.25,0.45))
for (i in 1:nrow(Calendar_Actual)) {
  if (Calendar_Actual$Week_Day[i]==1) {
    Calendar_Actual$Weekly_share[i]<-Weekly_share[1,]
  } else if (Calendar_Actual$Week_Day[i]==2) {
    Calendar_Actual$Weekly_share[i]<-Weekly_share[2,]
  } else if (Calendar_Actual$Week_Day[i]==3) {
    Calendar_Actual$Weekly_share[i]<-Weekly_share[3,]
  } else Calendar_Actual$Weekly_share[i]<-Weekly_share[4,]
}

Anchor<-as.data.frame(table(Calendar_Actual$Week_Day))

Calendar_Actual<-merge(Calendar_Actual,Anchor,by.x = "Week_Day",by.y = "Var1",all.x = TRUE)
Calendar_Actual$Weekly_share<-as.numeric(Calendar_Actual$Weekly_share)
Calendar_Actual$Weekly_share_final<-Calendar_Actual$Weekly_share/Calendar_Actual$Freq
names(Calendar_Actual)
Calendar_Actual<-Calendar_Actual[,c(2,9,8,4,5)]
colnames(Calendar_Actual)<-c("Lates_Invoicing_Date","Weekly_share_final","Frequency","Month","Work_Day")
Calendar_Actual<-do.call("rbind", replicate(24, Calendar_Actual, simplify = FALSE))

# All thats left is to merge final bind with Calendar Actual OP Data
# Conversion to weekly data
j<-1
for (k in 1:nrow(data_OP_final)) {
  for (i in unique(Calendar_Actual$Work_Day)) {
    Calendar_Actual$OP[j]<-Calendar_Actual$Weekly_share_final[j]*data_OP_final$OP[k]
    Calendar_Actual$SC.5[j]<-data_OP_final$SC[k]
    Calendar_Actual$Maintenance.Code[j]<-data_OP_final$Service[k]
    j<-j+1
  }
}

names(final_bind)
names(Calendar_Actual)
# Merging OP with Forecast Numbers.
data_OP_final_bind_merged<-merge(Calendar_Actual,final_bind,by.x = c("Maintenance.Code","SC.5","Work_Day"),
                                 by.y = c("Maintenance.Code","SC.5","Day"),all.x = TRUE)

write.csv(data_OP_final_bind_merged,"data_OP_final_bind_merged.csv",row.names = FALSE)
