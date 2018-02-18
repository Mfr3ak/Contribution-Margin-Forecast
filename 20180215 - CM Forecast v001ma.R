setwd("C:/Users/MADIMUL/Downloads/Work/CM Forecast/Processed Files/")

library(plyr)
library(dplyr)
library(lubridate)

# Run this code by loading different years
# Data Loading

Code_Map<-read.csv("Branch Codes.csv")
m_mtd_data_end<-NULL

for (i in c("mtd_2015.csv","mtd_2016.csv","mtd_2017.csv","mtd_2018.csv")){
  print(i)
  mtd_data<-read.csv(i)
  # merging datasets
  m_mtd_data<-merge(mtd_data,Code_Map,by.x = "Maintenance.Base.Code",by.y = "Branch.code",all.x = TRUE)
  # names(m_mtd_data)
  
  # Removing unnecessary columns for timeseries
  m_mtd_data_final<-m_mtd_data[,c(4,6,9,16)]
  
  # Aggregating CM at monthly level
  m_mtd_data_final$Invoicing.Date_1<-month(m_mtd_data_final$Invoicing.Date)
  m_mtd_data_final$Invoicing.Date_2<-year(m_mtd_data_final$Invoicing.Date)
  m_mtd_data_final<-m_mtd_data_final[,-1]
  m_mtd_data_final<-ddply(m_mtd_data_final,c("Invoicing.Date_1","Invoicing.Date_2","SC.5","Maintenance.Code"),summarize,Contribution.Margin=sum(Contribution.Margin))
  m_mtd_data_final<-na.omit(m_mtd_data_final)
  m_mtd_data_final$datestamp<-as.Date(paste(m_mtd_data_final$Invoicing.Date_2,m_mtd_data_final$Invoicing.Date_1,"28",sep = "-"))
  # sorting by month and SC
  # Change the year corresponding to file loaded
  m_mtd_data_final<-m_mtd_data_final[order(m_mtd_data_final$Invoicing.Date_1,m_mtd_data_final$SC.5),]
  print(paste0("Number of rows supposed to be in binded data: ",nrow(m_mtd_data_final)+nrow(m_mtd_data_end)))
  m_mtd_data_end<-rbind(m_mtd_data_end,m_mtd_data_final)
  print(paste0("Number of rows in current mtd file: ",nrow(m_mtd_data_final)))
  print(paste0("Number of rows in binded data: ",nrow(m_mtd_data_end)))
}

write.csv(m_mtd_data_end,"mtd_bind.csv",row.names = FALSE)

