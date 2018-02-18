#####################################################################
# Including count of LDT, MDT, HDT in the forecast for CM
# This code aggregates mtd's created by Tracker Master into Wide Format
# for Total Japan. No segregation at SC or Service Level.
# Need 1 User input for forecast_month ( line 30 )
####################################################################

setwd("C:/Users/MADIMUL/Downloads/Work/CM Forecast/Processed Files/")
Code_Map<-read.csv("Branch Codes.csv")

library(reshape2)
library(plyr)
library(dplyr)
library(lubridate)
library(tidyr)
library(corrplot)
library(pls)

data_final_total<-NULL
data_final_total_Service<-NULL
data_final_SC<-NULL
data_final_SC_Service<-NULL
pcr_pred_total_Service<-NULL
pcr_pred_SC<-NULL
pcr_pred_SC_Service<-NULL

# Test data for the next month being calculated by manual entry
# data_final_total_reg_test<-as.data.frame(matrix(c(34297,38292,21075,15773),1,4))
# colnames(data_final_total_reg_test)<-c("HDT_Count","LDT_Count","MDT_Count","Others_Count")
forecast_month_index<-2 # User Input

# Merging Branch Codes
# Datapreparation for the count of different classes of Vehicles for Regression
filenames = c("mtd_2015.csv","mtd_2016.csv","mtd_2017.csv")
print(paste0("Total Number of Files to be read:",length(filenames)))
data_final_total = as.data.frame(data_final_total)
data_final_total_Service = as.data.frame(data_final_total_Service)
data_final_SC = as.data.frame(data_final_SC)
data_final_SC_Service = as.data.frame(data_final_SC_Service)
for(i in filenames) {
  print(paste0("Reading Filename:",i))
  temp_data = read.csv(i,header = TRUE,stringsAsFactors = FALSE)
  temp_data<-merge(temp_data,Code_Map,by.x = "Maintenance.Base.Code",
              by.y = "Branch.code",all.x = TRUE)
  temp_data<-temp_data[,c(4,5,6,9,16)]
  
  temp_data$Month<-month(temp_data$Invoicing.Date)
  temp_data$Year<-year(temp_data$Invoicing.Date)
  master_data<-temp_data
  
  ########################################################################
  ############################ Total Japan ###############################
  # Calulating Sum of CM for Monthly Level for Total Japan
  temp_data_total_CM<-ddply(temp_data,c("Month","Year"),
              summarize,Contribution.Margin=sum(as.numeric(Contribution.Margin)))
  # temp_data_total_CM<-spread(temp_data_total_CM,key = "Vehicle.Classification",value = "Contribution.Margin")
  
  # Calculating Count of Vehicle type at Monthly Level for Total Japan
  temp_data_subset<-subset(temp_data,(temp_data$Maintenance.Code!="Field Fix" & temp_data$Maintenance.Code!="Accident Repair"))
  temp_data_count_total<-as.data.frame(table(temp_data_subset$Month,temp_data_subset$Vehicle.Classification))
  temp_data_count_total<-spread(temp_data_count_total,key = "Var2",value = "Freq")
  
  # Merging Datasets
  temp_data_merge_total<-(cbind(temp_data_total_CM,temp_data_count_total))
  colnames(temp_data_merge_total)<-c("Month","Year","Total_CM",
                               "Month","HDT_Count","LDT_Count","MDT_Count","Others_Count")
  temp_data_merge_total<-temp_data_merge_total[,-c(4)]
  
  ########################################################################
  ########################## Total Japan Service Level ###################
  # Calculating Sum of CM for Monthly Level for Service Level of Total Japan
  temp_data_total_service_CM<-ddply(temp_data,c("Month","Year","Maintenance.Code"),
                          summarize,Contribution.Margin=sum(as.numeric(Contribution.Margin)))
  temp_data_total_service_CM_sorted<-temp_data_total_service_CM[order(
    temp_data_total_service_CM$Month,temp_data_total_service_CM$Year,temp_data_total_service_CM$Maintenance.Code),]
  temp_data_total_service_CM_sorted<-subset(temp_data_total_service_CM_sorted,
                                            temp_data_total_service_CM_sorted$Maintenance.Code!="Field Fix" &
                                            temp_data_total_service_CM_sorted$Maintenance.Code!="Accident Repair")
  # temp_data_total_CM_service<-spread(temp_data_total_CM_service,key = "Maintenance.Code",value = "Contribution.Margin")
  
  # Calculating Count of Vehicle type at Monthly Level for Service level of Total Japan
  temp_data_subset<-subset(temp_data,(temp_data$Maintenance.Code!="Field Fix" & temp_data$Maintenance.Code!="Accident Repair"))
  temp_data_count_service_total<-as.data.frame(table(temp_data_subset$Month,temp_data_subset$Vehicle.Classification,temp_data_subset$Maintenance.Code))
  temp_data_count_service_total<-spread(temp_data_count_service_total,key = "Var2",value = "Freq")
  
  # Merging Datasets for Service of Total Japan
  temp_data_merge_total_Service<-(cbind(temp_data_total_service_CM_sorted,temp_data_count_service_total))
  colnames(temp_data_merge_total_Service)<-c("Month","Year","Maintenance.Code","Total_CM",
                               "Month","Maintenance.Code","HDT_Count","LDT_Count","MDT_Count","Others_Count")
  temp_data_merge_total_Service<-temp_data_merge_total_Service[,-c(5,6)]
  
  #########################################################################
  ############################ SC Level ###################################
  # Calculating Sum of CM for Monthly Level for Service Level of Total Japan
  temp_data_SC_CM<-ddply(temp_data,c("Month","Year","SC.5"),
                                    summarize,Contribution.Margin=sum(as.numeric(Contribution.Margin)))
  temp_data_SC_CM<-na.omit(temp_data_SC_CM)
  temp_data_SC_CM_sorted<-temp_data_SC_CM[order(temp_data_SC_CM$Month,temp_data_SC_CM$SC.5),]
  # temp_data_total_CM_service<-spread(temp_data_total_CM_service,key = "Maintenance.Code",value = "Contribution.Margin")
  
  # Calculating Count of Vehicle type at Monthly Level for Service level of Total Japan
  temp_data_subset<-subset(temp_data,(temp_data$Maintenance.Code!="Field Fix" & temp_data$Maintenance.Code!="Accident Repair"))
  temp_data_count_SC<-as.data.frame(table(temp_data_subset$Month,temp_data_subset$Vehicle.Classification,temp_data_subset$SC.5))
  temp_data_count_SC<-spread(temp_data_count_SC,key = "Var2",value = "Freq")
  temp_data_count_SC_sorted<-temp_data_count_SC[order(temp_data_count_SC$Var1,temp_data_count_SC$Var3),]
  
  # Merging Datasets for Service of Total Japan
  temp_data_merge_SC<-(cbind(temp_data_SC_CM_sorted,temp_data_count_SC_sorted))
  colnames(temp_data_merge_SC)<-c("Month","Year","SC.5","Total_CM",
                                  "Month","SC.5","HDT_Count","LDT_Count",
                                  "MDT_Count","Others_Count")
  temp_data_merge_SC<-temp_data_merge_SC[,-c(5,6)]
  
  
  ##########################################################################
  ##########################SC Service Level ###############################
  temp_data_SC_Service_CM<-ddply(temp_data,c("Month","Year","SC.5","Maintenance.Code"),
                         summarize,Contribution.Margin=sum(as.numeric(Contribution.Margin)))
  temp_data_SC_Service_CM<-na.omit(temp_data_SC_Service_CM)
  temp_data_SC_Service_CM_sorted<-temp_data_SC_Service_CM[order(temp_data_SC_Service_CM$Month,temp_data_SC_Service_CM$SC.5,temp_data_SC_Service_CM$Maintenance.Code),]
  temp_data_SC_Service_CM_sorted<-subset(temp_data_SC_Service_CM_sorted,temp_data_SC_Service_CM_sorted$Maintenance.Code!="Field Fix" & temp_data_SC_Service_CM_sorted$Maintenance.Code!="Accident Repair")
  
  # Calculating Count of Vehicle type at Monthly Level for Service level at SC level
  temp_data_subset<-subset(temp_data,(temp_data$Maintenance.Code!="Field Fix" & temp_data$Maintenance.Code!="Accident Repair"))
  temp_data_count_SC_Service<-as.data.frame(table(temp_data_subset$Month,temp_data_subset$Vehicle.Classification,temp_data_subset$SC.5,temp_data_subset$Maintenance.Code))
  temp_data_count_SC_Service<-spread(temp_data_count_SC_Service,key = "Var2",value = "Freq")
  temp_data_count_SC_Service_sorted<-temp_data_count_SC_Service[order(temp_data_count_SC_Service$Var1,temp_data_count_SC_Service$Var3,temp_data_count_SC_Service$Var4),]
  
  # Merging Datasets for Service of Total Japan
  temp_data_merge_SC_Service<-(cbind(temp_data_SC_Service_CM_sorted,temp_data_count_SC_Service_sorted))
  colnames(temp_data_merge_SC_Service)<-c("Month","Year","SC.5","Maintenance.Code","Total_CM",
                                  "Month","SC.5","Maintenance.Code","HDT_Count","LDT_Count",
                                  "MDT_Count","Others_Count")
  temp_data_merge_SC_Service<-temp_data_merge_SC_Service[,-c(6,7,8)]
  
  
  # Sanitory Check to bind Total Japan
  print(paste0("Rows temp_data:",nrow(temp_data_merge_total)))
  print(paste0("Rows supposed to be in final data:",nrow(temp_data_merge_total)+nrow(data_final_total)))
  data_final_total = as.data.frame(rbind(data_final_total,temp_data_merge_total))
  print(paste0("Rows in final data:",nrow(data_final_total)))
  
  # Sanitory Check to bind Total Services Japan
  print(paste0("Rows temp_data:",nrow(temp_data_merge_total_Service)))
  print(paste0("Rows supposed to be in final data:",(nrow(temp_data_merge_total_Service)+nrow(data_final_total_Service))))
  data_final_total_Service = as.data.frame(rbind(data_final_total_Service,temp_data_merge_total_Service))
  print(paste0("Rows in final data:",nrow(data_final_total_Service)))
  
  # Sanitory Check to bind SC Japan
  print(paste0("Rows temp_data:",nrow(temp_data_merge_SC)))
  print(paste0("Rows supposed to be in final data:",(nrow(temp_data_merge_SC)+nrow(data_final_SC))))
  data_final_SC = as.data.frame(rbind(data_final_SC,temp_data_merge_SC))
  print(paste0("Rows in final data:",nrow(data_final_SC)))
  
  # Sanitory Check to bind SC - Service Japan
  print(paste0("Rows temp_data:",nrow(temp_data_merge_SC_Service)))
  print(paste0("Rows supposed to be in final data:",(nrow(temp_data_merge_SC_Service)+nrow(data_final_SC_Service))))
  data_final_SC_Service = as.data.frame(rbind(data_final_SC_Service,temp_data_merge_SC_Service))
  print(paste0("Rows in final data:",nrow(data_final_SC_Service)))

}

# Actual Principal Component Analysis on the Data Prepared.
######################################################################
############################ Total ###################################
# Splitting Test and Train Data
plot(data_final_total$Total_CM,type = "b")
data_final_total_share<-data_final_total

# Creation of next months estimate of Vehicle counts
data_final_total_share_temp<-subset(data_final_total_share,data_final_total_share$Month==forecast_month_index)
data_final_total_share_temp<-data_final_total_share_temp[,c(4,5,6,7)]
Per_increase_2016_2015<-(data_final_total_share_temp[1,]-data_final_total_share_temp[2,])/data_final_total_share_temp[1,]
Per_increase_2017_2016<-(data_final_total_share_temp[2,]-data_final_total_share_temp[3,])/data_final_total_share_temp[2,]
Avg_Per_increase_total<-(Per_increase_2016_2015+Per_increase_2017_2016)/2
data_final_total_reg_test<-data_final_total_share_temp[3,]+(data_final_total_share_temp[3,]*Avg_Per_increase_total)
data_final_total<-data_final_total[,c(3,4,5,6,7)]

# data_final_total_reg_test<-data_final_total[nrow(data_final_total),]
data_final_total_reg_train<-data_final_total

'''
# Preparing data for the Regression
pairs(data_final_total_reg)
cor<-cor(data_final_total_reg)
cor
corrplot(cor)

# Normal Regression
reg<-lm(data_final_total_reg$Total_CM~.,data = data_final_total_reg)
summary(reg)

# Principal Component Analysis Explicit
data_final_total_reg_indep<-data_final_total_reg[,-1]
data_final_total_reg_dep<-data_final_total_reg[,1]
pcacomp_total<-prcomp(data_final_total_reg_indep,scale. = T,center = T)
pr_var<-pcacomp_total$sdev^2
pr_var<-pr_var/sum(pr_var)
plot(pr_var,type = "b") # Using Two Principal Components
data_final_total_reg_train<-as.data.frame(cbind(data_final_total_reg_dep,pcacomp_total$x))
colnames(data_final_total_reg_train)<-c("Total_CM","PCA1","PCA2","PCA3","PCA4")
data_final_total_reg_train<-as.data.frame(data_final_total_reg_train[,1:2])
pca_lm_fit_total<-lm(Total_CM~.,data = data_final_total_reg_train)
summary(pca_lm_fit_total)
plot(pca_lm_fit_total)
'''

# Principal Component Regression
pcr_fit_total<-pcr(data_final_total_reg_train$Total_CM~.,data = data_final_total_reg_train,scale = TRUE,validation = "CV")
summary(pcr_fit_total)
validationplot(pcr_fit_total,val.type = "MSEP")
pcr_pred_total<-as.data.frame(predict(pcr_fit_total,data_final_total_reg_test,ncomp=2))
pcr_pred_total$Service<-"ALL"
pcr_pred_total$SC<-"ALL"
# Try and get a method to automatically select the optimum number of Principal Components
# Currently the selection of the 3 Principal Components is arbitary so as not to take all the Components
# as well as taking only 2 makes it more redundant as only 60% variance is explained by the 2 components
colnames(pcr_pred_total)<-c("Forecast","Service","SC")

######################################################################
####################### Total Services Japan #########################
# Splitting Test and Train Data
Service<-unique(data_final_total_Service$Maintenance.Code)
pcr_pred_total_Service<-NULL
for(i in Service){
  temp_data_final_total_Service<-subset(data_final_total_Service,data_final_total_Service$Maintenance.Code==i)
  plot(temp_data_final_total_Service$Total_CM,type = "b")
  
  # Creation of Next months estimated Count of Vehicles
  temp_data_final_total_Service_share<-temp_data_final_total_Service[,c(1,4:8)]
  temp_data_final_total_Service_share<-subset(temp_data_final_total_Service_share,temp_data_final_total_Service$Month==forecast_month_index)
  temp_data_final_total_Service_share<-temp_data_final_total_Service_share[,3:6]
  Per_increase_2016_2015<-(temp_data_final_total_Service_share[1,]-temp_data_final_total_Service_share[2,])/temp_data_final_total_Service_share[1,]
  Per_increase_2017_2016<-(temp_data_final_total_Service_share[2,]-temp_data_final_total_Service_share[3,])/temp_data_final_total_Service_share[2,]
  Avg_Per_increase_total<-(Per_increase_2016_2015+Per_increase_2017_2016)/2
  temp_data_final_total_Service_reg_test<-temp_data_final_total_Service_share[3,]+(temp_data_final_total_Service_share[3,]*Avg_Per_increase_total)
  temp_data_final_total_Service_reg_train<-temp_data_final_total_Service[,4:8]
  
  # Principal Component Regression
  temp_pcr_fit_total_Service<-pcr(temp_data_final_total_Service_reg_train$Total_CM~.,data = temp_data_final_total_Service_reg_train,scale = TRUE,validation = "CV")
  summary(temp_pcr_fit_total_Service)
  validationplot(temp_pcr_fit_total_Service,val.type = "MSEP")
  temp_pcr_pred_total_Service<-as.data.frame(predict(temp_pcr_fit_total_Service,temp_data_final_total_Service_reg_test,ncomp=3))
  temp_pcr_pred_total_Service$Service<-i
  temp_pcr_pred_total_Service$SC<-"ALL"
  pcr_pred_total_Service<-as.data.frame(rbind(pcr_pred_total_Service,temp_pcr_pred_total_Service))
  print(i)
  print(pcr_pred_total_Service)
}
colnames(pcr_pred_total_Service)<-c("Forecast","Service","SC")

#########################################################################
############################ SC Level ###################################
# Splitting Test and Train Data
SC<-unique(data_final_SC$SC.5)
for(i in SC) {
  temp_data_final_SC<-subset(data_final_SC,data_final_SC$SC.5==i)
  
  # Creation of next months estimated count of Vechicles
  temp_data_final_SC_share<-temp_data_final_SC
  plot(temp_data_final_SC$Total_CM,type = "b")
  temp_data_final_SC_share<-temp_data_final_SC_share[,c(1,5:8)]
  temp_data_final_SC_share<-subset(temp_data_final_SC_share,temp_data_final_SC_share$Month==forecast_month_index)
  temp_data_final_SC_share<-temp_data_final_SC_share[,2:5]
  Per_increase_2016_2015<-(temp_data_final_SC_share[1,]-temp_data_final_SC_share[2,])/temp_data_final_SC_share[1,]
  Per_increase_2017_2016<-(temp_data_final_SC_share[2,]-temp_data_final_SC_share[3,])/temp_data_final_SC_share[2,]
  Avg_Per_increase_total<-(Per_increase_2016_2015+Per_increase_2017_2016)/2
  temp_data_final_SC_reg_test<-temp_data_final_SC_share[3,]+(temp_data_final_SC_share[3,]*Avg_Per_increase_total)
  temp_data_final_SC_reg_train<-temp_data_final_SC[,4:8]

  # Principal Component Regression
  temp_pcr_fit_SC<-pcr(temp_data_final_SC_reg_train$Total_CM~.,data = temp_data_final_SC_reg_train,scale = TRUE,validation = "CV")
  summary(temp_pcr_fit_SC)
  validationplot(temp_pcr_fit_SC,val.type = "MSEP")
  temp_pcr_pred_SC<-as.data.frame(predict(temp_pcr_fit_SC,temp_data_final_SC_reg_test,ncomp=3)) 
  temp_pcr_pred_SC$Service<-"ALL"
  temp_pcr_pred_SC$SC<-i
  pcr_pred_SC<-as.data.frame(rbind(pcr_pred_SC,temp_pcr_pred_SC))
}
colnames(pcr_pred_SC)<-c("Forecast","Service","SC")

#########################################################################
######################## SC Service Level ###############################
# Splitting Test and Train Data
SC<-unique(data_final_SC_Service$SC.5)
Service<-unique(data_final_SC_Service$Maintenance.Code)
for(j in Service) {
  for(i in SC) {
    temp_data_final_SC_Service<-subset(data_final_SC_Service,data_final_SC_Service$SC.5==i & data_final_SC_Service$Maintenance.Code==j)
    
    # Creation of next months estimated count of vehicles
    plot(temp_data_final_SC_Service$Total_CM,type = "b")
    temp_data_final_SC_Service_share<-temp_data_final_SC_Service[,c(1,6:9)]
    #
    temp_data_final_SC_Service_share<-subset(temp_data_final_SC_Service_share,temp_data_final_SC_Service_share$Month==forecast_month_index)
    temp_data_final_SC_Service_share<-temp_data_final_SC_Service_share[,2:5]
    Per_increase_2016_2015<-(temp_data_final_SC_Service_share[1,]-temp_data_final_SC_Service_share[2,])/temp_data_final_SC_Service_share[1,]
    Per_increase_2017_2016<-(temp_data_final_SC_Service_share[2,]-temp_data_final_SC_Service_share[3,])/temp_data_final_SC_Service_share[2,]
    Avg_Per_increase_total<-(Per_increase_2016_2015+Per_increase_2017_2016)/2
    temp_data_final_SC_Service_reg_test<-temp_data_final_SC_Service_share[3,]+(temp_data_final_SC_Service_share[3,]*Avg_Per_increase_total)
    temp_data_final_SC_Service_reg_train<-temp_data_final_SC_Service[,5:9]

    # Principal Component Regression
    temp_pcr_fit_SC_Service<-pcr(temp_data_final_SC_Service_reg_train$Total_CM~.,data = temp_data_final_SC_Service_reg_train,scale = TRUE,validation = "CV")
    summary(temp_pcr_fit_SC_Service)
    validationplot(temp_pcr_fit_SC_Service,val.type = "MSEP")
    temp_pcr_pred_SC_Service<-as.data.frame(predict(temp_pcr_fit_SC_Service,temp_data_final_SC_Service_reg_test,ncomp=3))
    temp_pcr_pred_SC_Service$Service<-j
    temp_pcr_pred_SC_Service$SC<-i
    colnames(temp_pcr_pred_SC_Service)<-c("Forecast","Service","SC")
    pcr_pred_SC_Service<-as.data.frame(rbind(pcr_pred_SC_Service,temp_pcr_pred_SC_Service))
  }
}
colnames(pcr_pred_SC_Service)<-c("Forecast","Service","SC")

##########################################################################
########## Binding all Forecasts into one file for Regression ############
pred_final<-rbind(pcr_pred_total,pcr_pred_total_Service,pcr_pred_SC,pcr_pred_SC_Service)
# write.csv(pred_final,"pred_final_test.csv",row.names = FALSE)

# % mixture of Regression and Timeseries comes out to be 37:63 for Timeseries:Regression
# The combined data for Regression and Timeseries is created in the Timeseries code
# First run the Timeseries code and then go to Regression Code.

###########################################################################
# This doesn't cut it , If we are supplying the test data set then we have to provide numbers
# at all levels the predictions are made. FUCK it.

###########################################################################
# Method -2
# try and isolate each month numbers accordingly and increase the actual numbers by avg increase %


###########################################################################
# Execution Order
# 1. N Dimensional Forecast
# 2. Time Series Forecast
# 3. Monthly Forecast to weekly Forecast
# 4. OP Calculations and Final Join
# 5. Actuals Current Month
###########################################################################
