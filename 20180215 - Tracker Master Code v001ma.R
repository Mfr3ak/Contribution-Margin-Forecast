require(RQuantLib)
library(bizdays)
current_month<-2

#Getting timestamp
timestamp_start = Sys.time()
# date_today = Sys.Date()
##List Directions
functions_dir = "C:/Users/MADIMUL/Downloads/Work/CM Forecast/R Functions/"
process_dir = "C:/Users/MADIMUL/Downloads/Work/CM Forecast/Processed Files/"
output_dir = "C:/Users/MADIMUL/Downloads/Work/CM Forecast/Processed Files/"
# common_file_dir = "C:/Users/SAILMOH/My Files/Discounting Structure/R Analysis/Common Files/"
# raw_data_dir = "C:/Users/SAILMOH/My Files/Discounting Structure/R Analysis/Services Database/Raw Data/"
current_dir = "C:/Users/MADIMUL/Downloads/Work/CM Forecast/Data/"
#################################################
# How to name file directory none and 1
# file_dir = "C:/Users/SAILMOH/My Files/Discounting Structure/R Analysis/Services Database/Raw Data/2017 Data/"
# file_dir_1 = "C:/Users/SAILMOH/My Files/Discounting Structure/R Analysis/Services Database/Raw Data/2016 Data/"
file_1_10 = "C:/Users/MADIMUL/Downloads/Work/CM Forecast/Data/1_10/"
# file_1_07 = "C:/Users/SAILMOH/My Files/Discounting Structure/R Analysis/Services Database/Raw Data/1_07/"
file_1_05 = "C:/Users/MADIMUL/Downloads/Work/CM Forecast/Data/1_05/"
file_dir_4_01 = "C:/Users/MADIMUL/Downloads/Work/CM Forecast/Data/4_01/"
# file_dir_OP = "C:/Users/SAILMOH/My Files/Discounting Structure/R Analysis/Services Database/Raw Data/OP Files"


library(data.table)
library(dplyr)
library(VIM)
library(lattice)
library(latticeExtra)
library(RODBC)
library(plyr)
library(lubridate)
library(readr)

setwd(functions_dir)
source("count_na_columns.R")
source("count_na_rows.R")
source("service_code_mapping.R")
source("service_process_hinmoku_code.R")
source("get_month.R")
source("get_year.R")
source("process_files_service_vt.R")
source("kimura_san_function.R")
source("get_401_cm.R")
source("ytd_progress.R")
source("get_week.R")
source("get_mtd_progress.R")
source("get_cm_perc.R")
source("get_historical_mtd.R")
source("get_vehicle_flow.R")
source("get_branch_tracker.R")

accounting_data = get_extra_cm_401()

# mtd_2015 = kimura_san_function("2015")
# mtd_2016 = kimura_san_function("2016")
# mtd_2017 = kimura_san_function("2017")
mtd_2018 = kimura_san_function("2018")
mtd_2018$month<-month(mtd_2018$`Invoicing Date`)
mtd_2018<-subset(mtd_2018,mtd_2018$month<current_month)
mtd_2018<-mtd_2018[,-c(12)]

## Writing to directory
# setwd("process_dir")
setwd("C:/Users/MADIMUL/Downloads/Work/CM Forecast/Processed Files/")
# write.csv(mtd_2015,"mtd_2015.csv")
# write.csv(mtd_2016,"mtd_2016.csv")
# write.csv(mtd_2017,"mtd_2017.csv")
write.csv(mtd_2018,"mtd_2018.csv")
write.csv(accounting_data,"accountingdata.csv")
