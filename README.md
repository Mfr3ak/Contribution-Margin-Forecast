# Contribution-Margin-Forecast

Forecast of Contribution Margin Using Time Series and Regression Models. Most of  the code part involves data cleaning and data arrangement for the Tableau Dashboard to work seamlessly. 

This Code is done in multiple parts and each part has its own specific use.

Files and their specific purposes

# 1 - N Dimensional Forecast
    Uses Regression Method for forecasting.
    Pricipal Component analysis is performed for removing collinearity

# 2 - Time Series Forecast
    As the name suggests this code does a time series forecast for the CM using the past 2 years data
    Auto Arima is used as the number of forecasts are about 21 and it becomes very difficult to adjust the time series for every model

# 3 - Monthly Forecast to Weekly Forecast
    This code lets you convert the monthly CM forecast to weekly/daily forecast and it does so by using the similarity of the calendar
    in Japan. It divides the monthly forecast into daily forecast by the same share as the CM contribution on the same working day as
    the previous year. This method is based on proportions.
   
# 4 - OP Calculation
    OP is already provided in two different files and both the files had to be used to come to the aggregations at the required level
    For compatibitliy with the tableau modelling the code has been set up in such a way to facilitate this
    
# 5 - Actual Current Month
    Acutals and fed to this code and aggregated at appropriate levels to compare apples to apples while considering numbers from OP
    and Acutals and the numbers from previous year.
    
# 6 - CM Forecast
    Creates a file by the name mtd_bind.csv which has be read for most of the codes above and it is a collection of all the data for
    the past 3 years with data cleaning steps and NA ignoring steps included. This code is reliant on the mtd's for each year created
    in the next code( # 7)
    
# 7 - Tracker Master Code
    This Code creates the mtd's which are base to each and every code above. Its main purpose is data cleaning which has to be done no 
    matter what as the data quality is pretty bad. This code uses kimura-san-function
    
# 8 - Kimura san function
    Data cleaning steps created as a function and it combines two different data sets "1-05" and "1-10" which are two different version
    of the same metric "total sales/revenue" from two different points of the vehicle journey. Data has to be reconsiled between the two
    datasets and then has to be passed on to mtd's and the workflow continues.
    
# 9 - Sequence of executing the codes.
    7,6,1,2,3,4,5
    
