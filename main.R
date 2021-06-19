#!/usr/bin/Rscript
library(data.table)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(fpp2)
library(readr)
options(scipen=999)
# global functions
source("./functions.R")

## ========================= Data Preparation  ==================================== ##

dt <- read_csv("Sales.csv", 
          col_types = cols(customer = col_character(), sales = col_number()))
setDT(dt)
# summary(dt$sales)

# With the simple summary statistics we were able to find that the data contains negative sales values. 
# It looks like that the negative sales value show order cancellations 
# Example of cancelled orders 
# To demonstrate this we can find some customers who have sum of all transactions as 0 


# Therefore we can remove the datapoints which include customers who negative total sum of sales over the time period
rm_customers <- unique(dt[, .(total_sales = round(sum(sales),0)), by= 'customer'][total_sales <= 0, ]$customer)

# length(unique(dt$customer)) #4372 total customers 
# length(rm_customers) # 55 customers have negative sum of total sales => remove 
clean <- dt[!(customer %in% rm_customers), ]

# aggregating the clean dataset further 
categ_vec <- c('date', 'customer')
agg.dt <- setDT(clean)[, .(sales = sum(sales)), by= categ_vec]

agg.dt[, ':='(
  month= month(date, label = T), 
  year= year(date)
)][, ':='(month_year = paste(month, year, sep='-'))]


# we also have to remove the data data for December as it is the month for which we are predicting the sales 
daily_sales_cust <- 
  agg.dt[,.(sales_per_cust = round(sum(sales)/length(unique(customer)), 2)), by= .(date,month,year, month_year)][month_year != 'Dec-2017',]


## ========================= Exploratory Analysis   ==================================== ##

# we can check the performance of sales over the period of time in the data 

explore.dt <- daily_sales_cust[, .(days = length(date) , mean_sales_per_cust = mean(sales_per_cust)) , by = (month_year)]

total.dt <- 
  dt[, .(total_cust_unique = length(unique(customer)) , total_sales = sum(sales)), 
      by= paste(month(date, label=T), year(date), sep='-')
     ]
names(total.dt) <- c('month_year' ,'unique_customers', 'total_sales' )
summary.dt <- merge(explore.dt, total.dt , all.x = T)
summary.dt <- summary.dt[order(factor(summary.dt$month_year, levels=unique( monthOrder ) ))]


# basic plots 
p4 <- 
  plotLine(summary.dt, summary.dt$month_year , summary.dt$mean_sales_per_cust , 'Monthly Sales Per Customer' , 'Sales Per Customer')

p5 <- 
  plotLine(summary.dt, summary.dt$month_year , summary.dt$unique_customers , 'Monthly Unique Customers' , 'Unique Customers')

p6 <- 
  plotLine(summary.dt, summary.dt$month_year , summary.dt$total_sales , 'Monthly Total Sales' , 'Sales')

grid.arrange(p4, p5 , p6 , nrow = 1)



## ========================= Modeling ======================================= ##

# converting data into time series 
y <- ts(daily_sales_cust[,'sales_per_cust'] , start=c(2016,12) , end=c(2017,11), frequency = 12)
# we will use the sales per customer data for every day to predict the sales per customer for the month of december 2017 
daily_sales_cust

# declare the timeseries data 

y <- ts(daily_sales_cust[,'sales_per_cust'] , start=c(2016,12) , end=c(2017,11), frequency = 12)


# time plot 
# we got rid of the trend 
# dy <- diff(y)
# autoplot(dy)+ ggtitle('Timeplot: Change in daily sales')

# we cannot find seasonality in the data becaue there are not enough data  
# ggseasonplot(dy) + ggtitle('Seasonal Plot - Change in Daily Sales Per Customer')
# ggsubseriesplot(dy)

# Data is not sufficient to find seasonality 

# Naive Forecasting method/benchmark method 
# since we cannot find the seasonality so ket us use the naive method to predict. 
naive_mod <- naive(y, h = 1)
summary(naive_mod)
# mod1 <- checkresiduals(naive_mod)
residual_naive <- 213.15
forecast_naive <- 264.51

# Residual sd => 213.15 => close to 0 are better 
# auto-correleation between confidence interval 2 dash lines 

# seasonal benchmark method is not applicable as  needs the data for seasonality 

# simple exponential smoothering 
se_model <- ses(y, h = 1)
summary(se_model)
 # mod2 <- checkresiduals(se_model)
residual_sem <- 138.79
forecast_sem <- 436.31


# Holt's trend method 
holt_model <- holt(y, h = 1)
summary(holt_model)
# mod3 <- checkresiduals(holt_model)
residual_holt <- 153.15
forecast_holt <- 406.00


# Arima model 
arima_model <- auto.arima(y, stepwise=FALSE , approximation = FALSE)
summary(arima_model)
# mod4 <- checkresiduals(arima_model)
residual_arima <- sqrt(17512) #132.32  #square root of variance 

fore_arima <- forecast::forecast(arima_model, h=1)
autoplot(fore_arima)
forecast_arima <- 436.32


# ============== Evaluation ========================= #

# ARIMA wins 
# The data for december is only for 8 days
# We can remove this column as also we intend to predict the monthly sales for december 

# ============== Conclusion ========================= #

# we can expect to have 436 eur per cusomer of sales for december based on last 12 months of the data 

# ============== Appendix ========================= #

# (1) ====> Summary Statistics
summary(agg.dt$sales)
# summary_data 
summary.dt 

# (2) =======> Example of cancelled orders 
example_cust <- dt[, .(total_sales = round(sum(sales),0)), by= 'customer'][total_sales==0, ]$customer
cancelled_ord <- dt[customer %in% unique(example_cust), ]
setorderv(cancelled_ord, c('customer', 'sales'))

# (3) =====> Outliers 


min <- removeOutliers(agg.dt$sales)$minValue
max <- removeOutliers(agg.dt$sales)$maxValue


p1 <- 
  ggplot(agg.dt) +
  aes(x="" , y = sales) +
  geom_boxplot(fill = "#0c4c8a") +
  ggtitle('Daily customer sales with outliers') +
  theme_minimal()

p2 <- 
  ggplot( agg.dt[(sales < max & sales > min), ] ) +
  aes(x="" , y = sales) +
  geom_boxplot(fill = "#0c4c8a") +
  ggtitle('Daily customer sales without outliers') +
  theme_minimal()


grid.arrange(p1, p2 , nrow = 1)

# (4) =====> Model Performance 



