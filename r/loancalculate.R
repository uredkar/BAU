#install.packages("FinancialMath")
#install.packages("derivmkts")
#library("rsstudioapi")
library("FinancialMath")
library("derivmkts")
library("ggplot2")
library("precrec")
library("fOptions")
library("optionstrat")
library("PerformanceAnalytics")
#PerformanceAnalytics#install.packages("tidyverse")
library("tidyverse")
library(optionstrat)
library(readxl)
library(ES)
library(tidyquant)
library(DT)
library(FinCal) 
library(formattable)
interest_rate = 0.07
loan_value = 68000
years = 23

loan_payments <- pmt(pv = -loan_value,r = interest_rate, n = years, fv = 0)
loan_payments


loan_payments <- pmt(pv = -loan_value,r = interest_rate, n = years, fv = 0)
loan_payments
loan_table <- matrix(ncol = 6, nrow = years)
loan_table <- as.data.frame(loan_table)
colnames(loan_table) <- paste(c("Year", 'Initial_Balance', "Payments", "Interest",
                                "Principal", "Ending_Balance"))


loan_table[1,1] <- 1
loan_table[1,2] <- loan_value
loan_table[1,3] <- loan_payments
loan_table[1,4] <- loan_value * interest_rate
loan_table[1,5] <- loan_payments - (loan_value * interest_rate)
loan_table[1,6] <- loan_value - (loan_payments - (loan_value * interest_rate))

for(i in 2:years) {
  
  loan_table[i,1] <- i
  loan_table[i,2] <- loan_table[(i-1), 6]
  loan_table[i,3] <- loan_payments
  loan_table[i,4] <- loan_table[i,2] * interest_rate
  loan_table[i,5] <- loan_payments - (loan_table[i,2] * interest_rate)
  loan_table[i,6] <- loan_table[i,2] - (loan_payments - (loan_table[i,2] * interest_rate))
  
}


loan_table <- loan_table %>%
  mutate(Ending_Balance = round(Ending_Balance,3))

loan_table
formattable(loan_table)
