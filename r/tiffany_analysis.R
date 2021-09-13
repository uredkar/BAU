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

current_dir = dirname(rstudioapi::getSourceEditorContext()$path)
current_dir 
setwd(current_dir)

df2021 = read.csv("tiffanny2021.csv")
colnames(df2021)
head(df2021)
df2021 %>% 
      select(Symbol,Date.Time,Transaction.Code,Transaction.Subcode,Expiration.Date,Strike,Call.Put,Fees,Amount,Price) %>% 
      arrange(desc(as.Date( Date.Time,'%m/d/Y %H:%M'))) %>%
      head

df2021 %>% filter(Symbol == "SQ") %>% arrange( Date.Time,Symbol,Expiration.Date,Strike,Call.Put)
df
df2021 %>% group_by(Symbol) %>%
  summarise(PL = sum(Amount),Fees = sum(Fees), na.rm = TRUE)
df2021 %>%
  summarise(across(everything(), ~ sum(., is.na(.), 0)))

df2021[,c("Date.Time","Amount")] %>% group_by()
df2020 = read.csv("tiffanny2020.csv")
df2020

