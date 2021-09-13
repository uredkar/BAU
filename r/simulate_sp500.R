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
df = read.csv("sp500yearly.txt")
plot(df$date,df$sp500)
plot(df$sp500,type='l',lwd=5)
hist(df$sp500)
plot(density(df$sp500))
var(df$sp500/100)
ES
plot(density(sample(df$sp500,10)/100))
plot(density(sample(df$sp500,10)))
initial_capital = 1000
sample_size = 20 # years
simulation_size = 50
set.seed(seed = NULL)
p = replicate(simulation_size,initial_capital*cumprod(1+sample(df$sp500,sample_size)/100),simplify = TRUE)
length(p[which(p < 1000)])/length(p)
length(p[which(p > 1000)])/length(p)
length(p[which(p > 2000)])/length(p)
length(p[which(p > 3000)])/length(p)
length(p[which(p > 4000)])/length(p)


#matplot(p,type=c("b"),pch=1)
dfgen <- data.frame(x=rep(1:sample_size,simulation_size), val=as.vector(p), 
                 variable=rep(paste0("sim", 1:simulation_size), each=sample_size))
# plot
#ggplot(data = dfgen, aes(x=x, y=val)) + geom_point(aes(colour=variable))
ggplot(data = dfgen, aes(x=x, y=val)) + geom_line(aes(colour=variable)) +
              geom_hline(yintercept = 2000) +
              geom_hline(yintercept = 3000, color="red") +
              geom_hline(yintercept = 4000) 
plot(as.vector(p))
hist(as.vector(p))# 

