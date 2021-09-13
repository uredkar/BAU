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
simulation_size = 600
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


# file.exists("Tiffany Trades Options Trade Journal.xls")
# xlsx_example <- read_excel("Tiffany Trades Options Trade Journal.xls")
# xlsx_example
# my_data <- read.table(file = "clipboard", 
#                       sep = "\t", header=TRUE)
# 
# write.csv(my_data, "tiffanny2020.csv", row.names=FALSE, quote=FALSE) 

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

tt = 1/12
daystoexpire = 30.0
S = 286
s=(-10:+10)+S; k=295; v=0.1445; r=0.01; tt=daystoexpire/365; d=0;
ds = covered.call(S=S,K=k,r=r,t=tt,sd=v,plot=TRUE)
profit = ds$Payoff["Profit"]
sum(profit)
str(profit)
profit
#covered.call(S=s,K=295,r=0.01,t=tt,sd=.15,plot=TRUE)
NoSteps <- 10
Asset <- S
Expiry <- tt
IntRate <- r
DividendRate <- 0.0
Volatility <- v
Strike <- k

genlattice <- function(X0 = 100, u = 1.1, d = .75, N = 5) {
  X <- c()
  X[1] <- X0
  count <- 2
  
  for (i in 1:N) {
    for (j in 0:i) {
      X[count] <- X0 * u^j * d^(i-j)
      count <- count + 1
    }
  }
  return(X)
}

genlattice(X0 = S,N= 5, u=1.1, d=.9)

genlattice.vanilla.european.call <- function(Asset, Volatility, IntRate, DividendRate, Strike, Expiry, NoSteps) {
  return (genlattice.european(Asset=Asset, Volatility=Volatility, IntRate=IntRate, DividendRate=DividendRate, Strike=Strike, Expiry=Expiry, NoSteps=NoSteps, Payoff=payoff.vanilla.call))
}

payoff.vanilla.call <- function(Asset, Strike){
  return( max(0, Asset - Strike))
}

payoff.vanilla.put <- function(Asset, Strike){
  return( max(Strike - Asset, 0))
}

genlattice.european <- function(Asset, Volatility, IntRate, DividendRate, Strike, Expiry, NoSteps, Payoff, Type){
  
  # Count the number of nodes on the tree.
  # For every step there are NoSteps + 1 node, so 
  # the total number of nodes would be summation of the
  # sequence from 1 until NoSteps +1
  
  
  count <- sum(1: (NoSteps + 1))
  
  # The objective is to have Asset and Option prices stored in a dataframe. (where are payoffs stored ?)
  # The mapping for the tree node (i,j) to a linear index inside the dataframe will need to be defined
  
  X <- data.frame(matrix(NA, nrow = count, ncol = 2))
  names(X) <- c("asset", "option")
  
  # Time between price movements
  dt = Expiry / NoSteps
  
  # Option price (and Asset) discount factor
  DiscountFactor <- exp(-IntRate * dt)
  
  # The up and down ticks with corresponding (synthetic) probabilities (Cox, Ross, Rubenstein Method)
  
  u = exp(Volatility * sqrt(dt))
  d = 1/u
  a = exp((IntRate - DividendRate) * dt)
  p = (a - d) / (u - d)
  
  
  # Up to now it was simply defining the variables and the rules for the option pricing calculation.
  # From now on, for every node asset price and the corresponding option price is calculated.
  # Starting from the last node of the tree (bottom right corner) and backwards
  
  for (i in NoSteps:0) {
    for (j in i:0) {
      X$asset[count] <- Asset * u^(i-j) * d^j
      
      # Compute payoff directly for last step's nodes,
      # otherwise use a formula.
      
      # More in detail, what happes is:
      #   1) the last column's payoff derives from the function which finds the max between asset and strike prices difference or zero
      #   These last nodes are identified as those elements on column i from the binominal tree where i = NoSteps
      #   2) walking backwards, the value of the option at a given node is the expected value of this option, meaning, the probability of this option going up times the value it will have when it goes up, plus the probability of this option going down times the value it will have when it comes down. Both values are then discounted for the delta time dt between the nodes.
      #   Since we have identified the option values at the last nodes, for each node position back we need to identify what are the future up and down movements.
      
      if (i == NoSteps) {
        X$option[count] <- Payoff(X$asset[count], Strike)
      } else {
        up   <- X$option[sum(1:(i+1), j, 1)]
        down <- X$option[sum(1:(i+1), j+1, 1)]
        X$option[count] <- DiscountFactor * (p * up + (1-p) * down)
      }
      
      count <- count - 1
    }
  }
  
  return(X)
}

# Generates a graph specification that can be fed into graphviz.
# Input: the binomial lattice produced by one of genlattice family functions.
dotlattice <- function(S, digits=2) {
  
  shape <- "plaintext"
  
  cat("digraph G {", "\n", sep="")
  cat("node[shape=",shape,"];","\n", sep="")
  cat("rankdir=LR;","\n")
  
  cat("edge[arrowhead=none];","\n")
  
  # Create a dot node for each element in the lattice
  for (i in 1:nrow(S)) {
    x <- round(S$asset[i], digits=digits)
    y <- round(S$option[i], digits=digits)
    
    # Detect the American tree and draw accordingly
    early.exercise <- ""
    if (("exercise" %in% colnames(S)) && S$exercise[i]) {
      early.exercise <- "shape=oval,"
    }
    
    cat("node", i, "[", early.exercise, "label=\"", x, ", ", y, "\"];", "\n", sep="")
  }
  
  # The number of levels in a binomial lattice of length N
  # is `$\frac{\sqrt{8N+1}-1}{2}$`
  L <- ((sqrt(8*nrow(S)+1)-1)/2 - 1)
  
  k<-1
  for (i in 1:L) {
    tabs <- rep("\t",i-1)
    j <- i
    while(j>0) {
      cat("node",k,"->","node",(k+i),";\n",sep="")
      cat("node",k,"->","node",(k+i+1),";\n",sep="")
      k <- k + 1
      j <- j - 1
    }
  }
  
  cat("}", sep="")
}
# call option

x <- genlattice.vanilla.european.call(Asset=S, Volatility=v, IntRate=r, DividendRate=0.0, Strike=k, 
                                      Expiry=tt, NoSteps=10)
y <- capture.output(dotlattice(x, digits=4))
cat(y, file="lattice.dot")

d1 <- (log(S/k) + (r + v^2/2) * tt)/(v * sqrt(tt))
d2 <- d1 - v * sqrt(tt)
phid1 <- pnorm(d1)
call_price <- S * phid1 - k * exp(-r * tt) * pnorm(d2)
call_price
# put option
d1 <- (log(S/k) + (r + v^2/2) * tt)/(v * sqrt(tt))
d2 <- d1 - v * sqrt(tt)
phimd1 <- pnorm(-d1)
put_price <- -S * phimd1 + k * exp(-r * tt) * pnorm(-d2)

c(call_price, put_price)


# call put option monte carlo
call_put_mc<-function(nSim=1000000, tau, r, sigma, S0, K) {
  
  Z <- rnorm(nSim, mean=0, sd=1)
  WT <- sqrt(tau) * Z
  ST = S0*exp((r - 0.5*sigma^2)*tau + sigma*WT)
  
  # price and standard error of call option
  simulated_call_payoffs <- exp(-r*tau)*pmax(ST-K,0)
  price_call <- mean(simulated_call_payoffs)
  sterr_call <- sd(simulated_call_payoffs)/sqrt(nSim)
  
  # price and standard error of put option
  simulated_put_payoffs <- exp(-r*tau)*pmax(K-ST,0)
  price_put <- mean(simulated_put_payoffs)
  sterr_put <- sd(simulated_put_payoffs)/sqrt(nSim)
  
  
  output<-list(price_call=price_call, sterr_call=sterr_call, 
               price_put=price_put, sterr_put=sterr_put)
  return(output)
  
}


set.seed(1)
results<-call_put_mc(n=1000000, tau=tt, r=r, sigma=v, S0=S, K=k)

results
antithetic_call_put_mc<-function(nSim, tau, r, sigma, S0, K) {
  
  Z <- rnorm(nSim, mean=0, sd=1)
  
  WT <- sqrt(tau) * Z
  # ST1 and ST2 and the antithetic variates
  ST1 = (S0*exp((r - 0.5*sigma^2)*tau + sigma*WT))
  ST2 = (S0*exp((r - 0.5*sigma^2)*tau + sigma*(-WT)))
  
  # call option price and standard error
  simulated_call_payoffs1 <- exp(-r*tau)*pmax(ST1-K,0)
  simulated_call_payoffs2 <- exp(-r*tau)*pmax(ST2-K,0)
  # get the average
  simulated_call_payoffs <- ( simulated_call_payoffs1 + simulated_call_payoffs2)/2
  price_call <- mean(simulated_call_payoffs)
  sterr_call <- sd(simulated_call_payoffs)/sqrt(nSim)
  
  
  # put option price and standard error
  simulated_put_payoffs1 <- exp(-r*tau)*pmax(K-ST1,0)
  simulated_put_payoffs2 <- exp(-r*tau)*pmax(K-ST2,0)
  # get the average
  simulated_put_payoffs <- (simulated_put_payoffs1+simulated_put_payoffs2)/2
  price_put <- mean(simulated_put_payoffs)
  sterr_put <- sd(simulated_put_payoffs)/sqrt(nSim)
  
  output<-list(price_call=price_call, sterr_call=sterr_call, 
               price_put=price_put, sterr_put=sterr_put )
  return(output)
  
}

set.seed(1)
results<-antithetic_call_put_mc(n=1000000, tau=tt, r=r, sigma=v, S0=S, K=k)

results

                                                                                                                                                                                                                                                                                                                           r.parentNode.insertBefore(s, r);
                                                                                                                                                                                                                                                                                                                                                     }
iv.calc <- function(type, price, s, x, t, r, d=0) {
  
  dvol <- function(type, price, volatility=0) {
    if(type == "call"){
      price -  callpremium(s = s, x = x, sigma = volatility, t = t, r = r, d = d)
    } else if(type == "put"){
      price -  putpremium(s = s, x = x, sigma = volatility, t = t, r = r, d = d)
    } else{
      stop("type must be either 'call' or 'put' ")
    }
  }
  
  
  
  volchart <- data.frame(vol = seq(0, 5, 0.001))
  volchart$distance <- dvol(type, price, volchart$vol)
  
  
  volchart$vol[match(min(abs(volchart$distance)), abs(volchart$distance))]
  
}
?prob.above
prob.above(spot = S, lower = S * 1.10, mean = 0, dsd = v, dte = daystoexpire)
prob.above(spot = S, mean = 0, dsd = v, dte = daystoexpire, p = 0.75, quantile = TRUE)

iv.calc(type = "call", price = 14, s = S, x = k, t = tt, r = r, d = 0)

par(mfrow=c(1,1))


delta = calldelta(s=s, x = k, sigma = v, t = tt, r = r, d = 0)
plot(delta)
calleval(s=s, x = k, sigma = v, t = tt, r =r, d = 0)

callgreek(greek = c("delta", "gamma", "theta", "vega", "rho", "premium"),
          s=s, x = k, sigma = v, t = tt, r = r, d = 0)

## Cox-Ross-Rubinstein Binomial Tree Option Model:
# Example 14.1 from Hull's Book:
CRRBinomialTreeOption(TypeFlag = "pa", S = s, X = k, 
                      Time = tt, r = r,b = 0.1, sigma = v, n = 5)
# Example 3.1.1 from Haug's Book:
CRRBinomialTreeOption(TypeFlag = "pa", S = s, X = k, 
                      Time = tt, r = r,b = 0.1, sigma = v, n = 5)
# A European Call - Compare with Black Scholes: 
CRRBinomialTreeOption(TypeFlag = "ce", S = s, X = k, 
                      Time = tt, r = r,b = 0.1, sigma = v, n = 5)
GBSOption(TypeFlag = "c", S = s, X = k, 
          Time = tt, r = r,b = 0.1, sigma = v)@price

## CRR - JR - TIAN Model Comparison:  
# Hull's Example as Function of "n":
par(mfrow = c(2, 1), cex = 0.7)
steps = 50
CRROptionValue =  JROptionValue = TIANOptionValue = 
  rep(NA, times = steps)
for (n in 3:steps) { 
  CRROptionValue[n] = CRRBinomialTreeOption(TypeFlag = "pa", S = s, 
                                            X = k, Time = tt, r = r, b = 0.1, sigma = 0.4, n = n)@price
  JROptionValue[n] = JRBinomialTreeOption(TypeFlag = "pa", S = 50, 
                                          X = k, Time = tt, r = r, b = 0.1, sigma = 0.4, n = n)@price
  TIANOptionValue[n] = TIANBinomialTreeOption(TypeFlag = "pa", S = s, 
                                              X = k, Time = tt, r = r, b = 0.1, sigma = 0.4, n = n)@price
}           
plot(CRROptionValue[3:steps], type = "l", col = "red", ylab = "Option Value")
lines(JROptionValue[3:steps], col = "green")
lines(TIANOptionValue[3:steps], col = "blue")
# Add Result from BAW Approximation:
BAWValue =  BAWAmericanApproxOption(TypeFlag = "p", S = 50, X = 50, 
                                    Time = 0.4167, r = 0.1, b = 0.1, sigma = 0.4)@price
abline(h = BAWValue, lty = 3)
title(main = "Convergence")
data.frame(CRROptionValue, JROptionValue, TIANOptionValue)

## Plot CRR Option Tree:
# Again Hull's Example:
v*4
CRRTree = BinomialTreeOption(TypeFlag = "ca", S = S, X = k, 
                             Time = tt, r = r, b = 0.01, sigma = v*4, n = 5)

BinomialTreePlot(CRRTree, dy = 1, cex = 0.8, ylim = c(-6, 7),
                 xlab = "n", ylab = "Option Value")
title(main = "Option Tree")
par(mfrow = c(1, 1), cex = 0.7)
binomopt(S, k, v, r, tt, d, nstep = 10, american = TRUE,
         putopt=FALSE, specifyupdn=FALSE, crr=FALSE, jarrowrudd=FALSE,
         up=1.5, dn=0.5, returntrees=FALSE, returnparams=FALSE,
         returngreeks=FALSE)

#s=229; k=235; v=0.30; r=0.01; tt=0.25; d=0;
greeks(bscall(s, k, v, r, tt, d), complete=TRUE, long=FALSE, initcaps=TRUE)
greeks2(bscall, list(s=s, k=k, v=v, r=r, tt=tt, d=d))
greeks2(bscall, list(s=s, k=k, v=v, r=r, tt=tt, d=d))[c('Delta', 'Gamma'), ]
S
k
bsopt(s, k, v, r, tt, d)
#bsopt(s, c(35, 40, 45), v, r, tt, d)
#bsopt(s, c(35, 40, 45), v, r, tt, d)[['Call']][c('Delta', 'Gamma'), ]

#k <- 100; v <- 0.30; r <- 0.08; tt <- 2; d <- 0
S <- seq(.5, 350, by=.5)
Call <- greeks(bscall(S, k, v, r, tt, d))
Put <- greeks(bsput(S, k, v, r, tt, d))
y <- list(Call=Call, Put=Put)
par(mfrow=c(4, 4), mar=c(2, 2, 2, 2)) ## create a 4x4 plot
for (i in names(y)) {
  for (j in rownames(y[[i]])) { ## loop over greeks
    plot(S, y[[i]][j, ], main=paste(i, j), ylab=j, type='l')
  }
}


call_long <- greeks(bscall(S, k, v, r, tt, d), long=TRUE,complete=TRUE)
#call_long[c('Delta', 'Gamma'), ]
ggplot2::ggplot(call_long, aes(x=s, y=value)) +
  geom_line() + facet_wrap(~greek, scales='free')


# option straggies
library(Rmpfr)
prices <- seq(700,950,1) # Vector of prices
strike <- 850 # strike price for both put and call 
premium_call <- 20 # option price call
premium_put <- 10  # option price put 

# call option payoff at expiration 
intrinsicValuesCall <- prices - strike - premium_call
payoffLongCall <- pmax(-premium_call,intrinsicValuesCall)

# put option payoff at expiration
intrinsicValuesPut <- strike - prices - premium_put
payoffLongPut <- pmax(-premium_put,intrinsicValuesPut)

# The payoff of the Strategy is the sum of the call and put payoff. Need
# to sum wise element by element between the two vectors
payoff <- rowSums(cbind(payoffLongCall,payoffLongPut))

# Make a DataFrame with all the variable to plot it with ggplot
results <- data.frame(cbind(prices,payoffLongCall,payoffLongPut,payoff))

ggplot(results, aes(x=prices)) + 
  geom_line(aes(y = payoffLongCall, color = "LongCall")) + 
  geom_line(aes(y = payoffLongPut, color="LongPut"))+
  geom_line(aes(y=payoff, color = 'Payoff')) +
  scale_colour_manual("", 
                      breaks = c("LongCall", "LongPut", "Payoff"),
                      values = c("darkred", "darkblue", "darkgreen")) + ylab("Payoff")+
  ggtitle("Long Straddle Payoff")  
