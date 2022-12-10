library(quantmod)
library(cabootcrs)
library(fOptions)
library(PerformanceAnalytics)
library(RColorBrewer)
library(plotly)
library(rugarch)
library(quantmod)
number_of_trading_days = 252
stock.sim = function(sigma, ndays = 90, S0 = 100, mu = 0.05) {
  N = rnorm(ndays)
  S = S0
  dT = 1/number_of_trading_days
  Sres = S
  for (i in 1:ndays) {
    dS = S * (mu * dT + sigma * sqrt(dT) * N[i])
    S = S + dS
    Sres = c(Sres, S)
  }
  return(Sres)
}

stock.sim = function(niters, sigma = 0.5, ndays = 90, S0 = 100, mu = 0.05) {
  N = rnorm(ndays)
  S = S0
  dT = 1/number_of_trading_days
  Sres = S
  for (i in 1:ndays) {
    dS = S * (mu * dT + sigma * sqrt(dT) * N[i])
    S = S + dS
    Sres = c(Sres, S)
  }
  return(Sres)
}

# exotice option payoff simulation
set.seed(500316995)
exo_stock.payoff = function(niters, sigma = 0.5, ndays = 90, S0 = 100, mu = 0.05, 
                            strike.price = 105, knockout.price = 130) {
  N = rnorm(ndays)
  S = S0
  dT = 1/number_of_trading_days
  Sres = S
  for (i in 1:ndays) {
    dS = S * (mu * dT + sigma * sqrt(dT) * N[i])
    S = S + dS
    Sres = c(Sres, S)
  }
  option.payoff = if (any(Sres >= knockout.price)) 
    0 else max(tail(Sres, 1) - strike.price, 0)
  
  return(option.payoff)
}

set.seed(500316995)
van_stock.payoff = function(niters, sigma = 0.5, ndays = 90, S0 = 100, mu = 0.05, 
                            strike.price = 105) {
  N = rnorm(ndays)
  S = S0
  dT = 1/number_of_trading_days
  Sres = S
  for (i in 1:ndays) {
    dS = S * (mu * dT + sigma * sqrt(dT) * N[i])
    S = S + dS
    Sres = c(Sres, S)
  }
  option.payoff = max(tail(Sres, 1) - strike.price, 0)
  
  return(option.payoff)
}

niters = seq(1000)
van.payoff = sapply(niters, van_stock.payoff)

van.density <- density(van.payoff)
niters = seq(1000)
exo.payoff = sapply(niters, exo_stock.payoff)

exo.density <- density(exo.payoff)
palette <- brewer.pal(7, "Dark2")
niters = seq(5)
price = sapply(niters, stock.sim)
matplot(price, type = "l", xlab = "Day", ylab = "Stock price", col = palette[1:5])
legend("topright", legend = niters, col = palette[1:5], lty = 1:5, cex = 0.8, title = "simulations")

sigmas = c(0.1, 0.25, 0.5, 0.75, 1)
price = sapply(sigmas, stock.sim)
matplot(price, type = "l", xlab = "Day", ylab = "Stock price")
legend("topright", legend = sigmas, col = 1:5, lty = 1:5, cex = 0.8, title = "sigma")


fig <- plot_ly(x = ~exo.density$x, y = ~exo.density$y, name = "exotice option", type = "scatter", 
               mode = "lines", fill = "tozeroy")
fig <- fig %>% add_trace(x = ~van.density$x, y = ~van.density$y, name = "vanilla call option", 
                         fill = "tozeroy")
fig <- fig %>% layout(title = "Option Pay-offs", xaxis = list(title = "Payoff"), 
                      yaxis = list(title = "Density"))

fig


fig <- plot_ly(y = exo.payoff, type = "box", boxpoints = "suspectedoutliers", name = "exotice option")
fig <- fig %>% add_trace(y = van.payoff, boxpoints = "suspectedoutliers", name = "vanilla call option")
fig <- fig %>% layout(title = "Option Pay-offs", xaxis = list(title = "Option Type"), 
                      yaxis = list(title = "Payoff"))

fig


# Price of $105 ($130) exotice options is the expected value of the payoffs
exo.price <- mean(exo.payoff)

# Price of $105 vanilla call options is the expected value of the payoffs
van.price <- mean(van.payoff)

Price_gap <- van.price - exo.price
compare_price <- data.frame(exotice_price = exo.price, vanilla_price = van.price, 
                            Price_gap = Price_gap)
knitr::kable(compare_price)
normalise_series <- function(xdat) xdat / coredata(xdat)[1]

library(quantmod)
data  <- getSymbols(c("SPY", "TAIL","SWAN","BND"),  from="2017-06-01")

head(SPY)
tail(SPY)
?plot.xts
tail(SPY$SPY.Adjusted)
mytheme <- chart_theme()
mytheme$col$line.col <- "darkgreen"
chart_Series(normalise_series(Cl(SPY)) - 1, theme = mytheme,name="spy tail swan bnd")
add_TA(normalise_series(Cl(TAIL)) - 1, on = 1, col = "red", lty = 3)
add_TA(normalise_series(Cl(SWAN)) - 1, on = 1, col = "red", lty = 3)
add_TA(normalise_series(Cl(BND)) - 1, on = 1, col = "blue", lty =2)

prices <- merge(Ad(SPY), Ad(TAIL),Ad(SWAN), Ad(BND))
ret = na.omit(Return.calculate(prices))
head(ret)
plot(ret)
par(mfrow=c(2,2))
acf(ret, main="Return ACF");
pacf(ret, main="Return PACF");
acf(ret^2, main="Squared return ACF");
pacf(ret^2, main="Squared return PACF")
par(mfrow=c(4,2))

for(colname in colnames(ret)){
  par(mfrow=c(1,7))  
  print(colname)
  r = ret[,colname]
  r <- na.omit(r)
  m=mean(r);s=sd(r);
  k = kurtosis(r)
  print(k)
  print(m)
  print(s)
  hist(r, nclass=40, freq=FALSE, main=paste(colname,'Return histogram'))
  curve(dnorm(x, mean=m,sd=s), from = -s*10, to = s*10, add=TRUE, col="blue")

  plot(density(r), main=paste(colname,' Return empirical distribution'))
  curve(dnorm(x, mean=m,sd=s), from = -s*10, to = s*10, add=TRUE, col="blue")
  
  plot(density(r), main='Return EDF - upper tail', xlim = c(s*2, s*10), ylim=c(0,2));
  curve(dnorm(x, mean=m,sd=s),  from = -s*10, to = s*10, add=TRUE, col="red")
  
  #plot(density(r), xlim=c(-s*10,s*10),log='y', main='Density on log-scale')
  #curve(dnorm(x, mean=m,sd=s), from=-s*10, to=s*10, log="y", add=TRUE, col="red")
  qqnorm(r);qqline(r);
  garch11.spec = ugarchspec(variance.model = list(model="sGARCH", garchOrder=c(1,1)), 
                            mean.model = list(armaOrder=c(0,0)))
  garch11.fit = ugarchfit(spec=garch11.spec, data=r)
  coef(garch11.fit)
  coef(garch11.fit)          #estimated coefficients
  vcov(garch11.fit)          #covariance matrix of param estimates
  infocriteria(garch11.fit)  #common information criteria list
  newsimpact(garch11.fit)    #calculate news impact curve
  signbias(garch11.fit)      #Engle - Ng sign bias test
  fitted(garch11.fit)        #obtain the fitted data series
  residuals(garch11.fit)     #obtain the residuals
  uncvariance(garch11.fit)   #unconditional (long-run) variance
  uncmean(garch11.fit)       #unconditional (long-run) mean
  
  ni.garch11 <- newsimpact(garch11.fit)
  plot(ni.garch11$zx, ni.garch11$zy, type="l", lwd=2, col="blue", 
       main="GARCH(1,1) - News Impact", 
       ylab=ni.garch11$yexpr, 
       xlab=ni.garch11$xexpr)
  
  egarch11.spec = ugarchspec(variance.model = list(model="eGARCH", garchOrder=c(1,1)), mean.model = list(armaOrder=c(0,0)))
  egarch11.fit = ugarchfit(spec=egarch11.spec, data=r)
  coef(egarch11.fit)
  
  ni.egarch11 <- newsimpact(egarch11.fit)
  plot(ni.egarch11$zx, ni.egarch11$zy, type="l", lwd=2, col="blue", main="EGARCH(1,1) - News Impact",
       ylab=ni.egarch11$yexpr, xlab=ni.egarch11$xexpr)
  
  tgarch11.spec = ugarchspec(variance.model = list(model="fGARCH", submodel="TGARCH", garchOrder=c(1,1)), 
                             mean.model = list(armaOrder=c(0,0)))
  tgarch11.fit = ugarchfit(spec=tgarch11.spec, data=r)
  coef(egarch11.fit)
  
  ni.tgarch11 <- newsimpact(tgarch11.fit)
  plot(ni.tgarch11$zx, ni.tgarch11$zy, type="l", lwd=2, col="blue", main="TGARCH(1,1) - News Impact",
       ylab=ni.tgarch11$yexpr, xlab=ni.tgarch11$xexpr)
  
  garch11.spec = ugarchspec(variance.model = list(garchOrder=c(1,1)), 
                            mean.model = list(armaOrder=c(0,0)),
                            fixed.pars=list(mu = 0, omega=0.1, alpha1=0.1,
                                            beta1 = 0.7))
  garch11.sim = ugarchpath(garch11.spec, n.sim=1000)
  
  garch11.fit = ugarchfit(spec=garch11.spec, data=r, out.sample=20)
  garch11.fcst = ugarchforecast(garch11.fit, n.ahead=10, n.roll=10)
  par(mfrow=c(1,1))
  plot(garch11.fcst, which='all')
  
}
par(mfrow=c(1,1))
head(na.omit(ret))
chartSeries(Ad(TAIL))
par(mfrow=c(2,2))

r8020 = Return.portfolio(ret,weights=c(0.40,0.03,0.17,0.40),geometric = TRUE,
                         reblance_on="weekly")



r8020tail1 = Return.portfolio(ret,weights=c(0.93,0.03,0.03,0.01),
                             reblance_on="weekly")

            
r8020tail2 = Return.portfolio(ret,weights=c(0.83,0.10,0.03,0.01),geometric = TRUE,
                     reblance_on="weekly")


r8020tailBond = Return.portfolio(ret,weights=c(0.14,0.03,0.03,0.80),geometric = TRUE,
                             reblance_on="weekly")

mul_rets <- merge(r8020, r8020tail1,r8020tail2, r8020tailBond)
head(mul_rets)
plot(mul_rets)
chart.CumReturns(mul_rets,wealth.index = TRUE,
                 lty=c(3,3,4,1),
                 col=c("red","blue","black","green"))


dd = rbind(maxDrawdown(ret,weights=c(0.93,0.0,0.06,0.01), geometric = TRUE),
           maxDrawdown(ret,weights=c(0.93,0.03,0.03,0.01), geometric = TRUE),
           maxDrawdown(ret,weights=c(0.93,0.05,0.01,0.01), geometric = TRUE),
           maxDrawdown(ret,weights=c(0.16,0.03,0.01,0.80), geometric = TRUE))
plot(dd,type="p",pch='.',cex=4,main="draw down increases with risk irrespective of TAIL")


chart.Correlation(ret, histogram=TRUE, pch="+")

SharpeRatio.annualized(ret, Rf = 0.01, scale = 12, geometric = TRUE)
SharpeRatio.annualized(ret, Rf = 0.01, scale = 4, geometric = TRUE)
SharpeRatio.annualized(ret, Rf = 0.01, scale = 252, geometric = TRUE)


S = as.vector(SPY$SPY.Adjusted)
X = as.vector(SPY$SPY.Adjusted)*.80
sigma = sd(tail(S,10))
sigma*sqrt(252)
#Time = 252/82 days
optprice = BSAmericanApproxOption(TypeFlag = "p", S = S, X = S*0.80, 
                                  Time = 1/3, r = 0.07, 
                                  sigma = 0.30, b = 0.03)

head(optprice@price)
plot(S,optprice@price)
optprice = BSAmericanApproxOption(TypeFlag = "p", S = 443.91, X = 354, Time = 1/3, r = 0.07, 
                                  sigma = 0.30, b = 0.03)
head(optprice@price)
ret = Return.calculate(SPY$SPY.Adjusted)
sd(ret$SPY.Adjusted,na.rm = TRUE)*sqrt(252/4)

data(edhec)
sd.annualized(edhec)
sd.annualized(edhec[,6,drop=FALSE])
# now for three periods:
sd.multiperiod(edhec[,6,drop=FALSE],scale=3)
head(optprice@price)


data(sp500ret)
# create a cluster object to be used as part of this demonstration

cluster = makePSOCKcluster(15)

spec = ugarchspec()
show(spec)

nrow(expand.grid(GARCH = 1:14, VEX = 0:1, VT = 0:1, Mean = 0:1, ARCHM = 0:2, ARFIMA = 0:1, MEX = 0:1, DISTR = 1:10))

spec = ugarchspec(variance.model = list(model = 'eGARCH', garchOrder = c(2, 1)), distribution = 'std')
setstart(spec) <- list(shape = 5)
#setbounds(spec)
fit = ugarchfit(spec, sp500ret[1:1000, , drop = FALSE], solver = 'hybrid')

spec = getspec(fit)
setfixed(spec) < - as.list(coef(fit))

