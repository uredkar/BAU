Symbols = c("VFINX", "VEURX", "VPACX", "VEIEX", "SH","VGSIX", "IXC", "GLD", "VWEHX", "VCLT", "DBV")
Names = c("Vanguard 500 Index",
          "Vanguard European", 
          "Vanguard Pacific",  
          "Vanguard Emerging Mkts",
          "ProShares Short S&P500",
          "Vanguard REIT Index Inv",
          "iShares S&P Global Energy",
          "SPDR Gold Shares",
          "Vanguard High-Yield Corporate",
          "Vanguard Long-Term Corp Bond",
          "PowerShares DB G10 Currency Harvest")

ShortNames = c("EQ:US", "EQ:EU", "EQ:AP", "EQ:EM", "EQ:SHORT", "RE:REIT", "CM:ENERGY","CM:PM","FI:HYC", "FI:LTC","FX:CARRY10")
S = c("EQ", "EQ", "EQ", "EQ", "EQ", "RE", "CM", "CM","FI","FI","FX")
mxweight = rep(0.5, length(Symbols))
names(mxweight) <- Symbols
m = length(Symbols)
currency("USD")
getSymbols.RData(Symbols, env = globalenv(), 
                 dir = getwd(), return.class = "xts", 
                 extension = "RData", 
                 col.names = c("Open", "High", "Low", "Close","Volume","Adjusted"), 
                 auto.assign = TRUE, verbose = TRUE)
load("FF.rda")
align_interp(Symbols)
stock(Symbols, currency="USD")
###############################################################################
# Convert to monthly
for(i in 1:m){
  r_eparse2(paste(Symbols[i],".m=to.monthly(",Symbols[i],",indexAt='endof')",sep=""))
}
Symbols.m = paste(Symbols, ".m", sep="")

xidx = NULL
for(i in 1:length(Symbols)){
  mztime = time(get(paste(Symbols[i],".m",sep=""), envir = globalenv()))
  uztime =  time(get(Symbols[i]))
  tmp = sapply(mztime, FUN = function(x) which(x == uztime))
  xidx = c(xidx, as.character(na.omit(uztime[tmp])))
}
xidx = sort(unique(xidx))
# end of month dates to use
xidx = xidx[-(1:57)]
n = length(xidx)

load(file="signalm.rda")
load(file="ema_fast.rda")
load(file="ema_slow.rda")

load(paste("riskreward.rda",sep=""))

wtsx = matrix(0, ncol = m, nrow=n)
for(i in 1:n){
  tmp<-try(load(file=paste("wts_",i,".rda",sep="")))
  if(inherits(tmp, 'try-error')) next()
  # develerage
  if( !is.na(riskreward[i,2]/riskreward[i,1]) && riskreward[i,2]/riskreward[i,1]> 0.5 ) wtsx[i,] = wts else wtsx[i,] = wts/1.5
}
wts = wtsx
wts = xts(wts, as.Date(xidx))
colnames(wts) = Symbols

save(wts, file = "alternative_weights.rda")

for(i in 1:m){
  r_eparse2(paste(Symbols[i],".m = merge(",Symbols[i],".m,signalm[,",i,"],join='left')",sep=""))
  r_eparse2(paste("colnames(",Symbols[i],".m)[7]='Indicator'",sep=""))
}

for(i in 1:m){
  r_eparse2(paste(Symbols[i],".m = merge(",Symbols[i],".m,wts[,",i,"],join='left')",sep=""))
  r_eparse2(paste("colnames(",Symbols[i],".m)[8] = 'weights'",sep=""))
}

# remove na's introduced from previous operations
for(i in 1:m){
  r_eparse2(paste(Symbols[i],".m = na.omit(",Symbols[i],".m)",sep=""))
}


###############################################################################
userargs = list(max_pos = 0.5, mgt_fee = 0.01/12)
userargs$transaction_fun = transaction_fun
userargs$FF = FF

zidx = NULL
for(i in 1:length(Symbols)){
  mztime = time(get(paste(Symbols[i],".m",sep=""), envir = globalenv()))
  uztime =  time(get(Symbols[i]))
  tmp = sapply(mztime, FUN = function(x) which(x == uztime))
  zidx = c(zidx, as.character(na.omit(uztime[tmp+1])))
}
zidx = sort(unique(zidx))
# These are the actual dates which we trade (next day from signal)
userargs$eomdates = zidx
userargs$Symbols.m = Symbols.m
detach('package:parma')
detach('package:rmgarch')
.instrument <<- new.env()
.blotter <<- new.env()
portalt = execute_strategy_fun(Symbols, signal_fun = signal_fun, rebalance_fun = rebalance_fun, 
                               transaction_fun = transaction_fun, cash_fun = cash_fun, risk_fun = NULL,
                               start_date = "1994-01-01", end_date = "2013-05-01", 
                               init_equity = 1e6, port_name = "Port", account_name = "Account", userargs = userargs)

save(portalt, file="portalt.rda")
