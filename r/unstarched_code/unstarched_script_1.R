#################################################################
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
getSymbols(Symbols, env = globalenv(), from="1980-01-01",
           dir = getwd(), return.class = "xts", 
           auto.assign = TRUE, verbose = TRUE)
align_interp(Symbols)
for(i in 1:m){
  r_eparse2(paste(Symbols[i],"=adjustOHLC(",Symbols[i],",use.Adjusted=TRUE)",sep=""))
}
FF = .getFamaFrench(enddate = Sys.Date())
saveSymbols(Symbols, file.path=getwd())
save(FF, file = "FF.rda")
stock(Symbols, currency="USD")
###############################################################################
# Convert to monthly
for(i in 1:m){
  r_eparse2(paste(Symbols[i],".m=to.monthly(",Symbols[i],",indexAt='endof')",sep=""))
}
Symbols.m = paste(Symbols, ".m", sep="")

# Assign Starting Values to filter
fast = rep(3,m)
slow = rep(10, m)
names(fast) = names(slow) <- Symbols.m
###############################################################################
# create returns and check for minimum data
X = ROC(Cl(VFINX), type = "continuous")
for(i in 2:m){
  X = cbind(X, ROC(Cl(get(Symbols[i])), type = "continuous"))
}
.has600points = function(X){
  idx = min(which(!is.na(X)))
  return( idx + 600 )
}
idx = apply(X, 2, FUN = function(x) .has600points(x))
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
###############################################################################
signalm = matrix(0, ncol = m, nrow = n)
signalm = xts(signalm, as.POSIXct(xidx))
colnames(signalm) <- Symbols

ema_fast = matrix(fast, ncol = m, nrow = n, byrow=TRUE)
ema_fast = xts(ema_fast, as.POSIXct(xidx))
colnames(ema_fast) <- Symbols

ema_slow = matrix(slow, ncol = m, nrow = n, byrow=TRUE)
ema_slow = xts(ema_slow, as.POSIXct(xidx))
colnames(ema_slow) <- Symbols

for(i in 1:n){
  tmp = sapply(Symbols.m, function(x) emaoptfn(Cl(get(x))[paste("/",xidx[i],sep=""),], slow[x], fast[x]))
  signalm[xidx[i], ] = as.integer(tmp[1,])
  ema_fast[xidx[i], ] = as.integer(tmp[3,])
  ema_slow[xidx[i], ] = as.integer(tmp[2,])
}
save(signalm, file="signalm.rda")
save(ema_fast, file="ema_fast.rda")
save(ema_slow, file="ema_slow.rda")
###############################################################################
spec = ugarchspec(mean.model=list(armaOrder=c(1,0)), variance.model=list(variance.targeting=TRUE), 
                  distribution = "std")
setbounds(spec)<-list(shape=c(4.05, 30))

f_dgp = function(i){
  wts = rep(0, m)
  sig = as.integer(signalm[xidx[i], ])
  inx = length(inc<-which(sig==1))
  if(inx==1){
    return(0)
  } else if(inx==0){
    return(0)
  } else{
    dx = which(xidx[i] == as.character(time(X)))
    Data = X[1:dx, inc]
    ix = dim(Data)[1] - apply(Data, 2, FUN = function(x) min(which(!is.na(x))))
    y = which(ix<600)
    eqd = NA
    if(length(y)>0){
      Data = Data[,-y]
      eqd = inc[y]
      inc = inc[-y]
    }
    Data = na.omit(Data)
    mc = ncol(Data)
    if(mc<4){
      return(0)
    }
    nr = nrow(Data)
    Data = Data[1:nr, ]
    Data = tail(Data, min(600,nr))
    msp = multispec(replicate(ncol(Data), spec))
    for(j in 1:mc){
      setstart(msp@spec[[j]])<-list(shape=4.5)
      setbounds(msp@spec[[j]])<-list(shape=c(4.01, 30))
    }
    xfit = multifit(msp, Data)
    mspec = cgarchspec(msp, distribution.model = list(copula="mvt", 
                                                      transformation = "parametric", time.varying = FALSE))
    fit = try(cgarchfit(mspec, Data, fit.control=list(eval.se = FALSE), solver="solnp", 
                        solver.control=list(trace=1), fit = xfit), silent = TRUE)
    save(fit, file=paste("fit_",i,".rda",sep=""))
    if(inherits(fit, 'try-error')) return(0)
    sim = cgarchsim(fit, n.sim = 25, m.sim = 5000, startMethod="sample")
    scen = exp(t(sapply(sim@msim$simX, FUN = function(x) colSums(x))))-1
    save(scen, file=paste("scen_",i,".rda",sep=""))
    return(ncol(scen))
  }
}
cl = makePSOCKcluster(ncores)
clusterEvalQ(cl, library(rmgarch))
clusterEvalQ(cl, library(rugarch))
clusterEvalQ(cl, library(xts))
clusterExport(cl, c("Symbols.m", "xidx", "m", "f_dgp","signalm","mxweight"))
clusterEvalQ(cl, source('customfn.R'))
clusterExport(cl, c(Symbols.m, "X","spec"))
ix = 1:n
sol = parLapply(cl,ix, function(i) f_dgp(i))
stopCluster(cl)


f_opt = function(i){
  wts = rep(0, m)
  sig = as.integer(signalm[xidx[i], ])
  inx = length(inc<-which(sig==1))
  if(inx==1){
    wts[inc] = as.numeric(mxweight[inc])
    save(wts, file=paste("wts_",i,".rda",sep=""))
    return(c(NA, NA))
  } else if(inx==0){
    save(wts, file=paste("wts_",i,".rda",sep=""))
    return(c(NA, NA))
  } else{
    dx = which(xidx[i] == as.character(time(X)))
    Data = X[1:dx, inc]
    ix = dim(Data)[1] - apply(Data, 2, FUN = function(x) min(which(!is.na(x))))
    y = which(ix<600)
    eqd = NA
    if(length(y)>0){
      Data = Data[,-y]
      eqd = inc[y]
      inc = inc[-y]
    }
    Data = na.omit(Data)
    mc = ncol(Data)
    if(mc<4){
      wts[which(sig==1)] = as.numeric(mxweight[which(sig==1)])
      if(sum(wts)>1) wts = wts/sum(wts)
      save(wts, file=paste("wts_",i,".rda",sep=""))
      return(c(NA, NA))
    }
    load(file=paste("scen_",i,".rda",sep=""))		
    arix = unique(which(scen<(-1), arr.ind=TRUE)[,1])
    if(length(arix)>1){
      scen = scen[-arix,]
      print(length(arix))
    }
    arix = unique(which(scen>1, arr.ind=TRUE)[,1])
    if(length(arix)>1){
      scen = scen[-arix,]
      print(length(arix))
    }
    # truncate
    ubx = as.numeric(mxweight[inc])
    b = min(1, sum(ubx))
    if(b<1){
      ubx = as.numeric(mxweight[inc])*1.5
      b = min(1, sum(ubx))
    }
    pspec = parmaspec(scenario = scen, forecast = colMeans(scen), target = NULL, 
                      targetType =  c("inequality", "equality"), 
                      risk = c("MAD", "MiniMax", "CVaR", "CDaR", "EV", "LPM", "LPMUPM")[6],
                      options = list(threshold = 999, moment = 4),
                      riskType = c("minrisk", "optimal")[2],
                      LB = rep(0.025, mc), UB = ubx, budget = b)
    sol = parmasolve(pspec, type = "NLP")
    w = weights(sol)
    wts[inc] = w
    if(!is.na(eqd)){
      # add back those which did not have enough history to qualify
      # for optimization
      wts[eqd] = 0.025
      # rebalance total to 100%
      if(sum(wts)>1) wts = wts/sum(wts)
    }
  }
  save(wts, file=paste("wts_",i,".rda",sep=""))
  return(c(parmarisk(sol),parmareward(sol)))
}
cl = makePSOCKcluster(ncores)
clusterEvalQ(cl, library(rmgarch))
clusterEvalQ(cl, library(rugarch))
clusterEvalQ(cl, library(parma))
clusterEvalQ(cl, library(xts))
clusterExport(cl, c("Symbols.m", "xidx", "m", "f_opt","signalm","mxweight"))
clusterEvalQ(cl, source("customfn.R"))
clusterExport(cl, c(Symbols.m, "X"))
sol = parLapply(cl,1:n, function(i) f_opt(i))
stopCluster(cl)

riskreward = t(sapply(sol, function(x) x))
riskreward = xts(riskreward, as.POSIXct(xidx))
colnames(riskreward) = c("LPM1","Reward")
save(riskreward, file=paste("riskreward.rda",sep=""))


wtsx = matrix(0, ncol = m, nrow=n)
for(i in 1:n){
  tmp<-try(load(file=paste("wts_",i,".rda",sep="")))
  if(inherits(tmp, 'try-error')) next()
  if( !is.na(riskreward[i,2]/riskreward[i,1]) && riskreward[i,2]/riskreward[i,1]> 0.5 ) wtsx[i,] = wts else wtsx[i,] = wts/1
}
wts = wtsx
wts = xts(wts, as.Date(xidx))
colnames(wts) = Symbols

save(wts, file = "optimal_weights.rda")

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
# don't make use of risk stops but this can be used with risk_fun
# userargs$portlimit = -0.02

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
# need to detach rmgarch as there is a conflict with 'last' method on array
# for some reason
detach('package:parma')
detach('package:rmgarch')
.instrument <<- new.env()
.blotter <<- new.env()
portopt = execute_strategy_fun(Symbols, signal_fun = signal_fun, rebalance_fun = rebalance_fun, 
                           transaction_fun = transaction_fun, cash_fun = cash_fun, risk_fun = NULL,
                           start_date = "1994-01-01", end_date = "2013-05-01", 
                           init_equity = 1e6, port_name = "Port", account_name = "Account", userargs = userargs)

save(portopt, file="portopt.rda")