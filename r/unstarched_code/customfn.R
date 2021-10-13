Sys.setenv(TZ="GMT")
library(FinancialInstrument)
library(blotter)
library(quantmod)
library(quantstrat)
library(ellipse)
library(PerformanceAnalytics)
library(rmgarch)
library(parma)
library(parallel)
################################################################################
# macd custom filter to be optimized every period
emaoptfn = function(x, slow0, fast0)
{
  if(length(x)>0){
    
    if(nrow(x)>=100)
    {
      fast = 2:6
      slow = 3:12
      ratio = seq(0.05, 0.95, by = 0.05)
      parms = expand.grid(fast, slow)
      idx  = which(parms[,1]>parms[,2])
      parms = parms[-idx,]
      idx  = which(parms[,1]==parms[,2])
      parms = parms[-idx,]
      pfunc = function(X, fast, slow, ratio)
      {
        Y = cbind(X, EMA(X, fast), EMA(X, slow))
        Y = na.omit(Y)
        I = xts(as.integer(Y[,2]>Y[,3]), index(Y))
        R = cbind(ROC(Y[,1], type="discrete"), lag(I,1))
        R = na.omit(R)
        port = sum(R[,1]*R[,2])/sd(R[,1]*R[,2])
        return(port)
      }
      maxp = apply(parms, 1, function(z) pfunc(x, z[1],z[2]))
      sol = as.numeric(tail(parms[which(maxp==max(maxp)),],1))
      slow = sol[2]
      fast = sol[1]
      Yslow = EMA(x, slow)
      Yfast = EMA(x, fast)
      Y = cbind(Yslow, Yfast)
      I = as.integer(Y[,1]<Y[,2])
    } else if(nrow(x)>slow0 && nrow(x)<100){
      Yslow = EMA(x, slow0)
      Yfast = EMA(x, fast0)
      Y = cbind(Yslow, Yfast)
      I = as.integer(Y[,1]<Y[,2])
      slow = slow0
      fast = fast0
    } else{
      I = 0
      slow = slow0
      fast = fast0
    }
  } else{
    I = 0
    slow = slow0
    fast = fast0
  }
  return(list(I = tail(I, 1), slow = slow, fast = fast))
}
#-------------------------------------------------------------------------------
.getFamaFrench = function(enddate = Sys.Date()){
  require(xts)
  ########################################################################
  # Small program to fetch and organize Fama-French factor data.
  # The idea is to make a table that could be used for SQL merges.
  ########################################################################
  # The URL for the data.
  ff.url <-"http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_daily.zip"
  # Function to remove leading and trailing spaces from a string
  ################################################################################
  #             First download Fama-French three-factor data                     #
  ################################################################################
  f <- tempfile()
  download.file(ff.url, f)
  file.list <- unzip(f, list=TRUE)
  ff_daily_factors <- read.fwf(unzip(f, files=as.character(file.list[1,1])), 
                               widths=c(8,8,8,8,10), header=FALSE,stringsAsFactors=FALSE, skip=5)
  # Clean the data
  n = dim(ff_daily_factors)
  ff_daily_factors = ff_daily_factors[-c(n-1, n), ]
  for (i in 2:5) ff_daily_factors[,i] <- na.omit(as.numeric(ff_daily_factors[,i]))
  for (i in 2:5) ff_daily_factors[,i] <- ff_daily_factors[,i]/100
  names(ff_daily_factors) <- c("date", "mktrf", "smb", "hml", "rf")
  ff_daily_factors$date <- as.Date(ff_daily_factors$date, format="%Y%m%d")
  ################################################################################
  #               Now download UMD (momentum) factor data                        #
  ################################################################################
  # Download the data and unzip it
  ff.url <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Momentum_Factor_daily.zip"
  f <- tempfile()
  download.file(ff.url, f)
  file.list <- unzip(f, list=TRUE)
  # Parse the data
  ff_mom_factor <- read.fwf(unzip(f, files=as.character(file.list[1,1])), 
                            widths=c(8,8), header=FALSE, stringsAsFactors=FALSE, skip=14)
  n = dim(ff_mom_factor)
  ff_mom_factor = ff_mom_factor[-c(n-1, n), ]
  # Clean the data
  ff_mom_factor[,2] <- na.omit(as.numeric(ff_mom_factor[,2]))/100
  names(ff_mom_factor) <- c("date", "umd")
  ff_mom_factor$date <- as.Date(ff_mom_factor$date, format="%Y%m%d")
  ################################################################################
  #                        Merge all the factor data                             #
  ################################################################################
  ff_daily_factors <- merge(ff_daily_factors, ff_mom_factor, by="date", all.x=TRUE)
  ff_daily_factors <- subset(ff_daily_factors, subset=!is.na(date))
  ff_daily_factors = as.xts(as.matrix(ff_daily_factors[,-1]), as.Date(ff_daily_factors[,1]))
  xt = time(ff_daily_factors)
  if(!is.finite(z<-min(which(xt>as.Date(enddate))))){
    dx = paste(as.character(tail(xt, 1)), enddate, "d", sep = "/")
    y  = timeBasedSeq(dx, retclass = "Date")
    y  = y[-1] 
    n = length(y)
    fx = matrix(tail(ff_daily_factors, 1), n, NCOL(ff_daily_factors), byrow = TRUE)
    fx[,c(1:3,5)] = 0
    fx = xts(fx, y)
    colnames(fx) = c( "mktrf", "smb", "hml", "rf", "umd")
    ff_daily_factors = rbind(ff_daily_factors, fx)
  } else{
    ff_daily_factors = ff_daily_factors[1:z,]
  }
  ################################################################################
  return(ff_daily_factors)
}
#-------------------------------------------------------------------------------
r_eparse2 = function(x){
  eval(parse(text = x), envir = .GlobalEnv)
}
#-------------------------------------------------------------------------------
align_interp = function(Symbols){
  m = length(Symbols)
  idx = NULL
  for(i in 1:m){
    idx = c(idx, as.character(time(get(Symbols[i]))))
  }
  idx = sort(unique(idx))
  tmp = as.xts(rep(1, length(idx)), as.POSIXct(idx))
  colnames(tmp) = "Extra"
  for(i in 1:m){
    xtmp = cbind(get(Symbols[i]), tmp)
    xtmp = xtmp[,-NCOL(xtmp)]
    xtmp  = na.locf(xtmp)
    xidx = min(which(!is.na(xtmp)))-1
    if(xidx>0) xtmp = xtmp[-c(1:xidx),]
    assign(Symbols[i], xtmp, envir = globalenv())
    rm(xtmp)
  }
  return(invisible(1))
}
#-------------------------------------------------------------------------------
pmind = function(x){
  z = x
  z[which(x==TRUE),1] = 1
  z[which(x==FALSE),1] = -1
  return(z)
}
#-------------------------------------------------------------------------------
custom_10colors = function(){
  z = colors()
  x = rep(0, 12)
  x[1] = z[175]
  x[2] = z[326]
  x[3] = z[553]
  x[4] = z[53]
  x[5] = z[614]
  x[6] = z[471]
  x[7] = z[565]
  x[8] = z[115]
  x[9] = z[624]
  x[10] = z[259]
  x[11] = z[149]
  x[12] = z[643]
  return(x)
}
#-------------------------------------------------------------------------------
getWeights = function(Account, Port, Dates){
  # based on end of start of month weighting
  TValue = Account$summary$End.Eq
  n = length(Dates)
  D = time(TValue)
  m = length(Port$symbols)
  cnames = names(Port$symbols)
  TDValue = rep(NA, n)
  idx = rep(NA, n)
  for(i in 1:n){
    idx[i] = try(min(which(D>Dates[i])), silent=TRUE)
    if(is.finite(idx[i])){
      TDValue[i] = TValue[idx[i]]
    } else{
      idx[i] = NA
    }
  }
  TDValue = xts(na.omit(TDValue), D[na.omit(idx)])
  n = length(TDValue)
  V = matrix(NA, ncol = m, nrow = n)
  for(i in 1:n){
    V[i,] = sapply(Port$symbols, FUN = function(x) zero_length(x$posPL$Pos.Value[time(TDValue)[i]], 0))
  }
  w = matrix(NA, ncol = m, nrow = n)
  for(i in 1:n){
    w[i,] = as.numeric(V[i,])/as.numeric(TDValue[i])
  }
  colnames(w) = cnames
  rownames(w) = as.character(time(TDValue))
  return(w)
}
#-------------------------------------------------------------------------------
updateAcct2 = function (name = "default", Dates = NULL) 
{
  Account <- getAccount(name)
  if (!is.null(attr(Account, "currency"))) {
    a.ccy.str <- attr(Account, "currency")
  }
  Portfolios = names(Account$portfolios)
  
  if(is.null(Dates)) Dates <- index(getPortfolio(Portfolios[1])$summary)
  
  #trim to only time prior to Dates
  if(last(index(Account$summary)) > .parseISO8601(Dates)$first.time) {
    whichi <- first(Account$summary[paste(.parseISO8601(Dates)$first.time, "::", sep = ""), which.i = TRUE])
    if (!is.null(whichi)) whichi = whichi - 1
    if (whichi < 1) whichi = 1
    Account$summary = Account$summary[1:whichi, ]
  }
  # Append the portfolio summary data to the portfolio slot
  for (pname in Portfolios) {
    Portfolio = getPortfolio(pname)
    if (!is.null(attr(Portfolio, "currency"))) {
      p.ccy.str <- attr(Portfolio, "currency")
    }
    # Test whether portfolio and account are of the same ccy
    psummary = Portfolio$summary[Dates]
    if (a.ccy.str != p.ccy.str) {
      # If not, translate the portfolio summary to the account currency
      CcyMult <- NA
      port_currency <- try(getInstrument(p.ccy.str), silent = TRUE)
      if (inherits(port_currency, "try-error") | !is.instrument(port_currency)) {
        warning("Currency", p.ccy.str, " not found, using currency multiplier of 1")
        CcyMult <- 1
      }
      else {
        FXrate.str <- paste(p.ccy.str, a.ccy.str, sep = "")
        FXrate <- try(get(FXrate.str), silent = TRUE)
        #TODO FIXME: this uses convention to sort out the rate, we should check $currency and $counter_currency and make sure directionality is correct 
        if (inherits(FXrate, "try-error")) {
          FXrate.str <- paste(a.ccy.str, p.ccy.str, sep = "")
          FXrate <- try(get(FXrate.str), silent = TRUE)
          if (inherits(FXrate, "try-error")) {
            warning("Exchange Rate", FXrate.str, " not found for symbol,',Symbol,' using currency multiplier of 1")
            CcyMult <- 1
          }
          else {
            invert = TRUE
          }
        }
      }
      if (is.na(CcyMult) && !is.na(FXrate)) {
        if (inherits(FXrate, "xts")) {
          CcyMult <- FXrate[Dates]
          CcyMult <- na.locf(merge(CcyMult, index(psummary)))
          CcyMult <- drop(CcyMult[index(psummary)])
        }
        else {
          CcyMult <- as.numeric(FXrate)
        }
      }
      else {
        CcyMult <- 1
      }
      if (isTRUE(invert)) {
        CcyMult <- 1/CcyMult
      }
      psummary <- psummary * CcyMult
    }
    Account$portfolios[[pname]] = rbind(Account$portfolios[[pname]], psummary)
  }
  summary = NULL
  # get the dimensions we need to work with 
  ## TODO Find more efficient way to establish dimensions of the result
  table = blotter:::.getByPortf(Account, "Net.Trading.PL", Dates)
  obsLength = length(index(table))
  obsDates = index(table)
  Attributes = c("Additions", "Withdrawals", "Realized.PL", 
                 "Unrealized.PL", "Int.Income", "Gross.Trading.PL", "Txn.Fees", 
                 "Net.Trading.PL", "Advisory.Fees", "Net.Performance", 
                 "End.Eq")
  for(Attribute in Attributes) {
    switch(Attribute,
           Realized.PL = ,
           Unrealized.PL = ,
           Gross.Trading.PL = ,
           Txn.Fees = ,
           Net.Trading.PL = {
             table = blotter:::.getByPortf(Account, Attribute, Dates)
             result = xts(rowSums(table,na.rm=TRUE),order.by=index(table))
           },
           Additions = , 
           Withdrawals = , 
           Int.Income = ,
           Advisory.Fees = ,
           Net.Performance = ,
           End.Eq = { 
             ## TODO no cash handling for now, add this in later, but for now, zeroes 
             result = xts(rep(0,obsLength),order.by=obsDates)
           }
    )
    colnames(result) = Attribute
    if(is.null(summary)) {summary=result}
    else {summary=cbind(summary,result)}
  }
  # Need to check whether Account was already altered in which case 
  # you add the results NOT bind them (else you'll get duplicate dates and problems
  # when calling the Ending Equity Update function.
  if(time(summary)[1] == tail(time(Account$summary),1)){
    Account$summary[nrow(Account$summary), ] <- Account$summary[nrow(Account$summary),] +  colSums(summary)
  } else{
    Account$summary <- rbind(Account$summary, summary)
  }
  assign(paste("account", name, sep = "."), Account, envir = .blotter)
  return(name)
}
#-------------------------------------------------------------------------------
tradeStats2 = function (Portfolios, Symbols, date_range = NULL)
{
  if(is.null(date_range)) dt = "/" else dt = date_range
  ret <- NULL
  for (Portfolio in Portfolios) {
    pname <- Portfolio
    if (!grepl("portfolio\\.", pname)) 
      Portfolio <- try(get(paste("portfolio", pname, sep = "."), 
                           envir = .blotter), silent = TRUE)
    else Portfolio <- try(get(pname, envir = .blotter), silent = TRUE)
    if (inherits(Portfolio, "try-error")) 
      stop(paste("Portfolio", pname, " not found, use initPortf() to create a new portfolio"))
    if (!inherits(Portfolio, "portfolio")) 
      stop("Portfolio", pname, "passed is not the name of a portfolio object.")
    if (missing(Symbols)) symbols <- names(Portfolio$symbols) else symbols <- Symbols
    for (symbol in symbols) {
      txn <- Portfolio$symbols[[symbol]]$txn[dt]
      posPL <- Portfolio$symbols[[symbol]]$posPL[dt]
      posPL <- posPL[-1, ]
      PL.gt0 <- txn$Net.Txn.Realized.PL[txn$Net.Txn.Realized.PL > 0]
      PL.lt0 <- txn$Net.Txn.Realized.PL[txn$Net.Txn.Realized.PL < 0]
      PL.ne0 <- txn$Net.Txn.Realized.PL[txn$Net.Txn.Realized.PL != 0]
      if (!nrow(PL.ne0)) next()
      GrossProfits <- sum(PL.gt0)
      GrossLosses <- sum(PL.lt0)
      ProfitFactor <- abs(GrossProfits/GrossLosses)
      AvgTradePL <- mean(PL.ne0)
      MedTradePL <- median(PL.ne0)
      StdTradePL <- sd(as.numeric(as.vector(PL.ne0)))
      NumberOfTxns <- nrow(txn) - 1
      NumberOfTrades <- nrow(PL.ne0)
      PercentPositive <- (nrow(PL.gt0)/nrow(PL.ne0)) * 100
      PercentNegative <- (nrow(PL.lt0)/nrow(PL.ne0)) * 100
      MaxWin <- max(txn$Net.Txn.Realized.PL)
      MaxLoss <- min(txn$Net.Txn.Realized.PL)
      AvgWinTrade <- mean(PL.gt0)
      MedWinTrade <- median(PL.gt0)
      AvgLossTrade <- mean(PL.lt0)
      MedLossTrade <- median(PL.lt0)
      AvgWinLoss <- AvgWinTrade/-AvgLossTrade
      MedWinLoss <- MedWinTrade/-MedLossTrade
      DailyPL <- apply.daily(PL.ne0, sum)
      AvgDailyPL <- mean(DailyPL)
      MedDailyPL <- median(DailyPL)
      StdDailyPL <- sd(as.numeric(as.vector(DailyPL)))
      Equity <- cumsum(posPL$Net.Trading.PL)
      if (!nrow(Equity)) {
        warning("No Equity rows for", symbol)
        next()
      }
      TotalNetProfit <- last(Equity)
      if (is.na(TotalNetProfit)) {
        warning("TotalNetProfit NA for", symbol)
        next()
      }
      Equity.max <- cummax(Equity)
      maxEquity <- max(Equity)
      minEquity <- min(Equity)
      endEquity <- last(Equity)
      if (endEquity != TotalNetProfit && last(txn$Pos.Qty) == 
            0) {
        warning("Total Net Profit for", symbol, "from transactions", 
                TotalNetProfit, "and cumulative P&L from the Equity Curve", 
                endEquity, "do not match. This can happen in long/short portfolios.")
        message("Total Net Profit for", symbol, "from transactions", 
                TotalNetProfit, "and cumulative P&L from the Equity Curve", 
                endEquity, "do not match. This can happen in long/short portfolios.")
      }
      MaxDrawdown <- -max(Equity.max - Equity)
      ProfitToMaxDraw <- -TotalNetProfit/MaxDrawdown
      tmpret <- data.frame(Portfolio = pname, Symbol = symbol, 
                           Num.Txns = NumberOfTxns, Num.Trades = NumberOfTrades, 
                           Total.Net.Profit = TotalNetProfit, Avg.Trade.PL = AvgTradePL, 
                           Med.Trade.PL = MedTradePL, Largest.Winner = MaxWin, 
                           Largest.Loser = MaxLoss, Gross.Profits = GrossProfits, 
                           Gross.Losses = GrossLosses, Std.Dev.Trade.PL = StdTradePL, 
                           Percent.Positive = PercentPositive, Percent.Negative = PercentNegative, 
                           Profit.Factor = ProfitFactor, Avg.Win.Trade = AvgWinTrade, 
                           Med.Win.Trade = MedWinTrade, Avg.Losing.Trade = AvgLossTrade, 
                           Med.Losing.Trade = MedLossTrade, Avg.Daily.PL = AvgDailyPL, 
                           Med.Daily.PL = MedDailyPL, Std.Dev.Daily.PL = StdDailyPL, 
                           Max.Drawdown = MaxDrawdown, Profit.to.Max.Draw = ProfitToMaxDraw, 
                           Avg.WinLoss.Ratio = AvgWinLoss, Med.WinLoss.Ratio = MedWinLoss, 
                           Max.Equity = maxEquity, Min.Equity = minEquity, 
                           End.Equity = endEquity)
      rownames(tmpret) <- symbol
      ret <- rbind(ret, tmpret)
    }
  }
  return(ret)
}
#-------------------------------------------------------------------------------
na2zero = function(x){
  z = x
  z[which(is.na(x))] = 0
  z[which(!is.finite(x))] = 0
  return(z)
}
#-------------------------------------------------------------------------------
icontribution = function(Symbols, Account, Port, Dates = "/")
{
  if (missing(Symbols)) symbols <- names(Port$symbols) else symbols <- Symbols
  A = Account$summary$End.Eq
  initEq = attr(Account, "initEq")
  if(Dates == "/"){
    T_Gain = as.numeric(tail(A[Dates],1))  - initEq
  } else{
    T_Gain = as.numeric(tail(A[Dates],1))  - as.numeric(A[Dates][1])
  }
  contribution = rep(0, length(Symbols))
  for(i in 1:length(Symbols)){
    posPL <- Port$symbols[[Symbols[i]]]$posPL[Dates]
    Equity <- colSums(posPL$Net.Trading.PL)
    contribution[i] = na2zero(Equity/T_Gain)
  }
  names(contribution) = Symbols
  contribution[is.na(contribution)]=0
  
  Interest = sum(Account$summary$Int.Income[Dates])/T_Gain
  MgtFees = sum(Account$summary$Withdrawals[Dates])/T_Gain
  TxnFees = sum(Account$summary$Txn.Fees[Dates])/T_Gain
  contribution = c(contribution, "Interest" = Interest, "Fees" = MgtFees, "TxnCosts" = TxnFees)
  return(contribution)
}
#-------------------------------------------------------------------------------
portfolio_stats = function(x, rf, scale = 252, Dates = "/"){
  x = x[Dates]
  if(is.xts(rf)) rf = rf[Dates] else rf = xts(rep(rf, nrow(x)), time(x))
  amean = as.numeric(Return.annualized(x, scale = scale, geometric = TRUE))
  asd = sd(as.numeric(x)) * sqrt(scale)
  b = cbind(x, rf)
  b = na.omit(b)
  as = as.numeric(SharpeRatio.annualized(b[,1], Rf = b[,2], scale = scale, geometric=FALSE))
  Z = cumprod(1+x)
  if(scale == 252){
    MZ = ROC(to.monthly(Z, indexAt = "endof", OHLC=FALSE), type = "discrete")
    MZ = na.omit(MZ)
    bestmonth = max(MZ, na.rm = TRUE)
    worstmonth = min(MZ, na.rm = TRUE)
    upmonth = sum(as.integer(MZ>=0))/nrow(MZ)
    dnmonth = sum(as.integer(MZ<0))/nrow(MZ)
  } else if(scale == 12){
    bestmonth = max(x, na.rm = TRUE)
    worstmonth = min(x, na.rm = TRUE)
    upmonth = sum(as.integer(x>=0))/nrow(x)
    dnmonth = sum(as.integer(x<0))/nrow(x)
  } else{
    stop("\nscale not supported")
  }
  mx = maxDrawdown(x)
  tbl = matrix(NA, ncol = 1, nrow = 8)
  tbl[1,1] = amean*100
  tbl[2,1] = asd*100
  tbl[3,1] = as
  tbl[4,1] = bestmonth*100
  tbl[5,1] = worstmonth*100
  tbl[6,1] = upmonth*100
  tbl[7,1] = dnmonth*100
  tbl[8,1] = mx*100
  rownames(tbl) = c("CAGR", "Vol", "Sharpe", "BestM", "WorstM", "UpM", "DnM", "MaxDraw")
  return(tbl)
}
#-------------------------------------------------------------------------------
eval_portfolio = function(Account, Benchmark, RiskFree, startyear, subyears = NULL, Names){
  if(!is.null(subyears)){
    n1 = length(subyears)%%3
    n2 = 3
  } else{
    n1 = 1
    n2 = 1
  }
  R = ROC(Account$Account$summary$End.Eq, type = "discrete")[-1]
  par(mfrow = c(n1,n2))
  xy = xy.coords(.index(R[startyear]), R[startyear,1])
  plot(cumprod(1+R[startyear]), main = "Strategy", ylab = "TW",
       axes = FALSE, type = "l", col = "steelblue", lwd=3, auto.grid = FALSE)
  axis(2)
  Y = (which(diff(as.integer(format(time(R[startyear]),"%Y")))!=0))
  axis(1, at = xy$x[Y], labels = as.character(format(time(R[startyear]),"%Y")[Y]))
  lines(cumprod(1+Benchmark[startyear]), col="tomato1", lty=2)
  legend("topleft", c("Strategy", "Benchmark"), 
         col = c("steelblue", "tomato1"), bty="n", lty=c(1, 2), lwd = c(3, 2), cex = 0.6)
  if(!is.null(subyears)){
    for(i in subyears){
      xy = xy.coords(.index(R[i]), R[i,1])
      plot(cumprod(1+R[i]), main = "Strategy", ylab = "TW",
           axes = FALSE, type = "l", col = "steelblue", lwd=3, auto.grid = FALSE)
      axis(2)
      Y = (which(diff(as.integer(format(time(R[i]),"%Y")))!=0))
      axis(1, at = xy$x[Y], labels = as.character(format(time(R[i]),"%Y")[Y]))
      lines(cumprod(1+Benchmark[i]), col="tomato1", lty=2)
      legend("topleft", c("Strategy", "Benchmark"), 
             col = c("steelblue", "tomato1"), bty="n", lty=c(1, 2), lwd = c(3, 2), cex = 0.6)
    }
  }
  dev.new()
  S <- names(Account$Port$symbols)
  m = length(S)
  longest = sapply(S, FUN = function(x) NROW(get(x, envir = globalenv())))
  longest = which(longest == max(longest))
  longsymbol = S[longest]
  w = getWeights(Account$Account, Account$Port, as.POSIXct(time(to.monthly(Cl(get(longsymbol)),indexAt='endof'))))
  n = NROW(w)
  exd = seq(2,n,by=2)
  yw = apply.yearly(w, "mean")
  yw = as.xts(yw)	
  par(xpd=T, mar=par()$mar+c(4,0,0,0))
  barplot(t(as.matrix(yw[startyear]))[,-1], col = rainbow(m, alpha = 0.6), names.arg = format(time(yw[startyear]), "%Y")[-1])
  title("Average Yearly Asset Weighting")
  legend(-0.5, -0.1, Names, col = rainbow(m, alpha=0.6), fill = rainbow(m, alpha=0.6), bty="n", cex = 1, ncol = 4)
  par(mar=c(5, 4, 4, 2) + 0.1)
  dev.new()
  Z = cumprod(1+R)
  MZ = ROC(to.monthly(Z, indexAt = "endof", OHLC=FALSE), type = "discrete")
  names(MZ) = "Total"
  tb = table.CalendarReturns(MZ[startyear], geometric = TRUE)
  tb = cbind(data.frame(year = rownames(tb)), tb)
  dprint(data = tb, fit.width = TRUE, center = FALSE, 
         fit.height  = TRUE, main = "Calendar Returns", 
         row.hl = row.hl(dx = seq(1,nrow(tb), by = 2), col = "lightgrey"), label = "year")	
  CBs <- style(frmt.bdy=frmt(fontfamily="HersheySans"), frmt.tbl=frmt(bty="o", lwd=1),
               frmt.col=frmt(fontfamily="HersheySans", bg="lightgrey", fontface="bold", lwd=2, bty="_"),
               frmt.grp=frmt(fontfamily="HersheySans", bg="lightgrey", fontface="bold"),
               frmt.main=frmt(fontfamily="HersheySans", fontface="bold", fontsize=16),
               frmt.ftn=frmt(fontfamily="HersheySans"),
               justify="left", cex = 1.4)
  ps = portfolio_stats(R, rf = RiskFree, scale = 252, Dates = startyear)
  ps = data.frame(N = rownames(ps), Stats = round(ps[,1],2))
  dprint(data = ps, fit.width = FALSE, center = FALSE, style = CBs, main = "", 
         row.hl = row.hl(dx = seq(1,nrow(tb), by = 2), col = "lightgrey"), label = "N",
         margins = c(4,4,4,4))
  return(ps)
}
#-------------------------------------------------------------------------------
eomidx = function(X){
  d = as.Date(time(X))
  d = as.integer(format(d, "%m"))
  idx = which(diff(d)>0)
  return(idx)
}
#-------------------------------------------------------------------------------
cump = function(x){ cumprod(1+x) }
#-------------------------------------------------------------------------------
strategy_stats = function(x, rf, benchmark, scale = 252, Dates = "/"){
  x = x[Dates]
  if(is.xts(rf)) rf = rf[Dates] else rf = xts(rep(rf, nrow(x)), time(x))
  if(!is.null(benchmark)) benchmark = benchmark[Dates]
  amean = as.numeric(Return.annualized(x, scale = scale, geometric = TRUE))
  if(!is.null(benchmark)) ameanb =  as.numeric(Return.annualized(benchmark, scale = scale, geometric = TRUE)) else ameanb = NA
  
  asd = sd(as.numeric(x)) * sqrt(scale)
  if(!is.null(benchmark)) asdb = sd(as.numeric(benchmark)) * sqrt(scale) else asdb = NA
  b = cbind(x, rf)
  b = na.omit(b)
  as = as.numeric(SharpeRatio.annualized(b[,1], Rf = b[,2], scale = scale, geometric=FALSE))
  Z = cumprod(1+x)
  if(!is.null(benchmark)){
    bb = cbind(benchmark, rf)
    bb = na.omit(bb)
    asb = as.numeric(SharpeRatio.annualized(bb[,1], Rf = bb[,2], scale = scale, geometric=FALSE))
    Zb = cumprod(1+benchmark)
    if(scale == 252){
      MZb = ROC(to.monthly(Zb, indexAt = "endof", OHLC=FALSE), type = "discrete")
      MZb = na.omit(MZb)
      bestmonthb = max(MZb, na.rm = TRUE)
      worstmonthb = min(MZb, na.rm = TRUE)
      upmonthb = sum(as.integer(MZb>=0))/nrow(MZb)
      dnmonthb = sum(as.integer(MZb<0))/nrow(MZb)
    } else if(scale == 12){
      bestmonthb = max(benchmark, na.rm = TRUE)
      worstmonthb = min(benchmark, na.rm = TRUE)
      upmonthb = sum(as.integer(benchmark>=0))/nrow(benchmark)
      dnmonthb = sum(as.integer(benchmark<0))/nrow(benchmark)
    } else{
      stop("\nscale not supported")
    }
  } else{
    asb = NA
    bestmonthb = worstmonthb = upmonthb = dnmonthb = NA
  }
  if(scale == 252){
    MZ = ROC(to.monthly(Z, indexAt = "endof", OHLC=FALSE), type = "discrete")
    MZ = na.omit(MZ)
    bestmonth = max(MZ, na.rm = TRUE)
    worstmonth = min(MZ, na.rm = TRUE)
    upmonth = sum(as.integer(MZ>=0))/nrow(MZ)
    dnmonth = sum(as.integer(MZ<0))/nrow(MZ)
  } else if(scale == 12){
    bestmonth = max(x, na.rm = TRUE)
    worstmonth = min(x, na.rm = TRUE)
    upmonth = sum(as.integer(x>=0))/nrow(x)
    dnmonth = sum(as.integer(x<0))/nrow(x)
  } else{
    stop("\nscale not supported")
  }
  mx = maxDrawdown(x)
  if(!is.null(benchmark)) mxb = maxDrawdown(benchmark) else mxb = NA
  if(!is.null(benchmark)){
    tbl = matrix(NA, ncol = 2, nrow = 8)
    tbl[1,1] = amean*100
    tbl[2,1] = asd*100
    tbl[3,1] = as
    tbl[4,1] = bestmonth*100
    tbl[5,1] = worstmonth*100
    tbl[6,1] = upmonth*100
    tbl[7,1] = dnmonth*100
    tbl[8,1] = mx*100
    
    tbl[1,2] = ameanb*100
    tbl[2,2] = asdb*100
    tbl[3,2] = asb
    tbl[4,2] = bestmonthb*100
    tbl[5,2] = worstmonthb*100
    tbl[6,2] = upmonthb*100
    tbl[7,2] = dnmonthb*100
    tbl[8,2] = mxb*100
    rownames(tbl) = c("CAGR", "Vol", "Sharpe", "BestM", "WorstM", "UpM", "DnM", "MaxDraw")
    Ra = x
    colnames(Ra) = "Ra"
    Rb = benchmark
    colnames(Rb) = "Rb"
    Rf = rf
    colnames(Rf) = "Rf"
    BB = cbind(Ra, Rb, Rf)
    BB = na.omit(BB)
    ans = table.CAPM(Ra = BB[,1], Rb = BB[,2], scale = scale, Rf = BB[,3],  digits = 4)
    ans = ans[-1, ,drop = FALSE]
    ans[5,1] = ans[5,1]*100
    ans = as.matrix(ans)
    ans = cbind(ans, matrix(NA, nrow = 11))
    tbl = rbind(tbl, ans)
    colnames(tbl) = c("Strategy", "Benchmark")
  } else{
    tbl = matrix(NA, ncol = 1, nrow = 8)
    tbl[1,1] = amean*100
    tbl[2,1] = asd*100
    tbl[3,1] = as
    tbl[4,1] = bestmonth*100
    tbl[5,1] = worstmonth*100
    tbl[6,1] = upmonth*100
    tbl[7,1] = dnmonth*100
    tbl[8,1] = mx*100
    rownames(tbl) = c("CAGR", "Vol", "Sharpe", "BestM", "WorstM", "UpM", "DnM", "MaxDraw")
    if(!is.null(benchmark)){
      Ra = x
      colnames(Ra) = "Ra"
      Rb = benchmark
      colnames(Rb) = "Rb"
      Rf = rf
      colnames(Rf) = "Rf"
      BB = cbind(Ra, Rb, Rf)
      BB = na.omit(BB)
      ans = table.CAPM(Ra = BB[,1], Rb = BB[,2], scale = scale, Rf = BB[,3],  digits = 4)
    }
    ans = ans[-1, ,drop = FALSE]
    ans[5,1] = ans[5,1]*100
    tbl = rbind(tbl, as.matrix(ans))
    colnames(tbl) = "Strategy"
  }
  return(tbl)
}
#-------------------------------------------------------------------------------
zero_length = function(x, pad = NA){
  ifelse(length(x) == 0, pad, x)
}
#-------------------------------------------------------------------------------
symbol_try = function(fun, ...){
  ans = try(fun(...), silent = TRUE)
  if(inherits(ans, "try-error")) ans = 0
  return(ans)
}
#-------------------------------------------------------------------------------
has_data = function(Symbol, Date){
  tt = time(get(Symbol, envir = globalenv()))
  if(is(tt)[1] == "POSIXct"){
    idx = zero_length(which(tt == as.POSIXct(Date)))
  } else if(is(tt)[1] == "Date"){
    idx = zero_length(which(tt == as.Date(Date)))
  } else{
    stop("\nDate format not recognized")
  }
  return(idx)
}
#-------------------------------------------------------------------------------
create_account_entry = function(Account, CurrentDate)
{
  cD = as.Date(CurrentDate)
  nm = c("Additions", "Withdrawals", "Realized.PL", 
         "Unrealized.PL", "Int.Income", "Gross.Trading.PL",
         "Txn.Fees", "Net.Trading.PL", "Advisory.Fees",
         "Net.Performance", "End.Eq")
  if(length(Account$summary[cD,])==0){
    Anext = xts(matrix(0, ncol = 11), cD)
    colnames(Anext) = nm
    Aeq = as.numeric(tail(Account$summary[,"End.Eq"], 1))
    Account$summary = rbind(Account$summary, Anext)
    Account$summary[cD, "End.Eq"] = Aeq
  }
  return(Account)
}
#-------------------------------------------------------------------------------
get_cash = function(Symbols, CurrentDate, account_name, portfolio_name)
{
  Eq = getEndEq(Account = account_name, CurrentDate)
  cur_qty = sapply(Symbols, FUN = function(s) symbol_try(fun = getPosQty, Portfolio = port_name, Symbol = s, Date = CurrentDate))
  current_price = sapply(Symbols, FUN = function(x) zero_length(get(x, envir = globalenv())[CurrentDate]))	
  cash = Eq - sum(cur_qty * current_price, na.rm = TRUE)
  return(cash)
}
#-------------------------------------------------------------------------------
# execute a strategy using a function to evaluate the signals, rebalancing and txn costs
execute_strategy_fun = function(Symbols, signal_fun, rebalance_fun, transaction_fun, 
                                cash_fun = NULL, risk_fun = NULL, init_equity = 1e6, 
                                start_date = NULL, end_date = NULL, 
                                port_name = "Port", account_name = "Account", 
                                userargs = list(), verbose = TRUE, ...)
{
  longest = sapply(Symbols, FUN = function(x) NROW(get(x, envir = globalenv())))
  longest = which(longest == max(longest))
  longsymbol = get(Symbols[longest], envir = globalenv())
  n = NROW(longsymbol)
  allDates = time(longsymbol)
  t_pos = NULL
  if(is.null(start_date)){
    start = 1
  } else{
    start = ifelse(!is.finite(min(which(allDates > as.Date(start_date)))), 1, min(which(allDates > as.Date(start_date))))
  }
  initPortf(name = port_name, symbols=Symbols, initDate=allDates[start])
  initAcct(name = account_name, portfolios =  port_name, initDate = allDates[start], 
           initEq = init_equity)
  
  # Assign Risk Management Ledger
  Ac = get(paste("account.", account_name, sep = ""), envir = .blotter)
  Ac$Risk = xts(x = 0, as.POSIXct(allDates[1]))
  colnames(Ac$Risk) = "RiskReduction"
  assign(paste("account.", account_name, sep = ""), Ac, envir = .blotter)
  
  # check in case the actual last available date is used for end_date
  # i = i+1
  if(is.null(end_date)){
    n = length(allDates)
  } else{
    n = ifelse(!is.finite(min(which(allDates > as.Date(end_date)))), n, min(which(allDates > as.Date(end_date))))
  }
  traded = 0
  for(i in start:n){
    CurrentDate = allDates[i]
    withdraw = 0
    dDate = which(allDates == CurrentDate)
    prevD = allDates[dDate-1]
    cur_qty = sapply(Symbols, FUN = function(s) getPosQty(Portfolio = port_name, Symbol = s, Date = CurrentDate))
    # signal check based on previous date (no contemporaneous signal/trading)
    cur_signal = signal_fun(Symbols, prevD, account_name, port_name, userargs)
    if(i>start) cur_equity = getEndEq(Account = account_name, prevD) else cur_equity = init_equity
    current_price = sapply(Symbols, FUN = function(x) zero_length(Cl(get(x, envir = globalenv())[prevD,])))		
    cur_cash = cur_equity - sum(cur_qty * current_price, na.rm = TRUE)
    if(!is.null(cash_fun)){
      cin = cash_fun(Symbols, CurrentDate, account_name, port_name, userargs, prevD = prevD, cur_cash = cur_cash)
      Ac = get(paste("account.", account_name, sep = ""), envir = .blotter)
      Ac = create_account_entry(Ac, CurrentDate)
      Ac$summary[CurrentDate, 'Int.Income'] = cin
      assign(paste("account.", account_name, sep = ""), Ac, envir = .blotter)
    } else{
      Ac = get(paste("account.", account_name, sep = ""), envir = .blotter)
      Ac = create_account_entry(Ac, CurrentDate)
      assign(paste("account.", account_name, sep = ""), Ac, envir = .blotter)
    }
    idx = NULL
    if(any(cur_qty>0)){
      # Case 1: We are in trade
      s_pos = NULL
      t_pos = which(cur_qty>0)
      # check whether we need to trade
      if(any(cur_signal!=0)){
        s_pos = which(cur_signal!=0)
        new_pos = rebalance_fun(Symbols, CurrentDate, account_name, port_name, userargs, signals = cur_signal, 
                                prevD = prevD, cur_equity  = cur_equity)
        # buy/sell
        idx = which(new_pos!=0)
        if(length(idx)>0){
          t_cost = transaction_fun(Symbols, CurrentDate, account_name, port_name, userargs, idx = idx, prevD = prevD, 
                                   cur_equity = cur_equity)
          withdraw = t_cost$fee
          tcost = t_cost$tcost
          for(j in 1:length(idx)){
            x = get(Symbols[idx[j]], envir = globalenv())
            # need to account for withdrawals
            addTxn(port_name, Symbol = Symbols[idx[j]], TxnDate  = CurrentDate, TxnQty = as.numeric(new_pos[idx[j]]), 
                   TxnPrice = as.numeric(Cl(x[CurrentDate,])), TxnFees = -tcost[idx[j]], verbose = verbose, eps = 1e-06)
          }
        }
        traded = 1
      }
      t_pos = unique(c(t_pos, s_pos))
    } else{
      # Case 2: We are not in trade
      t_pos = s_pos = NULL
      if(any(cur_signal>0)){
        # check whether we need to trade
        s_pos = which(cur_signal!=0)
        new_pos = rebalance_fun(Symbols, CurrentDate, account_name, port_name, userargs, 
                                signals = cur_signal, prevD = prevD, cur_equity = cur_equity)
        # buy/sell
        idx = which(new_pos!=0)
        t_cost = transaction_fun(Symbols, CurrentDate, account_name, port_name, 
                                 userargs, idx = idx, prevD = prevD, cur_equity  = cur_equity)
        withdraw = t_cost$fee
        tcost = t_cost$tcost
        for(j in 1:length(idx)){
          x = get(Symbols[idx[j]], envir = globalenv())
          addTxn(port_name, Symbol = Symbols[idx[j]], TxnDate  = CurrentDate, TxnQty = new_pos[idx[j]], 
                 TxnPrice = as.numeric(Cl(x[CurrentDate,])), TxnFees = -tcost[idx[j]], verbose = verbose, eps = 1e-06)
        }
        t_pos = unique(c(t_pos, s_pos))
        traded = 1
      }
    }
    # risk_fun
    # Evaluates either portfolio level or individual position risk
    # and either reduces exposure or puts on a hedge trade
    # ALL symbols are passed
    if(!is.null(risk_fun)){
      risk_pos = risk_fun(Symbols, CurrentDate, account_name, port_name, userargs, signals = cur_signal,
                          prevD = prevD, cur_equity = cur_equity)
      # buy/sell
      idx = which(!is.na(risk_pos))
      if(length(idx)>0){
        # idx holds the actual number of trades
        t_cost = transaction_fun(Symbols, CurrentDate, account_name, port_name, 
                                 userargs, idx = idx, prevD = prevD, cur_equity  = cur_equity)
        tcost = t_cost$tcost
        if(verbose) print("\nRisk_Function_Trades (start):")
        for(j in 1:length(idx)){
          x = get(Symbols[idx[j]], envir = globalenv())
          addTxn(port_name, Symbol = Symbols[idx[j]], TxnDate  = CurrentDate, TxnQty = risk_pos[idx[j]], 
                 TxnPrice = as.numeric(Cl(x[CurrentDate,])), TxnFees = -tcost[idx[j]], verbose = verbose, eps = 1e-06)
        }
        if(verbose) print("\nRisk_Function_Trades (end):")
        t_pos = unique(c(t_pos, idx))
        traded = 1
      }
    }
    if(length(t_pos)>0){
      updatePortf(Portfolio=port_name, Symbols = Symbols[t_pos], Dates = CurrentDate)
      updateAcct2(name=account_name, Dates = CurrentDate)
      if(abs(withdraw)>0){
        Ac = get(paste("account.", account_name, sep = ""), envir = .blotter)
        Ac$summary[CurrentDate, 'Withdrawals']= -withdraw
        assign(paste("account.", account_name, sep = ""), Ac, envir = .blotter)
      }
      #updateEndEq(Account = account_name, Dates = CurrentDate)
    }
    updateEndEq(Account = account_name, Dates = CurrentDate)
  }
  Account = getAccount(account_name)
  Port = getPortfolio(port_name)
  return(list(Account = Account, Port = Port))
}
#-------------------------------------------------------------------------------
my.plotcorr <- function (corr, outline = FALSE, col = "grey", upper.panel = c("ellipse", "number", "none"), lower.panel = c("ellipse", "number", "none"), diag = c("none", "ellipse", "number"), digits = 2, bty = "n", axes = FALSE, xlab = "", ylab = "", asp = 1, cex.lab = par("cex.lab"), cex = 0.75 * par("cex"), mar = 0.1 + c(2, 2, 4, 2), ...)
{
  # this is a modified version of the plotcorr function from the ellipse package
  # this prints numbers and ellipses on the same plot but upper.panel and lower.panel changes what is displayed
  # diag now specifies what to put in the diagonal (numbers, ellipses, nothing)
  # digits specifies the number of digits after the . to round to
  # unlike the original, this function will always print x_i by x_i correlation rather than being able to drop it
  # modified by Esteban Buz
  if (!require('ellipse', quietly = TRUE, character = TRUE)) {
    stop("Need the ellipse library")
  }
  savepar <- par(pty = "s", mar = mar)
  on.exit(par(savepar))
  if (is.null(corr))
    return(invisible())
  if ((!is.matrix(corr)) || (round(min(corr, na.rm = TRUE), 6) < -1) || (round(max(corr, na.rm = TRUE), 6) > 1))
    stop("Need a correlation matrix")
  plot.new()
  par(new = TRUE)
  rowdim <- dim(corr)[1]
  coldim <- dim(corr)[2]
  rowlabs <- dimnames(corr)[[1]]
  collabs <- dimnames(corr)[[2]]
  if (is.null(rowlabs))
    rowlabs <- 1:rowdim
  if (is.null(collabs))
    collabs <- 1:coldim
  rowlabs <- as.character(rowlabs)
  collabs <- as.character(collabs)
  col <- rep(col, length = length(corr))
  dim(col) <- dim(corr)
  upper.panel <- match.arg(upper.panel)
  lower.panel <- match.arg(lower.panel)
  diag <- match.arg(diag)
  cols <- 1:coldim
  rows <- 1:rowdim
  maxdim <- max(length(rows), length(cols))
  plt <- par("plt")
  xlabwidth <- max(strwidth(rowlabs[rows], units = "figure", cex = cex.lab))/(plt[2] - plt[1])
  xlabwidth <- xlabwidth * maxdim/(1 - xlabwidth)
  ylabwidth <- max(strwidth(collabs[cols], units = "figure", cex = cex.lab))/(plt[4] - plt[3])
  ylabwidth <- ylabwidth * maxdim/(1 - ylabwidth)
  plot(c(-xlabwidth - 0.5, maxdim + 0.5), c(0.5, maxdim + 1 + ylabwidth), type = "n", bty = bty, axes = axes, xlab = "", ylab = "", asp = asp, cex.lab = cex.lab, ...)
  text(rep(0, length(rows)), length(rows):1, labels = rowlabs[rows], adj = 1, cex = cex.lab)
  text(cols, rep(length(rows) + 1, length(cols)), labels = collabs[cols], srt = 90, adj = 0, cex = cex.lab)
  mtext(xlab, 1, 0)
  mtext(ylab, 2, 0)
  mat <- diag(c(1, 1))
  plotcorrInternal <- function() {
    if (i == j){ #diag behavior
      if (diag == 'none'){
        return()
      } else if (diag == 'number'){
        text(j + 0.3, length(rows) + 1 - i, round(corr[i, j], digits=digits), adj = 1, cex = cex)
      } else if (diag == 'ellipse') {
        mat[1, 2] <- corr[i, j]
        mat[2, 1] <- mat[1, 2]
        ell <- ellipse(mat, t = 0.43)
        ell[, 1] <- ell[, 1] + j
        ell[, 2] <- ell[, 2] + length(rows) + 1 - i
        polygon(ell, col = col[i, j])
        if (outline)
          lines(ell)
      }
    } else if (i >= j){ #lower half of plot
      if (lower.panel == 'ellipse') { #check if ellipses should go here
        mat[1, 2] <- corr[i, j]
        mat[2, 1] <- mat[1, 2]
        ell <- ellipse(mat, t = 0.43)
        ell[, 1] <- ell[, 1] + j
        ell[, 2] <- ell[, 2] + length(rows) + 1 - i
        polygon(ell, col = col[i, j])
        if (outline)
          lines(ell)
      } else if (lower.panel == 'number') { #check if ellipses should go here
        text(j + 0.3, length(rows) + 1 - i, round(corr[i, j], digits=digits), adj = 1, cex = cex)
      } else {
        return()
      }
    } else { #upper half of plot
      if (upper.panel == 'ellipse') { #check if ellipses should go here
        mat[1, 2] <- corr[i, j]
        mat[2, 1] <- mat[1, 2]
        ell <- ellipse(mat, t = 0.43)
        ell[, 1] <- ell[, 1] + j
        ell[, 2] <- ell[, 2] + length(rows) + 1 - i
        polygon(ell, col = col[i, j])
        if (outline)
          lines(ell)
      } else if (upper.panel == 'number') { #check if ellipses should go here
        text(j + 0.3, length(rows) + 1 - i, round(corr[i, j], digits=digits), adj = 1, cex = cex)
      } else {
        return()
      }
    }
  }
  for (i in 1:dim(corr)[1]) {
    for (j in 1:dim(corr)[2]) {
      plotcorrInternal()
    }
  }
  invisible()
}
symbol_stats = function(Symbols)
{
  # Mean, Median, Min, Max, SD, Skew, Kurtosis, AR(1), AR(2), ARCH-LM, JB
  tb = matrix(NA, nrow = length(Symbols), ncol = 11)
  rownames(tb) = Symbols
  colnames(tb) = c("Mean","Median","Min","Max","Stdev","Skewness","Kurtosis","LB(1)","LB(2)","ARCH-LM(1)", "JB")
  for(i in 1:length(Symbols))
  {
    x = as.numeric(ROC(Cl(get(Symbols[i])), na.pad=FALSE))
    tb[i, ] = .stats_vector(x)
  }
  return(tb)
}

.stats_vector = function(x)
{
  sv = rep(NA, 11)
  x = x[!is.na(x)]
  sv[1] = mean(x)
  sv[2] = median(x)
  sv[3] = min(x)
  sv[4] = max(x)
  sv[5] = sd(x)
  sv[6] = rugarch:::.skewness(x)
  sv[7] = rugarch:::.kurtosis(x)+3
  sv[8] = Box.test(x, lag = 1, type = "Ljung-Box")$p.value
  sv[9] = Box.test(x, lag = 2, type = "Ljung-Box")$p.value
  sv[10] = as.numeric(rugarch:::.archlmtest(scale(x), lags=1)$p.value)
  sv[11] = as.numeric(tseries::jarque.bera.test(x)$p.value)
  return(sv)
}
#####################################################################################################

signal_fun = function(Symbols, CurrentDate, account_name, port_name, userargs, ...){
  M = userargs$Symbols.m
  signals = rep(0, length(Symbols))
  signals = sapply(M, FUN = function(x){
    sym = get(x, envir = globalenv())
    ans = sym[CurrentDate,"Indicator"]
    if(length(ans)==0 || is.na(ans)) ans = 0
    ans
  })
  return(signals)
}
transaction_fun = function(Symbols, CurrentDate, account_name, port_name, userargs, ...){
  tmp = list(...)
  idx = tmp$idx
  prevD = tmp$prevD
  cur_equity = tmp$cur_equity
  mgtfee = userargs$mgt_fee
  T = length(idx)
  tcost = rep(NA, length(Symbols))
  if(T>0){
    tcost[idx] = 10
  }
  ldates = userargs$eomdates
  if(any(CurrentDate == ldates)){
    fee = cur_equity * mgtfee
  } else{
    fee = 0
  }
  return(list(tcost = tcost, fee = fee))
}

# reduces portfolio positions by half when portfolio running_month_loss<losslimit
risk_fun = function(Symbols, CurrentDate, account_name, port_name, userargs, ...)
{
  tmp = list(...)
  prevD = tmp$prevD
  curr_m = as.character(format(prevD, "%Y-%m"))
  # check to see whether we have already effected a risk-reduction for the 
  # month (only done once)
  A = getAccount(account_name)
  if(sum(A$Risk[curr_m,"RiskReduction"])>0){
    A$Risk = rbind(A$Risk, xts(0, as.POSIXct(CurrentDate)))
    pos_new = rep(NA, m)
    
  } else{
    R = ROC(A$summary$End.Eq, type = "discrete")
    xR = sum(R[curr_m], na.rm=TRUE)
    if(length(xR)>0){
      m = length(Symbols)
      if(xR<userargs$portlimit){
        cur_qty = sapply(Symbols, FUN = function(s) zero_length(getPosQty(Portfolio = port_name, Symbol = s, Date = prevD), pad=0))
        sell = which(abs(cur_qty)>0.0)
        pos_new = rep(NA, m)
        if(length(sell)>0) pos_new[sell]  = -cur_qty[sell]
        A$Risk = rbind(A$Risk, xts(1, as.POSIXct(CurrentDate)))
      } else{
        pos_new = rep(NA, m)
        A$Risk = rbind(A$Risk, xts(0, as.POSIXct(CurrentDate)))
      }
    } else{
      pos_new = rep(NA, m)
      A$Risk = rbind(A$Risk, xts(0, as.POSIXct(CurrentDate)))
    }
  }
  assign(paste("account.", account_name, sep = ""), A, envir = .blotter)
  return(pos_new)
}

# calculate interest received on previous day's cash balance
cash_fun = function(Symbols, CurrentDate, account_name, port_name, userargs, ...)
{
  tmp = list(...)
  prevD = tmp$prevD
  cur_cash = tmp$cur_cash
  r = zero_length(as.numeric(userargs$FF[prevD,"rf"]), pad = 0)
  cin = cur_cash * r
  return(cin)
}
# We rebalance when a new signal comes in (end of month)
rebalance_fun = function(Symbols, CurrentDate, account_name, port_name, userargs, ...){
  tmp = list(...)
  cur_signal = tmp$signals
  prevD = tmp$prevD
  cur_equity = tmp$cur_equity
  max_pos  = userargs$max_pos
  tfun = userargs$transaction_fun
  S = userargs$Symbols.m
  # weights are aligned on end of month signal
  w = sapply(S, FUN = function(x) zero_length(get(x)[prevD,"weights"], pad=0))
  # Current Qty and Equity based on previous periods closing
  cur_qty = sapply(Symbols, FUN = function(s) getPosQty(Portfolio = port_name, Symbol = s, Date = prevD))	
  current_price = sapply(Symbols, FUN = function(x) zero_length(Cl(get(x, envir = globalenv())[CurrentDate,])))
  m = length(cur_qty)
  sell = which(as.integer((cur_signal<=0) * (cur_qty>0))==1)
  buy = which( as.integer((cur_signal>0) * (cur_qty==0))==1)
  hold = which(as.integer( (cur_signal>0) * (cur_qty>0) ) ==1)
  pos_new = rep(0, m)
  # these are the expected transaction costs based on current prices
  # actual transaction costs are recalculated based on actual execution price (if
  # price is part of the cost calculation)
  mm = length(which(cur_signal>0))
  target = round(w, 4)
  tmp = tfun(Symbols, CurrentDate, account_name, port_name, userargs, idx = c(sell, buy), ...)
  tcost = sum(tmp$tcost, na.rm = TRUE)
  fee = tmp$fee
  total_eq = cur_equity - tcost - fee
  if(length(sell)>0) pos_new[sell]  = -cur_qty[sell]
  if(length(buy)>0)  pos_new[buy]  = floor(total_eq * target[buy]/current_price[buy])
  if(length(hold)>0) pos_new[hold] = floor(total_eq * target[hold]/current_price[hold]) - cur_qty[hold] 
  return(pos_new)
}