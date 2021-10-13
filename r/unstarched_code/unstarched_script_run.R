# Replication of results from blog article on "The Fallacy of 1/N and Static Weight Allocation"
# remember to place customfn.R in a working directory and set the directory ( 'setwd()' ) for saving 
# intermediate results
ncores = 20
# set number of parallel cores to use for the model
source("customfn.R")
# the optimal portfolio
source("unstarched_script_1.R")
rm(list = ls())
source("customfn.R")
# the optimal portfolio with auxillary timing from the DGP
source("unstarched_script_2.R")
rm(list = ls())
source("customfn.R")
# the equal weighted portfolio
source("unstarched_script_3.R")
# DON't clear after the last run

# Universe Statistics
print(symbol_stats(Symbols), digits = 4)
  

load(file="portopt.rda")
load(file="portalt.rda")
load(file="porteqw.rda")
RiskFree = FF$rf

Ropt = ROC(portopt$Account$summary$End.Eq, type = "discrete")[-1]
Ralt = ROC(portalt$Account$summary$End.Eq, type = "discrete")[-1]
Reqw = ROC(porteqw$Account$summary$End.Eq, type = "discrete")[-1]


# Tables can be reproduced as follows:
cbind(portfolio_stats(Ralt, rf = RiskFree, scale = 252, Dates = "1994/"),
      portfolio_stats(Ropt, rf = RiskFree, scale = 252, Dates = "1994/"),
      portfolio_stats(Reqw, rf = RiskFree, scale = 252, Dates = "1994/"),
      portfolio_stats(ROC(Cl(VFINX), na.pad=FALSE), rf = RiskFree, scale = 252, Dates = "1994/"))
# change the Dates for the subperiods as you would with xts indexing
# For the Ledoit-Wolf test you need to download the test (R code) from the site of Michael Wolf

# average yearly weights
load(file = "optimal_weights.rda")
yw = apply.yearly(wts, "mean")
yw = as.xts(yw)
jpeg("optimal_weights.jpeg", width = 600, height = 300, quality = 100)
par(xpd=T, mar=par()$mar+c(4,0,0,0))
barplot(t(as.matrix(yw))[,-1], col = c(rainbow(m)), names.arg = format(time(yw), "%Y")[-1])
title("Average Yearly Asset Weighting")
legend(-0.5, -0.2, ShortNames, col = c(rainbow(m)),
       fill = c(rainbow(m)), bty="n", cex = 0.9, ncol = 5)
par(mar=c(5, 4, 4, 2) + 0.1)
dev.off()

# number of securities in portfolio
jpeg("assetsinplay.jpeg", width = 800, height = 500, quality = 100)
xt = apply(wts, 1, function(x) length(which(x>0.0)))
colx = rep("steelblue", length(xt))
yt = which(xt<4)
colx[yt] = "tomato1"
barplot(xt, col = colx, border=NA, main = "Securities in Portfolio")
dev.off()




# per security contribution
jpeg("optimal_contribution.jpeg", width = 800, height = 500, quality = 100)
par(mfrow = c(2,1))
barplot(icontribution(Symbols, portopt$Account, portopt$Port, Dates = "1991/2003"), 
        col = c(rainbow(m),rep(colors()[7],3)), 
        main = "Per Asset Contribution to Total Period Net Profit\n [1991-2003]",
        names.arg = c(ShortNames, "Interest", "Fees","TxnCosts"),
        cex.main = 0.9, cex.names = 0.8, las  = 2)
barplot(icontribution(Symbols, portopt$Account, portopt$Port, Dates = "2003/"), 
        col = c(rainbow(m),rep(colors()[7],3)), 
        main = "Per Asset Contribution to Total Period Net Profit\n [2003-2013]",
        names.arg = c(ShortNames, "Interest", "Fees","TxnCosts"),
        cex.main = 0.9, cex.names = 0.8, las  = 2)
dev.off()

# per security active contribution

jpeg("active_contribution.jpeg", width = 800, height = 500, quality = 100)
par(mfrow = c(2,1))
barplot(icontribution(Symbols, portopt$Account, portopt$Port, Dates = "1994/2003")-icontribution(Symbols, porteqw$Account, porteqw$Port, Dates = "1994/2003"), 
        col = c(rainbow(m),rep(colors()[7],3)), 
        main = "Per Asset Active Contribution to Total Period Net Profit\n [1991-2003]",
        names.arg = c(ShortNames, "Interest", "Fees","TxnCosts"),
        cex.main = 0.9, cex.names = 0.8, las  = 2)
barplot(icontribution(Symbols, portopt$Account, portopt$Port, Dates = "2003/")-icontribution(Symbols, porteqw$Account, porteqw$Port, Dates = "2003/"), 
        col = c(rainbow(m),rep(colors()[7],3)), 
        main = "Per Asset Active Contribution to Total Period Net Profit\n [2003-2013]",
        names.arg = c(ShortNames, "Interest", "Fees","TxnCosts"),
        cex.main = 0.9, cex.names = 0.8, las  = 2)
dev.off()



###############################################################################
# Correlation Plot
Y = ROC(Cl(VFINX), type = "continuous")
for(i in 2:m){
  Y = cbind(Y, ROC(Cl(get(Symbols[i])), type = "continuous"))
}

C = cor(Y, use = "pairwise.complete.obs")
colnames(C) = rownames(C) = ShortNames
ord <- order(C[1,])
ordered.cor.si <- C[ord, ord]

jpeg("correlations.jpeg")
par(mfrow = c(1,1))
colsc=c(colors()[33],colors()[138], colors()[566])
colramp = colorRampPalette(colsc, space='Lab', interpolate="spline")
colors = colramp(0.8*length(C[1,]))
my.plotcorr(ordered.cor.si, col=colors[5*ordered.cor.si + 6], main="Asset Correlations", upper.panel = "none")
dev.off()
