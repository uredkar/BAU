#install.packages("FinancialMath")
#install.packages("derivmkts")
library("FinancialMath")
library("derivmkts")
library("ggplot2")
library("precrec")
s=229; k=235; v=0.30; r=0.01; tt=0.25; d=0;
par(mfrow=c(1,1 ))
t = covered.call(S=s,K=k,r=r,t=tt,sd=v,plot=TRUE)
t
s=229; k=235; v=0.30; r=0.01; tt=0.25; d=0;
greeks(bscall(s, k, v, r, tt, d), complete=TRUE, long=FALSE, initcaps=TRUE)
greeks2(bscall, list(s=s, k=k, v=v, r=r, tt=tt, d=d))
greeks2(bscall, list(s=s, k=k, v=v, r=r, tt=tt, d=d))[c('Delta', 'Gamma'), ]
bsopt(s, k, v, r, tt, d)
#bsopt(s, c(35, 40, 45), v, r, tt, d)
#bsopt(s, c(35, 40, 45), v, r, tt, d)[['Call']][c('Delta', 'Gamma'), ]

#k <- 100; v <- 0.30; r <- 0.08; tt <- 2; d <- 0
S <- seq(.5, 250, by=.5)
Call <- greeks(bscall(S, k, v, r, tt, d))
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

