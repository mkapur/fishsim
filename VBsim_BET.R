require(nlstools)
require(reshape2)
# 1. Select randomly up to [maybe 20 for every age and only 10 or so for ages>10?)] fish from ages 2-4, 4-6, 6-8 and so on
# 2. Fit growth curve
# 3. Repeat
# 4. From the 1000(?) fit plot the median K, Linf and add the 80%(?) conf internvals.

## gather params- based on mean and SD of M+F in Table 3
# l.inf.sim = rnorm(1000,160.3,2.2)
# K.sim = rnorm(1000,0.268,0.013)
# t0.sim = rnorm(1000,-1.01,0.09)

l.inf = 160
K = 0.268
t0 = -1.01
nsamp <- c(20,20,20,20,20,20,20,20,11,9,4,2,3,2) # if NO replacement use this one
#nsamp  <- c(20,20,20,20,20,20,20,20,20,20,10,10,10,10) #if with replacement use this one
#nsamp  <- c(20,20,20,20,20,20,20,20,20,20,10,10,0,0)
replace.TF <- T
nages <- 11

vbsim1 = read.csv("vbsim1.csv")
wantedcols1 <- 0*seq(1:nages)
for (ii in 1:nages) {
	wantedcols1[ii] <- max(which(!is.na(vbsim1[ii,])))
}
#wantedcols2 <- 0*seq(11:14)
#for (ii in 1:length(wantedcols2)) {
#	wantedcols2[ii] <- max(which(!is.na(vbsim1[ii+10,])))
#}

#BET = BET0[complete.cases(BET0[,3:4]),]
#BETsub = subset(BET, Decimal.age < 12.5)
# vbsim0  = sapply(age, function(t) l.inf.sim * (1-exp(-K.sim*(t - t0.sim)))) ## 1000 simulaed lengths for each age
# vbsim1 = cbind(age,t(vbsim0)) ## bind to age
## young ages pick 20 random columns and keep age, older ages pick 10 random & reshape
#vbsim = rbind(melt(data.frame(vbsim1[1:10, c(1, sample(ncol(vbsim1), 20))]), id = c("age"))[, c('age', 'value')],
 #              melt(data.frame(vbsim1[11:nrow(vbsim1), c(1, sample(ncol(vbsim1), 10))]), id = c("age"))[, c('age', 'value')])  
#nsamp <- c(20+0*(1:10),10+0*(11:14))

#a <- data.frame(age=NA, value=NA)
#for (ii in 1:nages) {
#	a <- rbind(a,melt(data.frame(vbsim1[ii, c(1, sample(2:wantedcols1[ii], nsamp[ii], replace=replace.TF))]), id = c("age"))[, c('age', 'value')])
#}
#a <- a[-1,]
##vbsim = rbind(melt(data.frame(vbsim1[1:10, c(1, sample(wantedcols1, 20))]), id = c("age"))[, c('age', 'value')],
##               melt(data.frame(vbsim1[11:nrow(vbsim1), c(1, sample(wantedcols2, 10))]), id = c("age"))[, c('age', 'value')])  

#names(a)[2] = 'l.exp'
### plot selected
## with(vbsim, plot(age,value, main = 'simulated von b'))

### Apply non linear least squares to retro-fit the parameters for each "observation"

#fit.vonB.length = nls(l.exp ~ l.inf * (1-exp(-K*(age - t0))),
#                      data = a,
#                      start=list(l.inf = l.inf, K = K, t0 = t0),
#                      control = list(maxiter = 150))

nboot <- 1000
K.boot <- NA*(1:nboot)
Linf.boot <- K.boot
for (jj in 1:nboot) {
	a <- data.frame(age=NA, value=NA)
	for (ii in 1:nages) {
		a <- rbind(a,melt(data.frame(vbsim1[ii, c(1, sample(2:wantedcols1[ii], nsamp[ii], replace=replace.TF))]), id = c("age"))[, c('age', 'value')])
	}
	a <- a[-1,]
	names(a)[2] = 'l.exp'
	fit.vonB.length = nls(l.exp ~ l.inf * (1-exp(-K*(age - t0))),
                      data = a,
                      start=list(l.inf = l.inf, K = K, t0 = t0),
                      control = list(maxiter = 150))
     
     K.boot[jj] <- coef(fit.vonB.length)[2]
     Linf.boot[jj] <- coef(fit.vonB.length)[1]
}
     
par(mfrow = c(1,2))
hist(K.boot, main = 'K', xlab = 'K') 
abline(v = median(K.boot), col = 'red', lwd = 3)
abline(v =  quantile(K.boot,0.1), col = 'blue', lwd = 3, lty = 2)
abline(v =  quantile(K.boot,0.9), col = 'blue', lwd = 3, lty = 2)
legend('topleft', legend = c(paste0('median = ', round(median(K.boot), 
	digits = 3)),'10%-90% CI'), lwd = 2, lty = c(1,2), col = c('red','blue'))

hist(Linf.boot, main = 'Linf', xlab = 'Linf') 
abline(v = median(Linf.boot), col = 'red', lwd = 3)
abline(v =  quantile(Linf.boot,0.1), col = 'blue', lwd = 3, lty = 2)
abline(v =  quantile(Linf.boot,0.9), col = 'blue', lwd = 3, lty = 2)
legend('topleft', legend = c(paste0('median = ', round(median(Linf.boot), 
	digits = 2)),'10%-90% CI'), lwd = 2, lty = c(1,2), col = c('red','blue'))


#CIs = confint2(fit.vonB.length, level = 0.8) ## provide raw values for 80% CI

#boots = nlsBoot(fit.vonB.length, niter = 1000) ## get out median

#par(mfrow = c(1,2))
#hist(boots$coefboot[,'K'], main = 'K', xlab = 'K') 
#abline(v = boots$bootCI['K','Median'], col = 'red', lwd = 3)
#abline(v = CIs['K','90 %'], col = 'blue', lwd = 3, lty = 2)
#abline(v = CIs['K','10 %'], col = 'blue', lwd = 3, lty = 2)
#legend('topleft', legend = c(paste0('median = ', round(boots$bootCI['K','Median'], digits = 3)),'10%-90% CI'), lwd = 2, lty = c(1,2), col = c('red','blue'))


#hist(boots$coefboot[,'l.inf'], main = 'Linf', xlab = 'Linf cm') 
#abline(v = boots$bootCI['l.inf','Median'], col = 'red', lwd = 3)
#abline(v = CIs['l.inf','90 %'], col = 'blue', lwd = 3, lty = 2)
#abline(v = CIs['l.inf','10 %'], col = 'blue', lwd = 3, lty = 2)
#legend('topleft', legend = c(paste0('median = ',round(boots$bootCI['l.inf','Median'])),'10%-90% CI'), 
#       lwd = 2, lty = c(1,2), col = c('red','blue'))
# dev.off()
