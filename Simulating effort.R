
# Simple effort model ===========================

# From Carruthers et al. (2012) - function built using equations provided in appendix A1

getFhist<-function(nsim,Esd,nyears,dFmin,dFmax,bb){
  
  ne<-nsim*10                                           # Number of simulated effort datasets
  dEfinal<-runif(ne,dFmin,dFmax)                        # Sample the final gradient in effort
  a<-(dEfinal-bb)/nyears                                # Derive slope to get there from intercept
  a<-array(a,dim=c(ne,nyears))                          # Slope array
  bb<-array(bb,dim=c(ne,nyears))                        # Intercept array
  x<-array(rep(1:nyears,each=ne),dim=c(ne,nyears))      # Year array
  dE<-a*x+bb                                            # Change in effort
  E<-array(NA,dim=c(ne,nyears))                         # Define total effort array
  E[,1]<-dE[,1]
  for(y in 2:nyears){
    E[,y]<-apply(dE[,1:y],1,sum)
  }
  E<-E/array(apply(E,1,mean),dim=c(ne,nyears))          # Standardise Effort to average 1
  cond<-apply(E,1,min)>0
  pos<-(1:ne)[cond]
  pos<-pos[1:nsim]
    
  E<-E[pos,]                                            # Sample only those without negative effort
  Emu<--0.5*Esd^2
  Eerr<-array(exp(rnorm(nyears*nsim,rep(Emu,nyears),rep(Esd,nyears))),c(nsim,nyears))
  E*Eerr
}


par(mfrow=c(1,3),mai=c(0.5,0.5,0.01,0.01),omi=c(0.4,0.4,0.4,0.04))

nyears<-60
nsim<-1000

Edat.hi <- getFhist(nsim=nsim,Esd=0,nyears=nyears,dFmin=-0.1,dFmax=0.1,bb=0.15)
Edat.med <- getFhist(nsim=nsim,Esd=0,nyears=nyears,dFmin=-0.1,dFmax=0.1,bb=0.25)
Edat.lo <-getFhist(nsim=nsim,Esd=0,nyears=nyears,dFmin=-0.15,dFmax=0.15,bb=0.35)

write.csv(data.frame('SIM' = rep(1:1000,each = 60),
                     'YEAR' = rep(seq(1,60,1),1000),
                     'FMORT' = as.vector(Edat.hi)), "G:/CAPAM/code/inputs/EdatHI.csv", row.names = F)

write.csv(data.frame('SIM' = rep(1:1000,each = 60),
                     'YEAR' = rep(seq(1,60,1),1000),
                     'FMORT' = as.vector(Edat.med)), "G:/CAPAM/code/inputs/EdatMED.csv", row.names = F)

write.csv(data.frame('SIM' = rep(1:1000,each = 60),
                     'YEAR' = rep(seq(1,60,1),1000),
                     'FMORT' = as.vector(Edat.lo)), "G:/CAPAM/code/inputs/EdatLOW.csv", row.names = F)


matplot(t(Edat.hi),type='l',labels=FALSE,ylim=c(0,2),ylab="Fishing mortality",xlab="Year")
legend('topleft',legend='Mean F = 0.26',bty='n')
mtext('0.0', side=2, line=1, at=0.0,cex=0.7)
mtext('0.1', side=2, line=1, at=0.5,cex=0.7)
mtext('0.2', side=2, line=1, at=1,cex=0.7)
mtext('0.3', side=2, line=1, at=1.5,cex=0.7)
mtext('0.4', side=2, line=1, at=2,cex=0.7)
mtext('40', side=1, line=1, at=0,cex=0.7)
mtext('50', side=1, line=1, at=10,cex=0.7)
mtext('60', side=1, line=1, at=20,cex=0.7)
mtext('70', side=1, line=1, at=30,cex=0.7)
mtext('80', side=1, line=1, at=40,cex=0.7)
mtext('90', side=1, line=1, at=50,cex=0.7)
mtext('100', side=1, line=1, at=60,cex=0.7)

matplot(t(Edat.med),type='l',labels=FALSE,ylim=c(0,2.7),yaxt="n",xlab="Year",ylab=" ")
legend('topleft',legend='Mean F = 0.18',bty='n')
mtext('40', side=1, line=1, at=0,cex=0.7)
mtext('50', side=1, line=1, at=10,cex=0.7)
mtext('60', side=1, line=1, at=20,cex=0.7)
mtext('70', side=1, line=1, at=30,cex=0.7)
mtext('80', side=1, line=1, at=40,cex=0.7)
mtext('90', side=1, line=1, at=50,cex=0.7)
mtext('100', side=1, line=1, at=60,cex=0.7)

matplot(t(Edat.lo),type='l',labels=FALSE,ylim=c(0,5),yaxt="n",xlab="Year",ylab=" ")
legend('topleft',legend="Mean F = 0.1",bty='n')
mtext('40', side=1, line=1, at=0,cex=0.7)
mtext('50', side=1, line=1, at=10,cex=0.7)
mtext('60', side=1, line=1, at=20,cex=0.7)
mtext('70', side=1, line=1, at=30,cex=0.7)
mtext('80', side=1, line=1, at=40,cex=0.7)
mtext('90', side=1, line=1, at=50,cex=0.7)
mtext('100', side=1, line=1, at=60,cex=0.7)






