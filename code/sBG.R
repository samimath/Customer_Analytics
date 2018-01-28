#R code for Session 1
#Arun Gopalakrishnan
#setwd('../scheo/Google Drive/WUSTL/Spring 2018/MKT 500T Customer Analytics/Customer_Analytics/')

sBG.data = read.csv("data/sBG_data.csv",header=T)
sBG.data$customers<-as.numeric(as.character(sBG.data$customers))

sBG.insamp.cum = c(1000,sBG.data[1:7,2]);
sBG.insamp.inc = -diff(sBG.insamp.cum);    #column D
survivors = sBG.insamp.cum[8];
## sBG function:
sBG.pmf = function(a, b, T = 1) {
  ret.pmf = vector("numeric",T); # from 1 to T
  ret.pmf[1] = a/(a+b);

  if (T > 1) {
    for (t in 2:T) {
      ret.pmf[t] = ret.pmf[t-1]*(b + t - 2)/(a + b + t - 1);
    }
  }
  return(ret.pmf);
}

sBG.LL = function(par,churn.data,survivors,debug.flag=F) {
  pmf = sBG.pmf(exp(par[1]),exp(par[2]),length(churn.data));
  LLval = sum( log(pmf)*churn.data ) + survivors*log(1-sum(pmf));
  if (debug.flag) {
    print(c(pmf,1-sum(pmf)));
    print(c(churn.data,survivors))
  }
  return (LLval);
}

 #alpha, beta > 0. need to impose constraint
 
 par.init=c(0,0); #concatenate
 sBG.result = optim(par=par.init,fn=sBG.LL,churn.data = sBG.insamp.inc, survivors = survivors,control=list(fnscale=-1,reltol=1e-18))
 par.final = exp(sBG.result$par)
 
 sBG.result = optim(par=sBG.result$par,fn=sBG.LL,churn.data = sBG.insamp.inc, survivors = survivors,control=list(fnscale=-1,reltol=1e-18))
 exp(sBG.result$par)
 
 print(sBG.LL(log(par.final),churn.data = sBG.insamp.inc, survivors = survivors,debug.flag=T),digits=14)
 print(sBG.LL(log(c(0.668083,3.806053)),churn.data = sBG.insamp.inc, survivors = survivors,debug.flag=T),digits=14)
 
 pmf.12 = sBG.pmf(par.final[1],par.final[2],12)
 1 - cumsum(pmf.12)
