library(readxl)
# Negative Binomial Disttribution
NBD.pmf<-function(count.data,par){
  N<-length(count.data)
  pmf.val = vector("numeric",N)
  for (n in 1:N){
    x<-count.data[n]
    gamma.top<-(gamma(x+par[1]))
    gamma.bot<-factorial(x)*gamma(par[1])
    p1<-par[2]/(1+par[2])
    p2<- 1-p1     
    pmf.val[n]<-(gamma.top/gamma.bot)*(p1^par[1])*(p2^x)  
  }
  return(pmf.val)
}

#R code for Customer Analytics Homework
# Sami Cheong 
# 2018-02-04
# Poisson Distribution

POIS.pmf<-function(count.data,par){
  N<-length(count.data)
  lambda<-par[1]
  pmf.val = vector("numeric",N)
  for (n in 1:N){
    x<-count.data[n]
    pmf.val[n]<-ifelse(lambda>0, (lambda^x)*exp(-lambda)/(factorial(x)),0)  
  }
  return(pmf.val)
}

# Zero-inflated Poisson
POIS.pmf.zero<-function(count.data,par){
  lambda<-par[1]
  p0<-par[2]

  zero.case<-p0+(1-p0)*POIS.pmf(count.data,par[1])
  nonzero.case<-(1-p0)*POIS.pmf(count.data,par[1])
  pmf<-ifelse(count.data==0,zero.case,nonzero.case)

  return(pmf)
}

# Zero-inflated NBD
NBD.pmf.zero<-function(count.data,par){
 p0 <-par[3]/(1+par[3])
 
 #p0 <-par[3]
 r<-par[1]
 alpha<-par[2]
 zero.case<-p0+(1-p0)*NBD.pmf(count.data,c(par[1],par[2]))
 nonzero.case<-(1-p0)*NBD.pmf(count.data,c(par[1],par[2]))
 pmf<-ifelse(count.data==0,zero.case,nonzero.case)
 #print(pmf)
 #print(pmf)
 return(pmf)
}

# Calculate likelihood 
getLL<-function(count.data,pmf,par,debug=F){
  
  # par[1] = r, # par[2] = alpha
  pmf.val<-pmf(count.data,par)
  
  #print(pmf.val)
  LL.val<-sum(log10(pmf.val))
  #print(NBDZ.LL)
  #print(NBD.LL)
  if(debug){
    plot(count.data,pmf.val,type = 'l')
    print(pmf)
    print(log10(pmf.val))
    print(LL.val)
  }
  
  return(LL.val)
}

BB.pmf<-function(count.data,par){
  
  N<-par[1]
  alpha<-par[2]
  beta<-par[3]
  
  pmf.val = vector("numeric",N)
  for (i in 1:N){
    x<-count.data$x[i]
    print(x)
    comb<-choose(N,x)
    pmf.val[i]<-(comb*beta(alpha+x,beta+N-x))/beta(alpha,beta)
    print(pmf.val)
  }
  return(pmf.val)


  
}

## HW2:

# data<-read_excel('data/khakichinos HW2 data.xlsx',sheet = 1)
# 
# Poisson.par.opt<-optim(par=2.5,fn=getLL,pmf=POIS.pmf,debug=F,
#                        count.data =data$Visits, method = 'L-BFGS-B', control=list(fnscale=-1),lower=1e-10,hessian = T)
# print('Optimal parameters for Poisson Distribution:')
# print(Poisson.par.opt$par)
# print('Log-likelihood:')
# print(Poisson.par.opt$value)
# 
# # Fit data with zero-inflated Poisson:
# POISZ.par.opt<-optim(par=c(0.1,0.5),fn=getLL,pmf=POIS.pmf.zero,debug=F,
#                      count.data =data$Visits, method = 'L-BFGS-B', control=list(fnscale=-1),lower=1e-10,hessian = T)
# 
# print('Optimal parameters for zero-inflated Poisson Distribution:')
# print(POISZ.par.opt$par)
# print('Log-likelihood:')
# print(POISZ.par.opt$value)
# 
# # Fit data with NBD:
# 
# NBD.par.opt<-optim(par=c(0.1,0.1),fn=getLL,pmf=NBD.pmf,
#                    count.data =data$Visits, method="L-BFGS-B",control=list(fnscale=-1), lower=1e-10)
# 
# print('Optimal paramters for NBD:')
# print(NBD.par.opt$par)
# print('Log-likelihood:')
# print(NBD.par.opt$value)
# 
# # Fit data with zero-inflated NBD:
# NBDZ.par.opt<-optim(par=c(0.2,0.2,5),fn=getLL,pmf=NBD.pmf.zero,
#                     count.data =data$Visits, method="L-BFGS-B",control=list(fnscale=-1), lower=1e-10)
# 
# print('Optimal paramters for zero-inflated NBD:')
# print(NBDZ.par.opt$par)
# print('Log-likelihood:')
# print(NBDZ.par.opt$value)
# 
