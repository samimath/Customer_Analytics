library(readxl)
data<-read_excel('data/khakichinos HW2 data.xlsx',sheet = 1)


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



POIS.pmf<-function(count.data,par){
  N<-length(count.data)
  lambda<-par[1]
  pmf.val = vector("numeric",N)
  for (n in 1:N){
    x<-count.data[n]
    pmf.val[n]<-(lambda^x)*exp(-lambda)/(factorial(x))  
  }
  return(pmf.val)
}

POIS.pmf.zero<-function(count.data,par){
  N<-length(count.data)
  lambda<-par[1]
  p0<-par[2]
  pmf.val = vector("numeric",N)
  for (n in 1:N){
    x<-count.data[n]
    zero.case<-p0+(1-p0)*POIS.pmf(count.data,par[1])
    nonzero.case<-(1-p0)*POIS.pmf(count.data,par[1])
    pmf.val[n]<-ifelse(count.data==0,zero.case,nonzero.case)
  }
  return(pmf.val)
}

NBD.pmf.zero<-function(count.data,par){
 #p0 <-par[3]/(1+par[3])
 
 p0 <-par[3]/(1+par[3])
 r<-par[1]
 alpha<-par[2]
 zero.case<-p0+(1-p0)*NBD.pmf(count.data,c(par[1],par[2]))
 nonzero.case<-(1-p0)*NBD.pmf(count.data,c(par[1],par[2]))
 pmf<-ifelse(count.data==0,zero.case,nonzero.case)
 #print(pmf)
 return(pmf)
}

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




par.init<-c(2,1)
par.opt<-optim(par=par.init,fn=getLL,pmf=NBD.pmf,
      count.data =data$Visits, method="L-BFGS-B",control=list(fnscale=-1), lower=1e-10)

par.opt<-optim(par=c(0.2,0.2,10),fn=getLL,pmf=NBD.pmf.zero,
               count.data =data$Visits, method="L-BFGS-B",control=list(fnscale=-1), lower=1e-10)


par.opt<-optim(par=2.5,fn=getLL,pmf=POIS.pmf,debug=F,
               count.data =data$Visits, method = 'L-BFGS-B', control=list(fnscale=-1),lower=1e-10,hessian = T)
