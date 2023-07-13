rm(list=ls())
library(mgcv)
library(MASS)
library(smoothmest)
library(mvtnorm)
fun1<-function(n1,n2,n3,n4,mu1,mu2,mu3,mu4,l1,l2,l3,l4)
{
  g1<-rig(n1,mu1,1/l1);g2<-rig(n2,mu2,1/l2);g3<-rig(n3,mu3,1/l3);g4<-rig(n4,mu4,1/l4)
  m1=mean(g1);m2=mean(g2);m3=mean(g3);m4=mean(g4)
  s1<-var(g1);s2<-var(g2);s3<-var(g3);s4<-var(g4)
  V1<-sqrt(s1/n1+s2/n2);V2<-sqrt(s2/n2+s3/n3);v3<-sqrt(s3/n3+s4/n4)
  T1=(m2-m1)/V1;T2=(m3-m2)/V2;T3=(m4-m3)/v3
  A=max(T1,T2,T3,na.rm = FALSE)
  mu=(n1*m1+n2*m2+n3*m3+n4*m4)/(n1+n2+n3+n4)
  L1<-1/(sum(1/g1)/n1-1/m1);L2<-1/(sum(1/g2)/n2-1/m2)
  L3<-1/(sum(1/g3)/n3-1/m3);L4<-1/(sum(1/g4)/n4-1/m4)
  N=(n1+n2+n3+n4)
  nu1=(n1*L1)/(mu*N);nu2=(n2*L2)/(mu*N);nu3=(n3*L3)/(mu*N);nu4=(n4*L4)/(mu*N)
  r1=mu^2/nu2;r2=mu^2/nu3
  s10=mu^2*(1/nu1+1/nu2);s20=mu^2*(1/nu2+1/nu3);s30=mu^2*(1/nu3+1/nu4)
  d=c(sqrt(s10),sqrt(s20),sqrt(s30))
  D=diag(d)
  R=matrix(c(s10,-r1,0,-r1,s20,-r2,0,-r2,s30),nrow=3,ncol=3,byrow=TRUE)
  ST=solve(D)%*%R%*%solve(D)
  q=qmvnorm(0.95,tail="lower.tail",mean=0,sigma=ST)$quantile
  #For Max-T
  z=0
  if(A>q)
  {
    z=z+1
  }
  return(z)
}
fun4<-function(n1,n2,n3,n4,mu1,mu2,mu3,mu4,l1,l2,l3,l4)
{
  # find the number of times statistic value is greater than the crtical value among 1000 values
  out<-replicate(1000,fun1(n1,n2,n3,n4,mu1,mu2,mu3,mu4,l1,l2,l3,l4))
  p<-sum(out)/1000
  return(p)
}
##Size values of Table 7.4
p<-replicate(5,fun4(20,25,30,20,1,1,1,1,1,1,1,1))
p;mean(p)
p<-replicate(5,fun4(20,25,30,20,5,5,5,5,1,1,1,1))
p;mean(p)
p<-replicate(5,fun4(20,25,30,20,10,10,10,10,1,1,1,1))
p;mean(p)

p<-replicate(5,fun4(20,25,30,20,1,1,1,1,1,2,3,4))
p;mean(p)
p<-replicate(5,fun4(20,25,30,20,5,5,5,5,1,2,3,4))
p;mean(p)
p<-replicate(5,fun4(20,25,30,20,10,10,10,10,1,2,3,4))
p;mean(p)

p<-replicate(5,fun4(20,25,30,20,1,1,1,1,1,5,8,10))
p;mean(p)
p<-replicate(5,fun4(20,25,30,20,5,5,5,5,1,5,8,10))
p;mean(p)
p<-replicate(5,fun4(20,25,30,20,10,10,10,10,1,5,8,10))
p;mean(p)

p<-replicate(5,fun4(20,25,30,20,1,1,1,1,0.1,0.4,0.8,1.2))
p;mean(p)
p<-replicate(5,fun4(20,25,30,20,5,5,5,5,0.1,0.4,0.8,1.2))
p;mean(p)
p<-replicate(5,fun4(20,25,30,20,10,10,10,10,0.1,0.4,0.8,1.2))
p;mean(p)

