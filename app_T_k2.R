
rm(list=ls())
library(mgcv)
fun1<-function(n1,n2,mu1,mu2,l1,l2)
{
  g1=rig(n1,mu1,1/l1)
  g2=rig(n2,mu2,1/l2)
  S1=var(g1);S2=var(g2)
  m1=mean(g1);m2=mean(g2)
  V=sqrt(S1/n1+S2/n2)
  T=(m2-m1)/V
  neu=(S1/n1+S2/n2)^2
  dno=(S1^2/(n1^2*(n1-1))+S2^2/(n2^2*(n2-1)))
  df=neu/dno
  df
  q=qt(0.05, df, lower.tail=FALSE)
  a=0
  if(T>q )
    a=a+1
  return(a)
}
fun2<-function(n1,n2,mu1,mu2,l1,l2)
{
  X<-replicate(10000,fun1(n1,n2,mu1,mu2,l1,l2))
  alpha=mean(X)
  return(alpha)
}

##Size values of Table 7.1

p<-replicate(5,fun2(8,6,1,1,50,50));mean(p)
p<-replicate(5,fun2(8,6,10,10,50,50));mean(p)
p<-replicate(5,fun2(8,6,1,1,1,1));mean(p)
p<-replicate(5,fun2(30,30,1,1,1,1));mean(p)
p<-replicate(5,fun2(28,28,1,1,1,1));mean(p)
p<-replicate(5,fun2(26,26,1,1,1,1));mean(p)
p<-replicate(5,fun2(22,22,1,1,1,1));mean(p)
p<-replicate(5,fun2(20,20,1,1,1,1));mean(p)
p<-replicate(5,fun2(18,18,1,1,1,1));mean(p)
p<-replicate(5,fun2(15,15,1,1,1,1));mean(p)
p<-replicate(5,fun2(10,10,1,1,1,1));mean(p)
p<-replicate(5,fun2(10,10,1,1,3,3));mean(p)
p<-replicate(5,fun2(10,10,1,1,4,4));mean(p)
p<-replicate(5,fun2(10,10,1,1,5,5));mean(p)
p<-replicate(5,fun2(10,10,1,1,2,2));mean(p)
p<-replicate(5,fun2(10,10,1,1,3,3));mean(p)
p<-replicate(5,fun2(10,10,1,1,4,4));mean(p)
p<-replicate(5,fun2(10,10,1,1,5,5));mean(p)
p<-replicate(5,fun2(15,15,1,1,2,2));mean(p)
p<-replicate(5,fun2(15,15,5,5,5,5));mean(p)
p<-replicate(5,fun2(15,15,5,5,10,10));mean(p)
p<-replicate(5,fun2(10,10,5,5,10,10));mean(p)
p<-replicate(5,fun2(15,10,1,1,2,3));mean(p)
p<-replicate(5,fun2(10,15,1,1,2,3));mean(p)

p<-replicate(5,fun2(10,15,1,1,3,2));mean(p)
p<-replicate(5,fun2(20,15,1,1,3,2));mean(p)
p<-replicate(5,fun2(20,10,1,1,3,6));mean(p)
p<-replicate(5,fun2(8,10,1,1,3,4));mean(p)
p<-replicate(5,fun2(15,20,1,1,3,4));mean(p)
p<-replicate(5,fun2(20,25,1,1,3,4));mean(p)
p<-replicate(5,fun2(25,20,1,1,3,4));mean(p)
p<-replicate(5,fun2(25,20,2,2,3,4));mean(p)
p<-replicate(5,fun2(25,20,4,4,3,4));mean(p)
p<-replicate(5,fun2(25,20,6,6,3,4));mean(p)
p<-replicate(5,fun2(25,20,8,8,3,4));mean(p)
p<-replicate(5,fun2(25,20,10,10,3,4));mean(p)
p<-replicate(5,fun2(25,20,12,12,3,4));mean(p)
p<-replicate(5,fun2(25,20,14,14,3,4));mean(p)
p<-replicate(5,fun2(25,20,14,14,4,5));mean(p)
p<-replicate(5,fun2(25,20,14,14,5,6));mean(p)
p<-replicate(5,fun2(25,20,14,14,6,6));mean(p)
p<-replicate(5,fun2(25,20,14,14,16,16));mean(p)
p<-replicate(5,fun2(25,20,14,14,20,22));mean(p)
