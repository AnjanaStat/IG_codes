rm(list=ls())
library(mgcv)

fun1<-function(n1,n2,n3,n4,mu,l1,l2,l3,l4)
{
  g1<-rig(n1,mu,1/l1);g2<-rig(n2,mu,1/l2);g3<-rig(n3,mu,1/l3);g4<-rig(n4,mu,1/l4)
  m1=mean(g1);m2=mean(g2);m3=mean(g3);m4=mean(g4)
  s1<-var(g1);s2<-var(g2);s3<-var(g3);s4<-var(g4)
  V1<-sqrt(s1/n1+s2/n2);V2<-sqrt(s2/n2+s3/n3);v3<-sqrt(s3/n3+s4/n4)
  T1=(m2-m1)/V1;T2=(m3-m2)/V2;T3=(m4-m3)/v3
  A=max(T1,T2,T3,na.rm = FALSE)
  return(A)
}
fun2<-function(n1,n2,n3,n4,mu,l1,l2,l3,l4)
{
  x<-replicate(1000,fun1(n1,n2,n3,n4,mu,l1,l2,l3,l4))
  y<-sort(x,decreasing=FALSE)
  c<-y[950]
  return(c)
}

fun3<-function(n1,n2,n3,n4,mu1,mu2,mu3,mu4,l1,l2,l3,l4)
{
  g1<-rig(n1,mu1,1/l1);g2<-rig(n2,mu2,1/l2);g3<-rig(n3,mu3,1/l3);g4<-rig(n4,mu4,1/l4)
  m1=mean(g1);m2=mean(g2);m3=mean(g3);m4=mean(g4)
  s1<-var(g1);s2<-var(g2);s3<-var(g3);s4<-var(g4)
  V1<-sqrt(s1/n1+s2/n2);V2<-sqrt(s2/n2+s3/n3);v3<-sqrt(s3/n3+s4/n4)
  T1=(m2-m1)/V1;T2=(m3-m2)/V2;T3=(m4-m3)/v3
  A=max(T1,T2,T3,na.rm = FALSE)
  #mu=(n1*(1/s1)*m1+n2*(1/s2)*m2+n3*(1/s3)*m3)/(n1*(1/s1)+n2*(1/s2)+n3*(1/s3))
  mu=(n1*m1+n2*m2+n3*m3+n4*m4)/(n1+n2+n3+n4)
  L1<-sum(1/g1)/n1-1/m1
  L2<-sum(1/g2)/n2-1/m2
  L3<-sum(1/g3)/n3-1/m3
  L4<-sum(1/g4)/n4-1/m4
  out=fun2(n1,n2,n3,n4,mu,1/L1,1/L2,1/L3,1/L4)
  a=0
  if(A>out)
    a=a+1
  return(a)
}
fun4<-function(n1,n2,n3,n4,mu1,mu2,mu3,mu4,l1,l2,l3,l4)
{
  out<-replicate(1000,fun3(n1,n2,n3,n4,mu1,mu2,mu3,mu4,l1,l2,l3,l4))
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

