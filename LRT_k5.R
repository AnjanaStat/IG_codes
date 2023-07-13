rm(list=ls())
library(Iso)
library(mgcv)
fun1<-function(n1,n2,n3,n4,n5,mu,l1,l2,l3,l4,l5)
{
  g1=rig(n1,mu,1/l1);g2=rig(n2,mu,1/l2);g3=rig(n3,mu,1/l3)
  g4=rig(n4,mu,1/l4);g5=rig(n5,mu,1/l5)
  m1=mean(g1);m2=mean(g2);m3=mean(g3);m4=mean(g4);m5=mean(g5)
  p1=sum(g1);p2=sum(g2);p3=sum(g3);p4=sum(g4);p5=sum(g5)
  q1=sum(1/g1);q2=sum(1/g2);q3=sum(1/g3);q4=sum(1/g4);q5=sum(1/g5)
  imu=(n1*m1+n2*m2+n3*m3+n4*m4+n5*m5)/(n1+n2+n3+n4+n5)
  is1=n1/(sum(q1-1/m1));is2=n2/(sum(q2-1/m2));is3=n3/(sum(q3-1/m3))
  is4=n4/(sum(q4-1/m4));is5=n5/(sum(q5-1/m5))
  x13=imu
  t1=is1;t2=is2;t3=is3;t4=is4;t5=is5
  repeat
  {
    nmu=(n1*m1*t1+n2*m2*t2+n3*m3*t3+n4*m4*t4+n5*m5*t5)/(n1*t1+n2*t2+n3*t3+n4*t4+n5*t5)
    ns1=(n1*nmu^2)/sum((g1-nmu)^2/g1);ns2=(n2*nmu^2)/sum((g2-nmu)^2/g2)
    ns3=(n3*nmu^2)/sum((g3-nmu)^2/g3);ns4=(n4*nmu^2)/sum((g4-nmu)^2/g4)
    ns5=(n5*nmu^2)/sum((g5-nmu)^2/g5)
    D=abs(x13-nmu)
    D1=max(D)
    if(D1<=0.000001)
    {
      break
    }
    x13=nmu
    t1=ns1;t2=ns2;t3=ns3;t4=ns4;t5=ns5
  }
  x<-c(m1, m2, m3, m4, m5)
  s1<-sum(1/g1)/n1-1/m1;s2<-sum(1/g2)/n2-1/m2;s3<-sum(1/g3)/n3-1/m3
  s4<-sum(1/g4)/n4-1/m4;s5<-sum(1/g5)/n5-1/m5
  s<-c(s1, s2, s3, s4,s5)
  x0=x
  s0=s
  x8<-x0
  repeat
  {
    w1=c(n1/s0[1],n2/s0[2],n3/s0[3],n4/s0[4],n5/s0[5])
    x6=pava(x0, w1)
    s11<-(sum((g1-x6[1])^2/g1))/(n1*x6[1]^2)
    s12<-(sum((g2-x6[2])^2/g2))/(n2*x6[2]^2)
    s13<-(sum((g3-x6[3])^2/g3))/(n3*x6[3]^2)
    s14<-(sum((g4-x6[4])^2/g4))/(n4*x6[4]^2)
    s15<-(sum((g5-x6[5])^2/g5))/(n5*x6[5]^2)
    v=c(s11, s12, s13, s14, s15)
    s0<-v
    x10=abs(x8-x6)
    p=max(x10[1], x10[2], x10[3], x10[4], x10[5], na.rm = FALSE)
    if(p<=0.0000001)
    {
      break
    }
    #rm(x8)
    x8<-x6
  }
  LR=((t1*s0[1])^(n1/2))*((t2*s0[2])^(n2/2))*((t3*s0[3])^(n3/2))*((t4*s0[4])^(n4/2))*((t5*s0[5])^(n5/2))
  return(LR) 
}
#X<-replicate(1000,fun1(20,30,25,40,30,1,4,3,5,6,7))
#plot(X)

fun2<-function(n1,n2,n3,n4,n5,mu,l1,l2,l3,l4,l5)
{
  x<-replicate(1000,fun1(n1,n2,n3,n4,n5,mu,l1,l2,l3,l4,l5))
  y<-sort(x,decreasing=FALSE)
  c<-y[50]
  return(c)
}
fun3<-function(n1,n2,n3,n4,n5,mu1,mu2,mu3,mu4,mu5,l1,l2,l3,l4,l5)
{
  g1=rig(n1,mu1,1/l1);g2=rig(n2,mu2,1/l2);g3=rig(n3,mu3,1/l3)
  g4=rig(n4,mu4,1/l4);g5=rig(n5,mu5,1/l5)
  m1=mean(g1);m2=mean(g2);m3=mean(g3);m4=mean(g4);m5=mean(g5)
  p1=sum(g1);p2=sum(g2);p3=sum(g3);p4=sum(g4);p5=sum(g5)
  q1=sum(1/g1);q2=sum(1/g2);q3=sum(1/g3);q4=sum(1/g4);q5=sum(1/g5)
  imu=(n1*m1+n2*m2+n3*m3+n4*m4+n5*m5)/(n1+n2+n3+n4+n5)
  is1=n1/(sum(q1-1/m1));is2=n2/(sum(q2-1/m2));is3=n3/(sum(q3-1/m3))
  is4=n4/(sum(q4-1/m4));is5=n5/(sum(q5-1/m5))
  barx=(n1*m1*is1+n2*m2*is2+n3*m3*is3+n4*m4*is4+n5*m5*is5)/(n1*is1+n2*is2+n3*is3+n4*is4+n5*is5)
  x13=imu
  t1=is1;t2=is2;t3=is3;t4=is4;t5=is5
  repeat
  {
    nmu=(n1*m1*t1+n2*m2*t2+n3*m3*t3+n4*m4*t4+n5*m5*t5)/(n1*t1+n2*t2+n3*t3+n4*t4+n5*t5)
    ns1=(n1*nmu^2)/sum((g1-nmu)^2/g1);ns2=(n2*nmu^2)/sum((g2-nmu)^2/g2)
    ns3=(n3*nmu^2)/sum((g3-nmu)^2/g3);ns4=(n4*nmu^2)/sum((g4-nmu)^2/g4)
    ns5=(n5*nmu^2)/sum((g5-nmu)^2/g5)
    D=abs(x13-nmu)
    D1=max(D)
    if(D1<=0.000001)
    {
      break
    }
    x13=nmu
    t1=ns1;t2=ns2;t3=ns3;t4=ns4;t5=ns5
  }
  x<-c(m1, m2, m3, m4, m5)
  s1<-sum(1/g1)/n1-1/m1;s2<-sum(1/g2)/n2-1/m2;s3<-sum(1/g3)/n3-1/m3
  s4<-sum(1/g4)/n4-1/m4;s5<-sum(1/g5)/n5-1/m5
  s<-c(s1, s2, s3, s4,s5)
  x0=x
  s0=s
  x8<-x0
  repeat
  {
    w1=c(n1/s0[1],n2/s0[2],n3/s0[3],n4/s0[4],n5/s0[5])
    x6=pava(x0, w1)
    s11<-(sum((g1-x6[1])^2/g1))/(n1*x6[1]^2)
    s12<-(sum((g2-x6[2])^2/g2))/(n2*x6[2]^2)
    s13<-(sum((g3-x6[3])^2/g3))/(n3*x6[3]^2)
    s14<-(sum((g4-x6[4])^2/g4))/(n4*x6[4]^2)
    s15<-(sum((g5-x6[5])^2/g5))/(n5*x6[5]^2)
    v=c(s11, s12, s13, s14, s15)
    s0<-v
    x10=abs(x8-x6)
    p=max(x10[1], x10[2], x10[3], x10[4], x10[5], na.rm = FALSE)
    if(p<=0.0000001)
    {
      break
    }
    #rm(x8)
    x8<-x6
  }
  LR=((t1*s0[1])^(n1/2))*((t2*s0[2])^(n2/2))*((t3*s0[3])^(n3/2))*((t4*s0[4])^(n4/2))*((t5*s0[5])^(n5/2))
  out<-fun2(n1,n2,n3,n4,n5,barx,1/s1,1/s2,1/s3,1/s4,1/s5)
  a=0
  if(LR<out)
    a=a+1
  return(a) 
}
fun4<-function(n1,n2,n3,n4,n5,mu1,mu2,mu3,mu4,mu5,l1,l2,l3,l4,l5)
{
  out<-replicate(1000,fun3(n1,n2,n3,n4,n5,mu1,mu2,mu3,mu4,mu5,l1,l2,l3,l4,l5))
  p<-sum(out)/1000
  return(p)
}

##Size values of Table 7.5 

X<-replicate(5,fun4(5,5,5,5,5,1,1,1,1,1,1,1,1,1,1))
X;mean(X)
X<-replicate(5,fun4(5,5,5,5,5,1,1,1,1,1,1,1,5,5,5))
X;mean(X)
X<-replicate(5,fun4(5,5,5,5,5,1,1,1,1,1,1,1,5,10,10))
X;mean(X)
X<-replicate(5,fun4(5,5,5,5,5,1,1,1,1,1,1,1,5,1,10))
X;mean(X)
X<-replicate(5,fun4(5,5,5,5,5,1,1,1,1,1,10,10,10,10,10))
X;mean(X)

X<-replicate(5,fun4(5,5,5,5,5,5,5,5,5,5,1,1,1,1,1))
X;mean(X)
X<-replicate(5,fun4(5,5,5,5,5,5,5,5,5,5,1,1,5,5,5))
X;mean(X)
X<-replicate(5,fun4(5,5,5,5,5,5,5,5,5,5,1,1,5,10,10))
X;mean(X)
X<-replicate(5,fun4(5,5,5,5,5,5,5,5,5,5,1,1,5,1,10))
X;mean(X)
X<-replicate(5,fun4(5,5,5,5,5,5,5,5,5,5,10,10,10,10,10))
X;mean(X)

X<-replicate(5,fun4(5,5,5,5,5,10,10,10,10,10,1,1,1,1,1))
X;mean(X)
X<-replicate(5,fun4(5,5,5,5,5,10,10,10,10,10,1,1,5,5,5))
X;mean(X)
X<-replicate(5,fun4(5,5,5,5,5,10,10,10,10,10,1,1,5,10,10))
X;mean(X)
X<-replicate(5,fun4(5,5,5,5,5,10,10,10,10,10,1,1,5,1,10))
X;mean(X)
X<-replicate(5,fun4(5,5,5,5,5,10,10,10,10,10,10,10,10,10,10))
X;mean(X)

X<-replicate(5,fun4(15,10,15,10,15,1,1,1,1,1,1,1,1,1,1))
X;mean(X)
X<-replicate(5,fun4(15,10,15,10,15,1,1,1,1,1,1,1,5,5,5))
X;mean(X)
X<-replicate(5,fun4(15,10,15,10,15,1,1,1,1,1,1,1,5,10,10))
X;mean(X)
X<-replicate(5,fun4(15,10,15,10,15,1,1,1,1,1,1,1,5,1,10))
X;mean(X)
X<-replicate(5,fun4(15,10,15,10,15,1,1,1,1,1,10,10,10,10,10))
X;mean(X)

X<-replicate(5,fun4(15,10,15,10,15,5,5,5,5,5,1,1,1,1,1))
X;mean(X)
X<-replicate(5,fun4(15,10,15,10,15,5,5,5,5,5,1,1,5,5,5))
X;mean(X)
X<-replicate(5,fun4(15,10,15,10,15,5,5,5,5,5,1,1,5,10,10))
X;mean(X)
X<-replicate(5,fun4(15,10,15,10,15,5,5,5,5,5,1,1,5,1,10))
X;mean(X)
X<-replicate(5,fun4(15,10,15,10,15,5,5,5,5,5,10,10,10,10,10))
X;mean(X)

X<-replicate(5,fun4(15,10,15,10,15,10,10,10,10,10,1,1,1,1,1))
X;mean(X)
X<-replicate(5,fun4(15,10,15,10,15,10,10,10,10,10,1,1,5,5,5))
X;mean(X)
X<-replicate(5,fun4(15,10,15,10,15,10,10,10,10,10,1,1,5,10,10))
X;mean(X)
X<-replicate(5,fun4(15,10,15,10,15,10,10,10,10,10,1,1,5,1,10))
X;mean(X)
X<-replicate(5,fun4(15,10,15,10,15,10,10,10,10,10,10,10,10,10,10))
X;mean(X)


X<-replicate(5,fun4(30,20,30,20,30,1,1,1,1,1,1,1,1,1,1))
X;mean(X)
X<-replicate(5,fun4(30,20,30,20,30,1,1,1,1,1,1,1,5,5,5))
X;mean(X)
X<-replicate(5,fun4(30,20,30,20,30,1,1,1,1,1,1,1,5,10,10))
X;mean(X)
X<-replicate(5,fun4(30,20,30,20,30,1,1,1,1,1,1,1,5,1,10))
X;mean(X)
X<-replicate(5,fun4(30,20,30,20,30,1,1,1,1,1,10,10,10,10,10))
X;mean(X)

X<-replicate(5,fun4(30,20,30,20,30,5,5,5,5,5,1,1,1,1,1))
X;mean(X)
X<-replicate(5,fun4(30,20,30,20,30,5,5,5,5,5,1,1,5,5,5))
X;mean(X)
X<-replicate(5,fun4(30,20,30,20,30,5,5,5,5,5,1,1,5,10,10))
X;mean(X)
X<-replicate(5,fun4(30,20,30,20,30,5,5,5,5,5,1,1,5,1,10))
X;mean(X)
X<-replicate(5,fun4(30,20,30,20,30,5,5,5,5,5,10,10,10,10,10))
X;mean(X)

X<-replicate(5,fun4(30,20,30,20,30,10,10,10,10,10,1,1,1,1,1))
X;mean(X)
X<-replicate(5,fun4(30,20,30,20,30,10,10,10,10,10,1,1,5,5,5))
X;mean(X)
X<-replicate(5,fun4(30,20,30,20,30,10,10,10,10,10,1,1,5,10,10))
X;mean(X)
X<-replicate(5,fun4(30,20,30,20,30,10,10,10,10,10,1,1,5,1,10))
X;mean(X)
X<-replicate(5,fun4(30,20,30,20,30,10,10,10,10,10,10,10,10,10,10))
X;mean(X)


X<-replicate(5,fun4(30,5,40,8,7,1,1,1,1,1,1,1,1,1,1))
X;mean(X)
X<-replicate(5,fun4(30,5,40,8,7,1,1,1,1,1,1,1,5,5,5))
X;mean(X)
X<-replicate(5,fun4(30,5,40,8,7,1,1,1,1,1,1,1,5,10,10))
X;mean(X)
X<-replicate(5,fun4(30,5,40,8,7,1,1,1,1,1,1,1,5,1,10))
X;mean(X)
X<-replicate(5,fun4(30,5,40,8,7,1,1,1,1,1,10,10,10,10,10))
X;mean(X)

X<-replicate(5,fun4(30,5,40,8,7,5,5,5,5,5,1,1,1,1,1))
X;mean(X)
X<-replicate(5,fun4(30,5,40,8,7,5,5,5,5,5,1,1,5,5,5))
X;mean(X)
X<-replicate(5,fun4(30,5,40,8,7,5,5,5,5,5,1,1,5,10,10))
X;mean(X)
X<-replicate(5,fun4(30,5,40,8,7,5,5,5,5,5,1,1,5,1,10))
X;mean(X)
X<-replicate(5,fun4(30,5,40,8,7,5,5,5,5,5,10,10,10,10,10))
X;mean(X)

X<-replicate(5,fun4(30,5,40,8,7,10,10,10,10,10,1,1,1,1,1))
X;mean(X)
X<-replicate(5,fun4(30,5,40,8,7,10,10,10,10,10,1,1,5,5,5))
X;mean(X)
X<-replicate(5,fun4(30,5,40,8,7,10,10,10,10,10,1,1,5,10,10))
X;mean(X)
X<-replicate(5,fun4(30,5,40,8,7,10,10,10,10,10,1,1,5,1,10))
X;mean(X)
X<-replicate(5,fun4(30,5,40,8,7,10,10,10,10,10,10,10,10,10,10))
X;mean(X)

