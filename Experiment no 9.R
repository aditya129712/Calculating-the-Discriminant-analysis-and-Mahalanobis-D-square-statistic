#The R-programming to obtain the solution for the given problem-
  x1=c(65,66,63,60,60,62,71,60,68,71,67,76,64,70,60,66,57,68,67,84,67,89,66,69,80,65,69,70,70,87)
  x1
  dim(x1)=c(10,3)
  dim(x1)
  x2=c(75,56,71,66,72,64,59,66,71,68,64,65,70,65,75,72,75,69,65,65,54,70,72,86,72,68,90,80,80,71,74,76,75,72,73,90)
  x2
  dim(x2)=c(12,3)
  dim(x2)
  m1=mat.or.vec(3,1)
  m2=mat.or.vec(3,1)
  for(i in 1:3){
    m1[i]=mean(x1[,i])
    m2[i]=mean(x2[,i])}
  mean=array(c(m1-m2),dim=c(3,1))
  mean
  n1=10
  n2=12
  p=3
  var11=mat.or.vec(3,1)
  var12=mat.or.vec(3,1)
  var21=mat.or.vec(3,1)
  var22=mat.or.vec(3,1)
  var31=mat.or.vec(3,1)
  var32=mat.or.vec(3,1)
  for(i in 1:3){
    var11[i]=cov(x1[,1],x1[,i])*((n1-1)/(n1+n2-2))
    var12[i]=cov(x2[,1],x2[,i])*((n2-1)/(n1+n2-2))}
  for(i in 1:3){
    var21[i]=cov(x1[,2],x1[,i])*((n1-1)/(n1+n2-2))
    var22[i]=cov(x2[,2],x2[,i])*((n2-1)/(n1+n2-2))}
  for(i in 1:3){
    var31[i]=cov(x1[,2],x1[,i])*((n1-1)/(n1+n2-2))
    var32[i]=cov(x2[,2],x2[,i])*((n2-1)/(n1+n2-2))}
  s1=c(var11,var21,var31)
  s1
  dim(s1)=c(3,3)
  dim(s1)
  s2=c(var12,var22,var32)
  s2
  dim(s2)=c(3,3)
  dim(s2)
  s_p=s1+s2
  s_p
  d2=t(mean)%*%solve(s_p)%*%mean
  d2
  cal=((n1+n2-p-1)/(p*(n1+n2-2)))*((n1*n2)/(n1+n2))*d2
  cal
  tab=qf(0.95,3,18,0)
  tab
  
  