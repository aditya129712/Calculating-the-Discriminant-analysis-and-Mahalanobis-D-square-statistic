#The R-programming to obtain the solution for the given problem-
  
  x1=c(12,45,44,52,8,39,71,38,38,47,34,56,35,63,32,48,84,57,51,62)
  dim(x1)=c(10,2)
  x2=c(56,67,49,89,58,53,56,78,64,58,54,66,72,97,76,32,81,98,78,40)
  dim(x2)=c(10,2)
  dim(x2)
  mean1=mat.or.vec(2,1)
  mean2=mat.or.vec(2,1)
  for(i in 1:2){
    mean1[i]=mean(x1[,i])
    mean2[i]=mean(x2[,i])}
  mean=array(c(mean1-mean2),dim=c(2,1))
  mean
  n1=10
  n2=10
  var11=mat.or.vec(2,1)
  var12=mat.or.vec(2,1)
  var21=mat.or.vec(2,1)
  var22=mat.or.vec(2,1)
  for(i in 1:2){
    var11[i]=cov(x1[,1],x1[,i])*((n1-1)/(n1+n2-2))
    var12[i]=cov(x2[,1],x2[,i])*((n2-1)/(n1+n2-2))}
  for(i in 1:2){
    var21[i]=cov(x1[,2],x1[,i])*((n1-1)/(n1+n2-2))
    var22[i]=cov(x2[,2],x2[,i])*((n2-1)/(n1+n2-2))}
  s1=c(var11,var21)
  s1
  dim(s1)=c(2,2)
  dim(s1)
  s2=c(var12,var22)
  s2
  dim(s2)=c(2,2)
  dim(s2)
  s_p=s1+s2
  s_p
  D2=t(mean)%*%solve(s_p)%*%mean
  D2
  p=2
  cal_value=((n1+n2-p-1)/(p*(n1+n2-2)))*((n1*n2)/(n1+n2))*D2
  cal_value
  tab_value=qf(0.95,2,17,0)
  tab_value
  x0=array(c(52,70),dim=c(2,1))
  x0
  y0=t(mean)%*%solve(s_p)%*%x0
  y0
  m=D2/2
  m
  