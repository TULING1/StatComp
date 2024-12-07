## -----------------------------------------------------------------------------
n<-30
n_sim<-10000
skew_value<-numeric(n_sim)
for(i in 1:n_sim){
  sample_data<-rnorm(n)
  skew_value[i]<-sum((sample_data-mean(sample_data))^3)*(n^0.5)/((sd(sample_data)*(n-1)^0.5)^3)
}
quantiles<-quantile(skew_value,probs=c(0.025,0.05,0.95,0.975))
standard_error<-sqrt(6/n)
cat("estimate quantiles of skewness: \n")
print(quantiles)
cat("standard error:\n")
print(standard_error)
theory_quantiles<-qnorm(c(0.025,0.05,0.95,0.975),mean=0,sd=(6/n)^0.5)
cat("theoretical quantiles of large sample approximation:\n")
print(theory_quantiles)

## -----------------------------------------------------------------------------
count5test=function(x,y){
  X<-x-mean(x)
  Y<-y-mean(y)
  outx<-sum(X>max(Y))+sum(X<min(Y))
  outy<-sum(Y>max(X))+sum(Y<min(X))
  return(as.integer(max(c(outx,outy))>5))
}
sigma1 <- 1
sigma2 <- 1.5
alpha<-0.055
num<-c(5,20,100)
m<-1000
power_five<-power_F<-numeric(3)
count_five_power<-F_test_power<-numeric(3)
for(i in 1:3){
  count_five_reject<-0
  f_test_reject<-0
  for(j in 1:m){
    x<-rnorm(num[i], 0, sigma1)
    y<-rnorm(num[i], 0, sigma2)
    
    count_five_reject<-count_five_reject+count5test(x,y)
    f_test_reject<-f_test_reject+(var.test(x,y)$p.value<alpha)
  }
  count_five_power[i]<-count_five_reject/m
  F_test_power[i]<-f_test_reject/m
}
rbind(count_five_power,F_test_power)

