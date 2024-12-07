## -----------------------------------------------------------------------------
N<-1000
n_null<-950
n_alt<-50
alpha<-0.1
m<-1e4

results<-matrix(0, nrow = 3, ncol = 2)
rownames(results)<-c("FWER", "FDR", "TPR")
colnames(results)<-c("Bonferroni correction", "B-H correction")

for(i in 1:m){
  p_null<-runif(n_null)
  p_alt<-rbeta(n_alt,0.1,1)
  p_value<-c(p_null,p_alt)
  
  bonf_p<-p.adjust(p_value,method = "bonferroni")
  bh_p<-p.adjust(p_value,method="BH")
  
  reject_bonf<-bonf_p<alpha
  true_positive_bonf<-sum(reject_bonf[(n_null+1):N])
  false_positive_bonf<-sum(reject_bonf[1:n_null])
  fwer_bonf<-ifelse(false_positive_bonf>0,1,0)
  fdr_bonf<-ifelse(sum(reject_bonf)>0,false_positive_bonf/sum(reject_bonf),0)
  tpr_bonf<-true_positive_bonf/n_alt
  
  reject_bh<-bh_p<alpha
  true_positive_bh<-sum(reject_bh[(n_null+1):N])
  false_positive_bh<-sum(reject_bh[1:n_null])
  fwer_bh<-ifelse(false_positive_bh>0,1,0)
  fdr_bh<-ifelse(sum(reject_bh)>0,false_positive_bh/sum(reject_bh),0)
  tpr_bh<-true_positive_bh/n_alt
  
  results["FWER", "Bonferroni correction"]<-results["FWER", "Bonferroni correction"]+fwer_bonf
  results["FDR", "Bonferroni correction"]<-results["FDR", "Bonferroni correction"]+fdr_bonf
  results["TPR", "Bonferroni correction"]<-results["TPR", "Bonferroni correction"]+tpr_bonf
  
  results["FWER", "B-H correction"]<-results["FWER", "B-H correction"]+fwer_bh
  results["FDR", "B-H correction"]<-results["FDR", "B-H correction"]+fdr_bh
  results["TPR", "B-H correction"]<-results["TPR", "B-H correction"]+tpr_bh
}
result<-results/m
print(round(result,4))

## -----------------------------------------------------------------------------
library(boot)
set.seed(111)
hours<-c(3,5,7,18,43,85,91,98,100,130,230,487)
n<-12
B<-1000

mle=function(value,indices){
  return(length(value[indices])/sum(value[indices]))
}
lambda_hat<-mle(hours)
lambda_b<-boot(data = hours,statistic = mle,R = B)

round(c(bias=mean(lambda_b$t)-lambda_hat,standard_error=sd(lambda_b$t)),4)

## -----------------------------------------------------------------------------
set.seed(111)

mle2=function(value,indices){
  return(sum(value[indices])/length(value[indices]))
}
lambda_b_back<-boot(data = hours,statistic = mle2,R = B)
bootstrap_ci <- boot.ci(lambda_b_back,conf =0.95, type = c("norm","basic", "perc", "bca"))

print(bootstrap_ci)

