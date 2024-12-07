## -----------------------------------------------------------------------------
library(Rcpp)
library(StatComp)
# 参数设置
n = 100
a = 10

# 运行R和Rcpp版本

x_r = gibbsR(n, a)
x_cpp = gibbsC(n, a)

qqplot(x_r[,1], x_cpp[,1], main = "QQ Plot for X", 
       xlab = "R Gibbs Samples", ylab = "Rcpp Gibbs Samples")
abline(0, 1, col = "red")

qqplot(x_r[,2], x_cpp[,2], main = "QQ Plot for Y", 
       xlab = "R Gibbs Samples", ylab = "Rcpp Gibbs Samples")
abline(0, 1, col = "red")

library(microbenchmark)

benchmark_results = microbenchmark(
  R_version = gibbsR(n, a),
  Rcpp_version = gibbsC(n, a),
  times = 10
)

print(benchmark_results)


