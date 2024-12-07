## ----warning=FALSE------------------------------------------------------------
library(lpSolve)

objective <- c(4, 2, 9)

constraints <- matrix(c(2, 1, 1,
                        1, -1, 3), 
                      nrow = 2, byrow = TRUE)

rhs <- c(2, 3)

direction <- c("<=", "<=")

solution <- lp("min", objective, constraints, direction, rhs, compute.sens = TRUE)

cat("Optimal value:", solution$objval, "\n")
cat("Optimal solution (x, y, z):", solution$solution, "\n")


## -----------------------------------------------------------------------------
formulas <- list(
 mpg ~ disp,
 mpg ~ I(1 / disp),
 mpg ~ disp + wt,
 mpg ~ I(1 / disp) + wt
 )

v1<-vector("list",4)
for (i in 1:4){
v1[i] <- lapply(i,function(i) {lm(formulas[[i]], data = mtcars)})
}

## -----------------------------------------------------------------------------
bootstraps <- lapply(1:10, function(i) {
 rows <- sample(1:nrow(mtcars), rep = TRUE)
 mtcars[rows, ]
})

v2<-vector("list",10)
for (i in 1:10){
v2[i] <- lapply(bootstraps[i], lm, formula = mpg ~ disp)
}

## -----------------------------------------------------------------------------
rsq <- function(mod) summary(mod)$r.squared
# question 3
sapply(v1, rsq)
#question 4
sapply(v2, rsq)

## -----------------------------------------------------------------------------
trials <- replicate(
         100,
         t.test(rpois(10, 10), rpois(7, 10)),
         simplify = FALSE
       )
sapply(trials, function(x) x[["p.value"]])
sapply(trials, "[[", "p.value")

## -----------------------------------------------------------------------------
chisq_test_fast <- function(x, y) {
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("Both inputs must be numeric vectors")
  }
  if (any(is.na(x)) || any(is.na(y))) {
    stop("Input vectors must not contain missing values")
  }
  n <- rbind(x, y)
  n1 <- rowSums(n)
  n2 <- colSums(n)
  k <- sum(n)
  m <- tcrossprod(n1,n2) / k

  x_stat <- sum((n - m)^2 / m)
  df <- (length(n1) - 1) * (length(n2) - 1)
  p.value <- pchisq(x_stat, df = df, lower.tail = FALSE)

  list(x_stat = x_stat, df = df, p.value = p.value)
}

## -----------------------------------------------------------------------------
library(fastmatch)
fast_table <- function(x, y) {
  if (!is.integer(x)) x <- as.integer(x)
  if (!is.integer(y)) y <- as.integer(y)
  
  a_sort <- sort(unique(a))
  b_ssort <- sort(unique(b))
  
  a_len <- length(a_sort)
  b_len <- length(b_sort)
  
  dim <- c(a_len, b_len)
  pr <- a_len * b_len
  dn <- list(a = a_sort, b = b_sort)
  
  bin <- fmatch(a, a_sort) + a_len * fmatch(b, b_sort) - a_len
  y <- tabulate(bin, pr)
  
  y <- array(y, dim = dim, dimnames = dn)
  class(y) <- "table"
  
  y
}

