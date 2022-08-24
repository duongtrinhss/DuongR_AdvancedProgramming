#===============================================#
#                  RcppArmadillo                #
#===============================================#


# db <- tools::CRAN_package_db()
# db <- db[!duplicated(db[,1]),]  # rows: nb of pkgs,
# nTot <- nrow(db)
# nRcpp <- length(tools::dependsOnPkgs("Rcpp", recursive=FALSE, installed=db))
# nCompiled <- table(db[, "NeedsCompilation"])[["yes"]]
# propRcpp <- nRcpp / nCompiled * 100
# data.frame(tot=nTot, totRcpp = nRcpp, totCompiled = nCompiled,
#            RcppPctOfCompiled = propRcpp)

#install.packages("RcppArmadillo")
library(Rcpp)
library(RcppArmadillo)
library(tictoc)

# Test ====
Rcpp::sourceCpp(code = '
#include <RcppArmadillo.h>
#ifdef _OPENMP
# include <omp.h>
#endif

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
void omp_test()
{
#ifdef _OPENMP
    Rprintf("OpenMP threads available: %d\\n", omp_get_max_threads());
#else
    Rprintf("OpenMP not supported\\n");
#endif
}
')
omp_test()

# Linear Regression ====
sourceCpp("/Users/duongtrinh/Dropbox/FIELDS/Data Science/R_Data Science/R Practice/CPPDuong/RcppArmadillo.cpp")
help("fastLm")

vec <- rnorm(1000)

inner1(vec, vec)
inner2(vec, vec)

a1(diag(2))
a2(1:5)
a3(diag(2))
a4(diag(2))
a5(matrix(0, nrow = 2, ncol = 3))
a7(diag(2))


a8(2,3,3/4)
a9(2,3)

Z <- matrix(rnorm(6),2,3)
Z

a10(Z,1,2)
a11(Z,1)
a13(Z)

str(mtcars) 

y <- mtcars$mpg
X <- as.matrix(mtcars[,-1])
str(X)
str(y)
fit1 <- lm(y ~ 0+X)
fit2 <- fastLM(X,y)

install.packages("inline")
library(inline)
g <- cxxfunction(signature(vs="numeric"),
                 plugin = "RcppArmadillo",
                 body = '
                 arma::vec v = Rcpp::as<arma::vec>(vs);
                 arma::mat op = v*v.t();
                 double ip = arma::as_scalar(v.t()*v);
                 return Rcpp::List::create(Rcpp::Named("outer") = op,
                                           Rcpp::Named("inner") = ip);
')

g(7:11)

help(cxxfunction)


# An improved Kalman filter implemented in R ====
kalmanfilter <- function(pos) {
  
  kalmanfilter <- function(z) {
    ## predicted state and covariance
    xprd <- A %*% xest
    pprd <- A %*% pest %*% t(A) + Q
    
    S <-  H %*% t(pprd) %*% t(H) + R
    B <-  H %*% t(pprd)
    kalmangain <- t(solve(S,B))
    
    ## estimated state and covariance, assign to vars in parent env
    xest <<- xprd + kalmangain %*% (z - H%*% xprd)
    pest <<- pprd - kalmangain %*% H %*% pprd
    
    
    # compute the estiamted measurements
    y <- H %*% xest
  }
  
  dt <- 1
  A <- matrix(c(1, 0, dt, 0, 0, 0, 0, 1, 0, dt, 0, 0, # x, y
                0, 0, 1, 0, dt, 0, 0, 0, 0, 1, 0, dt, # Vx, Vy
                0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,  1), # Ax, Ay
              6, 6, byrow = TRUE)
  H <- matrix(c(1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0),
              2, 6, byrow = TRUE)
  Q <- diag(6)
  R <- 1000 * diag(2)
  
  xest <- matrix(0,6,1)
  pest <- matrix(0,6,6)
  
  N <- nrow(pos)
  y <- matrix(NA, N, 2)
  
  
  for (i in 1:N) {
    y[i,] <- kalmanfilter(t(pos[i,,drop=FALSE]))
  }
  invisible(y)
}

# BAYESIAN REGRESSION ====

## R function ====
library("matlab")
library("MCMCpack")
BayesRegrR <- function(y, X, nsave = 10000, nburn = 1000) {
  k <- ncol(X)
  n <- nrow(X)
  
  # =====| Initialize parameters
  B0 <- 1000*eye(k)
  b0 = zeros(k,1)
  alpha0 = 0.002
  delta0 = 0.002
  
  sigmasq = 1
  
  # =====| Storage space for Gibbs draws
  beta_draws = zeros(k,nsave)
  sigmasq_draws = zeros(1,nsave)
  
  #==========================================================================
  #====================| GIBBS ITERATIONS START HERE |=======================
  for (iter in 1:(nburn + nsave)) {
    #=====|Draw beta
    Bn <- solve(t(X)%*%X/sigmasq + solve(B0))
    bn <- Bn%*%(t(X)%*%y/sigmasq + solve(B0)%*%b0)
    H <- chol(Bn)
    beta <- bn + t(H)%*%matrix(rnorm(k),k,1)
    
    #=====|Draw sigma^2
    resid <- y - X%*%beta
    alphan <- alpha0 + n
    deltan <- delta0 + t(resid)%*%resid
    sigmasq <- rinvgamma(1, shape = alphan/2, scale = deltan/2)
    
    #=====|Save draws
    if (iter > nburn) {
      beta_draws[,iter-nburn] <- beta
      sigmasq_draws[,iter-nburn] <- sigmasq
    }
  }
  return(list(betadraws=beta_draws,sigmasqdraws=sigmasq_draws))
}

## RcppArmadillo function ====
sourceCpp("/Users/duongtrinh/Dropbox/FIELDS/Data Science/R_Data Science/R Practice/CPPDuong/BayesRegr.cpp")
set.seed(2107)
n = 100
k = 1
X = matrix(rnorm(n),n,1)
sigmasq_true <-  1
beta_true <- 0.5
y <- X%*%beta_true + sqrt(sigmasq_true)*rnorm(n,0,1)

set.seed(2107)
n = 100
k = 3
X = cbind(rep(1,n),rnorm(n,0,1),rbinom(n,2,0.5))
sigmasq_true <-  4
beta_true <- c(1, -1.5, 2)

y <- X%*%beta_true + sqrt(sigmasq_true)*rnorm(n,0,1)

tic()
resRcpp <- BayesRegr(y,X,nsave=10000,nburn=1000)
toc()
str(resRcpp)
beta_meansRcpp <- rowMeans(resRcpp$betadraws)
mean(resRcpp$sigmasqdraws)
plot(resRcpp$betadraws[3,])

tic()
resR <- BayesRegrR(y,X,nsave=10000,nburn=1000)
toc()
str(resR)
beta_means <- rowMeans(resR$betadraws)
mean(resR$sigmasqdraws)
plot(resR$betadraws[1,])

library(purrr)
map_dbl(as.data.frame(t(resR$betadraws)), mean)
map_dbl(as.data.frame(t(resR$betadraws)), median)


beta_OLS <-  as.vector(solve(t(X)%*%X)%*%(t(X)%*%y))

cbind(beta_true, beta_OLS, beta_means, beta_meansRcpp)

library(bench)
bench::mark(BayesRegrR = BayesRegrR(y,X,nsave=10000,nburn=1000), BayesRegrRcpp = BayesRegr(y,X,nsave=10000,nburn=1000))

library(microbenchmark)
microbenchmark(BayesRegrR = BayesRegrR(y,X,nsave=10000,nburn=1000), BayesRegrRcpp = BayesRegr(y,X,nsave=10000,nburn=1000))


# Check Armadillo functions

C <- cppFunction(depends = "RcppArmadillo",
'
  arma::mat C() {
  arma::mat A(4, 5, arma::fill::randu);
  arma::mat B(4, 5, arma::fill::randu);
  
  return A*B.t();
  }
'
)

C <- cppFunction(depends = "RcppArmadillo",
                 '
  arma::mat C() {
  arma::vec x(5, arma::fill::randu);

  return diagmat(1/x);
  }
'
)

C <- cppFunction(depends = "RcppArmadillo",
                 '
  arma::mat C() {
  arma::mat A(4,5, arma::fill::randu);

  return chol(A.t()*A, "lower");
  }
'
)

C <- cppFunction(depends = "RcppArmadillo",
                  '
  arma::rowvec C() {
  arma::colvec y(5, arma::fill::randu);

  return y.t();
  }
'
)
C()
typeof(BayesRegr)


C <- cppFunction(depends = "RcppArmadillo",
                 '
  double C() {
  arma::colvec y(5, arma::fill::randu);

  return y.n_elem;
  }
'
)

C <- cppFunction(depends = "RcppArmadillo",
                 '
  double C(arma::colvec& y) {
  return R::rgamma(1,1);
  }
'
)
C(1:3)

square(y)[0]- pow(2,2) +

C <- cppFunction(depends = "RcppArmadillo",
                 '
  NumericVector C(NumericVector y) {
  Function f("rnorm");
  return f(5, Named("sd") = 2, _["mean"] = y);
  }
'
)

set.see

rnorm(0,1)

C(c(1:3))

set.seed(123)
rgamma(3, shape = 1, scale = 1:3)

set.seed(123)
sapply(1:3, FUN = rgamma, shape = 1, n = 1)

library(purrr)
set.seed(123)
map_dbl(1:3, ~rgamma(1, shape = 1, scale = .x))


set.seed(123)
evalCpp("R::rnorm(0,1)")

set.seed(123)
rnorm(3,0,1:3)

set.seed(123)
stats::rnorm(3,0,1:3)

set.seed(123)
evalCpp("Rcpp::rnorm(3,0,1)")

Rcpp::evalCpp("sum(randg(3, arma::distr_param(1,1)))", depends = "RcppArmadillo")


sourceCpp("/Users/duongtrinh/Dropbox/FIELDS/Data Science/R_Data Science/R Practice/CPPDuong/BayesRegr.cpp")
sourceCpp("/Users/duongtrinh/Dropbox/FIELDS/Data Science/R_Data Science/R Practice/CPPDuong/BayesRegrSP.cpp")

set.seed(2107)
n = 100
k = 3
X = cbind(rep(1,n),rnorm(n,0,1),rbinom(n,2,0.5))
sigmasq_true <-  4
beta_true <- c(0, -1.5, 2)

y <- X%*%beta_true + sqrt(sigmasq_true)*rnorm(n,0,1)

tic()
resRcpp <- BayesRegr(y,X,nsave=10000,nburn=1000)
toc()

tic()
resSPRcpp <- BayesRegrSP(y,X,nsave=10000,nburn=1000, prior ="student-t")
toc()
str(resSPRcpp)
betaSP_meansRcpp <- rowMeans(resSPRcpp$betadraws)
mean(resSPRcpp$sigmasqdraws)

tic()
resSPRcpp2 <- BayesRegrSP(y,X,nsave=10000,nburn=1000, prior ="lasso")
toc()
str(resSPRcpp2)
betaSP_meansRcpp2 <- rowMeans(resSPRcpp2$betadraws)
mean(resSPRcpp2$sigmasqdraws)



