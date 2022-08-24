# Gibbs sampler for LINEAR REGRESSION model
# Independent Normal Inverse Gamma prior
# =====================================================================================                  
#
# =====================================================================================
# Written by Duong Trinh
# University of Glasgow
# This version: March 2022
# =====================================================================================
library("matlab")
setwd("/Users/duongtrinh/Dropbox/FIELDS/Data Science/R_Data Science/RDataDuong")

#==============================| LOAD DATA |===============================
df <- read.csv("fringe.csv")
str(df)
y <- as.matrix(df$hrearn)
xvar <- c(3,4,7,8,5,6,13,12)
X <- unname(as.matrix(df[,xvar]))
X <- cbind(ones(length(y),1),X)

#============================| PRELIMINARIES |=============================
n = 100
k = 1
X = matrix(rnorm(n),n,1)
sigma_true <-  1
beta_true <- 0.5
y <- X%*%beta_true + sigma_true*matrix(rnorm(n),n,1)


k <- ncol(X)
n <- nrow(X)
nburn <-  1000
nsave <-  10000

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

par(mfrow = c(1,1))
plot(beta_draws[1,])
mean(beta_draws)
plot(density(beta_draws))


# Gibbs sampler - Rcpp
# =====================================================================================                  
#
# =====================================================================================
# Written by Duong Trinh
# University of Glasgow
# This version: June 2022
# =====================================================================================
library(Rcpp)
library(microbenchmark)
library(bench)


# Gibbs sampler ====
gibbs_r <- function(N, thin) {
  mat <- matrix(nrow = N, ncol = 2)
  x <- y <- 0
  for (i in 1:N) {
    for (j in 1:thin) {
      x <- rgamma(1, 3, y * y + 4)
      y <-  rnorm(1, 1 / (x + 1), 1 / sqrt(2 * (x + 1)))
    }
    mat[i, ] <- c(x, y)
  }
  mat
}

gibbs_r(100,10)


sourceCpp("/Users/duongtrinh/Dropbox/FIELDS/Data Science/R_Data Science/R Practice/CPPDuong/gibbs_cpp.cpp")
gibbs_cpp(100,10)

bench::mark(
  gibbs_r(100,10),
  gibbs_cpp(100,10),
  check = FALSE
)

microbenchmark(
  gibbs_r(100,10),
  gibbs_cpp(100,10)
)


colMeans(gibbs_r(100,10))
colMeans(gibbs_cpp(100,10))

# R vectorisation versus C++ vectorisation ====
vacc1a <- function(age, female, ily) {
  p <- 0.25 + 0.3 * 1 / (1 -exp(0.04 * age)) + 0.1 * ily
  p <-  p * if (female) 1.25 else 0.75
  p <- max(0,p)
  p <- min(1,p)
  p
}

vacc1 <- function(age, female, ily) {
  n <- length(age)
  out <- numeric(n)
  for (i in seq_len(n)) {
    out[i] <- vacc1a(age[i], female[i], ily[i])
  }
  out
}

vacc1b <- function(age, female, ily) {
  p <- 0.25 + 0.3 * 1 / (1 -exp(0.04 * age)) + 0.1 * ily
  p <-  p * ifelse(female, 1.25, 0.75)
  p <- pmax(0,p)
  p <- pmin(1,p)
  p
}

vacc1(age, female, ily)
vacc1b(age, female, ily)


ifelse(female, 1.25, 0.75)
age == pmin(age)


sourceCpp("/Users/duongtrinh/Dropbox/FIELDS/Data Science/R_Data Science/R Practice/CPPDuong/vectorisation.cpp")
vacc3(age, female, ily)

## Data sample
n <- 1000
age <-  floor(rnorm(n, mean = 50, sd = 10))
female <- sample(c(T, F), n, rep = TRUE)
ily <- sample(c(T, F), n, prob = c(0.8, 0.2), rep = TRUE)

stopifnot(
  all.equal(vacc1(age, female, ily), vacc3(age, female, ily)),
  all.equal(vacc1b(age, female, ily), vacc3(age, female, ily))
)

## Comparison
bench::mark(
  vacc1 = vacc1(age, female, ily),
  vacc2 = vacc1b(age, female, ily),
  vacc3 = vacc3(age, female, ily)
)


# Gibbs sampler for LINEAR REGRESSION model with shrinkage priors
# Independent Normal Inverse Gamma prior
# =====================================================================================  

BayesRegr <- function(y, X, nsave, nburn, prior, option_error) {
  return(beta_hat, sigma2_hat, signals)
}


X <- matrix(runif(1:15),3,5)
p <- ncol(X)
n <- nrow(X)

# ==========| Define priors

# prior for sigma2
c <- 0.1
d <- 0.1

# prior for Lam (student-t parameter)
v <- 1

# prior for beta
Q <- 0.01*diag(rep(1,p))        # initialize prior variance for beta

if (prior >= 1 & prior <= 4) {
  prior_type <- 'CTS'# continuos priors
} else if (prior >= 5 & prior <= 13) {
  prior_type <- 'SSVS'
} else if (prior == 14) {
  prior_type <- 'NOSHRINK'
}

if (prior_type == 'CTS') {
  b0     = NaN;
  rho0   = NaN;
  tau    = NaN;
  r      = NaN;
  kappa  = NaN;
  xi     = NaN;
  nu     = NaN;
  lambda = NaN;
  
  if (prior == 1) {
    method = 'student-t';
    b0     = 0.001;
    rho0   = 1;
  } else if (prior == 2) {
    method = 'lasso';
    tau    = 0.01*rep(1,p);
    r      = 1;
    kappa  = 1;
  } else if (prior == 3) {
    method = 'horseshoe-slice';
    lambda = 0.1*rep(1,p);
    tau    = 0.1;
  } else if (prior == 4) {
    method = 'horseshoe-mixture';
    kappa = 1;
    xi    = 1;
    nu    = 1;
  }
} else if (prior_type == 'SSVS') {
  tau1 = 1*rep(1,p);
  tau0 = (1/p)*rep(1,p);
  pi0 = 0.2;
  rho0 = NaN;
  b0 = NaN;
  b1 = NaN;
  
  kappa0 = NaN;
  kappa1 = NaN;
  
  kappa = NaN;
  xi    = NaN;
  nu    = NaN; 
  
  if (prior == 5) {
    method = 'ssvs_normal';
    fvalue = dt(sqrt(2.1*log(p+1)),5);
    tau1 = max(100*tau0, pi0*tau0/((1-pi0)*fvalue));
  } else if (prior == 6) {
    method = 'ssvs_student_1';
    rho1 = 1;
    b1   = 1;
  } else if (prior == 7) {
    method = 'ssvs_student_2';
    rho0 = 1;
    b1 = 1;
  } else if (prior == 8) {
    method = 'ssvs_student_3';
    rho0 = 1;
    b0 = 0.0001;
    rho1 = 1;
    b1 = 1;
  } else if (prior == 9) {
    method = 'ssvs_lasso_1';
    kappa1 = 3;
  } else if (prior == 10) {
    method = 'ssvs_lasso_2';
    kappa1 = 3;
  } else if (prior == 11) {
    method = 'ssvs_lasso_3';
    kappa1 = 3;
  } else if (prior == 12) {
    method = 'ssvs_horseshoe_1'
    kappa = 1;
    xi = 1;
    nu = rep(1,p);
  } else if (prior == 13) {
    method = 'ssvs_horseshoe_2';
    kappa = 1;
    xi = 1;
    nu = rep(1,p);
  }
} else if (prior_type == 'NOSHRINK') {
  method = 'NoShrinkage';
  Q = 10*diag(rep(1,p))
}

# ==========| Initialize parameters
beta <- 
  
  
  # ==========| Storage space for Gibbs draws
  beta_draws   <- 
  sigma2_draws <- 
  pi0_draws    <- 
  gamma_draws  <- 
  
  
#==========================================================================
#====================| GIBBS ITERATIONS START HERE |=======================

