#===============================================#
#                         RCPP                  #
#===============================================#
library(Rcpp)
library(inline)
src <- '
  Function sort(x) ;
  return sort ( y, Named("decreasing", true));
'

fun <- cxxfunction(signature(x="function",
                             y="ANY"),
                   src, plugin = "Rcpp")

fun(sort, sample(1:5, 10, TRUE))

fun(sort, sample(LETTERS[1:5], 10, TRUE))



src <- '
  RNGScope scp;
  Rcpp::Function rt("rt");
  return rt(5,3);
'

fun <- cxxfunction(signature(),
                   src, plugin = "Rcpp")

set.seed(42)
fun() # rt(5,3)
fun()


# bisection method to find x such that f[x]==y for an increasing f
bisc <- function (f, x0, x1, tol = 1e-6,  itmax = 1000) {
  it <- 1
  f0 <- f(x0)
  f1 <- f(x1)
  mid <- (x0+x1)/2
  fmid <- f(mid)
  if (f0*f1 > 0) {
    return("Incorrect Initial Guesses.")
  } else {
    # print("****************")
    # print("Bisection Method")
    # print("****************")
    while ((it<itmax)&(abs(fmid)>tol)) {
      # print(paste0("Interation-",it,": x = ", mid, "and f(x) = ", fmid))
      if (fmid*f0 <0) x1 <- mid else x0 <- mid
      mid <- (x0+x1)/2
      fmid <- f(mid)
      it <- it + 1
    }
    return(mid)
  }
}  


f <- function(x) 
{
  cos(x) - x * exp(x)
}

bisc(f, x0 = 0, x1 = 1)
bisc(f, x0 = -1, x1 = 0)



library("Rcpp")
library("inline")

sourceCpp("/Users/duongtrinh/Dropbox/FIELDS/Data Science/R_Data Science/R Practice/CPPDuong/HelloWorld/bisc_rcpp.cpp")

bisc_rcpp(f, x0 = -1, x1 = 0)
bisc_rcpp(f, x0 = 0, x1 = 1)

uniroot(f, c(0,1))

library(microbenchmark)
library(bench)
microbenchmark(sol_R  = bisc(f, x0 = -1, x1 = 0), sol_Rcpp = bisc_rcpp(f, x0 = -1, x1 = 0))
microbenchmark(sol_R  = bisc(f, x0 = 0, x1 = 1), sol_Rcpp = bisc_rcpp(f, x0 = 0, x1 = 1), sol_Runi = uniroot(f, c(0,1)))
bench::mark(sol_R  = bisc(f, x0 = 0, x1 = 1), sol_Rcpp = bisc_rcpp(f, x0 = 0, x1 = 1), sol_Runi = uniroot(f, c(0,1))$root)



src <- '
  Rcpp::List input(data);
  Rcpp::Function f(fun);
  Rcpp::List output(input.size());
  std::transform(input.begin(), input.end(), output.begin(),f);
  output.names() = input.names();
  return output;
'

cpp_lapply <- cxxfunction(signature(data = "list", fun = "function"),
                          src, plugin = "Rcpp")
cpp_lapply(faithful, summary)
lapply(faithful, summary)

bench::mark(use_R  = lapply(faithful, summary), use_Rcpp = cpp_lapply(faithful, summary))


# Fibonacci Example ====

fibR <- function(n) {
  if (n == 0) return (0)
  if (n == 1) return (1)
  return (fibR(n-1) + fibR(n-2))
}

incltxt <- '
int fibonacci(const int x) {
    if (x == 0) return(0);
    if (x == 1) return(1);
    return (fibonacci(x-1) + fibonacci(x - 2));
}'

fibRcpp <- cxxfunction(signature(xs = "int"),
                       plugin = "Rcpp",
                       incl = incltxt,
                       body = '
                       int x = Rcpp::as<int>(xs);
                       return Rcpp::wrap( fibonacci(x) );
                       ')

library(Rcpp)
sourceCpp("/Users/duongtrinh/Dropbox/FIELDS/Data Science/R_Data Science/R Practice/CPPDuong/Fibonacci.cpp")

fibRcpp2 <- fibonacci

fibRcpp3 <- cppFunction('
int fibonacci(const int x) {
    if (x < 2)
        return x;
    else
    return (fibonacci(x-1) + fibonacci(x - 2));
}
')

bench::mark(fibR = fibR(20), fibRcpp = fibRcpp(20), fibRcpp2 = fibRcpp2(20), fibRcpp3 = fibRcpp3(20))


# memoization solution
mfibR <- local({
  memo <-  c(1,1, rep(NA, 1000))
  f <-  function(x) {
    if (x == 0) return(0)
    if (x < 0) return(NA)
    if (x > length(memo))
      stop("x too big for implementation")
    if (!is.na(memo[x])) return(memo[x])
    ans <- f(x-2) + f(x=1)
    memo[x] <<- ans
    ans
  }
})

mfibR(4)
fibR(20)



