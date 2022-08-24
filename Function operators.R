# Header 1 ====
## Header 2 ====
### Header 3 ====

# Prerequisites ====
library(purrr)
library(memoise)

# Def: function operator is a function that takes one (or more) functions as input and returns a function as output. 

chatty <- function(f) {
  force(f)
  
  function(x, ...) {
    res <- f(x, ...)
    cat("Processing ", x, "\n", sep = "")
    res
  }
}

f <- function(x) x^2
s <-  c(3,2,1)

purrr::map_dbl(s, chatty(f))

# Existing function operators ====
## Capturing errors with purrr::safely()
x <-  list(
   c(0.512, 0.165, 0.717),
   c(0.064, 0.781, 0.427),
   c(0.890, 0.785, 0.495),
   "oops"
 )


out <-  rep(NA_real_, length(x))

for (i in seq_along(x)) {
  out[[i]] <- sum(x[[i]])
}

out

map_dbl(x, sum)

safe_sum <- safely(sum)
out <- transpose(map(x,safely(sum)))
str(out)

ok <- map_lgl(out$error, is.null)
ok
x[!ok]
out$result[ok]
