#################################################
#           MEASURING PERFORMANCE               #
#################################################


# Header 1 ====
## Header 2 ====
### Header 3 ====


# Prerequisites ====
library(profvis)
library(bench)

# Profiling ====

f <- function() {
  pause(0.1)
  g()
  h()
}

g <- function() {
  pause(0.1)
  h()
}

h <- function() {
  pause(0.1)
}

tmp <- tempfile()
Rprof(tmp, interval = 0.1)
f()
Rprof(NULL)
writeLines(readLines(tmp))

## Visualising Profiles ====
source("profiling-example.R")
profvis(f())

## Memory profiling
x <- integer()
for (i in 1:1e4) {
  x <- c(x,i)
}


# Microbenchmarking ====
 x <- runif(100)
(lb <- bench::mark(
  sqrt(x),
  x^0.5
))

plot(lb)
lb[c("expression", "min", "median", "itr/sec", "n_gc")]




