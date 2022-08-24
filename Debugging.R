#################################################
#                     DEBUGGING                 #
#################################################

# Header 1 ====
## Header 2 ====
### Header 3 ====


# Locating errors ====

f <- function(a) g(a)
g <- function(b) h(b)
h <- function(c) i(c)
i <- function(d) {
  if (!is.numeric(d)) {
    stop("'d' must be numeric", call. = FALSE)
  }
  d + 10
}

f("a")

# Lazy evaluation ====

j <- function() k()
k <- function() stop("Oops!", call. = FALSE)
f(j())

rlang::abort(f(j()))

rlang::last_trace()
rlang::with_abort(f(j()))


# Interative debugger ====

g <- function(b) {
  browser()
  h(b)
}
g(10)
h(10)

help("browser")








