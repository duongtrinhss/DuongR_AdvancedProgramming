# Header 1 ====
## Header 2 ====
### Header 3 ====

# Prerequisites ====
library(rlang)
library(ggplot2)
library(scales)

# Def: A function factory is a function that makes functions. 

# Factory fundamentals ====
power1 <- function(exp) {
   function(x) {
     x ^ exp
   }
 }

 
square <- power1(2)
cube <-  power1(3)

square
cube

env_print(square)
env_print(cube)

fn_env(square)$exp
fn_env(cube)$exp

## Forcing evaluation ====
power1
x <- 2
square <-  power1(x)
x <- 3
square(2) # return 2^3


power2 <- function(exp) {
  force(exp)
  function(x) {
    x ^ exp
  }
}

x <- 2
square <-  power2(x)
x <- 3
square(2) # return 2^2


## Stateful functions ====
new_counter <-  function() {
  i <- 0
  
  function() {
    i <<-  i + 1 # super assigment operator to rebinds an existing name found in a parent environment
    i
  }
}

counter_one <- new_counter()
counter_two <- new_counter()

counter_one()
counter_two()
# >> they are independent counts

## Garbage collection ====
f1 <- function(n) {
  x <- runif(n)
  m <- mean(x)
  function() m
}

library(lobstr)
g1 <- f1(1e6)
lobstr::obj_size(g1)


f2 <- function(n) {
  x <- runif(n)
  m <- mean(x)
  rm(x)
  function() m
}

g2 <- f2(1e6)
lobstr::obj_size(g2)


# Graphical factories ====



# Statistical factories ====
## Box-Cox transformation ====

# Consider first a simple two argument function
boxcox <- function(x, lambda) {
  stopifnot(length(lambda) == 1)
  
  if (lambda == 0) {
    log(x)
  } else {
    (x ^ lambda - 1)/ lambda
  }
}

# Reformulate as a function factory 
boxcox2 <- function(lambda) {
  if (lambda == 0) {
    function(x) log(x)
  } else {
    function(x) (x ^ lambda - 1) / lambda
  }
}

stat_boxcox <-  function(lambda) {
  stat_function(aes(colour = lambda), fun = boxcox2(lambda), size = 1)
}


ggplot(data.frame(x = c(0, 5)), aes(x)) + 
  lapply(c(0.5, 1, 1.5), stat_boxcox) +
  scale_color_viridis_c(limits = c(0, 1.5))

ggplot(data.frame(x = c(0.01, 1)), aes(x)) + 
  lapply(c(0.5, 0.25, 0.1, 0), stat_boxcox) +
  scale_color_viridis_c(limits = c(0, 1.5))

## Boostrap resampling ====

boot_permute <-  function(df, var) {
  n <- nrow(df)
  force(var)
  
  function() {
    col <-  df[[var]]
    col[sample(n, replace = TRUE)]
  }
}

boot_mtcars1 <- boot_permute(mtcars, "mpg")
head(boot_mtcars1())
head(boot_mtcars1())

boot_model <- function(df, formula) {
  mod <-  lm(formula, data = df)
  fitted <-  unname(fitted(mod))
  resid <-  unname(resid(mod))
  rm(mod)
  
  function() {
    fitted + sample(resid)
  }
}

boot_mtcars2 <- boot_model(mtcars, mpg ~ wt)
head(boot_mtcars2())
head(boot_mtcars2())

## Maximum likelihood estimation ====


# Function factories and functionals
names <-  list(
  square = 2,
  cube = 3,
  root = 1/2,
  cuberoot = 1/3,
  reciprocal = -1
)

funs <-  purrr::map(names, power1)

funs$root(64)
funs$root

# Three alternative ways
with(funs, root(64))

attach(funs)
roor(64)
detach(funs)

rlang::env_bind(globalenv(), !!!funs)
root(64)
rlang::env_unbind(globalenv(), names(funs))











