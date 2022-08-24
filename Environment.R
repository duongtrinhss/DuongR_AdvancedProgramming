#################################################
#                   ENVIRONMENT                 #
#################################################


# Header 1 ====
## Header 2 ====
### Header 3 ====


# Basic ====
library(rlang)

e1 <- env(
  a = FALSE,
  b = "a", 
  c = 2.3,
  d = 1:3
)

typeof(e1)
e1

e1$d <- e1

env_print(e1)
env_print(e1$d)

env_names(e1)

## Important Environments ====
global_env()
current_env()
identical(global_env(), current_env())


## Parents ====
e2a <- env(d = 4, e = 5)
e2b <- env(e2a, a = 1, b = 2, c = 3)

env_parents(e2a)
env_parents(e2b)

e2c <- env(empty_env(), d = 4, e = 5)
env_parents(e2c)


env_parents(e2b, last = empty_env())


## Supper assignment ====
x <- 0
f <- function() {
  x <<- 1
}
f()
x


# Recursing over environments ====

where <- function(name, env = caller_env()) {
  if (identical(env, empty_env())) {
    # Base case
    stop("Can't find", name, call. = FALSE)
  } else if (env_has(env, name)) {
    # Success case
    env
  } else {
    # Recursive case
    where(name, env_parent(env))
  }
}

caller_env()

where("e1")
where("mean")


# Special environments ====
search()
search_envs()
env_parents(base_env())
env_parents(global_env())

## Function environment ====

## Namespaces ====
sd

## Execution environments ====
g <- function(x) {
  if (!env_has(current_env(), "a")) {
    message("Defining a")
    a <- 1
  } else {
    a <- a + 1
  }
  a
}

g(10)

h <- function(x) {
  # 1
  a <- 2 #2
  x + a
}

y <-  h(1) # 3


h2 <- function(x) {
  a <- x*2
  current_env()
}


e <- h2(x = 10)
env_print(e)
fn_env(h2)

# 5 Call stacks ====

f <-  function(x) {
  g(x = 2)
}

g <- function(x) {
  h(x = 3)
}
  
h <- function(x) {
  stop()
}


f(x = 1)

traceback()

h <- function(x) {
  lobstr::cst()
}

f(x = 1)


a <- function(x) b(x)
b <- function(x) c(x)
c <- function(x) x
a(f())

## Frames ====


## Dynamic scope ====


# 6 As data structures ====

