#################################################
#                   FUNCTION                    #
#################################################


# Header 1 ====
## Header 2 ====
### Header 3 ====


# Function fundamentals ====

## Function components ====

f0 <- function(x,y) {
  x + y
}

formals(f0) # list of arguments
body(f0)
environment(f0) # the data structure that determines how the function finds the values associated with the names

attr(f0, "srcref") # source reference - source code

## Primitive functions ====

# Def: only found in the base package
sum
`[`

typeof(sum)
typeof(`[`)


## First-class functions ====
# R functions are objects in their own right, a language property often called “first-class functions”.

lapply(mtcars, function(x) length(unique(x)))
Filter(function(x) !is.numeric(x), mtcars)
integrate(function(x) sin(x) ^ 2, 0, pi)

## Invoking a function ====
args <- list(1:10, na.rm = TRUE) # a list containing the function arguments
do.call(mean, args)


# Lazy evaluation ====

h01 <- function(x) {
  10
}

h01(stop("this is an error!"))

## Promises ====
double <- function(x) {
  message("Calculating...")
  x * 2
}

h03 <- function(x) {
  c(x,x)
}

x <- 10

h03(double(x))
# (!) Cannot manipulate promises with R code

## Default arguments ====

h04 <- function(x = 1, y = x * 2, z = a + b) {
  a <- 10
  b <- 100
  c(x, y, z)
}
h04(x = 2, y = 3)


h05 <- function(x = ls()) {
  a <- 1
  x
}

h05() # ls() evaluated inside h05
h05(ls()) # ls() evaluated in global environment

## Missing arguements ====
missing(x)

args(sample)
x <- 1:10
sample(x)

sample <- function(x, size = NULL, replace = FALSE, prob = NULL) {
  if (is.null(size)) {
    size <-  length(x)
    
  x[sample.int(length(x), size, replace = replace, prob = prob)]
  }
}

`%||%` <-  function(lhs, rhs) {
  if (!is.null(lhs)) {
    lhs
  } else {
    rhs
  }
}

sample <- function(x, size = NULL, replace = FALSE, prob = NULL) {
  
  size <- size %||% length(x)  
  x[sample.int(length(x), size, replace = replace, prob = prob)]

}

# ... (dot-dot-dot) ====


# Exiting a function ====

## Implicit versus explicit returns ====

## Invisible values ====
j03 <- function() invisible(1)
j03() 
j03() + 2 
print(j03())
str(withVisible(j03()))

## Exit handlers ====
j06 <- function(x) {
  cat("Hello\n")
  on.exit(cat("Goodbye!\n"), add = TRUE)
  
  if (x) {
    return(10)
  } else {
    stop("Error")
  }
}
# use on.exit to set up the exit handler which is run regardless of whether the function exits normally or with an error.
j06(TRUE)
j06(FALSE)

# Function forms ====
## Rewriting to prefix form ====
x <- 2
y <- 3
x + y
`+`(x,y)

df <- data.frame(matrix(runif(9,0,1),3,3))
names(df) <- c("x", "y", "z")
'names<-'(df, c("x", "y", "z"))

lapply(1:9, `+`, 1)


## Prefix form ====

k01 <- function(abcdef, bcde1, bcde2) {
  list(a = abcdef, b1 = bcde1, c = bcde2)
}

str(k01(1,2,3))

str(k01(2, 3, abcdef = 1))
str(k01(2, 3, a = 1))
str(k01(2, 3, ab = 1))
str(k01(2, 3, bcde1 = 1))
str(k01(2, 3, bcd = 1))

options(warnPartialMatchArgs = TRUE)
x <- k01(a = 1, 2, 3)

'%+%' <-  function(a,b) paste0(a,b)
"new " %+% "string"

'%\\%' <-  function(a,b) paste(a,b)

"a" %% "b"

"a" %/\% "b"

`%-%` <- function(a, b) paste0("(", a, " %-% ", b, ")")
"a" %-% "b" %-% "c"
"a" %-% "b" %-% "c"

# Replacement functions

`second<-` <- function(x, value) {
  x[2] <- value
  x
}


x <- 1:10
tracemem(x)

`for`

`function`









