# Header 1 ====
## Header 2 ====
### Header 3 ====

# Prerequisites ====
# install.packages("purrr", dependencies = T)
library(purrr)

# First functional ====
triple <-  function(x) x*3
map(1:3, triple)

simple_map <- function(x, f, ...){
  out <- vector("list", length(x))
  for (i in seq_along(x)) {
    out[[i]] <- f(x[[i]], ...)
  }
  out
}

x <- 1:3

simple_map(x, triple)

## Producing atomic vectors ====
# In purrr
map_chr(mtcars, typeof)
map_lgl(mtcars, is.double)
n_unique <- function(x) length(unique(x))
map_int(mtcars, n_unique)
map_dbl(mtcars, mean)

map_dbl(mtcars, ~length(unique(.x)))
as_mapper(~ length(unique(.x)))

x <- list(
  list(-1, x = 1, y = c(2), z = "a"),
  list(-2, x = 4, y = c(5,6), z = "b"),
  list(-3, x = 8, y = c(9,10,11))
)

map_dbl(x, "x") # select by name
map_dbl(x, 1)   # select by position
map_dbl(x, list("y",1))


# In Base R
vapply(mtcars, mean, FUN.VALUE = double(1))
vapply(mtcars, is.double, FUN.VALUE = logical(1))

## Passing arguments with ---- ====
plus <- function(x,y) x + y

x <-  c(0, 0, 0, 0)

map_dbl(x, plus, runif(1))
map_dbl(x, ~plus(.x, runif(1)))

map(x , mean, trim = 0.1)

boostrap_summary <- function(x, f) {
  f(sample(x, replace = TRUE))
}

#simple_map(mtcars, boostrap_summary, f = mean)
#error is due to the fact that name matching beats position matching

map(mtcars, boostrap_summary, f = mean)












