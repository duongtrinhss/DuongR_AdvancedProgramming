# f <- function() {
#   pause(0.1)
#   g()
#   h()
# }
# 
# g <- function() {
#   pause(0.1)
#   h()
# }
# 
# h <- function() {
#   pause(0.1)
# }

f <- function() {
  x <- integer()
  for (i in 1:1e4) {
    x <- c(x,i)
  }
}

# f <- function() {
#   x <- integer(1e4)
#   for (i in 1:1e4) {
#     x[i] <-  i
#   }
# }


