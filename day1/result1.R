library(dplyr)
library(rio)

data <- rio::import("input.txt", setclass = "tibble")
data

# Part one
calc_distances <- function(x, y) {
  n <- length(x)
  result <- integer(n) - 1
  
  for(i in 1:n) {
    x_min <- min(x)
    y_min <- min(y)
    result[i] <- abs(min(x) - min(y))
    x_min_pos <- which(x_min == x)[1]
    y_min_pos <- which(y_min == y)[1]
    x <- x[-x_min_pos]
    y <- y[-y_min_pos]
  }
  # some checks
  if (min(result) < 0 ) {
    warning("Result not calculated (-1 value)")
  } 
  if (length(x) > 0 || length(y) > 0) {
    warning("Not all elements from x or y are empty")
  }
  return(sum(result))  
}
calc_distances(data$V1, data$V2) 
