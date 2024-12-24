library(rio)

data <- rio::import("input.txt", setclass = "tibble")

# Part two
calc_similarity <- function(x, y) {
  n <- length(x)
  result <- integer(n) - 1
  
  for(i in 1:n) {
    result[i] <- sum(y[y == x[i]])
  }
  # Some checks
  if (min(result) < 0 ) {
    warning("Result not calculated (-1 value)")
  } 
  return(sum(result))
}

calc_similarity(data$V1, data$V2)
