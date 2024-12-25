library(stringr)

data <- readLines("input.txt") |> 
  str_split(" ") |> 
  lapply(as.numeric)

check_row <- function(x, direct = c("up", "down")) {
  n <- length(x)
  
  if (direct == "up") {
    dir_op <- -c(1:3)
  } else if (direct == "down") {
    dir_op <- (1:3)
  } else
    stop("Wrong direct parameter")
  
  for (i in seq_len(n - 1)) {
    if (!((x[i] - x[i + 1]) %in% dir_op))
      return("unsafe")
  }
  
  return("safe")
}

is_valid <- function(data) {
  n <- length(data)
  result <- integer(n) - 1
  
  for (i in seq_len(n)) {
    # check up or down
    if (data[[i]][1] < data[[i]][2]) {
      direct <- "up"
    } else if (data[[i]][1] > data[[i]][2]) {
      direct <- "down"
    } else {
      result[i] <- "unsafe"
      next
    }
    result[i] <- check_row(data[[i]], direct)
  }
  return(result)
}
is_valid(data) |> 
  table()
