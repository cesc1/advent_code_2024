library(stringr)

data <- readLines("input.txt") |> 
  str_split(" ") |> 
  lapply(as.numeric)

check_sub_row <- function(x, direct = c("up", "down")) {
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

check_row <- function(x) {
  x_extra <- lapply(seq_along(x), function(del, x) {
    x[-del]
  }, x = x)

  for (x_i in x_extra) {
    res = check_sub_row(x_i, direct = "up")
    if (res == "safe")
      return ("safe")
  }
  for (x_i in x_extra) {
    res = check_sub_row(x_i, direct = "down")
    if (res == "safe")
      return ("safe")
  }
  return ("unsafe")
}

is_valid <- function(data) {
  n <- length(data)
  result <- integer(n) - 1
  
  for (i in seq_len(n)) {
    result[i] <- check_row(data[[i]])
  }
  return(result)
}
is_valid(data) |> 
  table()
