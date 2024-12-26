library(stringr)
library(tibble)
library(dplyr)

check_start <- function(x) {
  res <- str_starts(x, "mul\\(")
  
  if (res) {
    return(list(
      x = str_sub(x, 5), 
      nexxt = 2))
  } else {
    return(list(
      x = str_sub(x, 2), 
      nexxt = 1))
  }
}

check_num1 <- function(x) {
  res <- str_extract(x, "^[:digit:]{1,3},")
  if (!is.na(res)) {
    return(list(
      x = str_sub(x, nchar(res) + 1),
      nexxt = 3,
      num = as.numeric(str_sub(x, end = nchar(res) - 1))
    ))
  } else {
    return(list(
      x = str_sub(x, 2),
      nexxt = 1
    ))
  }
}

check_num2 <- function(x) {
  res <- str_extract(x, "^[:digit:]{1,3}\\)")
  if (!is.na(res)) {
    return(list(
      x = str_sub(x, nchar(res) + 1),
      nexxt = 4,
      num = as.numeric(str_sub(x, end = nchar(res) - 1))
    ))
  } else {
    return(list(
      x = str_sub(x, 2),
      nexxt = 1
    ))
  }
}

extract_nums <- function(x) {
  res <- tibble(num1 = 0, num2 = 0)
  x <- list(x = x)
  while(nchar(x$x) > 0) {
    x <- check_start(x$x)
    if (x$nexxt == 1) next
    
    x <- check_num1(x$x)
    if (x$nexxt == 1) next
    res1 <- x$num
    
    x <- check_num2(x$x)
    if (x$nexxt == 1) next
    res2 <- x$num
    res = res |> 
      add_row(num1 = res1, 
              num2 = res2)
  }
  return(res)
}

data <- readLines("input.txt")

calc_result <- function(data) {
  data |> 
    lapply(extract_nums) |>
    lapply(function(data) {
      data |> 
        mutate(result = num1 * num2) |> 
        summarise(sum = sum(result)) |> 
        unlist()
    }) |> 
    unlist()
}

calc_result(data) |> 
  sum()
