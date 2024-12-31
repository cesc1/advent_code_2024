# Packages
library(dplyr)
library(stringr)
library(tidyr)

# Read data
read_data <- function(path) {
  raw <- readLines(path)
  data_split <- which(raw == "")
  stopifnot(length(data_split != 1) || 
            data_split < 2)
  
  data1 <- raw[1:(data_split - 1)]
  data2 <- raw[(data_split + 1):length(raw)]
  
  return(list(
    rules = data1,
    check = data2
  ))
}

# Preprocessing
create_rules <- function(data) {
  tibble(comb = data$rules) |> 
    mutate(x = str_split_i(comb, "\\|", 1),
           y = str_split_i(comb, "\\|", 2),
           .keep = "unused") |> 
    mutate(val = 1) |> 
    arrange(y) |> 
    tidyr::pivot_wider(names_from = y, values_from = val) |> 
    arrange(x) |> 
    rename(name_row = x)
}

create_checks <- function(data) {
  str_split(data$check, ",")
}

# Check if it's ordered!
is_ordered <- function(x, y, rules) {
  row <- rules |> 
    filter(name_row == x)
  if (!nrow(row) == 1 || !(y %in% colnames(row))) {
    return(!is_ordered(y, x, rules))
  }
  val <- row |> 
    select(all_of(y)) |> 
    pull() |> 
    is.na()
  return(!val)
}

check_if_sorted <- function(x, rules) {
  if (length(x) <= 1)
    return(TRUE)
  n = length(x) - 1
  
  for(i in seq_len(n)) {
    if (!is_ordered(x[i], x[i + 1], rules)) {
      return(FALSE)
    }
  }
  return(TRUE)
  return(x)
}

bubble_sort <- function(x, rules) {
  if (length(x) <= 1)
    return(x)
  n = length(x) - 1
  
  for(n_i in n:2) {
    for(i in 1:n_i) {
      if (!is_ordered(x[i], x[i + 1], rules)) {
        aux = x[i]
        x[i] = x[i + 1]
        x[i + 1] = aux
      }
    }
  }
  return(x)
}