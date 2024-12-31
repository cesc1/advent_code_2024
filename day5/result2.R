source("utils.R")

raw <- read_data("data/input.txt")

rules <- create_rules(raw)
checks <- create_checks(raw)

## A bit slow but it works...
result <- 0
for(check in checks) {
  new_x <- bubble_sort(check, rules)
  if (!identical(check, new_x)) {
    middle_i <- ceiling(length(new_x) / 2)
    result <- result + as.numeric(new_x[middle_i])
  }
}
result
