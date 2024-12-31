source("utils.R")

raw <- read_data("data/input.txt")

rules <- create_rules(raw)
checks <- create_checks(raw)

## Do bubble sort with is_ordered()
result <- 0
for(check in checks) {
  if (check_if_sorted(check, rules)) {
    middle_i <- ceiling(length(check) / 2)
    result <- result + as.numeric(check[middle_i])
  }
}
result
