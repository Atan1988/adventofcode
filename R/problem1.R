library(rvest)
library(dplyr)

#part 1
get_fuel <- function(x) {
 sum(x %/% 3 - 2)
}

input <- readr::read_csv("data/input day1.csv") %>% pull(input)

##answer
get_fuel(input)

#part 2 
get_fuel_recurs <- function(x) {
  fuel <- get_fuel(x)
  sum_fuel <- fuel
  while (get_fuel(fuel) > 0) {
    fuel <- get_fuel(fuel)
    sum_fuel <- sum_fuel + fuel
  }
  return(sum_fuel)
}

#answer
purrr::map_dbl(input, get_fuel_recurs) %>% sum()
