
lb <- 152085
ub <- 670283

get_digts <- function(x) {
  n <- log10(x) %>% ceiling()
  x %% 10^seq(n, 1, -1) %/% 10^seq(n - 1, 0, -1) 
}

validate_num <- function(x) {
  digits <- get_digts(x)
  chk1 <- sum(digits[2:length(digits)] == 
                digits[1:(length(digits)-1)]) > 0
  chk2 <- sum(digits[2:length(digits)] <
    digits[1:(length(digits)-1)]) == 0
  return(chk1 & chk2)
}

validate_num_vec <- function(x) {
  x[x %>% purrr::map_lgl(validate_num)]
}

tictoc::tic()
nums <- validate_num_vec(seq(lb, ub, 1)) 
tictoc::toc()

length(nums)