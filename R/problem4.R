library(dplyr)
library(dtplyr)
lb <- 152085
ub <- 670283

get_digts <- function(x) {
  n <- log10(x) %>% ceiling()
  x %% 10^seq(n, 1, -1) %/% 10^seq(n - 1, 0, -1) 
}

validate_num <- function(x) {
  n <- log10(max(x)) %>% ceiling()
  
  tictoc::tic()
  num_df <- tibble::tibble(num = x) %>% 
    dplyr::slice(rep(1:n(), each=n))
  tictoc::toc()
  
  tictoc::tic()
  num_df <- num_df %>% lazy_dt() %>% 
    dplyr::group_by(num) %>% 
    dplyr::mutate(digit = seq(n(), 1, -1))
  tictoc::toc()
  
  tictoc::tic()
  num_df <- num_df %>% dplyr::mutate(
    val = num %% 10^digit %/% 10^(digit - 1)
  ) %>% group_by(num) %>% 
    dplyr::mutate(lagval = lag(val, default = -98, 1), 
                  lagval2 = lag(val, default = -99, 2),
                  leadval = lead(val, default = 98, 1), 
                  leadval2 = lead(val, default = 99, 2)
                  )
  tictoc::toc()
  
  tictoc::tic()
  num_df <- num_df %>% 
    dplyr::mutate(
      match2 = (val == lagval | val == leadval),
      match3 = (val == lagval & val == lagval2) | 
               (val == lagval & val == leadval) |
               (val == leadval & val == leadval2), 
      match2_only = match2 & (!match3), 
      non_dec = leadval >= val
    )
  tictoc::toc()
  
  tictoc::tic()
  sum_df <- num_df %>% 
    group_by(num) %>% 
    summarise_at(vars(contains('match'), non_dec), sum)
  tictoc::toc()
  
  return(sum_df)
}

sum_df <- validate_num(seq(lb, ub, 1))

tictoc::tic()
sum_df %>% filter(match2 > 0, non_dec == n) %>% count() %>% as_tibble()
tictoc::toc()

tictoc::tic()
sum_df %>% filter(match2_only > 0, non_dec == n) %>% count() %>% as_tibble()
tictoc::toc()