library(dplyr)
options("scipen"=100)

get_eq_df <- function(program){
  tmp <- strsplit(program, ' => ') %>% purrr::map_chr(~.[2]) %>% 
    strsplit(" ")
  
  tmp1 <- strsplit(program, ' => ') %>% purrr::map_chr(~.[1]) %>% 
    strsplit(", ") 
  
  material_df <- 1:length(tmp1) %>% 
    purrr::map_df(function(x) {
      tt <- tmp1[[x]]
      tibble(mat_quant = tt %>% strsplit(" ") %>% purrr::map_dbl(~as.numeric(.[1])), 
             mat_name = tt %>% strsplit(" ") %>% purrr::map_chr(~.[2]), 
             eq_id = x
      )
    })
  
  eq_df <- tibble(prod_quant = tmp %>% purrr::map_dbl(~as.numeric(.[1])), 
                  prod_name = tmp %>% purrr::map_chr(~.[2]), 
                  eq_id = seq(1, length(tmp), 1)
  ) 
  
  eq_df1 <- material_df %>% left_join(eq_df) %>% 
    mutate(basic = ifelse(mat_name == "ORE", 1, 0))
  return(eq_df1)
}

use_extra <- function(mat_req){
  resource_df <- mat_req$resource_df
  extra_df <- mat_req$extra_df
  
  resource_df <- resource_df %>% 
    left_join(extra_df %>% select(mat, extra_q = q), by = "mat") %>% 
    mutate(q1 = q, q = pmax(q + ifelse(is.na(extra_q), 0, extra_q), 0), 
           extra_left = pmin(extra_q + q1, 0))
  
  #basic_req <- resource_df %>% filter(basic == 1) %>% select(mat, q, basic)
  
  extra_df <-  bind_rows(extra_df %>% anti_join(resource_df, by = 'mat'),
                         resource_df %>% filter(extra_left <0 ) %>% 
                           select(mat, q := extra_left)
  )
  return(list(resource_df = resource_df, extra_df = extra_df))
}

run_1iter <- function(mat_req, eq_df1) {
  mat_req <- use_extra(mat_req)
  resource_df <- mat_req$resource_df
  extra_df <- mat_req$extra_df
  
  basic_req <- resource_df[resource_df$basic == 1, c('mat', 'q', 'basic')]
  
  resource_calc <-purrrlyr::by_row(
    resource_df[resource_df$q>0 & resource_df$basic == 0, ], function(row) {
    tmp_df <- eq_df1[eq_df1$prod_name == row$mat, ]
    if ( identical(tmp_df$basic, rep(1, nrow(tmp_df)) )) 
       return(list(resource_df = row, extra_df = tibble()))
    
    tmp_df$bulk <- with(tmp_df, ceiling(row$q / prod_quant))
    tmp_df$q1 <- with(tmp_df, bulk * prod_quant)
    tmp_df$mat_quant1 <- with(tmp_df, mat_quant * bulk)
    tmp_df$extra_q <- with(tmp_df, q1 - row$q)
    
    resource_df <- tmp_df[, c('mat_quant1', 'mat_name')] 
    colnames(resource_df) <- c('q', 'mat')
    
    tmp_df$q <- with(tmp_df, -1 * extra_q)
    tmp_df$mat <- tmp_df$prod_name
    extra_df <- unique(tmp_df[abs(tmp_df$q)>0, c('q', 'mat')])
    return(list(resource_df = resource_df, extra_df = extra_df))
  }) %>% 
    pull(.out) 
  
  resource_df <- resource_calc %>% purrr::map(~.$resource_df) %>% 
    bind_rows(., basic_req) %>% 
    group_by(mat) %>% summarise(q = sum(q))%>% 
    left_join(eq_df1 %>% select(mat = prod_name, basic) %>% 
                distinct(), by = 'mat')
  
  extra_df <- resource_calc %>% purrr::map(~.$extra_df) %>% 
    bind_rows(., extra_df )
  return(list(resource_df = resource_df, extra_df = extra_df))
}

init_mat_req <- function(eq_df1, extra_df, N) {
  resource_df <- eq_df1 %>% filter(prod_name == "FUEL") %>% 
    select(q = mat_quant, mat = mat_name) %>% 
    left_join(eq_df1 %>% select(mat = prod_name, basic) %>% 
                distinct(), by = 'mat') %>% 
    mutate(q = q * N)
  if (is.null(extra_df)) extra_df <- tibble(q = 1, mat = "NA") %>% .[0, ]
  return(list(resource_df = resource_df, extra_df = extra_df))
}

calc_ORE <- function(eq_df1, extra_df = NULL, N = 1){
  mat_req <- init_mat_req(eq_df1, extra_df = extra_df, N=N)
  i <- 1
  while( sum(mat_req$resource_df$basic) < nrow(mat_req$resource_df)) {
    mat_req <- run_1iter(mat_req, eq_df1 )
    #cat(i, '\r'); i= i+1
  }
  #print(i)
  return(mat_req)
}

program <- readLines("data/input day14.txt")
eq_df1 <- get_eq_df(program)

mat_req <- calc_ORE(eq_df1)
final_mat_req <- mat_req$resource_df %>% 
  inner_join(eq_df1, by = c('mat' = 'prod_name', 'basic')) %>% 
  mutate(bulk = ceiling(q / prod_quant), q1 = bulk * mat_quant)

sum(final_mat_req$q1)

#part 2
program <- readLines("data/input day14.txt")
eq_df1 <- get_eq_df(program)


ORE_used <- 0; j <- 0; extra_df <- NULL
usage <- NULL; ORE_limit <- 1e12;
no_extra <- list()

calc_ORE_f <- function(eq_df1, N) {
  kkk <- calc_ORE(eq_df1, N=N)
  kkk <- use_extra(kkk)
  final_mat_req <- inner_join(kkk$resource_df, eq_df1,
                              by = c('mat' = 'prod_name', 'basic')) %>% 
    mutate(bulk = ceiling(q / prod_quant), q1 = bulk * mat_quant)
  sum(final_mat_req$q1)
}

min <- 1e12 %/% calc_ORE_f(eq_df1, 1)
max <- 5 * min
i <- 1
while (max > (min + 1)) {
  mean <- (max + min) %/%2
  if (calc_ORE_f(eq_df1, N = mean)>1e12){
    max <-  mean
  } else {
    min <- mean
  }
  cat(i, '\r');i = i+1
}



break_j <- 150000
tictoc::tic()
while( ORE_used < ORE_limit) {
  mat_req <- calc_ORE(eq_df1, extra_df)
  mat_req <- use_extra(mat_req)
  
  extra_df <- mat_req$extra_df
  
  final_mat_req <- inner_join(mat_req$resource_df, eq_df1,
                      by = c('mat' = 'prod_name', 'basic')) %>% 
    mutate(bulk = ceiling(q / prod_quant), q1 = bulk * mat_quant)

  extra_df <- bind_rows(
    final_mat_req %>% mutate(extra_q = q - bulk * prod_quant) %>%
      select(mat, q := extra_q),
    extra_df
  ) %>% group_by(mat) %>% summarise_all(sum) %>% filter(abs(q)>0)
  
  usage[length(usage)+1] <- sum(final_mat_req$q1)
  ORE_used <- ORE_used + sum(final_mat_req$q1)
  cat(ORE_used, ', ', j, ' \r') 
  if (ORE_used < ORE_limit) j= j+1
  if (nrow(extra_df) == 0) break
  if (j > break_j) break
}
tictoc::toc()

ORE_used;length(usage);j

(a <- cumsum(usage)[cumsum(usage)<(1e12 %% ORE_used)] %>% length())
(b <- 1e12 %/% ORE_used * length(usage))
 a + b
 
 1e12/((cumsum(usage) / 1:length(usage)) %>% min())