library(dplyr)
library(combinat)
library(zeallot)


update_vol <- function(pt1, pt2) {
  pt1$vol <- ifelse(pt1$pos > pt2$pos, pt1$vol - 1, 
                     ifelse(pt1$pos < pt2$pos, pt1$vol + 1, pt1$vol))
  
  pt2$vol <- ifelse(pt2$pos > pt1$pos, pt2$vol - 1, 
                     ifelse(pt2$pos < pt1$pos, pt2$vol + 1, pt2$vol))
  return(list(pt1, pt2))
}

upd_st <- function(test_state, n = 1) {
  iter_df <- combinat::combn(seq(1, length(test_state), 1), 2) %>% t() %>% as_tibble()
  states <- 1:(n+1) %>% purrr::map(function(x)return(NA))
  states[[1]] <- test_state
  prob_bar <- dplyr::progress_estimated(n)
  for (j in 1:n) {
    for (i in 1:nrow(iter_df)) {
      res <- update_vol(test_state[[iter_df$V1[i]]], test_state[[iter_df$V2[i]]])
      test_state[[iter_df$V1[i]]] <- res[[1]]
      test_state[[iter_df$V2[i]]] <- res[[2]]
    }
    test_state <- purrr::map(test_state, 
                             function(pt){pt$pos <- pt$pos + pt$vol; return(pt)})
    states[[j+1]] <- test_state
    prob_bar$tick()$print()
  }
  return(states)
}

print_st <- function(st) {
  st %>% purrr::map_df(function(x) {
    bind_cols(x$pos %>% t() %>% as_tibble(), x$vol %>% t() %>% as_tibble())
  })
}

calc_energy <- function(st) {
  st %>% purrr::map_df(function(x) {
    tibble(pot = sum(abs(x$pos)), kin = sum(abs(x$vol)), total = pot * kin)
  })
}

get_energy <- function(upd_sts) {
  energy_dfs <- 1:length(upd_sts) %>% purrr::map_df(function(i) {
    upd_sts[[i]] %>% calc_energy() %>% mutate(step = i-1) %>%
      mutate(id = seq(1, n(), 1))
  }) 
  return(energy_dfs)
}

get_path <- function(upd_sts) {
  prog_bar <- dplyr::progress_estimated(length(upd_sts))
  1:length(upd_sts) %>% purrr::map_df(function(i){
    res <- upd_sts[[i]] %>% print_st() %>% 
      mutate(id = seq(1, 4,1), step= i-1)
    prog_bar$tick()$print()
    return(res)
  })
}

get_path_v <- function(upd_sts) {
  get_one <- function(i) {
    append(1:3 %>% purrr::map(~purrr::map_dbl(upd_sts, 
                                              function(st) {st[[i]]$pos[.]})),
           1:3 %>% purrr::map(~purrr::map_dbl(upd_sts, 
                                              function(st) {st[[i]]$vol[.]}))) %>% 
      as_tibble(.name_repair = make.names) %>% 
      `colnames<-`(c('x', 'y', 'z', 'x1', 'y1', 'z1')) %>% 
      mutate(step = seq(0, n()-1, 1), id = i)
  }
  tmp_df <- 1:4 %>% purrr::map_df(get_one) %>% arrange(step, id)%>% group_by(id) %>% 
    mutate(dist = sqrt( (x - lag(x))^2 + (y-lag(y))^2 + (z-lag(z))^2 ))
  
  tmp_df %>% ungroup() %>% 
    left_join(tmp_df %>% filter(is.na(dist)) %>% 
                select(id, x0:=x, y0:=y, z0:=z), by = 'id') %>% 
    mutate(dist_orig = sqrt( (x - x0)^2 + (y - y0)^2 + (z - z0)^2 ))
}

init_state <- list(
  list(pos = c(x=19, y=-10, z=7), 
       vol = c(x=0, y=0, z=0)), 
  list(pos = c(x=1, y=2, z=-3), 
       vol = c(x=0, y=0, z=0)),
  list(pos = c(x=14, y=-4, z=1), 
       vol = c(x=0, y=0, z=0)),
  list(pos = c(x=8, y=7, z=-6), 
       vol = c(x=0, y=0, z=0))
)

test_state <- list(
  list(pos = c(x= -1, y=  0, z=  2), 
       vol = c(x=0, y=0, z=0)), 
  list(pos = c(x=  2, y=-10, z= -7), 
       vol = c(x=0, y=0, z=0)),
  list(pos = c(x=  4, y= -8, z=  8), 
       vol = c(x=0, y=0, z=0)),
  list(pos = c(x=  3, y=  5, z= -1), 
       vol = c(x=0, y=0, z=0))
)

test_state2 <- list(
  list(pos = c(x=-8, y=-10, z=0), 
       vol = c(x=0, y=0, z=0)), 
  list(pos = c(x=5, y=5, z=10), 
       vol = c(x=0, y=0, z=0)),
  list(pos = c(x=2, y=-7, z=3), 
       vol = c(x=0, y=0, z=0)),
  list(pos = c(x=9, y=-8, z=-3), 
       vol = c(x=0, y=0, z=0))
)

tictoc::tic()
test_sts <- upd_st(test_state, n = 2772)
tictoc::toc()
test_sts %>% .[[length(.)]] %>% print_st()
test_sts %>% .[[length(.)]] %>% calc_energy()

tictoc::tic()
test_sts_path <- get_path_v(test_sts)
tictoc::toc()

x_align <- test_sts_path %>% 
  group_by(step) %>% 
  summarise(dist = sum(abs(x-x0)), vol = sum(abs(x1))) %>% 
  filter(dist == 0, vol == 0) %>% pull(step) %>% .[2]

y_align <- test_sts_path %>% 
  group_by(step) %>% 
  summarise(dist = sum(abs(y-y0)), vol = sum(abs(y1))) %>% 
  filter(dist == 0, vol == 0) %>% pull(step) %>% .[2]

z_align <- test_sts_path %>% 
  group_by(step) %>% 
  summarise(dist = sum(abs(z-z0)), vol = sum(abs(z1))) %>% 
  filter(dist == 0, vol == 0) %>% pull(step) %>% .[2]

pracma::Lcm(pracma::Lcm(x_align, y_align), z_align)

tictoc::tic()
test_sts2 <- upd_st(test_state2, n = 50000)
test_sts2 %>% .[[length(.)]] %>% print_st()
test_sts2 %>% .[[length(.)]] %>% calc_energy()
tictoc::toc()

tictoc::tic()
test_sts2_path <- get_path_v(test_sts2)
tictoc::toc()

x_align <- test_sts2_path %>% 
  group_by(step) %>% 
  summarise(dist = sum(abs(x-x0)), vol = sum(abs(x1))) %>% 
  filter(dist == 0, vol == 0) %>% pull(step) %>% .[2]

y_align <- test_sts2_path %>% 
  group_by(step) %>% 
  summarise(dist = sum(abs(y-y0)), vol = sum(abs(y1))) %>% 
  filter(dist == 0, vol == 0) %>% pull(step) %>% .[2]

z_align <- test_sts2_path %>% 
  group_by(step) %>% 
  summarise(dist = sum(abs(z-z0)), vol = sum(abs(z1))) %>% 
  filter(dist == 0, vol == 0) %>% pull(step) %>% .[2]

pracma::Lcm(pracma::Lcm(x_align, y_align), z_align)

library(profvis)

tictoc::tic()
#profvis::profvis({
  upd_sts <- upd_st(init_state, n = 500000)
#})
tictoc::toc()
upd_sts %>% .[[1000]] %>% print_st()
upd_sts %>% .[[1000]] %>% calc_energy()

tictoc::tic()
upd_sts_path <- get_path_v(upd_sts)
tictoc::toc()

x_align <- upd_sts_path %>% 
  group_by(step) %>% 
  summarise(dist = sum(abs(x-x0)), vol = sum(abs(x1))) %>% 
  filter(dist == 0, vol == 0) %>% pull(step) %>% .[2]

y_align <- upd_sts_path %>% 
  group_by(step) %>% 
  summarise(dist = sum(abs(y-y0)), vol = sum(abs(y1))) %>% 
  filter(dist == 0, vol == 0) %>% pull(step) %>% .[2]

z_align <- upd_sts_path %>% 
  group_by(step) %>% 
  summarise(dist = sum(abs(z-z0)), vol = sum(abs(z1))) %>% 
  filter(dist == 0, vol == 0) %>% pull(step) %>% .[2]

pracma::Lcm(pracma::Lcm(x_align, y_align), z_align)
