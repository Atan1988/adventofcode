library(dplyr)
library(combinat)


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
      c(test_state[[iter_df$V1[i]]], test_state[[iter_df$V2[i]]]) %<-%
        update_vol(test_state[[iter_df$V1[i]]], test_state[[iter_df$V2[i]]])
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

test_sts <- upd_st(test_state, n = 2772)
test_sts %>% .[[length(.)]] %>% print_st()
test_sts %>% .[[length(.)]] %>% calc_energy()
 
tictoc::tic()
test_sts_path <- get_path(test_sts)
tictoc::toc()

test_sts_path1 <- test_sts_path %>% group_by(id) %>% 
  mutate(dist = sqrt( (x - lag(x))^2 + (y-lag(y))^2 + (z-lag(z))^2 )) %>% 
  left_join(test_sts_path1 %>% filter(is.na(dist)) %>% 
              select(id, x0:=x, y0:=y, z0:=z), by = 'id') %>% 
  mutate(dist_orig = sqrt( (x - x0)^2 + (y - y0)^2 + (z - z0)^2 ))

df <- test_sts_path1  %>% filter(id == 4)

df %>% filter(dist == 0)%>% mutate(chg = step - lag(step, default = 0))

df$dist[-1] %>% table() %>% sort(decreasing = T)

library("scatterplot3d")
df1 <- df[1:10, ]
scatterplot3d(x = df1$x, y = df1$y, z = df1$z)

pt_df <- test_sts_path %>% filter(step <= 2, id == 1)

test_sts_path %>% filter(id == 1, step <= 2)

test_energy <- get_energy(test_sts)
test_energy_per_step <- test_energy  %>% group_by(step) %>% 
  summarise(tot = sum(total)) 

test_energy_per_step %>% 
  ggplot(aes(x = step, y = tot)) + geom_point()

tictoc::tic()
test_sts2 <- upd_st(test_state2, n = 200000)
test_sts2 %>% .[[length(.)]] %>% print_st()
test_sts2 %>% .[[length(.)]] %>% calc_energy()
tictoc::toc()

tictoc::tic()
test_sts2_path <- get_path(test_sts2)
tictoc::toc()

test_sts2_path1 <- test_sts2_path %>% group_by(id) %>% 
  mutate(dist = sqrt( (x - lag(x))^2 + (y-lag(y))^2 + (z-lag(z))^2 ))

df <- test_sts2_path1  %>% filter(id == 1)
df %>% filter(dist == 0)%>% mutate(chg = step - lag(step, default = 0))


test_energy2 <- get_energy(test_sts2)
test_energy_per_step2 <- test_energy2  %>% group_by(step) %>% 
  summarise(tot = sum(total)) 

test_energy_per_step2 %>% 
  ggplot(aes(x = step, y = tot)) + geom_point()

upd_sts <- upd_st(init_state, n = 50000)
upd_sts %>% .[[1000]] %>% print_st()
upd_sts %>% .[[1000]] %>% calc_energy()

upd_sts_path <- 1:length(upd_sts) %>% purrr::map_df(function(i){
  upd_sts[[i]] %>% print_st() %>% 
    mutate(id = seq(1, 4,1), step= i-1)
})

upd_energy <- get_energy(upd_sts)
upd_energy_per_step <- upd_energy %>% group_by(step) %>% 
  summarise(tot = sum(total))

upd_energy_per_step %>% 
  ggplot(aes(x = step, y = tot)) + geom_point()
