library(dplyr)
library(zeallot)

get_orbit_map <- function(map) {
  orbit_map <- strsplit(map, ")") %>% purrr::map_df(function(x){
    tibble::tibble(object = x[1], orbit = x[2])
  })
  unique_obj <- c(orbit_map$object, orbit_map$orbit) %>% unique()
  return(list(orbit_map, unique_obj))
}

get_orbit <- function(orbit_map, obj){
  orbit_counter <- 0
  obj_chain <- c(obj)
  for (i in 1:nrow(orbit_map)) {
    tmp_df <- orbit_map %>% filter(orbit == obj)
    if (nrow(tmp_df) == 0) break
    orbit_counter <- orbit_counter + 1
    obj <- tmp_df$object
    obj_chain[i+1] <- obj
  }
  obj_chain <- tibble(objs = obj_chain, level = seq(0, length(obj_chain)-1, 1))
  return(list(orbit_counter, obj_chain))
}

get_orbit_chksum <- function(orbit_map, unique_obj){
  unique_obj %>% purrr::map_dbl(~get_orbit(orbit_map, obj = .)[[1]]) %>% sum()
}

map <- c('COM)B', 'B)C', 'C)D', 'D)E'
         , 'E)F', 'B)G', 'G)H', 'D)I'
         , 'E)J', 'J)K', 'K)L')

c(orbit_map, unique_obj) %<-% get_orbit_map(map)
get_orbit_chksum(orbit_map, unique_obj)

map <- readr::read_csv('data/input day6.csv') %>% pull(orbit)
c(orbit_map, unique_obj) %<-% get_orbit_map(map)
get_orbit_chksum(orbit_map, unique_obj)

#part 2
map <- c('COM)B', 'B)C', 'C)D', 'D)E', 'E)F', 'B)G', 'G)H',
         'D)I', 'E)J', 'J)K', 'K)L', 'K)YOU', 'I)SAN')
c(orbit_map, unique_obj) %<-% get_orbit_map(map)
get_orbit(orbit_map, "YOU")[[2]] %>% 
  inner_join(get_orbit(orbit_map, "SAN")[[2]], by = 'objs') %>% 
  mutate(total_transfer = level.x + level.y - 2) %>% 
  arrange(total_transfer)

map <- readr::read_csv('data/input day6.csv') %>% pull(orbit)
c(orbit_map, unique_obj) %<-% get_orbit_map(map)
get_orbit(orbit_map, "YOU")[[2]] %>% 
  inner_join(get_orbit(orbit_map, "SAN")[[2]], by = 'objs') %>% 
  mutate(total_transfer = level.x + level.y - 2) %>% 
  arrange(total_transfer)