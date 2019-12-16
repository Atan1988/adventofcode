library(dplyr)
library(ggplot2)
source('R/intcomputer.R')
options("scipen"=100)



coord_adjust <- function(curr_pos, mv = 1) {
  if (mv == 1) return(curr_pos + c(0, 1))
  if (mv == 2) return(curr_pos + c(0, -1))
  if (mv == 3) return(curr_pos + c(-1, 0))
  if (mv == 4) return(curr_pos + c(1, 0))
}
obj_detect <- function(out){
  ifelse(
    out == 0, '#', 
     ifelse(out == 1, "", "!")
  )
}

update_map <- function(new_coords, objs, map){
  purrr::map(1:4, 
    function(i){
      map[[paste(new_coords[[i]], collapse = ",")]] <<- objs[i]
    }
  )
  map
}

update_programs <- function(new_coords, res_list, programs){
  purrr::map(1:4, 
    function(i){
      programs[[paste(new_coords[[i]], collapse = ",")]] <<- res_list[[i]]$program
    }
  )
  programs
}

plot_map <- function(map, curr_pos) {
  tmp_coord <- strsplit(names(map), ",")
  tibble(x = c(as.numeric(purrr::map_chr(tmp_coord, ~.[1])), curr_pos[1]), 
         y = c(as.numeric(purrr::map_chr(tmp_coord, ~.[2])), curr_pos[2]), 
         typ = c(unlist(map), "+")
         ) %>% filter(typ != '') %>% 
    ggplot(aes(x=x, y =y, shape = typ)) + geom_point()
}

sample_mv <- function(opts) {
  if (length(opts) == 1) return(opts)
  sample(opts, 1)
}

update_visited <- function(visited, new_coords){
  id <- paste(new_coords, collapse = ",")
  if (is.null(visited[[id]]) ) visited[[id]] <- 0
  visited[[id]] <- visited[[id]]+1
  return(visited)
}

program <- scan("data/input day15.txt",sep=",")
set.seed(12345)
map <- list()
visited <- list('0, 0' = 1)
programs <- list('0,0' = program)
curr_pos <- c(0, 0)
i <- 1

while (i <= 2000) {
  res_list <- purrr::map(1:4, 
                         ~intcomputer(program = program, inputs = ., 
                                      input_pointer = 1))
  out <- res_list %>% purrr::map_dbl(~.$outputs)
  new_coords <- purrr::map(1:4, ~coord_adjust(curr_pos = curr_pos, .))
  objs <- obj_detect(out)
  map <- update_map(new_coords, objs, map)
  programs <- update_programs(new_coords, res_list, programs)
  
  ids <- purrr::map_chr(new_coords, function(x) paste(x[1], x[2], sep = ','))
  Vs <- visited[ids] %>% purrr::map(function(x)ifelse(is.null(x), 0, x)) %>% unlist()
  options <- which(objs != "#")
  least_v_options <- options[which(Vs[options]==min(Vs[options]))]
  
  next_mv <- sample_mv(least_v_options)
  curr_pos <- new_coords[[next_mv]]; visited <- update_visited(visited, new_coords[[next_mv]])
  program <- res_list[[next_mv]]$program
  cat(i, '\r');i = i+1
  if (2 %in% out) break
}

plot_map(map, curr_pos)
