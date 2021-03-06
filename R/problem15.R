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

get_neighbor_id <- function(id) {
  curr_p <-  as.numeric(strsplit(id, ",")[[1]])
  purrr::map(1:4, ~coord_adjust(curr_p, .)) %>% 
    purrr::map_chr(~paste(.[1], .[2], sep = ","))
}

trim_path_df <- function(path_df) {
  dup <- path_df %>% filter(ct>1) %>% select(x, y) %>% distinct()
  
  for (i in 1:nrow(dup)){
    dup_stps <- path_df %>% filter(x == dup$x[i], y==dup$y[i]) %>% pull(steps)
    if (length(dup_stps) > 1) path_df <- path_df %>% 
        filter(!steps %in% seq((dup_stps[1] + 1), dup_stps[2], 1))
  }
  path_df %>% ungroup() %>% mutate(steps = seq(0, n()-1, 1))
}

program <- scan("data/input day15.txt",sep=",")
set.seed(12345)
map <- list()
visited <- list('0,0' = 1)
programs <- list('0,0' = program)
curr_pos <- c(0, 0)
i <- 1
paths <- c('0,0')
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
  paths[length(paths)+1] <- ids[next_mv]
  program <- res_list[[next_mv]]$program
  cat(i, '\r');i = i+1
  if (2 %in% out) break
}

paths_tmp <- strsplit(paths, ',')
path_df <- tibble(steps = seq(0, length(paths_tmp) - 1, 1),
                  x = as.numeric(purrr::map_chr(paths_tmp, ~.[1])), 
                  y = as.numeric(purrr::map_chr(paths_tmp, ~.[2]))
            )%>% group_by(x, y) %>% 
  mutate(ct = n(), t = seq(1, n(), 1))

path_df <- trim_path_df(path_df)
path_df %>% pull(steps) %>% max()

plot_map(map, curr_pos = c(0, 0))

#part 2
program <- scan("data/input day15.txt",sep=",")
set.seed(12345)
map <- list()
visited <- list('0,0' = 1)
programs <- list('0,0' = program)
curr_pos <- c(0, 0)
i <- 1
paths <- c('0,0')
while (i <= 100) {
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
  paths[length(paths)+1] <- ids[next_mv]
  program <- res_list[[next_mv]]$program
  cat(i, '\r');i = i+1
  #if (2 %in% out) break
  if ((visited %>% purrr::map_dbl(~ifelse(.>1, 1, 0)) %>% sum()) >= length(visited)) break
}

plot_map(map, curr_pos = c(-9, 20))


orig_program <- scan("data/input day15.txt",sep=",")
curr_pos <- c(0, 0)
res_list <- purrr::map(1:4, 
                       ~intcomputer(program = orig_program, inputs = ., 
                                    input_pointer = 1))
out <- res_list %>% purrr::map_dbl(~.$outputs)
map <- update_map(new_coords = purrr::map(1:4, ~coord_adjust(curr_pos = curr_pos, .))
            , objs = obj_detect(out), map = list())
programs <- update_programs(new_coords = purrr::map(1:4, ~coord_adjust(curr_pos = curr_pos, .)),
                            res_list, programs = list('0,0'=orig_program))
visited <- list('0,0' = 1)
paths <- c('0,0')

set.seed(12345)
##map the whole thing
for (i in 1:2000){
  ##list unvisisted and find next point to visit
  unvisisted <- map[!names(map) %in% names(visited) & map != "#"]
  if (length(unvisisted) == 0) break
  opts <- 1:length(unvisisted)
  pt_to_explor <- sample_mv(opts)
  new_id <- names(unvisisted)[[pt_to_explor]]
  curr_pos <- as.numeric(strsplit(new_id, ",")[[1]])
  ##jump to the program of new id
  program <- programs[[new_id]]
  visited <- update_visited(visited, as.numeric(strsplit(new_id, ",")[[1]]))
  ##map out surroundings
  res_list <- purrr::map(1:4, 
                         ~intcomputer(program = program, inputs = ., 
                                      input_pointer = 1))
  out <- res_list %>% purrr::map_dbl(~.$outputs)
  new_coords <- purrr::map(1:4, ~coord_adjust(curr_pos = curr_pos, .))
  objs <- obj_detect(out)
  map <- update_map(new_coords, objs, map)
  programs <- update_programs(new_coords, res_list, programs)
  cat(i, '\r');i = i+1
}

plot_map(map, curr_pos = c(0,0))

##spread the O2 :)
O2 <- names(map[map=="!"])

for (i in 1:300) {
  ##spread
  next_O2 <- purrr::map(O2, function(x) 
    purrr::map(1:4, ~coord_adjust(strsplit(x, ',') %>% .[[1]] %>% as.numeric(), .)) %>% 
      purrr::map_chr(~paste(.[1], .[2], sep = ','))
    ) %>% unlist()
  O2 <- unique(append(O2, next_O2[next_O2 %in% names(visited)])) 
  cat(i, '\r');
  if(length(O2) == length(visited)) break
}
O2