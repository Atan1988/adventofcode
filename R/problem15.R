library(dplyr)
library(ggplot2)
source('R/intcomputer.R')
options("scipen"=100)

program <- scan("data/input day15.txt",sep=",")

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

map <- list()
curr_pos <- c(0, 0)

res_list <- purrr::map(1:4, 
              ~intcomputer(program = program, inputs = ., 
                           input_pointer = 1))
out <- res_list %>% purrr::map_dbl(~.$outputs)
new_coords <- purrr::map(1:4, ~coord_adjust(curr_pos = curr_pos, .))
objs <- obj_detect(out)
