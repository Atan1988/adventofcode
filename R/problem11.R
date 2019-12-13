library(dplyr)
source('R/intcomputer.R')
options("scipen"=100)


change_direc <- function(direc, direc_input) {
  #0 for left, 1 for right
  if (direc_input == 0) return(direc + pi/2)
  if (direc_input == 1) return(direc - pi/2)
  return(NULL)
}

new_coord <- function(curr_coord, direc) {
  x_incre <- round(cos(direc), 10)
  y_incre <- round(sin(direc), 10)
  return(c(curr_coord[1] + x_incre, curr_coord[2] + y_incre))
}

###robot state is a list
##with elements for curr_coord, curr_direc, data_df
robot_program <- function(robot_state){
  
}

direc <- pi/2
program <- readr::read_csv('data/input day11.csv') %>% pull(program)