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
  return(tibble(x = curr_coord$x[1] + x_incre, 
                y = curr_coord$y[1] + y_incre))
}

init_robot_state <- function(curr_direc, program, init_clr = 0) {
  if (init_clr == 0) {
    white_df <- tibble(x = 1, y = 1) %>% .[0, ]
  } else {
    white_df <- tibble(x = 0, y = 0)
  }
  
  list(
    curr_coord = tibble(x = 0, y = 0), 
    curr_direc = curr_direc, 
    white_df = white_df, 
    path_df = tibble(x = 1, y = 1, clr = 1, id = 1) %>% .[0, ],
    program = program, 
    pointer = 1, 
    input_pointer = 1, 
    done = 0, rbase = 0, 
    stps = 3000
  )
}

chk_color <- function(curr_coord, white_df) {
  tmp_df <- curr_coord %>% inner_join(white_df, by = c("x", "y") )
  if (nrow(tmp_df) > 0) return(1) else return(0)
}
edit_color <- function(curr_coord, white_df, color) {
  if (color == 1) return(bind_rows(curr_coord, white_df))
  if (color == 0) return(white_df %>% 
                    anti_join(curr_coord, by = c("x", "y")))
}
###robot state is a list
##with elements for curr_coord, curr_direc, white_df, program
robot_program <- function(robot_state){
  curr_clr <- chk_color(robot_state$curr_coord, robot_state$white_df) 
  c(outputs, robot_state$program, robot_state$pointer, 
    input_pointer, robot_state$done, 
    robot_state$rbase, i) %<-% intcomputer(robot_state$program, 
              inputs = curr_clr, 
              pointer = robot_state$pointer, 
              input_pointer = 1, rbase = robot_state$rbase)
  if (!outputs[1] %in% c(0, 1)) cat('output wrong!\n')
  robot_state$path_df <- bind_rows(robot_state$path_df, 
                      robot_state$curr_coord %>% 
            mutate(clr = outputs[1], id = nrow(robot_state$path_df)+1)) 
  robot_state$white_df <- edit_color(
    curr_coord = robot_state$curr_coord, 
              white_df = robot_state$white_df, color = outputs[1])
  
  robot_state$curr_direc <- change_direc(robot_state$curr_direc, 
                                         outputs[2])
  robot_state$curr_coord <- new_coord(
    curr_coord = robot_state$curr_coord, 
    direc = robot_state$curr_direc)
  
  robot_state$outputs <- outputs
  return(robot_state)
}

##run the robot
run_robot <- function(curr_direc = pi/2, program = program, 
                      init_clr = 0) {
  robot_state <- init_robot_state(curr_direc = curr_direc, 
                                  program = program, 
                                  init_clr = init_clr)
  i <- 1
  #pts <- list(1)
  #robot_states <- list(); robot_states[[1]] <- robot_state
  while (robot_state$done == 0)
  {
    robot_state <- robot_program(robot_state)
    #pts <- append(pts, robot_state$pointer)
    i <- i + 1
    if (i >= 63) break
  }
  print(i)
  #robot_state$pts <- pts
  return(robot_state)
}

direc <- pi/2
program <- scan("data/input day11.txt",sep=",")

#part 1
tictoc::tic()
robot_state <- run_robot(curr_direc = pi/2, program = program, 
                         init_clr = 0)
tictoc::toc()
robot_state$path_df %>% select(x, y) %>% distinct() %>% nrow()
robot_state$path_df %>% arrange(x, y) %>% 
  group_by(x, y) %>% filter(id == max(id)) %>% 
  filter(clr == 1) %>% ggplot(aes(x = x, y = y)) + geom_point()
#part 2 
program <- scan("data/input day11.txt",sep=",")
tictoc::tic()
robot_state1 <- run_robot(curr_direc = pi/2, program = program, 
                         init_clr = 1)
tictoc::toc()

chk <- bind_cols(robot_state1$path_df, pos[1:249, ] %>% as_tibble()) %>% 
  mutate(diff = abs(x - V1) + abs(y - V2) + abs(clr - V3))

robot_state1[[250]]$path_df -> xxx

m=matrix(nrow=43,ncol=6)
for(i in 1:nrow(pos)){
  m[pos[i,1]+1,abs(pos[i,2])+1]=pos[i,3]
}
image(0:42,0:5,m,col=c("black","white"),ylim=c(6,-1),xlim=c(-1,43))

library(ggplot2)
pts <- robot_state1 %>% purrr::map_dbl(~.$pointer)
rbs <- robot_state1 %>% purrr::map_dbl(~.$rbase)
programs <- robot_state1 %>% purrr::map(~.$program)
coords <- robot_state1 %>% purrr::map(~.$curr_coord)
white_dfs <- robot_state1 %>% purrr::map(~.$white_df)
outputs <- robot_state1 %>% purrr::map(~.$outputs)

which(!pts %in% c(1, 475));which(program > 2^32);

i <- 200
programs[[i]][pts[i]:(pts[i]+20)];pts[i];rbs[i]; 
chk_color(coords[[i]], white_dfs[[i]]);programs[[i]][programs[[i]] < 0];i;i=i+1

i <- 249;
program <- programs[[i]]; input <- chk_color(coords[[i]], white_dfs[[i]]);
pointer <- pts[i];rbase <- rbs[i]; done <- 0

c(outputs = outputs, program = program, 
  pointer = pointer, input_pointer = input_pointer, 
  done = done, rbase = rbase, i = i) %<-% intcomputer(program, inputs = input, 
                              pointer = pointer, 
                              input_pointer = 1, done = done, rbase = rbase, stps = 1)
  program[pointer:(pointer+20)];rbase;pointer
  
  
robot_state1[[length(robot_state1)]]$path_df %>% .[1:200, ] %>% 
  filter(!id %in% which(!pts %in% c(1, 475))) %>% 
  arrange(x, y) %>% 
  group_by(x, y) %>% filter(id == max(id)) %>% 
  filter(clr == 1) %>% ggplot(aes(x = x, y = y)) + geom_point()


robot_state1$white_df
plot(x = robot_state1$white_df$x, y = robot_state1$white_df$y)
max_x <- robot_state1$white_df$x %>% max()
min_x <- robot_state1$white_df$x %>% min()
max_y <- robot_state1$white_df$y %>% max()
min_y <- robot_state1$white_df$y %>% min()

white_df1 <- robot_state1$white_df %>% 
  mutate(col = x - min_x +1, 
         row = max_y - y + 1) %>% 
  mutate(val = 255)

img <- bind_rows(
  purrr::cross_df(list(col = sort(unique(white_df1$col)), 
                       row = sort(unique(white_df1$row)))) %>% 
    mutate(val = 0) %>% 
    anti_join(white_df1 %>% select(col, row)), 
  white_df1 %>% select(col, row, val)
) %>% 
  arrange(row, col) %>% 
  tidyr::spread(col, val) %>% 
  select(-row) %>% as.matrix()

cv2 <- reticulate::import('cv2')
cv2$imwrite('problem11 part2.png', img)
