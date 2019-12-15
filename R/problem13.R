library(dplyr)
library(ggplot2)
source('R/intcomputer.R')
options("scipen"=100)

program <- scan("data/input day13.txt",sep=",")

res <- intcomputer(program, stp = 30000); res$done; res$i
outputs <- res$outputs

tiles <- tibble(x = outputs[which(1:length(outputs) %% 3 == 1)],
       y = -outputs[which(1:length(outputs) %% 3 == 2)], 
       type = as.character(outputs[which(1:length(outputs) %% 3 == 0)])
       )

tiles %>% group_by(type) %>% tally()

tiles %>% filter(type != '0') %>% 
  ggplot(aes(x = x, y = y, shape = type)) + 
  geom_point()

#part 2
program <- scan("data/input day13.txt",sep=",")

plot_game <- function(tiles) {
  df <- tiles %>% group_by(x, y) %>% filter(id == max(id)) 
  
  plot_df <- df %>% anti_join(tibble(x = -1 , y = 0), by = c("x", "y"))
  score_df <- df %>% filter(x == -1 & y == 0)
  if (nrow(score_df) == 0) score <- 0 else score <- score_df$type[1]
  
  plot_df %>% 
    filter(type != "0") %>% ggplot(aes(x = x, y = y, shape = type)) + 
    geom_point() + theme_bw() + 
    labs(caption = paste0('score: ', score))
}

init_game <- function(program) {
  program[1] <- 2
  res <- intcomputer(program, stp = 100000)
  outputs <- res$outputs
  
  res$tiles <- tibble(x = outputs[which(1:length(outputs) %% 3 == 1)],
                  y = -outputs[which(1:length(outputs) %% 3 == 2)], 
                  type = as.character(outputs[which(1:length(outputs) %% 3 == 0)])
  ) %>% mutate(id = seq(1, n(), 1))
  
  res$p <- plot_game(res$tiles)
  res$input_pointer <- 1
  return(res)
}

play_1m <- function(res, input = 1) {
  tiles <- res$tiles
  res1 <- res; res1$outputs <- NULL; res1$i <- NULL; 
  res1$tiles <- NULL; res1$p <- NULL
  res1$inputs <- input; res1$stp <- 100000
  res1 <- do.call(intcomputer, args = res1)
  
  outputs <- res1$outputs
  
  if(is.null(outputs)) {
    res1$tiles <- res$tiles
    res1$p <- res$p
    return(res1)
  }
  
  res1$tiles <- tibble(x = outputs[which(1:length(outputs) %% 3 == 1)],
                  y = -outputs[which(1:length(outputs) %% 3 == 2)], 
                  type = as.character(outputs[which(1:length(outputs) %% 3 == 0)]))
  
  res1$tiles <- bind_rows(res$tiles, res1$tiles) %>% mutate(id = seq(1, n(), 1))
  
  res1$p <- plot_game(res1$tiles)
  res1$input_pointer <- 1
  return(res1)
}

game_run <- function(res = NULL, program = program, input = NULL){
  if (is.null(input)) {
    res <- init_game(program)
  } else {
    res <- play_1m(res, input = input)
  }
  return(res)
}

games_state <- list(); i = 1
games_state[[i]] <- game_run(res = NULL, program = program, input = NULL);games_state[[i]]$p;i=i+1
games_state[[i]] <- game_run(res = games_state[[i-1]], 
                             program = NULL, input = -1);games_state[[i]]$p;i=i+1

games_state <- list();inputs <- list(); paddles <- list(); balls <- list()
games_state[[1]] <- game_run(res = NULL, program = program, input = NULL);games_state[[1]]
n <- 7500; prog_bar <- dplyr::progress_estimated(n)
for (i in 2:n) {
  paddle <- games_state[[i-1]]$tiles %>% filter(type == '3') %>% filter(id==max(id))
  ball_pos <- games_state[[i-1]]$tile %>% filter(type == '4') %>% filter(id == max(id))

  if (i < 3) input <- 0 else {
    ball_pos <- games_state[(i-2):(i-1)] %>% 
      purrr::map_df(~.$tile %>% filter(type == '4') %>% filter(id == max(id)))
    ball_vol <- c(ball_pos$x[2] - ball_pos$x[1], ball_pos$y[2] - ball_pos$y[1])
    #if (ball_pos$y[2] == -17) ball_vol <- -1 * ball_vol
    ball_pos_nxt <- c(ball_pos$x[2] + ball_vol[1], ball_pos$y[2] + ball_vol[2])
    input <- pmax(pmin(ball_pos_nxt[1] - paddle$x[1], 1),-1)
    if (ball_pos_nxt[2] == -18 & ball_pos$x[2] == paddle$x[1] ) input <- 0
  }
  inputs[[i-1]] <- input
  paddles[[i-1]] <- paddle; balls[[i-1]] <- ball_pos[nrow(ball_pos), ]
  games_state[[i]] <- game_run(res = games_state[[i-1]], 
                               program = NULL, input = input);
  #plot(games_state[[i]]$p)
  prog_bar$tick()$print()
  if (games_state[[i]]$done == 1) break
}
games_state[[i]]$p

paddles %>% bind_rows() %>% head(); balls %>% bind_rows() %>% head()
