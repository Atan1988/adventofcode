library(dplyr)
library(ggplot2)
library(gtools)
source('R/intcomputer.R')
options("scipen"=100)

program <- scan("data/input day17.txt",sep=",")

tictoc::tic()
res <- intcomputer(program, stp = 300000); res$done; res$i
tictoc::toc()

res_print <- function(outputs){
  cat(chr(outputs)%>% paste(collapse = ""))
}
out <- res$outputs
res_print(out)

cols <- (which(outputs == 10) %>% min()) - 1
out_vec <- outputs[outputs != 10] 
out_mat <- out_vec %>% matrix(ncol = cols)

neighbor <- 1:length(out_vec) %>% purrr::map(function(x){
  idx <- c(x, x-1, x+1, x-61, x+61) %>% ifelse(.<1, 99999, .)
  out_vec[idx] %>% ifelse(is.na(.), 46, .)
})

neighbor_sum <- purrr::map_dbl(neighbor, ~sum(.))

intersections <- which(neighbor_sum  == 175)
row <- (intersections %/% cols) + 1
col <- intersections %% cols
sum((row - 1) * (col - 1))

dir_calc <- function(offset) {
  dplyr::case_when(offset == 1 ~ 0, 
                   offset == ">" ~ 0, 
                   offset == -61 ~ pi/2,
                   offset == "^" ~ pi/2,
                   offset == 61 ~ 3 * pi/2,
                   offset == "v" ~ 3 *pi/2,
                   offset == -1 ~ pi, 
                   offset == "<" ~ pi
                   )
}

conv_dir <- function(dir){
  dplyr::case_when(dir == ">" ~ 1, 
                   dir == "^" ~ -61,
                   dir == "v" ~ 61,
                   dir == "<" ~ -1
  )
}

turn_calc <- function(path_dir, bot_dir){
  diff <-  ((c(-2*pi, 0, 2*pi) + dir_calc(path_dir)) - dir_calc(bot_dir))
  diff <- diff[abs(diff) == min(abs(diff))]
  if (diff < 0) return("R")
  if (diff > 0) return("L")
  return('O')
}

update_path <- function(base_path_dir, neighbor, intersections, bot_pos, bot_dir) {
  path_dir <- base_path_dir[which(neighbor[[bot_pos]][-1] == 35)] %>% .[. != (-1*bot_dir)]
  if (length(path_dir) == 0) return(NULL)
  if (bot_dir %in% path_dir) path_dir <- bot_dir
  if (bot_pos %in% intersections) 
    return(list(bot_pos = bot_pos + bot_dir, 
                path = 1, bot_dir = bot_dir))
  turn_diff <- turn_calc(path_dir, bot_dir)
  if (turn_diff == "O") 
    return(list(bot_pos = bot_pos + path_dir, 
                path = 1, bot_dir = bot_dir))
  if (turn_diff %in% c("L", "R")) 
    return(list(bot_pos = bot_pos, path = turn_diff, 
                bot_dir = turn_bot(bot_dir, turn_diff)))
}

turn_bot <- function(bot_dir, turn_diff){
  angle <- dir_calc(bot_dir) + ifelse(turn_diff == "R", -pi/2, 
                            ifelse(turn_diff == "L", pi/2, 0))
  dir <- paste(round(cos(angle), 0), round(sin(angle), 0), sep =",")
  dplyr::case_when(dir == "1,0" ~ 1, dir == "-1,0" ~ -1, 
                   dir == "0,-1" ~ 61, dir == '0,1' ~ -61)
}
##part 2 
trackout <- function(out_vec, neighbor, intersections){
  base_path_dir <- c(-1, 1, -61, 61)
  bot_pos <- which(chr(out_vec) %in% c("<", ">", "^", "v"))
  bot_dir <- conv_dir(chr(out_vec)[bot_pos])
  paths <- c()
  pos_ls <- c()
  res <- list(bot_pos = bot_pos, 
              bot_dir = bot_dir)
  
  i <- 1
  while(!is.null(res)){
    res <- append(res, list(base_path_dir = base_path_dir, neighbor = neighbor, 
                            intersections = intersections))

    res <- do.call(update_path, res)
    if (is.null(res)) break
    paths[i] <- res$path; pos_ls[i] <- res$bot_pos; res$path <- NULL
    i <- i + 1
    if(i>392) break
  }
  return(list(paths = paths, pos_ls = pos_ls))
}

result <- trackout(out_vec, neighbor, intersections)
paths <- result$paths
pos_ls <- result$pos_ls

turns <- c(which(paths %in% c("L", "R")), length(paths))
F_stps <- 1:(length(turns)-1) %>% 
  purrr::map_dbl(function(x) sum(as.numeric(paths[turns[x]:turns[x+1]]),na.rm = T))
paths1 <- c()
paths1[(1:(length(turns)-1) -1)*2+1] <- paths[turns[-length(turns)]]
paths1[(1:(length(turns)-1) -1)*2+2] <- F_stps

A <- c("L", ',', '12', ',', 'R', ',', '8', ',', "L", ','
       , '6', ',', 'R', ',', '8', ',', 'L', ',', '6', "\n")
B <- c("R", ',', "8", ',', "L", ',', "12", ',', "L"
       , ',', "12", ',', 'R', ',', '8', '\n')
C <- c("L", ',', "6", ',', "R", ',', "6", ',', "L", ',', "12", '\n')

main_routine <- c("A", ',', "B", ',', "A", ',', "A", ',', "B", ',',
                  "C", ',', "B", ',', "C", ',', "C", ',', "B", "\n")
program <- scan("data/input day17.txt",sep=",")
program[1] <- 2 

asc_main <- asc(main_routine)
asc_A <- asc(A) %>% unlist()
asc_B <- asc(B) %>% unlist()
asc_C <- asc(C) %>% unlist()
asc_video_feed <- asc(c('n', '\n'))

tictoc::tic()
res <- intcomputer(program, 
          inputs = c(asc_main), 
          stp = 3000000); res$done; res$i
tictoc::toc()

tictoc::tic()
res1 <- res; res1$inputs = asc_A; res1$stp <- 3e6; 
res1$outputs <- NULL; res1$input_pointer <- 1; res1$i <- NULL
res1 <- do.call(intcomputer, res1); res1$done; res1$i
tictoc::toc()

tictoc::tic()
res2 <- res1; res2$inputs = asc_B; res2$stp <- 3e6; 
res2$outputs <- NULL; res2$input_pointer <- 1; res2$i <- NULL
res2 <- do.call(intcomputer, res2); res2$done; res2$i
tictoc::toc()

tictoc::tic()
res3 <- res2; res3$inputs = asc_C; res3$stp <- 3e6; 
res3$outputs <- NULL; res3$input_pointer <- 1; res3$i <- NULL
res3 <- do.call(intcomputer, res3); res3$done; res3$i
tictoc::toc()

tictoc::tic()
res4 <- res3; res4$inputs = asc_video_feed; res4$stp <- 3e6; 
res4$outputs <- NULL; res4$input_pointer <- 1; res4$i <- NULL
res4 <- do.call(intcomputer, res4); res4$done; res4$i
tictoc::toc()

res4$outputs
