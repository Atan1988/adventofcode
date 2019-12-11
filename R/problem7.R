library(dplyr)
library(zeallot)
library(combinat)
source('R/intcomputer.R')

amplifiers <- function(program, sequence) {
  out1 <- intcomputer(program, inputs = c(sequence[1], 0))[[1]]
  out2 <- intcomputer(program, inputs = c(sequence[2], out1))[[1]]
  out3 <- intcomputer(program, inputs = c(sequence[3], out2))[[1]]
  out4 <- intcomputer(program, inputs = c(sequence[4], out3))[[1]]
  out5 <- intcomputer(program, inputs = c(sequence[5], out4))[[1]]
  return(c(out1, out2, out3, out4, out5))
}

program <- c(3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
             1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0)
sequence <- c(1,0,4,3,2)

amplifiers(program, sequence)


program <- readr::read_csv("data/input day7.csv") %>% pull(Program)
sequences <- permn(seq(0, 4, 1))
#part1
chk_df <- 1:length(sequences) %>% 
  purrr::map_df(function(num) {
    tibble(seq = num, out = amplifiers(program, sequences[[num]])[5])
  })

sequences[[chk_df %>% filter(out == max(out)) %>% pull(num)]]
chk_df %>% filter(out == max(out)) %>% pull(out)

#part2 
program <- c(3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
             27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5)
sequence <- c(9,8,7,6,5)

amplifiers_boost <- function(program, sequence) {
  n_seq <- length(sequence)
  programs <- 1:n_seq  %>% purrr::map(~identity(program))
  inputs <- rep(0, n_seq )
  
  for (i in 1:n_seq ) {
    input_pt <- (i - 1) %% n_seq 
    if (input_pt == 0) input_pt <- n_seq
    c(inputs[i], programs[[i]]) %<-% intcomputer(programs[[i]], 
                                          inputs = c(sequence[i], inputs[input_pt]))
  }
  return(list(programs, inputs))
}

res <- amplifiers_boost(program, sequence)
