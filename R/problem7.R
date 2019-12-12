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
one_run <- function(programs, sequence, inputs, pointers, input_pointers, dones) {
  n_seq <- length(sequence)
  for (i in 1:n_seq ) {
    input_pt <- (i + 1) %% n_seq 
    if (input_pt == 0) input_pt <- n_seq
    c(inputs[[input_pt]][length(inputs[[input_pt]])+1], 
      programs[[i]], pointers[i], input_pointers[i], 
      dones[i]) %<-% intcomputer(programs[[i]], 
                      inputs = inputs[[i]], pointer = pointers[i], 
                      input_pointer = input_pointers[i],
                      done = dones[i])
  }
  return(list(programs, sequence, inputs, pointers, input_pointers, dones))
}

amplifiers_boost <- function(program, sequence) {
  n_seq <- length(sequence)
  programs <- 1:n_seq  %>% purrr::map(~identity(program))
  inputs <- sequence %>% purrr::map(~.); inputs[[1]][2] <- 0
  pointers <- rep(1, n_seq)
  input_pointers <- rep(1, n_seq)
  dones <- rep(0, n_seq)
 
  #print(inputs); tibble::tibble(pointers, input_pointers, dones)
  for (j in 1:300) {
    c(programs, sequence, inputs, pointers, input_pointers, dones) %<-% 
      one_run(programs, sequence, inputs, pointers, input_pointers, dones)
    if (sum(dones) == n_seq) break
  }
  return(list(programs, inputs))
}

program <- c(3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
             -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
             53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10)
sequence <- c(9,7,8,5,6)
res <- amplifiers_boost(program, sequence)
res[[2]][[1]] %>% .[length(.)]

program <- readr::read_csv("data/input day7.csv") %>% pull(Program)
sequences <- permn(seq(5, 9, 1))

prog <- dplyr::progress_estimated(length(sequences))
chk_df2 <- 1:length(sequences) %>% 
  purrr::map_df(function(num) {
    res <- tibble(seq = num, 
                  out = (amplifiers_boost(program, 
                    sequences[[num]])[[2]][[1]]) %>% .[length(.)])
    prog$tick()$print()
    return(res)
  })

chk_df2 %>% pull(out) %>% max()
