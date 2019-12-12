library(dplyr)
source('R/intcomputer.R')
options("scipen"=100)

#tests
program <- c(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99)
intcomputer(program, inputs = NULL )

program <- c(1102,34915192,34915192,7,4,7,99,0)
intcomputer(program, inputs = NULL )

program <- c(104,1125899906842624,99)
intcomputer(program, inputs = NULL )

#part1
program <- readr::read_csv("data/input day9.csv") %>% pull(program)
res <- intcomputer(program, inputs = 1 )
res$outputs

#part2
program <- readr::read_csv("data/input day9.csv") %>% pull(program)
res <- intcomputer(program, inputs = 2, stp = 1000000)
res$outputs;res$i

# library(profvis)
# 
# profvis({
#   res <- intcomputer(program, inputs = 2, stp = 1000000)
# })