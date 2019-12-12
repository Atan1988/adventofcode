library(dplyr)
library(zeallot)

instr_codes <- function(x) {
  x %% 10^seq(5, 1, -1) %/% 10^seq(4, 0, -1) 
}

op3 <- function(program, input, pointer) {
  program[program[pointer + 1] + 1] <- input
  return(list(program, pointer = pointer + 2))
}

op4 <- function(program, pointer, instructions) {
  if (instructions[3] == 0) {
    program[program[pointer + 1] + 1] -> output
  } else {
    program[pointer + 1] -> output
  }
  return(list(output, pointer = pointer + 2))
}

op1 <- function(program, pointer, instructions) {
  param_mode <- instructions[1:3] %>% rev()
  pars <- program[seq(pointer + 1, pointer + 3, 1)]
  params <- ifelse(param_mode == 0, program[pmax(pars,0) + 1], pars)
  program[pars[3]+1] <- sum(params[1:2])
  return(list(program, pointer = pointer + 4))
}

op2 <- function(program, pointer, instructions) {
  param_mode <- instructions[1:3] %>% rev()
  pars <- program[seq(pointer + 1, pointer + 3, 1)]
  params <- ifelse(param_mode == 0, program[pmax(pars,0) + 1], pars)
  program[pars[3]+1] <- prod(params[1:2])
  return(list(program, pointer = pointer + 4))
}

op5 <- function(program, pointer, instructions) {
  param_mode <- instructions[1:3] %>% rev()
  pars <- program[seq(pointer + 1, pointer + 3, 1)]
  params <- ifelse(param_mode == 0, program[pmax(pars,0) + 1], pars)
  
  if (params[1] != 0) pointer <- params[2] + 1 else pointer <- pointer + 3
  return(list(program, pointer))
}

op6 <- function(program, pointer, instructions) {
  param_mode <- instructions[1:3] %>% rev()
  pars <- program[seq(pointer + 1, pointer + 3, 1)]
  params <- ifelse(param_mode == 0, program[pmax(pars,0) + 1], pars)
  
  if (params[1] == 0) pointer <- params[2] + 1 else pointer <- pointer + 3
  return(list(program, pointer))
}

op7 <- function(program, pointer, instructions) {
  param_mode <- instructions[1:3] %>% rev()
  pars <- program[seq(pointer + 1, pointer + 3, 1)]
  params <- ifelse(param_mode == 0, program[pmax(pars,0) + 1], pars)
  
  if (params[1] < params[2]) {
    program[pars[3] + 1] <- 1 } else {
      program[pars[3] + 1] <- 0 
    }
  return(list(program, pointer = pointer + 4))  
}

op8 <- function(program, pointer, instructions) {
  param_mode <- instructions[1:3] %>% rev()
  pars <- program[seq(pointer + 1, pointer + 3, 1)]
  params <- ifelse(param_mode == 0, program[pmax(pars,0) + 1], pars)
  
  if (params[1] == params[2]) {
    program[pars[3] + 1] <- 1 } else {
      program[pars[3] + 1] <- 0 
    }
  return(list(program, pointer = pointer + 4))  
}


intcomputer <- function(program, input) {
  orig_prog <- program
  pointer <- 1
  for (i in 1:300) {
    instructions <- instr_codes(program[pointer])
    #print(instructions)
    opcode <- instructions[4]*10 + instructions[5]
    if (opcode == 3) op3(program, input, pointer) %->% c(program, pointer)
    if (opcode == 4) {
         op4(program, pointer, instructions) %->% c(output, pointer); 
         cat('out ', output, '\n')}
    if (opcode == 1) op1(program, pointer, instructions) %->% c(program, pointer)
    if (opcode == 2) op2(program, pointer, instructions) %->% c(program, pointer)
    if (opcode == 5) op5(program, pointer, instructions) %->% c(program, pointer)
    if (opcode == 6) op6(program, pointer, instructions) %->% c(program, pointer)
    if (opcode == 7) op7(program, pointer, instructions) %->% c(program, pointer)
    if (opcode == 8) op8(program, pointer, instructions) %->% c(program, pointer)
    if (opcode == 99) break
  }
  return(output)
}

source('R/intcomputer.R')
program <- readr::read_csv("data/input day5.csv") %>% 
  pull(program)

intcomputer(program, inputs = 1)[[1]] 

intcomputer(program, inputs = 5)[[1]]