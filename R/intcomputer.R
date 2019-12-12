library(dplyr)
library(zeallot)

get_digts <- function(x) {
  n <- log10(x) %>% ceiling()
  x %% 10^seq(n, 1, -1) %/% 10^seq(n - 1, 0, -1) 
}

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


intcomputer <- function(program, inputs, pointer = 1, input_pointer = 1, done = 0) {
  orig_prog <- program
  for (i in 1:3000) {
    instructions <- instr_codes(program[pointer])
    #print(instructions)
    opcode <- instructions[4]*10 + instructions[5]
    if (opcode == 3) {
      if (input_pointer > length(inputs)) {done <-0; break}
      op3(program, inputs[input_pointer], pointer) %->% c(program, pointer)
      input_pointer <- input_pointer + 1
    }
    if (opcode == 4) {
      op4(program, pointer, instructions) %->% c(output, pointer); 
      #cat('out ', output, '\n')
    }
    if (opcode == 1) op1(program, pointer, instructions) %->% c(program, pointer)
    if (opcode == 2) op2(program, pointer, instructions) %->% c(program, pointer)
    if (opcode == 5) op5(program, pointer, instructions) %->% c(program, pointer)
    if (opcode == 6) op6(program, pointer, instructions) %->% c(program, pointer)
    if (opcode == 7) op7(program, pointer, instructions) %->% c(program, pointer)
    if (opcode == 8) op8(program, pointer, instructions) %->% c(program, pointer)
    if (opcode == 99) {done <- 1; break}
  }
  return(list(output, program, pointer, input_pointer, done))
}
