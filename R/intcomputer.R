library(dplyr)
library(zeallot)

get_digts <- function(x) {
  n <- log10(x) %>% ceiling()
  x %% 10^seq(n, 1, -1) %/% 10^seq(n - 1, 0, -1) 
}

instr_codes <- function(x) {
  x %% 10^seq(5, 1, -1) %/% 10^seq(4, 0, -1) 
}

int_parm_calc <- function(program, pointer, instructions, rbase) {
  param_mode <- instructions[1:3] %>% rev()
  pars <- program[seq(pointer + 1, pointer + 2, 1)]
  pars <- ifelse(is.na(pars), 0, pars)
  params <- ifelse(param_mode[1:2] == 0, program[pmax(pars,0) + 1],
                   ifelse(param_mode == 2, program[pmax(pars + rbase,0) + 1], pars))
  if (param_mode[3] == 2) {
    params[3] <- program[pointer+3] + rbase
  } else {
    params[3] <- program[pointer+3]
  }
  params <- ifelse(is.na( params), 0,  params)
  return(list(pars, params))
}

op3 <- function(program, input, pointer, instructions, rbase) {
  if (instructions[3] == 2) {
    program[program[pointer + 1] + rbase + 1] <- input
  } else {
    program[program[pointer + 1] + 1] <- input
  }
 
  return(list(program, pointer = pointer + 2))
}

op4 <- function(program, pointer, instructions, rbase) {
  if (instructions[3] == 0) {
    program[program[pointer + 1] + 1] -> output
  } else if (instructions[3] == 2) {
    program[program[pointer + 1] + rbase + 1] -> output
  } else {
    program[pointer + 1] -> output
  }
  return(list(output, pointer = pointer + 2))
}


op1 <- function(program, pointer, instructions, rbase = 0) {
  c(pars, params) %<-% int_parm_calc(program, pointer, instructions, rbase)
  program[params[3]+1] <- sum(params[1:2])
  return(list(program, pointer = pointer + 4))
}

op2 <- function(program, pointer, instructions, rbase = 0) {
  c(pars, params) %<-% int_parm_calc(program, pointer, instructions, rbase)
  program[params[3]+1] <- prod(params[1:2])
  return(list(program, pointer = pointer + 4))
}

op5 <- function(program, pointer, instructions, rbase = 0) {
  c(pars, params) %<-% int_parm_calc(program, pointer, instructions, rbase)
  if (params[1] != 0) pointer <- params[2] + 1 else pointer <- pointer + 3
  return(list(program, pointer))
}

op6 <- function(program, pointer, instructions, rbase = 0) {
  c(pars, params) %<-% int_parm_calc(program, pointer, instructions, rbase)
  if (params[1] == 0) pointer <- params[2] + 1 else pointer <- pointer + 3
  return(list(program, pointer))
}

op7 <- function(program, pointer, instructions, rbase = 0) {
  c(pars, params) %<-% int_parm_calc(program, pointer, instructions, rbase)
  if (params[1] < params[2]) {
    program[params[3] + 1] <- 1 } else {
      program[params[3] + 1] <- 0 
    }
  return(list(program, pointer = pointer + 4))  
}

op8 <- function(program, pointer, instructions, rbase = 0) {
  c(pars, params) %<-% int_parm_calc(program, pointer, instructions, rbase)
  if (params[1] == params[2]) {
    program[params[3] + 1] <- 1 } else {
      program[params[3] + 1] <- 0 
    }
  return(list(program, pointer = pointer + 4))  
}

op9 <- function(program, pointer, instructions, rbase = 0) {
  c(pars, params) %<-% int_parm_calc(program, pointer, instructions, rbase)
  rbase <- rbase + params[1]
  return(list(program, pointer = pointer + 2, rbase))
}

intcomputer <- function(program, inputs = NULL, pointer = 1, 
                        input_pointer = 1, done = 0, rbase = 0, stps = 3000) {
  orig_prog <- program
  outputs <- NULL
  for (i in 1:stps) {
    instructions <- instr_codes(program[pointer])
    #print(instructions)
    opcode <- instructions[4]*10 + instructions[5]
    if (opcode == 3) {
      if (input_pointer > length(inputs)) {done <-0; break}
      op3(program, inputs[input_pointer], pointer, instructions, rbase) %->% c(program, pointer)
      input_pointer <- input_pointer + 1
    }
    if (opcode == 4) {
      op4(program, pointer, instructions, rbase) %->% c(output, pointer); 
      #cat('out ', output, '\n')
      outputs <- c(outputs, output)
    }
    if (opcode == 1) op1(program, pointer, instructions, rbase) %->% c(program, pointer)
    if (opcode == 2) op2(program, pointer, instructions, rbase) %->% c(program, pointer)
    if (opcode == 5) op5(program, pointer, instructions, rbase) %->% c(program, pointer)
    if (opcode == 6) op6(program, pointer, instructions, rbase) %->% c(program, pointer)
    if (opcode == 7) op7(program, pointer, instructions, rbase) %->% c(program, pointer)
    if (opcode == 8) op8(program, pointer, instructions, rbase) %->% c(program, pointer)
    if (opcode == 9) op9(program, pointer, instructions, rbase) %->% c(program, pointer, rbase)
    if (opcode == 99) {done <- 1; break}
  }
  return(list(outputs = outputs, program = program, 
              pointer = pointer, input_pointer = input_pointer, 
              done = done, rbase = rbase, i = i))
}
