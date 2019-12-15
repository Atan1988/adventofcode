library(dplyr)

IntComputer <- R6Class("intcomputer", list(
  program = NULL, 
  inputs = NULL, 
  pointer = 1, 
  input_pointer = 1, 
  done = 0, 
  rbase = 0,
  run = function() {
    instructions <- instr_codes(self$program[self$pointer])
    
    invisible(self)
  })
)