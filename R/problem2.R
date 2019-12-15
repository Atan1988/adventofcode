library(dplyr)

# add <- function(x, y) return(x + y)
# mult <- function(x, y) return(x * y)
#   
# int_computer <- function(input, noun = NULL, verb = NULL) {
#   x <- input
#   if (!is.null(noun)) x[2] <- noun
#   if (!is.null(verb)) x[3] <- verb
#   cur_pos <-  1
#   while (x[cur_pos] %in% c(1, 2)) {
#     if (x[cur_pos] == 1) opt <- add
#     if (x[cur_pos] == 2) opt <- mult
#     x[x[cur_pos + 3] + 1] <- opt(x[x[cur_pos + 1] + 1], 
#                                  x[x[cur_pos + 2] + 1])
#     cur_pos <- cur_pos + 4
#     if (x[cur_pos] == 99) break
#     if (!x[cur_pos] %in% c(1, 2, 99)){
#      print('some shit went wrong!')
#      break
#     }
#   }
#   return(list(input = input, result = x))
# }

input <- c(1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,
           3,2,6,1,19,1,19,5,23,2,10,23,27,2,27,13,
           31,1,10,31,35,1,35,9,39,2,39,13,43,1,43,5,
           47,1,47,6,51,2,6,51,55,1,5,55,59,2,9,59,63,2,6,
           63,67,1,13,67,71,1,9,71,75,2,13,75,79,1,79,10,83,
           2,83,9,87,1,5,87,91,2,91,6,95,2,13,95,99,1,99,5,103,
           1,103,2,107,1,107,10,0,99,2,0,14,0)
 
int_run <- function(program, noun, verb) {
  program[1:2] <- c(noun, verb)
  intcomputer(program = program)
} 

int_run(input, 102, 2)

res <- purrr::cross_df(list(noun = seq(0, 99, 1), 
                     verb = seq(0, 99, 1))) %>% 
  purrrlyr::by_row(~tibble::tibble(val = int_computer(input, 
                  .$noun, .$verb)[[2]][1])) %>% 
  tidyr::unnest()

res %>% filter(val == 19690720) %>% 
  mutate(answer = noun * 100 + verb)
