library(dplyr)
library(ggplot2)
source('R/intcomputer.R')
options("scipen"=100)

program <- scan("data/input day17.txt",sep=",")

tictoc::tic()
res <- intcomputer(program, stp = 300000); res$done; res$i
tictoc::toc()

res_print <- function(outputs){
  symbol <- ifelse(outputs == 46, ".", 
                   ifelse(outputs == 35, "#", '\n')) %>% paste(collapse = "")
  cat(symbol)
}
res_print(res$outputs)

cols <- (which(outputs == 10) %>% min()) - 1
out_vec <- outputs[outputs != 10] 
out_mat <- out_vec %>% matrix(ncol = cols)

neighbor <- 1:length(out_vec) %>% purrr::map(function(x){
  idx <- c(x, x-1, x+1, x-61, x+61) %>% .[.>=1 & .<=length(out_vec)]
  out_vec[idx]
})

neighbor_sum <- purrr::map_dbl(neighbor, ~sum(.))

intersections <- which(neighbor_sum  == 175)
row <- (intersections %/% cols) + 1
col <- intersections %% cols
sum((row - 1) * (col - 1))
