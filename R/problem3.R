library(dplyr)

lineinput <- readr::read_csv("data/input day3.csv")

line1 <- lineinput$line1
line2 <- lineinput$line2

parse_pts <- function(line) {
  n <- length(line)
  xs <- rep(0, n+1)
  ys <- rep(0, n+1)
  xs_path <- vector(mode = "list", n)
  ys_path <- vector(mode = 'list', n)
  path_cts <- vector(mode = 'list', n)
  sumsteps <- 0
  for (i in 1:n) {
    instr <- line[i]
    dir <- substr(instr, 1, 1)
    steps <- substr(instr, 2, nchar(instr)) %>% as.numeric()

    xs_path[[i]] <- switch(dir, 
           R = seq(xs[i] + 1, xs[i] + steps, 1), 
           L = seq(xs[i] - 1, xs[i] - steps, -1),
           D = rep(xs[i], steps), 
           U = rep(xs[i], steps)
           )
    
    ys_path[[i]] <- switch(dir, 
           R = rep(ys[i], steps), 
           L = rep(ys[i], steps),
           D = seq(ys[i] - 1, ys[i] - steps, -1), 
           U = seq(ys[i] + 1, ys[i] + steps, 1)
    )
    
    path_cts[[i]] <- seq(sumsteps + 1, sumsteps + steps, 1)
    xs[i+1] <- xs_path[[i]][length(xs_path[[i]])]
    ys[i+1] <- ys_path[[i]][length(ys_path[[i]])]
    sumsteps <- sumsteps + steps
  }
  pts_df <- tibble(x = unlist(xs_path), 
                   y = unlist(ys_path), 
                   i = unlist(path_cts)) %>% 
            dplyr::distinct()
  return(pts_df)
}

intersects <- parse_pts(line1) %>% 
  inner_join(parse_pts(line2), by = c('x', 'y')) %>% 
  mutate(dist = abs(x) + abs(y), 
         tot_steps = i.x + i.y)
intersects
