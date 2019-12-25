library(dplyr)

input_map <- readLines('data/input day18.txt')

input_map1 <- input_map %>% purrr::map(function(x){
  1:nchar(x) %>% purrr::map_chr(~substr(x, ., .))
}) %>% unlist() %>% 
  matrix(ncol = 81, byrow = T)

col <- nchar(input_map[1])
non_wall <- which(input_map1 != '#')
keys <- which(input_map1 %in% letters)
doors <- which(input_map1 %in% LETTERS)

tree_structure <- non_wall %>% purrr::map(
  function(x){
    child_loc = x + c(-1, 1, -col, col)
    names(child_loc) <- c('-1', '1', -col, col)
    child_val <- input_map1[child_loc]
    list(
      child_loc = child_loc[child_loc %in% non_wall], 
      child_val = child_val[child_loc %in% non_wall], 
      val = input_map1[x], 
      loc = x
    )
  }
)
names(tree_structure) <- non_wall
intersections <- tree_structure %>% 
  purrr::map(function(x){
    num_dir <- length(unique(as.numeric(names(x$child_loc))))
    return(num_dir)
  }) %>% unlist()

intersects_3or4 <- intersections[intersections>2]

at_pos <- which(input_map1 == '@')
at_pos
tree_structure[[as.character(which(input_map1 == 'a'))]]

closest_intsect <- function(y, dir, cap = 1000) {
  i <- 0; orig <- y;
  door <- NULL
  while (length(y$child_loc) < 3 | y$loc == orig$loc ){
    if (dir %in% names(y$child_loc)) tmp_dir <- dir
    if (!dir %in% names(y$child_loc)) {
      tmp_dir <- names(y$child_loc) %>% 
        .[!. %in% as.character(-1 * as.numeric(dir))] %>% .[1]
    }
    dir <- tmp_dir; tmp_y <- tree_structure[[as.character(y$child_loc[tmp_dir])]]
    if (tmp_y$val %in% LETTERS) door <- c(door, tmp_y$val)
    y <- tmp_y 
    i <- i + 1; 
    if(i> cap) break
    if(length(y$child_loc) == 1) break
  }
  if (length(y$child_loc) == 1) return(NULL)
  return(list(loc = y$loc, step = i, door = door))
}
search_for_closest_intsects <- function(intsec) {
  x <- tree_structure[[intsec]]
  intersects <- purrr::map(names(x$child_loc), ~closest_intsect(x, .))
  names(intersects) <- names(x$child_loc)
  intersects[sapply(intersects , is.null)] <- NULL
  res <- list(intersects)
  names(res) <- x$loc
  return(res)
}

intersect_map <- names(intersects_3or4) %>% 
  purrr::map(
    function(x) {
      #print(x)
      search_for_closest_intsects(x)
  })
names(intersect_map ) <- names(intersects_3or4)

dist_btw_intsects <- function(intsec1, intsec2, depth = 30){
  res <- intsec1
  for (i in 1:depth) {
    res <- res %>% purrr::map(function(x){
      tmp_x <- strsplit(x, "_")[[1]] 
      x1 <- tmp_x[length(tmp_x)]
      if (x1 == intsec2) return(x)
      loc <- purrr::map_chr(intersect_map[[x1]][[1]], ~.$loc)
      if (length(tmp_x)>=2) {
        x2 <- tmp_x[length(tmp_x)-1]
        loc <- loc[!loc %in% x2]
      }
      if (length(loc)>0) res1 <- paste0(x, "_", loc) else res1 <- x
      return(res1)
    }) %>% unlist()
  }
}

loc0 <- as.character(which(input_map1 == '@'))
loc1 <- as.character(which(input_map1 == 'G'))

close_intsec0 <- search_for_closest_intsects(loc0)
close_intsec1 <- search_for_closest_intsects(loc1)