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

closest_intsect <- function(y, dir, cap = 1000) {
  i <- 0; orig <- y;
  door <- NULL; key <- NULL
  while (length(y$child_loc) < 3 | y$loc == orig$loc ){
    if (dir %in% names(y$child_loc)) tmp_dir <- dir
    if (!dir %in% names(y$child_loc)) {
      tmp_dir <- names(y$child_loc) %>% 
        .[!. %in% as.character(-1 * as.numeric(dir))] %>% .[1]
    }
    dir <- tmp_dir; tmp_y <- tree_structure[[as.character(y$child_loc[tmp_dir])]]
    if (tmp_y$val %in% LETTERS) door <- c(door, tmp_y$val)
    if (tmp_y$val %in% letters) key <- c(key, tmp_y$val)
    y <- tmp_y 
    i <- i + 1; 
    if(i> cap) break
    if(length(y$child_loc) == 1) break
  }
  if (length(y$child_loc) == 1) return(NULL)
  return(list(loc = y$loc, step = i, door = door, key = key))
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

calc_dist <- function(chain){
  moves <- strsplit(chain, '_')[[1]]
  start <- intersect_map[[moves[1]]]
  step <- 0; door <- NULL; key <- NULL
  for (i in 2:length(moves)){
    tmp_int <- intersect_map[[moves[i-1]]]
    idx <- which(purrr::map_chr(tmp_int[[1]], ~as.character(.$loc)) == moves[i])
    step <- step + tmp_int[[1]][[idx]]$step
    door <- c(door, tmp_int[[1]][[idx]]$door)
    key <- c(key, tmp_int[[1]][[idx]]$key)
  }
  return(tibble(chain = chain, step = step, 
                door = paste(sort(door), collapse = ","), 
                key = paste(sort(key), collapse = ",")))
}
dist_btw_intsects <- function(intsec1, intsec2, depth = 30){
  if (intsec1 == intsec2) {
    return(tibble(chain = intsec1, step = 0, door = "", key = ""))
  }
  res <- intsec1; #tic(); depth = 30;
  for (i in 1:depth) {
    res <- res %>% strsplit("_") %>% 
      purrr::map(function(x){
      tmp_x <- x; chain_x <- paste(x, collapse = "_")
      x1 <- tmp_x[length(tmp_x)]
      if (x1 == intsec2) return(chain_x)
      loc <- purrr::map_chr(intersect_map[[x1]][[1]], ~.$loc)
      if (length(tmp_x)>=2) {
        x2 <- tmp_x[length(tmp_x)-1]
        loc <- loc[!loc %in% x2]
      }
      if (length(loc)>0) res1 <- paste0(chain_x,  "_", loc) else res1 <- chain_x
      return(res1)
    }) %>% unlist()
    trim_idx <- res %>% strsplit('_') %>%
        purrr::map_lgl(function(x) max(table(x))>1)
    res[which(!trim_idx)] -> res
    trim_idx2 <- res %>% strsplit('_') %>%
      purrr::map_lgl(function(x) x[length(x)] != intsec2 & length(x) < i )
    res[which(!trim_idx2)] -> res
  }; #toc()
  res1 <- res[which(res %>% strsplit('_') %>% purrr::map_lgl(~.[length(.)] == intsec2))]
  dist_res1 <- purrr::map_df(res1, calc_dist) %>% 
    group_by(door) %>% filter(step == min(step))
  return(dist_res1)
}

get_child_int <- function(intsec){
  tibble::tibble(
    loc  = purrr::map_chr(intsec[[1]], ~as.character(.$loc)), 
    step = purrr::map_dbl(intsec[[1]], ~.$step), 
    door = purrr::map_chr(intsec[[1]], ~paste(sort(.$door), collapse = ",")), 
    key = purrr::map_chr(intsec[[1]], ~paste(sort(.$key), collapse = ","))
  )
}

add_doors <- function(x, y, z){
  door_list <- list(x, y, z)
  purrr::pmap_chr(door_list, function(a, b, c){
    tmp <- paste(unique(sort(strsplit(c(a, b, c), ',') %>% unlist()) %>% 
                   .[. != ""]), collapse = ",")
  })
}

find_dist_btw_points <- function(loc0, loc1, depth = 30) {
  
  close_intsec0 <- search_for_closest_intsects(loc0)
  close_intsec1 <- search_for_closest_intsects(loc1)
  
  if (loc0 %in% names(intersect_map)){
    intsec_df0 <- tibble(loc = loc0, step = 0, door = "", key = "")
  } else {
    intsec_df0 <- get_child_int(close_intsec0)
  }

  if (loc1 %in% names(intersect_map)) {
    intsec_df1 <- tibble(loc = loc1, step = 0, door = "", key = "")
  } else {
    intsec_df1 <- get_child_int(close_intsec1)
  }

  purrr::cross_df(list(intsec0 = intsec_df0$loc, intsec1 = intsec_df1$loc)) %>% 
    inner_join(intsec_df0, by = c('intsec0' = 'loc')) %>% 
    inner_join(intsec_df1, by = c('intsec1' = 'loc')) %>% 
    #filter(intsec0 != intsec1) %>% 
    purrrlyr::by_row(~dist_btw_intsects(.$intsec0, .$intsec1, depth = depth)) %>% 
    tidyr::unnest(cols = '.out') %>% 
    mutate(tot_step = step + step.x + step.y)  %>% 
    mutate(tot_door = add_doors(door, door.x, door.y)) %>% 
    mutate(tot_key = add_doors(key, key.x, key.y)) -> test_df #%>% 
    #group_by(tot_door) %>% 
    #filter(tot_step == min(tot_step)) 
  return(test_df)
}

find_key <- function(key){
  print(key)
  loc0 <- as.character(which(input_map1 == '@'))
  loc1 <- as.character(which(input_map1 == key))
  
  find_dist_btw_points(loc0, loc1) %>% ungroup() %>% 
    select(tot_step, tot_door, tot_key, chain) %>% distinct() %>% 
    mutate(loc1 = !!key, 
           chain = paste0(chain, '_', !!loc1)) 
}

trim_routes <- function(routes) {
  keep <- routes %>% 
    purrr::map(function(x){
      tmp_r <- routes[grepl(x, routes)]
      if (max(nchar(tmp_r))> nchar(x)) return(NULL)
      return(x)
    }) %>% unlist()
  keep
}

check_route <- function(chain, keys0){
  mvs <- chain %>% strsplit('_') %>% .[[1]]
  tmp_key <- keys0;
  key_collected <- NULL;
  key_used <- NULL
  for (i in 2:length(mvs) ){
    if (i == length(mvs)){
      tmp_int <- search_for_closest_intsects(mvs[i])
      tmp_y <- tmp_int[[1]][[which(tmp_int[[1]] %>% purrr::map_lgl(~.$loc == mvs[i-1]))]]
      tmp_y$door <- rev(tmp_y$door) 
      tmp_y$key <- rev(tmp_y$key) %>% .[!. %in% tmp_key]
    } else {
      tmp_int <- intersect_map[[mvs[i-1]]]
      idx <- which(tmp_int[[1]] %>% purrr::map_lgl(~.$loc == mvs[i]))
      if (length(idx) == 0) return(tibble(open = F))
      tmp_y <- tmp_int[[1]][[idx]]
    }
   
    if (!is.null(tmp_y$key)) {
      tmp_key <- c(tmp_key, tmp_y$key)
      key_collected <- c(key_collected, tmp_y$key)
    }
    
    if (!is.null(tmp_y$door)) {
      key_required <- tolower(tmp_y$door)
      if(sum(key_required %in% tmp_key) < length(key_required) ) return(tibble(open = F, 
                    key_used = paste(key_used, collapse = ","), 
                    key_collected = paste(key_collected, collapse = ",")))
      key_used <- c(key_used, key_required)
    }

  }
  return(tibble(open = T, key_used = paste(key_used, collapse = ","), 
                key_collected = paste(key_collected, collapse = ",")))
}

get_chain_length <- function(chain, start_pt = NULL){
  mvs <- chain %>% strsplit("_") %>% .[[1]]
  step <- 0
  if (!is.null(start_pt)) {
    alternate_start <- which(mvs == start_pt)
    mvs <- mvs[alternate_start:length(mvs)]
  }
  for (i in 2:length(mvs) ){
    if (i == length(mvs)){
      tmp_int <- search_for_closest_intsects(mvs[i])
      tmp_y <- tmp_int[[1]][[which(tmp_int[[1]] %>% purrr::map_lgl(~.$loc == mvs[i-1]))]]
      tmp_y$door <- rev(tmp_y$door) 
      tmp_y$key <- rev(tmp_y$key) %>% .[!. %in% tmp_key]
    } else {
      tmp_int <- intersect_map[[mvs[i-1]]]
      idx <- which(tmp_int[[1]] %>% purrr::map_lgl(~.$loc == mvs[i]))
      if (length(idx) == 0) return(tibble(open = F))
      tmp_y <- tmp_int[[1]][[idx]]
    }
    step <- step + tmp_y$step
  }
  return(step)
}

clear_all_routes <- function(key_res){
  keys <- list(); 
  chains <- list();
  rounds <- list()
  i <- 1; l_chain <- 0
  while (l_chain < length(key_res$chain)){
    #print(i)
    if (i == 1) {
      tmp_keys <- NULL
      tmp_chains <- NULL
    } else {
      tmp_keys <- keys[[i-1]]
      tmp_chains <- chains[[i-1]]
    }
    round_chk <- key_res %>% 
      filter(!chain %in% tmp_chains) %>% 
      purrrlyr::by_row(
        function(row){
          #print(row$loc1)
          check_route(row$chain, keys0 = tmp_keys)
        }
      ) %>% tidyr::unnest(cols = '.out') %>% 
      mutate(key_collected = ifelse(open, 
              paste0(key_collected, ifelse(key_collected != "", ",", ""), loc1), 
              key_collected), 
             rounds = i)  
    
    round0 <- round_chk %>% filter(open)
    
    round0_b <- round0 %>% 
      filter(key_collected %in% trim_routes(key_collected))
    
    keys[[i]] <- c(tmp_keys, round0_b$key_collected) %>% strsplit(",") %>% 
      unlist() %>% unique() %>% sort()
    chains[[i]] <- unique(c(tmp_chains, round_chk %>% 
                            filter(loc1 %in% keys[[i]]) %>% pull(chain)))
    rounds[[i]] <- round0_b
    l_chain <- length(chains[[i]])
    i <- i + 1; if (i>80) break
  }
  return(list(keys = keys, chains = chains, rounds = rounds))
}

print_map <- function(input, pts){
  pts <- as.numeric(pts)
  start <- pts[1]; end <- pts[length(pts)]
  middle <- pts[2:(length(pts)-1)]
  input[start] <- '?'; 
  input[end] <-  '!'; 
  input[middle] <- seq(0, min(9, length( middle)-1), 1)
  writeLines(1:81 %>% purrr::map_chr(~paste(input[., ], collapse = "")), 'ppp.txt')
}

min_dist <- function(curr_pos, chain, depth = 40){
  chain %>% strsplit("_") %>% .[[1]] %>% 
    purrr::map_df(function(x) {
      #print(x)
      find_dist_btw_points(loc0 = curr_pos, 
                loc1 = x, depth = depth) %>% 
                 select(tot_step) %>% mutate(pt = x)}) %>% 
    filter(tot_step == min(tot_step)) %>% 
    mutate(chain = chain) %>% distinct()
}

filter_keys <- function(key_rq, tmp_keys){
  key_rq %>% strsplit(",") %>% 
    purrr::map_lgl(function(x) sum(x %in% tmp_keys) == length(x))
}

run_routes <- function(first_chain){
  l_chain <- 0
  all_chains <- rounds_df %>% pull(chain)
  tmp_chains <- c(first_chain)
  tmp_keys <- c(rounds_df %>% filter(chain == first_chain) %>% 
                  pull(key_collected) %>% strsplit(",") %>% unlist())
  step <- rounds_df %>% filter(chain == first_chain) %>% pull(tot_step)
  
  i <- 1
  while(l_chain < length(all_chains)){
    chain_end <- tmp_chains[i] %>% strsplit("_") %>% .[[1]] %>% .[length(.)]
    
    candidate_df <- rounds_df %>% filter(!chain %in% tmp_chains) 
    
    if(nrow(candidate_df)> 1)  candidate_df <- candidate_df %>% 
      filter(filter_keys(key_used, tmp_keys))
    
    dist_df <- candidate_df$chain %>% pbapply::pblapply(function(x) 
    {min_dist(chain_end, x, 50)}, cl = 5) %>% bind_rows() 
    
    dist_df1 <- dist_df %>% 
      inner_join(rounds_df %>% select(chain, key_used, key_collected, rounds)) %>% 
      distinct() %>% 
      purrrlyr::by_row(~get_chain_length(.$chain, .$pt), .to = 'chain_length') %>% 
      tidyr::unnest(cols = 'chain_length')
    
    ##candidate df by filter out chains we don't have keys
    sel_chain <- dist_df1 %>% filter(tot_step == min(tot_step)) %>% 
      filter(chain_length == min(chain_length))
    
    step <- step + sel_chain$tot_step + sel_chain$chain_length
    tmp_chains <- c(tmp_chains, sel_chain$chain)
    tmp_keys <- c(tmp_keys, sel_chain$key_collected) %>% 
      strsplit(",") %>% unlist() %>% unique()
    l_chain <- length(tmp_chains)
    i <- i+1
  }
  return(step)
}

key_res <- pbapply::pblapply(letters, find_key, cl = 5) %>% bind_rows()

key_res <- key_res %>% 
  mutate(chain = gsub(paste0(3281 + c(0, 1, -1, 81, -81), "_") %>% 
                        paste(collapse = "|"), "", chain)) %>% 
  distinct()

key_res1 <- key_res %>% filter(purrr::map2_lgl(.x = loc1, 
            .y = tot_key, function(x,y) !grepl(x, y)))

clear_res <- clear_all_routes(key_res1)
  
rounds_df <- clear_res$rounds %>% bind_rows() %>% 
  arrange(rounds, tot_step)

round0_chains <- rounds_df %>% filter(rounds == 1) %>% pull(chain)
first_chain <- round0_chains[6]

library(tictoc)
tic()
res_list <- purrr::map(round0_chains, run_routes)
toc()

###part 2
input_mapb <- readLines('data/input day18b.txt')

input_mapb1 <- input_mapb %>% purrr::map(function(x){
  1:nchar(x) %>% purrr::map_chr(~substr(x, ., .))
}) %>% unlist() %>% 
  matrix(ncol = 81, byrow = T)

get_row_col <- function(df) {
  df %>% 
    mutate(
      cols = num %/% 81 + 1,
      rows = num %% 81,
      quad = case_when(
        rows <= 41 & cols <= 41 ~ '1', 
        rows <= 41 & cols >= 41 ~ '2', 
        rows >= 41 & cols <= 41 ~ '3', 
        rows >= 41 & cols >= 41 ~ '4'
      )
    )
}

rounds_df1 <- rounds_df %>% 
  mutate(  num = chain %>% strsplit("_") %>% 
             purrr::map_chr(~.[2]) %>% as.numeric()) %>% get_row_col()

start <- tibble::tibble(num = which(input_mapb1 == "@")) %>% get_row_col()


vault1 <- input_map1[1:41, 1:41]
vault2 <- input_map1[1:41, 41:81]
vault3 <- input_map1[41:81, 1:41]
vault4 <- input_map1[41:81, 41:81]

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

at_locs <- which(input_map1 == '@')
