library(dplyr)
library(purrr)
library(combinat)



set_ast_df <- function(tst_input) {
  1:length(tst_input) %>% 
    purrr::map_df(function(i) {
      row_txt <- tst_input[i]
      1:nchar(row_txt) %>% 
        purrr::map_df(function(x)
          tibble(col = x - 1, txt = substr(row_txt, x, x)) %>% 
            mutate(row = i - 1)
        )
    }) %>% mutate(id = seq(1, n(), 1))
}

tst_input <- readr::read_csv("data/input day10.csv" ) %>% pull(map)

ast_df <- set_ast_df(tst_input)
asts <- ast_df %>% filter(txt == "#")
ast_pair_df <- purrr::cross_df(list(id1 =asts$id, id2 = asts$id)) %>% 
  filter(id1 != id2) %>% arrange(id1, id2) 

ast_pair_df1 <- ast_pair_df %>% 
  inner_join(ast_df, by = c('id1' = 'id')) %>% 
  inner_join(ast_df, by = c('id2' = 'id')) %>% 
  mutate(h_dist = col.y - col.x, 
         h_dist_sign = ifelse(h_dist>=0, 1, -1),
         v_dist = -(row.y - row.x),
         dist = sqrt((row.y - row.x)^2 + (col.y - col.x)^2),
         slope = (row.y - row.x)/(col.y - col.x)) %>% 
  group_by(id1, slope, h_dist_sign) %>% 
  mutate(ct = n()) %>% arrange(id1, slope, h_dist_sign) 

ast_pair_df2 <- ast_pair_df1 %>% 
  filter(dist == min(dist))

best_df <- ast_pair_df2 %>% group_by(id1, col.x, row.x) %>% tally() %>% 
  ungroup() %>% filter(n == max(n))
best_df


#part2
laser_df <- ast_pair_df1 %>% filter(id1 == best_df$id1)
laser_df1 <- laser_df %>% ungroup() %>% 
  mutate(sort = round(v_dist/dist * h_dist_sign, 5)) %>% 
  arrange(-h_dist_sign, -sort, dist) %>% 
  group_by(h_dist_sign, sort) %>% 
  mutate(laser_round = seq(1, n(), 1)) %>% 
  arrange(laser_round, -h_dist_sign, -sort) %>% 
  ungroup() %>% 
  mutate(rank = seq(1, n(), 1))

rows <- c(1, 2, 3, 10, 20, 50, 100, 199, 200, 201, 299)
laser_df1[rows, ] %>% 
  select(id2, col.y, row.y, rank) %>% 
  mutate(answer = col.y * 100 + row.y)

