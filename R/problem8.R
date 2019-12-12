library(dplyr)
library(readr)

img <- "123456789012"
w <- 3; h <- 2

get_img_layer <- function(img, w, h){
  digits <- 1:nchar(img) %>% 
    purrr::map_chr(~substr(img, ., .)) %>% as.numeric()
  layer_pixels <- w * h
  layer_n <- length(digits) / layer_pixels
  
  1:layer_n %>% purrr::map_df(function(i) {
    tibble(val = digits[((i-1)*layer_pixels + 1):(i * layer_pixels)]) %>% 
      mutate(layer_id = i, 
             position = seq(1, n(), 1))
  })
}

img <- readr::read_csv("data/input day8.csv", 
            col_types = cols(img = col_character())) %>% pull(img)

img_layer <- get_img_layer(img, w = 25, h = 6)

layer_zeros <- img_layer %>% filter(val == 0) %>% 
  group_by(layer_id) %>% summarise(layer_zero = n())

fewest_zeros <- layer_zeros %>% filter(layer_zero == min(layer_zero)) %>% 
  pull(layer_id)
num_1 <- img_layer %>% 
  filter(layer_id == fewest_zeros, val == 1) %>% nrow()
num_2 <- img_layer %>% 
  filter(layer_id == fewest_zeros, val == 2) %>% nrow()

##answer:
num_1 * num_2

##part2
final_img <- img_layer %>% 
  filter(val != 2) %>% 
  group_by(position) %>% 
  filter(layer_id == min(layer_id)) %>% 
  arrange(position) %>% 
  pull(val) %>% matrix(nrow = 6, byrow = T)

cv2 <- reticulate::import('cv2')
cv2$imwrite('final_img.png', final_img * 255)
