library(dplyr)

input <- '59702216318401831752516109671812909117759516365269440231257788008453756734827826476239905226493589006960132456488870290862893703535753691507244120156137802864317330938106688973624124594371608170692569855778498105517439068022388323566624069202753437742801981883473729701426171077277920013824894757938493999640593305172570727136129712787668811072014245905885251704882055908305407719142264325661477825898619802777868961439647723408833957843810111456367464611239017733042717293598871566304020426484700071315257217011872240492395451028872856605576492864646118292500813545747868096046577484535223887886476125746077660705155595199557168004672030769602168262'
num <- 1:nchar(input) %>% purrr::map_dbl(~as.numeric(substr(input, ., .)))

create_phase_mat <- function(n){
  base_pat <- c(0, 1, 0, -1)
  
  phase_mat <- purrr::map(1:n, function(i){
    u_pat <- rep(base_pat, each=i)
    pat <- rep(u_pat, n %/% length(u_pat) + 1)[2:(n+1)]
    return(pat)
  }) %>% unlist() %>% matrix(., ncol = n, byrow = F)
            
}

input <- '59702216318401831752516109671812909117759516365269440231257788008453756734827826476239905226493589006960132456488870290862893703535753691507244120156137802864317330938106688973624124594371608170692569855778498105517439068022388323566624069202753437742801981883473729701426171077277920013824894757938493999640593305172570727136129712787668811072014245905885251704882055908305407719142264325661477825898619802777868961439647723408833957843810111456367464611239017733042717293598871566304020426484700071315257217011872240492395451028872856605576492864646118292500813545747868096046577484535223887886476125746077660705155595199557168004672030769602168262'
num <- 1:nchar(input) %>% purrr::map_dbl(~as.numeric(substr(input, ., .)))
phase_mat <- create_phase_mat(n = length(num))

#nums <- list()
for (i in 1:100 ){
  #nums[[i]] <- num
  num <- abs(as.vector(num %*% phase_mat)) %% 10
}
  
num[1:8]

#part2 
quick_chk <- function(num, times = 10, row = 1, pos = 1) {
  base_pat <- c(0, 1, 0, -1)
  n <- length(num)
  upat <- rep(base_pat, each = row)
  pat <- rep(upat, ceiling(n * times / length(upat)) + 1 )[2:(n * times +1)]
  pat[(1:(n * times)) %% n == pos]
}

quick_chk(num, times = 10000, row = 1, pos = 1) %>% table()

create_phase_mat_2 <- function(n, times = 10000){
  base_pat <- c(0, 1, 0, -1)
  #prog_bar <- dplyr::progress_estimated(n * times)
  
  phase_mat <- purrr::map(1:(n*times), function(i){
    u_pat <- rep(base_pat, each=i)
    pat <- rep(u_pat, (n*times) %/% length(u_pat) + 1)[2:((n*times)+1)]
    l_u_pat <- length(u_pat)
    
    pat1 <- purrr::map_dbl(1:n, function(k){
      sum(pat[seq(0, times -1, 1) * n + k])
    })
  
    #prog_bar$tick()$print()
    return(pat1)
  }) %>% unlist() %>% matrix(., ncol = n, byrow = F)
  
  return(phase_mat)
}

input <- '03036732577212944063491565474664'
num <- 1:nchar(input) %>% purrr::map_dbl(~as.numeric(substr(input, ., .)))
numf <- rep(num, 10000); (skip <- num[1:7] %>% paste(collapse = "") %>% as.numeric()) 

library(profvis)

profvis::profvis({
  tictoc::tic()
  phase_mat <- create_phase_mat_2(n = nchar(input), times = 1000)
  tictoc::toc()
})


for (i in 1:100 ){
  #nums[[i]] <- num
  num <- abs(as.vector(num %*% phase_mat)) %% 10
}


numf[(skip + 1):(skip + 8)]


  