Day17
================
Johannes Friedrich
17.12.2022

## Read input

``` r
jet_dir <-  ifelse(strsplit(readLines("input.txt"), "")[[1]] == ">", 1, -1)

shapes <- list(
  line = seq(0+0i,3+0i,length.out = 4),
  cross = c(1+0i, 0+1i,1+1i,2+1i,1+2i),
  l = c(0+0i,1+0i,2+0i, 2+1i,2+2i),
  I = seq(0+0i,0+3i,length.out = 4),
  block = c(0+0i,1+0i,0+1i,1+1i)
)
```

## Part 1

``` r
occupied <- seq(0-1i,6-1i,length.out =7)
####
crash <- FALSE
jet_idx <- shape_idx <- 1
     
n <- 2022
for (rock in 1:n){
  ## appear
  falling <- shapes[[shape_idx]] + 2 + max(Im(occupied) + 4)*1i
    
  while(crash == FALSE){
    ## jet
    falling <- falling + jet_dir[jet_idx]
    if(min(Re(falling)) < 0 || 
       max(Re(falling)) > 6 ||
      length(intersect(falling, occupied)) !=0) { falling <- falling - jet_dir[jet_idx]}
    jet_idx <- (jet_idx %% length(jet_dir)) + 1
    ## get down
    falling <- falling - 1i
    ## check crash
    if (length(intersect(occupied, falling)) !=0) {
      falling <- falling + 1i
      occupied <- c(occupied, falling)
      break
    }
  }
  shape_idx <- (shape_idx %% 5)+ 1
}

max(Im(occupied))+1
```

    ## [1] 3119

## Part 2
