Day09
================
Johannes Friedrich
09.12.2022

## Read input

``` r
input <- read.table("input.txt", sep = " ", col.names = c("dir", "value"))
tail_path <- head_pos <- tail_pos <- 0+0i

positions <- cumsum(c(R = 1, L = -1, U = 1i, D = -1i)[input$dir] * input$value)
```

## Part 1

``` r
for (step in seq_along(input$dir)){
  new_path <- seq(head_pos, positions[step], length.out = input$value[step]+1)
  head_pos <- positions[step]
  ## remove entry 1 (tail is already there)
  ## and the last one is the position of the head -> FALSE
  tail_idx_ok <- c(tail(abs(new_path - tail_pos) >=2,-1), FALSE)
  tail_path <- c(tail_path, new_path[tail_idx_ok])
  tail_pos <- tail(tail_path,1)
}

length(unique(tail_path))
```

    ## [1] 5735

## Part 2
