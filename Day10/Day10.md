Day10
================
Johannes Friedrich
10.12.2022

## Read input

``` r
input <- as.integer(unlist(strsplit(readLines("input.txt"), " ")))
```

    ## Warning: NAs introduced by coercion

``` r
input[which(is.na(input))] <- 0L
```

## Part 1

``` r
idx <- seq(20L, 220L, 40L)
sum(cumsum(c(1L,input))[idx] * idx)
```

    ## [1] 17940

## Part 2

``` r
reg <- cumsum(c(1L,input)) + 1L
res <- sapply(1:240, \(x) if ((x%%40) %in% (reg[(x)]-1):(reg[(x)]+1)) "#" else ".")
res <- matrix(res, nrow = 6, ncol = 40, byrow = TRUE)
```

``` r
apply(res, 1, paste0, collapse = " ")
```

    ## [1] "# # # # . . # # . . # # # . . . # # . . . . # # . # # # # . . . # # . # # # # ."
    ## [2] ". . . # . # . . # . # . . # . # . . # . . . . # . # . . . . . . . # . . . . # ."
    ## [3] ". . # . . # . . . . # # # . . # . . # . . . . # . # # # . . . . . # . . . # . ."
    ## [4] ". # . . . # . . . . # . . # . # # # # . . . . # . # . . . . . . . # . . # . . ."
    ## [5] "# . . . . # . . # . # . . # . # . . # . # . . # . # . . . . # . . # . # . . . ."
    ## [6] "# # # # . . # # . . # # # . . # . . # . . # # . . # . . . . . # # . . # # # # ."
