Day06
================
Johannes Friedrich
06.12.2022

## Read input

``` r
input <- strsplit(readLines("input.txt"), "")[[1]]
```

## Part 1

``` r
find_unique <- function(startIdx, len){
  t <- rle(sort(input[startIdx:(startIdx+len-1)]))
  return(all(t$lengths == 1))
}
which(sapply(seq_along(input), find_unique, 4))[1] + 3
```

    ## [1] 1100

## Part 2

``` r
which(sapply(seq_along(input), find_unique, 14))[1] + 13
```

    ## [1] 2421
