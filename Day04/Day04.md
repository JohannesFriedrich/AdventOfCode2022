Day04
================
Johannes Friedrich
04.12.2022

## Read input

``` r
input <- lapply(strsplit(readLines("input.txt"), "\\D+"), as.integer)
```

## Part 1

``` r
sum(sapply(input, \(x) (x[1] >= x[3] & x[2] <= x[4]) | (x[3] >= x[1] & x[4] <= x[2])))
```

    ## [1] 560

## Part 2

``` r
sum(sapply(input, \(x) length(intersect(c(x[1]:x[2]), c(x[3]:x[4]))) != 0))
```

    ## [1] 839
