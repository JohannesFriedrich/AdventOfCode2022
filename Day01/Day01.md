Day01
================
Johannes Friedrich
01.12.2022

## Read input

``` r
input <- scan(file = "input.txt", what = integer(), blank.lines.skip = FALSE)

data <- split(input, cumsum(is.na(input)))
data <- sapply(data, \(x) sum(x,na.rm = TRUE))
```

## Part 1

``` r
max(data)
```

    ## [1] 74198

## Part 2

``` r
sum(sort(data, decreasing = TRUE)[1:3])
```

    ## [1] 209914
