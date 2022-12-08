Day08
================
Johannes Friedrich
08.12.2022

## Read input

``` r
input <- t(sapply(strsplit(readLines("input.txt"), ""), as.integer))
```

## Part 1

``` r
check_vis <- function(x,y){
  up <- all(input[x,y] > input[1:(x-1),y])
  down <- all(input[x,y] > input[(x+1):nrow(input),y])
  left <- all(input[x,y] > input[x,1:(y-1)])
  right <- all(input[x,y] > input[x,(y+1):ncol(input)])
  return(any(up, down, left, right))
  }

sum(sapply(2:(nrow(input)-1), \(x) sapply(2:(ncol(input)-1),  \(y)  check_vis(x,y)))) + 2*ncol(input) + 2*nrow(input) - 4
```

    ## [1] 1843

## Part 2

``` r
check_vis <- function(x,y){
  up <-  c(input[x,y] > input[(x-1):1,y], FALSE)
  down <- c(input[x,y] > input[(x+1):nrow(input),y], FALSE)
  left <- c(input[x,y] > input[x,(y-1):1], FALSE)
  right <- c(input[x,y] > input[x,(y+1):ncol(input)], FALSE)
  
  t <- sapply(list(up, down, left, right), \(x) which(x == FALSE))
  return(prod(sapply(t, \(x) ifelse(length(x) == 1,  x-1, x[1]))))
}

res <- sapply(2:(nrow(input)-1), \(x) sapply(2:(ncol(input)-1),  \(y)  check_vis(x,y)))

max(res)
```

    ## [1] 180000
