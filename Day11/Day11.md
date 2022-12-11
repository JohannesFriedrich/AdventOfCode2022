Day11
================
Johannes Friedrich
11.12.2022

## Read input

``` r
get_integers <- function(x) as.integer(regmatches(x, gregexpr("[0-9]+",x))[[1]])

init <- function(file) {
  input <- readLines(file)
  input <- split(input, rep(1:(length(which(input == ""))+1), each = 7))
  lapply(input, \(x){
    return(list(
      old = get_integers(x[2]),
      operation = substr(x[3], 20, nchar(x[3])),
      test = get_integers(x[4]),
      true_to = get_integers(x[5]),
      false_to = get_integers(x[6]),
      inspections = 0L
    ))
  })
}
```

## Part 1

``` r
data <- init("input.txt")
```

    ## Warning in split.default(input, rep(1:(length(which(input == "")) + 1), : data
    ## length is not a multiple of split variable

``` r
for (round in 1:20){
  for (monkey in seq_along(data)){
    
    current_monkey <- data[[monkey]]
    if (length(current_monkey$old) != 0) {
      worry_levels <- eval(parse(text = current_monkey$operation), envir = list2env(current_monkey))%/% 3
      test_ok <-  worry_levels %% current_monkey$test == 0L
      for (i in seq_along(test_ok)){
        data[[monkey]]$inspections <- data[[monkey]]$inspections+1
        if (isTRUE(test_ok[i])) {
          data[[current_monkey$true_to+1]]$old <-  c(data[[current_monkey$true_to+1]]$old, worry_levels[i]) 
        } else {
          data[[current_monkey$false_to+1]]$old <-  c(data[[current_monkey$false_to+1]]$old, worry_levels[i])
        }
      }
      data[[monkey]]$old <- c()
    }
  }
  
}

prod(tail(sort(sapply(data, \(x) x$inspections)),2))
```

    ## [1] 120056

## Part 2

``` r
data <- init("input.txt")
```

    ## Warning in split.default(input, rep(1:(length(which(input == "")) + 1), : data
    ## length is not a multiple of split variable

``` r
gcd <- prod(sapply(data, \(x) x$test))


for (round in 1:10000){
  for (monkey in seq_along(data)){
    
    current_monkey <- data[[monkey]]
    temp_envir <- list2env(current_monkey)
    if (length(current_monkey$old) != 0) {
      worry_levels <- eval(parse(text = current_monkey$operation), envir = temp_envir) %% gcd
      test_ok <-  worry_levels %% current_monkey$test == 0L
      for (i in seq_along(test_ok)){
        data[[monkey]]$inspections <- data[[monkey]]$inspections + 1L
        if (isTRUE(test_ok[i])) {
          data[[current_monkey$true_to+1]]$old <-  c(data[[current_monkey$true_to+1]]$old, worry_levels[i])
        } else {
          data[[current_monkey$false_to+1]]$old <-  c(data[[current_monkey$false_to+1]]$old, worry_levels[i])
        }
      }
      data[[monkey]]$old <- c()
    }
  }
  
}

prod(tail(sort(sapply(data, \(x) x$inspections)),2))
```

    ## [1] 21816744824
