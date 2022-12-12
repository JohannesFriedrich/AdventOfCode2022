Day12
================
Johannes Friedrich
12.12.2022

## Read input

``` r
input <- strsplit(readLines("input.txt"), "")
nrow <- length(input)
ncol <- length(input[[1]])
input <- unlist(input)

idx_E <- which(input == "E")
idx_S <- which(input == "S")

convert <- c(1:26,1,26)
names(convert) <- c(letters, "S", "E")
input <- convert[input]
```

## Part 1

``` r
# function to get all neighbors of given index k
map_k <- function(k) {
  m <- k %% ncol
  k + c(if (k < ncol*(nrow-1)+1) ncol, if (k > ncol) -ncol, if (m != 1) -1, if (m != 0) 1)
}

BFS <- function(start_idx){
  visited <- rep(FALSE, length(input))
  visited[start_idx] <- TRUE
  distance <- rep(0, length(input))
  
  queue <- c(start_idx)

  while (isFALSE(visited[idx_E]))  {
    if (length(queue) > 0){
      curr_idx <- queue[1]
      neighbors <- map_k(curr_idx)[(input[curr_idx] - input[map_k(curr_idx)]) >= -1] #neighbor edges
      queue <- c(queue, setdiff(neighbors[!visited[neighbors]], queue))
    
      distance[neighbors[!visited[neighbors]]] <- distance[curr_idx]+1L
      visited[neighbors] <- TRUE
      queue <- queue[-1]
    } else { return(NA) }
  }
  
  return(distance[idx_E])
}

BFS(idx_S)
```

    ## [1] 383

## Part 2

``` r
res <- sapply(which(input == 1), BFS)
min(res, na.rm = TRUE)
```

    ## [1] 377
