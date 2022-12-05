Day 05
================
Johannes Friedrich
05.12.2022

## Read input

``` r
input <- readLines("input.txt")
split <- which(input == "")
nrStacks <- tail(as.integer(strsplit(input[split-1], "\\s")[[1]]), 1)
  
instructions <- lapply(input[(split+1):(length(input))], \(x) {
  match <- gregexpr("[0-9]+",x)
  as.integer(regmatches(x, match)[[1]])
  })

field <- read.fwf("input.txt", c(rep(4, nrStacks-1), 3), n = length(input)-length(instructions)-2)
field <- lapply(field, \(x) {
  t <- gsub("\\s+|\\[|\\]","",x)
  t[which(nchar(t) != 0)]
  })
```

## Part 1

``` r
perform_moves <- function(instructions, field){
  for (instr in instructions){
    for (moves in 1:instr[1]){
      field[[instr[3]]] <- c(field[[instr[2]]][1],field[[instr[3]]])
      field[[instr[2]]] <- tail(field[[instr[2]]],-1)
    }
  }
  return(paste0(sapply(field, "[",1), collapse = ""))
}

perform_moves(instructions, field)
```

    ## [1] "FWNSHLDNZ"

## Part 2

``` r
perform_moves <- function(instructions, field){
  for (instr in instructions){
      field[[instr[3]]] <- c(field[[instr[2]]][1:instr[1]],field[[instr[3]]])
      field[[instr[2]]] <- tail(field[[instr[2]]],-instr[1])
  }
  return(paste0(sapply(field, "[",1), collapse = ""))
}
perform_moves(instructions, field)
```

    ## [1] "RNRGDNFQG"
