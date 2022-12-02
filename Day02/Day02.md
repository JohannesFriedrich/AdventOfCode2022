Day02
================
Johannes Friedrich
02.12.2022

## Read input

``` r
input <- read.table("input.txt", sep = " ")
```

Winning conditions: X \> C, Y \> A, Z \> B

``` r
check_points <- function(round){
  if ((round[2] == "X" & round[1] == "A") |(round[2] == "Y" & round[1] == "B") | (round[2] == "Z" & round[1] == "C")) return(3)
  else if (round[2] == "X" & round[1] == "C") return(6)
  else if (round[2] == "Y" & round[1] == "A") return(6)
  else if (round[2] == "Z" & round[1] == "B") return(6)
  else return(0)
}
```

## Part 1

``` r
points_shape <- c(X = 1, Y = 2, Z = 3)[input$V2]
points_win <- apply(input, 1, \(round) check_points(round))

sum(points_win) +  sum(points_shape)
```

    ## [1] 15337

## Part 2

“X means you need to lose,Y means you need to end the round in a draw,
and Z means you need to win. Good luck!”

``` r
check_condition <- function(round){
  if (round[2] == "X") {
    if (round[1] == "A") return("Z")
    if (round[1] == "B") return("X")
    if (round[1] == "C") return("Y")
  }
  if (round[2] == "Y") {
    if (round[1] == "A") return("X")
    if (round[1] == "B") return("Y")
    if (round[1] == "C") return("Z")
    }
  if (round[2] == "Z") {
    if (round[1] == "A") return("Y")
    if (round[1] == "B") return("Z")
    if (round[1] == "C") return("X")
  }
}

input$V2 <- apply(input, 1, \(round) check_condition(round))

points_shape <- c(X = 1, Y = 2, Z = 3)[input$V2]
points_win <- apply(input, 1, \(round) check_points(round))

sum(points_win) +  sum(points_shape)
```

    ## [1] 11696
