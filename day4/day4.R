library(adventdrob)
library(tidyverse)
raw <- advent_input(day=4, year=2024)
input <- grid_matrix(raw, x)

# Part 1
# horizontal
splitm <- split(input, row(input))
h <- sum(unlist(map(splitm, ~sum(str_count(str_flatten(.),"XMAS") + str_count(str_flatten(.),"SAMX")))))
# vertical
splitm <- split(input, col(input))
v <- sum(unlist(map(splitm, ~sum(str_count(str_flatten(.),"XMAS") + str_count(str_flatten(.),"SAMX")))))
# rising
splitm <- split(input, row(input) + col(input))
r <- sum(unlist(map(splitm, ~sum(str_count(str_flatten(.),"XMAS") + str_count(str_flatten(.),"SAMX")))))
# descending
splitm <- split(input, row(input) - col(input))
d <- sum(unlist(map(splitm, ~sum(str_count(str_flatten(.),"XMAS") + str_count(str_flatten(.),"SAMX")))))

sum(h+v+r+d)

# Part 2
input <- grid_tidy(raw, x)
inputnw <- input |> mutate(row=row+1, col=col+1) |> select(row, col, valnw=value)
inputne <- input |> mutate(row=row+1, col=col-1) |> select(row, col, valne=value)
inputsw <- input |> mutate(row=row-1, col=col+1) |> select(row, col, valsw=value)
inputse <- input |> mutate(row=row-1, col=col-1) |> select(row, col, valse=value)

all <- left_join(input, inputnw) |> left_join(inputne) |> left_join(inputsw) |> left_join(inputse)

all |> filter(value=='A') |> filter(valne=="M", valnw=='M',valse=="S", valsw=="S") -> case1
all |> filter(value=='A') |> filter(valne=="S", valnw=='S',valse=="M", valsw=="M") -> case2
all |> filter(value=='A') |> filter(valne=="M", valnw=='S',valse=="M", valsw=="S") -> case3
all |> filter(value=='A') |> filter(valne=="S", valnw=='M',valse=="S", valsw=="M") -> case4

nrow(case1)+nrow(case2)+nrow(case3)+nrow(case4)
