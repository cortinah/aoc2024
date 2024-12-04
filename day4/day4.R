library(adventdrob)
library(tidyverse)
input <- advent_input(day=4, year=2024)
input <- grid_matrix(input, x)
pad <- matrix(" ",nrow = 200, ncol = 200)
pad[31:170,31:170] <- input

# Part 1

# horizontal
splitm <- split(pad, row(pad))
h <- sum(unlist(map(splitm, ~sum(str_count(str_flatten(.),"XMAS|SAMX")))))

# vertical
splitm <- split(pad, col(pad))
v <- sum(unlist(map(splitm, ~sum(str_count(str_flatten(.),"XMAS|SAMX")))))

# rising
splitm <- split(pad, row(pad) + col(pad))
r <- sum(unlist(map(splitm, ~sum(str_count(str_flatten(.),"XMAS|SAMX")))))

# descending
splitm <- split(pad, row(pad) - col(pad))
d <- sum(unlist(map(splitm, ~sum(str_count(str_flatten(.),"XMAS|SAMX")))))

sum(h+v+r+d)
1573 too low
