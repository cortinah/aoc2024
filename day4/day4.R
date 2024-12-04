library(adventdrob)
library(tidyverse)
library(omnibus)
input <- advent_input(day=4, year=2024)
input <- grid_matrix(input, x)
pad <- matrix(" ",nrow = 220, ncol = 220)
pad[41:180,41:180] <- input

# Part 1
h <- v <- r1 <- r2 <- numeric(length=nrow(pad))
for(i in seq_len(nrow(pad))) {
h[i] <- str_count(str_flatten(pad[i,]),"XMAS|SAMX")
}

# rotate 90
r <- t(pad[nrow(pad):1,])
r[is.na(r)] <- ''
for(i in seq_len(nrow(r))) {
  v[i] <- str_count(str_flatten(r[i,]),"XMAS|SAMX")
}

# rotate 45
r <- rotateMatrix(pad, 46)
r[is.na(r)] <- ''
for(i in seq_len(nrow(r))) {
  r1[i] <- str_count(str_flatten(r[i,]),"XMAS|SAMX")
}

# rotate 135
r <- rotateMatrix(pad, 135)
r[is.na(r)] <- ''
for(i in seq_len(nrow(r))) {
  r2[i] <- str_count(str_flatten(r[i,]),"XMAS|SAMX")
}

r1[is.na(r1)] <- 0
r2[is.na(r2)] <- 0
sum(h+v+r1+r2)
1573 too low
