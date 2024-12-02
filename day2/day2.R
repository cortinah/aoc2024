library(adventdrob)
library(tidyverse)
input <- advent_input(day=2, year=2024)

# Part 1
input |> separate_wider_delim(x, delim=" ", names_sep = ".", too_few = "align_start") -> input
input |> mutate(across(everything(), as.numeric)) -> input
input |> rowid_to_column("ID") -> input

input |> mutate(
  a=x.2 - x.1,
  b=x.3 - x.2,
  c=x.4 - x.3,
  d=x.5 - x.4,
  e=x.6 - x.5,
  f=x.7 - x.6,
  g=x.8 - x.7) -> output

output |> rowwise() |> mutate(absmax = abs(max(c_across(a:g), na.rm = T))) |>
  mutate(absmin = abs(min(c_across(a:g), na.rm = T))) -> output

output |> filter(absmin!=0, absmax<=3, absmin<=3, absmax!=0) -> output

output |> filter(sign(a)==sign(b)) |> filter(sign(b)==sign(c)) |>
  filter(sign(c)==sign(d)) |> filter(is.na(e) || (sign(d)==sign(e))) |>
  filter(is.na(f) || (sign(e)==sign(f))) |> filter(is.na(g) || (sign(f)==sign(g))) -> outputpart1

# Part 2

dampener <- function(input) {
  input |> mutate(
    a=x.2 - x.1,
    b=x.3 - x.2,
    c=x.4 - x.3,
    d=x.5 - x.4,
    e=x.6 - x.5,
    f=x.7 - x.6) -> output

  output |> rowwise() |> mutate(absmax = abs(max(c_across(a:f), na.rm = T))) |>
    mutate(absmin = abs(min(c_across(a:f), na.rm = T))) -> output

  output |> filter(absmin!=0, absmax<=3, absmin<=3, absmax!=0) -> output

  output |> filter(sign(a)==sign(b)) |> filter(sign(b)==sign(c)) |>
    filter(is.na(d) || (sign(c)==sign(d))) |> filter(is.na(e) || (sign(d)==sign(e))) |>
    filter(is.na(f) || (sign(e)==sign(f))) -> output
  return(output)

}

input |> select(-2) -> output; colnames(output) <- colnames(input)[1:8]; dampener(output) -> outputx1
input |> select(-3) -> output; colnames(output) <- colnames(input)[1:8]; dampener(output) -> outputx2
input |> select(-4) -> output; colnames(output) <- colnames(input)[1:8]; dampener(output) -> outputx3
input |> select(-5) -> output; colnames(output) <- colnames(input)[1:8]; dampener(output) -> outputx4
input |> select(-6) -> output; colnames(output) <- colnames(input)[1:8]; dampener(output) -> outputx5
input |> select(-7) -> output; colnames(output) <- colnames(input)[1:8]; dampener(output) -> outputx6
input |> select(-8) -> output; colnames(output) <- colnames(input)[1:8]; dampener(output) -> outputx7
input |> select(-9) -> output; colnames(output) <- colnames(input)[1:8]; dampener(output) -> outputx8

length(unique(c(outputpart1$ID,outputx1$ID,
              outputx2$ID, outputx3$ID, outputx4$ID, outputx5$ID, outputx6$ID, outputx7$ID, outputx8$ID)))
