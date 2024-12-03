library(tidyverse)

input <- read_lines("input.txt") |> str_flatten()

# Part 1
out <- str_extract_all(input, "mul\\([0-9]{1,3},[0-9]{1,3}\\)")
out <- as.numeric(str_extract_all(out, "[0-9]{1,3}")[[1]])
sum(out[c(T,F)]*out[c(F,T)])


# Part 2
out <- str_replace_all(input, "don't\\(\\).*?do\\(\\)","")
out <- str_replace_all(out, "don't\\(\\).*","")

out <- str_extract_all(out, "mul\\([0-9]{1,3},[0-9]{1,3}\\)")
out <- as.numeric(str_extract_all(out, "[0-9]{1,3}")[[1]])
sum(out[c(T,F)]*out[c(F,T)])
