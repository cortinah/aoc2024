library(adventdrob)
library(tidyverse)
input <- advent_input(day=1, year=2024)

# Part 1
input |> separate_wider_delim(x, delim="   ", names=c('a','b')) -> input

# input |> separate_wider_regex(x, patterns = c(a="\\d+","\\s+",b="\\d+") )
sum(abs(sort(as.numeric(input$a)) - sort(as.numeric(input$b))))

# Part 2
count <- as.data.frame(table(input$b))
colnames(count)[1] <- 'a'
left <- data.frame(a=input$a)
left <- left_join(left,count)
left |> mutate(prod=as.numeric(a)*Freq) -> left
sum(left$prod, na.rm = T)
