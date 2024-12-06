library(adventdrob)
library(tidyverse)
input <- advent_input(day=6, year=2024)
input <- grid_tidy(input, x)

# Part 1
input |> filter(value=='^') -> initial
input$value <- ifelse(input$value=='^', 'x', input$value)
input$id <- 1:nrow(input)

step <- function(ilist) {

  irow=ilist[[1]]; icol=ilist[[2]]; idir=ilist[[3]]

  if (idir=='u') {nextpos <- input |> filter(row==(irow-1), col==icol) |> select(value)
                 if (as.character(nextpos)=='.' || nrow(nextpos)==0 || as.character(nextpos)=='x') {irow <- (irow-1)} else {idir <- rotate(idir)} }
  else if (idir=='d') {nextpos <- input |> filter(row==(irow+1), col==icol) |> select(value)
                 if (as.character(nextpos)=='.' || nrow(nextpos)==0 || as.character(nextpos)=='x') {irow <- (irow+1)} else {idir <- rotate(idir)} }
  else if (idir=='l') {nextpos <- input |> filter(row==irow, col==(icol-1)) |> select(value)
                 if (as.character(nextpos)=='.' || nrow(nextpos)==0 || as.character(nextpos)=='x') {icol <- (icol-1)} else {idir <- rotate(idir)} }
  else if (idir=='r') {nextpos <- input |> filter(row==irow, col==(icol+1)) |> select(value)
                 if (as.character(nextpos)=='.' || nrow(nextpos)==0 || as.character(nextpos)=='x') {icol <- (icol+1)} else {idir <- rotate(idir)} }

  return(list(irow, icol, idir, as.character(nextpos)))

}

rotate <- function(dir) {
  directions <- c('u','r','d','l')
  return (directions[((which(directions==dir)) %% 4)+1]) }


stepcount <- 1
nextstep <- list(initial$row, initial$col, 'u')

repeat {
#  print(paste(stepcount, nextstep))
  nextstep <- step(nextstep)

 if (nextstep[[1]]==0 || nextstep[[1]]==131) break()
 if (nextstep[[2]]==0 || nextstep[[2]]==131) break()
 stepcount <- stepcount + 1
 input |> filter(row==nextstep[[1]], col==nextstep[[2]]) |> select(id) -> walkid
 input[as.numeric(walkid),"value"] <- 'x'
}

input |> filter(value=='x') |> nrow()

## Part 2
