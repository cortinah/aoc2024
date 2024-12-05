library(adventdrob)
library(tidyverse)
input <- advent_input(day=5, year=2024)
rules <- input[1:1176,];  pages <- input[1178:1373,]
rules |> separate_wider_delim(x, delim="|", names=c('before','after')) -> rules
pages |> separate_wider_delim(x, delim=',',names_sep = 'x', too_few = 'align_start') -> pages

# Part 1
check_row <- function(row, rules) {
        row <- row[!is.na(as.numeric(row))]
        l <- length(row)
        page_checked = 1L
        while(page_checked < l) {
        i = page_checked
        while (i<l){
        check <- nrow(filter(rules, before==as.numeric(row[i]), after==as.numeric(row[i+1])))
        if (check==0) return(FALSE)
        i <- i+1  }
        page_checked <- page_checked + 1 }
        return(TRUE) }

checked <- logical(nrow(pages))
len <- numeric(nrow(pages))
for (i in 1:nrow(pages)) checked[i] <- check_row(pages[i,], rules)
for (i in 1:nrow(pages)) len[i] <- length(pages[i,!is.na(as.numeric(pages[i,]))])
len <- ceiling(len/2)
pages <- cbind(pages, len)

passed <- pages[checked,]
sum <- 0
for (i in 1:nrow(passed)) sum <- sum + as.numeric(passed[i, passed[i,"len"]])


# Part 2
pages$len <- NULL
failed <- pages[!checked,]

fix_row <- function(row, rules) {
  row <- row[!is.na(as.numeric(row))]
  l <- length(row)
  page_checked = 1L
  while(page_checked < l) {
    i = page_checked
    while (i<l){
      check <- nrow(filter(rules, before==as.numeric(row[i]), after==as.numeric(row[i+1])))
      if (check==0) {temp <- row[i]; row[i]<-row[i+1]; row[i+1]<-temp; page_checked=1L; i=0}
      i <- i+1  }
    page_checked <- page_checked + 1 }
  return(as.numeric(row)) }

fixed <- list()
for (i in 1:nrow(failed)) fixed[[i]] <- fix_row(failed[i,], rules)

expand <- function(t) get("t")[1:23]
fixed <- map(fixed, expand)
fixed <- as.data.frame(do.call(rbind, fixed))

len <- numeric(nrow(fixed))
for (i in 1:nrow(fixed)) len[i] <- length(fixed[i,!is.na(as.numeric(fixed[i,]))])
len <- ceiling(len/2)
fixed <- cbind(fixed, len)

sum <- 0
for (i in 1:nrow(fixed)) sum <- sum + as.numeric(fixed[i, fixed[i,"len"]])
