# 01.R
# Day 1 of Advent of Code 2026.
# It ain't pretty, but it gets the job done.

## Setup.
renv::restore(prompt = FALSE)
# Required packages.
require(data.table)
# Read input.
d <- readLines("input/01")
# Prep data.
index <- 0:99
index <- rep(index, 1E5)
pos <- which(index == 0)[length(which(index == 0))/2]
index[pos]
d <- data.table(n = c(50, as.numeric(gsub("\\D+", "", d))), d = c("R", gsub("\\d+", "", d)))

## Part 1.
d[, n := fifelse(d == "L", n * -1, n)]
d[, c := cumsum(n)]
d[, c := c + pos]
d[, c := index[c]]
sum(d$c == 0)
# 1102

## Part 2.
d[, p := cumsum(n)]
d[, p := p + pos]
d[, x := as.numeric(NA)]

# A slow loop, however, it works. The indexing is gross because I'm lazy, dumb. It's convoluted
# because you need to exclude the starting/end positions.
for(i in 1:(nrow(d)-1)) {
    xx <- as.numeric(sum(index[c(d$p[i]:d$p[i+1])][-1][1:length(index[c(d$p[i]:d$p[i+1])][-1])-1] == 0))
    d[i, x := xx]
}

sum(d$c == 0) + sum(d$x, na.rm = T)
# 6175


