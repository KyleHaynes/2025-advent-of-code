# 09.R
# Day 9 of Advent of Code 2026.
# Like day 1:8, only using R as a resource (so R help if needed / no gen ai or s/o etc)

## Setup.
renv::restore(prompt = FALSE)
options(scipen = 999)
# Required packages.
require(data.table)
# Read input.
dd = readLines("input/09eg")
d = data.table(expand.grid(dd, dd))
d = d[Var1 != Var2]
d[, `:=`(
    x1 = as.numeric(gsub(",.*", "", Var1)),
    y1 = as.numeric(gsub(".*,", "", Var1)),
    x2 = as.numeric(gsub(",.*", "", Var2)),
    y2 = as.numeric(gsub(".*,", "", Var2))
)]
lens = 20
m <- matrix(1:lens, nrow = lens, ncol = lens, byrow = TRUE)
m[] = ""

dd <- data.table(dd)
dd[, `:=`(
    x = as.numeric(gsub(",.*", "", dd)),
    y = as.numeric(gsub(".*,", "", dd))
)]


## Part 1.
d[, l := abs(x1 - x2)+1]
d[, w := abs(y1 - y2)+1]
d[, a := l * w]
tail(d[order(a)], 1)$a
# 4763932976

## Part 2.
d

for(i in 1:(nrow(dd)-1)){
    p1 = dd[i, c(x, y)]
    p2 = dd[i+1, c(x, y)]
    x_or_y = fifelse(which(p1 == p2) == 2, "x", "y")
    # browser()
    if(x_or_y == "x"){
        m[p1[1]:p1[2], p2[2]] <- "X"
    } else {
        m[p1[2], p1[1]:p2[1]] <- "X"
    }
    # browser()
    # m[]
}

as.matrix(1:10, 1:10)
