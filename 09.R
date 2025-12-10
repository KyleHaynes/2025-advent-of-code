# 09.R
# Day 9 of Advent of Code 2026.
# Like day 1:8, only using R as a resource (so R help if needed / no gen ai or s/o etc)

## Setup.
renv::restore(prompt = FALSE)
options(scipen = 999)
# Required packages.
require(data.table)
require(fastmatch)
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
DD <- copy(dd)
dd <- copy(DD)
D <- copy(d)
d <- copy(D)

dr = data.table()

for(i in 1:(nrow(dd))){
    p1 = dd[i, c(y, x)]
    if(i == nrow(dd)){
        p2 = dd[1, c(y, x)]
    } else {
        p2 = dd[i+1, c(y, x)]
    }
    x_or_y = fifelse(which(p1 == p2) == 2, "x", "y")
    # browser()
    if(x_or_y == "x"){
        tmp = data.table(p1[1]:p2[1], p1[2])
    } else {
        tmp <- data.table(p1[1], p1[2]:p2[2])
    }
    dr = rbind(dr, tmp)
}

fun = function(position1, position2){
    # browser()
    x_min <- min(position1[1], position2[1]) + 1
    x_max <- max(position1[1], position2[1]) - 1
    y_min <- min(position1[2], position2[2]) + 1
    y_max <- max(position1[2], position2[2]) - 1
    c(x = c(x_min, x_max), y = c(y_min, y_max))
}
d[, vally := NA]
d <- d[order(-a)]
for(i in 1:nrow(d)){
    if(i == 1000) print(i)
    if(i == 10000) print(i)
   d[l <<- row.names(d) == i & (x1 == x2 | y1 == y2), vally := TRUE]
   if(any(l)) next
   test <- fun(c(d[i, y1], d[i, x1]), c(d[i, y2], d[i, x2]))
   test <- dr[V1 %between% c( test[[1]],  test[[2]]) & V2 %between% c( test[[3]],  test[[4]])]
   ifelse(nrow(test) == 0, d[i, vally := TRUE], d[i, vally := FALSE])
}

d[(vally),]




