# 04.R
# Day 4 of Advent of Code 2026.
# Like day 1:3, only using R as a resource (so R help if needed / no gen ai or s/o etc)

## Setup.
renv::restore(prompt = FALSE)
options(scipen = 999)
# Required packages.
require(data.table)
require(reshape2)
require(fastmatch)
# Read input.
d = readLines("input/04")
d = data.frame(rbindlist(list(strsplit(d, ""))))
names(d) = 1:ncol(d)
rownames(d) = 1:nrow(d)
d = data.table(melt(as.matrix(d)))
setnames(d, c("row", "col", "v"))
setkey(d, row, col)


## Part 1.
fun <- function(d, pos, lt = 4){
    if(d[i]$v != "@") return(FALSE)
    r = as.numeric(d[pos]$row)
    ca = as.numeric(d[pos]$col)
    l = d[row %between% c(r-1, r+1) & col %between% c(ca-1, ca+1)][(row != r | col != ca)]$v
    sum(l == "@") < lt
}
for(i in 1:nrow(d)){
    d[i, qual := fun(d, i)]
}
sum(d[(qual)]$qual)
# 1467

## Part 2.
for(j in 1:1000){
    d[(qual), v := "x"]
    d[(qual), qual := FALSE]
    for(i in which(d$v == "@")){
        d[i, qual := fun(d, i)]
    }
    d[(qual), v := "x"]
    if(sum(d[(qual)]$qual) == 0) break
}
d[v == "x", .N]
# 8484
