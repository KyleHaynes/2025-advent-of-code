# 03.R
# Day 3 of Advent of Code 2026.
# Like day 1:2, only using R as a resource (so R help if needed / no gen ai or s/o etc)

## Setup.
renv::restore(prompt = FALSE)
options(scipen = 999)
# Required packages.
require(data.table)
# Read input.
d = data.table(v = readLines("input/03"))

## Part 1.
d[, sum := as.numeric(lapply(v, function(x){
    as.numeric(sum(as.numeric(data.table(t(combn(strsplit(x, "")[[1]], 2)))[, t := paste0(V1, V2)][order(-t)][1]$t)))
}))]
sum(d$sum)
# 17445

## Part 2.
# Define a gross function.
fun = function(x){
    x = strsplit(x, "")[[1]]
    vec = as.numeric()
    for(i in 11:0){
        tm = x[1:(length(x) - i)]
        max_post = which(x == max(tm))[1]
        max_val = as.numeric(x[which(x == max(tm))[1]])
        x = x[(max_post + 1):length(x)]
        vec = paste0(vec, max_val)
    }
    as.numeric(vec)
}
d[, sum2 := as.numeric(lapply(v, fun))]
sum(d$sum2)
# 173229689350551
