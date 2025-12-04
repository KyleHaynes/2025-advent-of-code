# 02.R
# Day 2 of Advent of Code 2026.
# Like day 1, only using R as a resource (so R help if needed / no gen ai or s/o etc)

## Setup.
renv::restore(prompt = FALSE)
options(scipen = 999)
# Required packages.
require(data.table)
# Read input.
d = readLines("input/02")

## Part 1.
# Prep data.
d = strsplit(d, ",")[[1]]
d = gsub("\\-", ":", d)
d = lapply(d, function(x) {
    eval(parse(text=x))
})
d = unlist(d, use.names = F)
d = data.table(v = d, n = nchar(d))
d[, even := n %% 2]
d[even == 0, l := substring(v, 1, n/2)]
d[even == 0, r := substring(v, (n/2)+1, n)]
sum(as.numeric(d[even == 0 & l == r]$v))
# 803030

## Part 2.
md = max(d$n)
# 10 digits is the max string length, so 2 chunks of 5 is the max.
fun = function(x){
    is_valid = FALSE
    chnks = 1:ceiling(nchar(x)/2)
    for(i in chnks){
        a = seq(1, nchar(x), i)
        b = seq(1, nchar(x), i)+(i-1)
        if(any(b > nchar(x))) next
        r = substring(x, a, b)
        m = max(nchar(r))
        rr = r[nchar(r) == m]
        if(length(unique(rr)) == 1 & length(rr) > 1){
            is_valid = TRUE
            break
        }
    }
    is_valid
}
system.time(d[, valid := sapply(v, fun)])
# Expectedly, pathetically slow :(

sum(as.numeric(d[(valid)]$v))
# 48631958998