# 05.R
# Day 5 of Advent of Code 2026.
# Like day 1:4, only using R as a resource (so R help if needed / no gen ai or s/o etc)

## Setup.
renv::restore(prompt = FALSE)
options(scipen = 999)
# Required packages.
require(data.table)
require(fastmatch)
# Read input.
d = data.table(v = readLines("input/05"))
ran = d[v %flike% "-"]
ran[, v := gsub("\\-", ":", v)]
d = d[!v %flike% "-" & v != ""]
d[, qual := FALSE]

## Part 1.
for(i in 1:nrow(ran)){
    d[(!qual) & as.numeric(v) %between% c(gsub(":.*", "", ran[i]$v), gsub(".*:", "", ran[i]$v)), qual := TRUE]
}
d[(qual), .N]
# 821

## Part 2.
ran[, l := as.numeric(gsub(":.*", "", v))]
ran[, r := as.numeric(gsub(".*:", "", v))]
ran[r < l] # all positive ranges.

ran[, con := list(list())]
for(i in 1:nrow(ran)){
    for(j in 1:nrow(ran)){
       test = ran[i]$r %between% c(ran[j]$l, ran[j]$r) | ran[i]$l %between% c(ran[j]$l, ran[j]$r)
       if(test) ran[j, con := list(c(con[[1]], i))]
    }
}

for(i in 1:nrow(ran)){
    for(j in which(1:nrow(ran) != i)){
        test <- any(ran[i]$con[[1]] %in% ran[j]$con[[1]]) | any(ran[j]$con[[1]] %in% ran[i]$con[[1]])
        if(test) ran[i, con := list(sort(unique(c(ran[i]$con[[1]] , ran[j]$con[[1]]))))]
    }
}
for(i in 1:nrow(ran)){
    for(j in which(1:nrow(ran) != i)){
        test <- any(ran[i]$con[[1]] %in% ran[j]$con[[1]]) | any(ran[j]$con[[1]] %in% ran[i]$con[[1]])
        if(test) ran[i, con := list(sort(unique(c(ran[i]$con[[1]] , ran[j]$con[[1]]))))]
    }
}
for(i in 1:nrow(ran)){
    for(j in which(1:nrow(ran) != i)){
        test <- any(ran[i]$con[[1]] %in% ran[j]$con[[1]]) | any(ran[j]$con[[1]] %in% ran[i]$con[[1]])
        if(test) ran[i, con := list(sort(unique(c(ran[i]$con[[1]] , ran[j]$con[[1]]))))]
    }
}

n = lapply(ran$con, function(x)  x[!x %in% NA])

ran[, ll := as.character(NA)]
ran[, rr := as.character(NA)]

for(i in 1:nrow(ran)){
    if(length(n[[i]]) == 0) next
    rows <- unique(c(i, n[[i]]))
    ran[i, ll := as.character(min(unlist(ran[rows, .(l, r)])))]
    ran[i, rr := as.character(max(unlist(ran[rows, .(l, r)])))]
}

f = rbind(ran[is.na(ll)], unique(ran[!is.na(ll)], by = c("ll", "rr")))
f[is.na(ll), `:=`(
    ll = l,
    rr = r
)]

f[, res := length(eval(parse(text=paste(ll, ":", rr)))), 1:nrow(f)]

f[!duplicated(con)]

sum(f$res)
#352719385730620