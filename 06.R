# 06.R
# Day 6 of Advent of Code 2026.
# Like day 1:5, only using R as a resource (so R help if needed / no gen ai or s/o etc)

## Setup.
renv::restore(prompt = FALSE)
options(scipen = 999)
# Required packages.
require(data.table)
# Read input.
d = readLines("input/06")
last = d[length(d)]
split = as.vector(gregexpr("\\+ ?|\\* ?", last)[[1]])
d = lapply(d, function(d) substring(d, split, c(shift(split-1, -1, fill = split[length(split)]+4))-1))
d = as.data.table(d)
D = copy(d)
varsi = 1:(ncol(d)-1)
vm = max(varsi)+1
vars = paste0("V", varsi)
varm = paste0("V", vm)
for(i in vars) d[, (i) := as.numeric(get(i))][]


## Part 1.
d[, res := fifelse(get(varm) %flike% "*", prod(as.numeric(.SD[, ..vars])), sum(as.numeric(.SD[, ..vars]))), 1:nrow(d)]
sum(d$res)
# 5060053676136


## Part 2.
d <- copy(D)
d[, res2 := {
    v = strsplit(as.character(.SD[, ..varsi]), "")
    v = as.data.table(v)
    v = v[, r := {paste(unlist(.SD)[!unlist(.SD) %in% NA], collapse = "")}, 1:nrow(v)]$r
    fifelse(.SD[, ..vm] %flike% "+", sum(as.numeric(v)), prod(as.numeric(v)))
}, 1:nrow(d)]

sum(d$res2)
# 9695042567249