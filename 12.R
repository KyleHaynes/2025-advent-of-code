# 12.R
# Day 12 of Advent of Code 2026.
# Like day 1:11, only using R as a resource (so R help if needed / no gen ai or s/o etc)

## Setup.
renv::restore(prompt = FALSE)
options(scipen = 999)
# Required packages.
require(data.table)
# Read input.
d = readLines("input/12")
puz = d[d %plike% "x"]
puz = rbindlist(lapply(strsplit(puz, " "), function(x) data.table(t(x))))
te = d[!d %plike% "x"]
te = list(
    "1" = te[2:4],
    "2" = te[7:9],
    "3" = te[12:14],
    "4" = te[17:19],
    "5" = te[22:24],
    "6" = te[27:29]
)
te = unlist(lapply(te, function(x) sum(strsplit(paste(x, collapse = ""), "")[[1]] == "#")), use.names = F)

## Part 1.
puz[, num_hash := {
    (as.numeric(V2) * te[1]) + 
    (as.numeric(V3) * te[2]) + 
    (as.numeric(V4) * te[3]) + 
    (as.numeric(V5) * te[4]) + 
    (as.numeric(V6) * te[5]) +
    (as.numeric(V7) * te[6])
}]
puz[, sr := as.numeric(gsub("x.*", "", V1)) * as.numeric(gsub(".*x|:", "", V1))]
puz[!sr < num_hash, .N]
# 538

## Part 2.
