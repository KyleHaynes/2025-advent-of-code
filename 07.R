# 07.R
# Day 7 of Advent of Code 2026.
# Like day 1:6, only using R as a resource (so R help if needed / no gen ai or s/o etc)

## Setup.
renv::restore(prompt = FALSE)
options(scipen = 999)
# Required packages.
require(data.table)
require(stringr)
# Read input.
d = readLines("input/07")
d = strsplit(d, "")
d = as.data.table(d)
d[, which(sapply(d, function(x) all(x == "."))) := NULL]
setnames(d, paste0("V", 1:NCOL(d)))
d = t(d)

## Part 1.
for(i in 1:(nrow(d)-1)){
    start <- d[i, ] %in% c("S")
    nex <- d[i+1, ] == "."
    if(any(start & nex)) d[i+1, start & nex] = "|"
    start <- d[i, ] %in% c("|", "S")
    nex <- d[i + 1, ] == "^"
    if (any(start & nex)) {
        d[i + 1, c(which(start & nex) - 1, which(start & nex) + 1)] = "|"
    }
    start <- d[i, ] %in% c("|")
    nex <- d[i + 1, ] == "."
    if (any(start & nex)) {
        d[i + 1, c(which(start & nex))] <- "|"
    }
}
 
cn <- as.numeric()
for(i in nrow(d):1){
    s = sum(d[i - 1, ] == "|" & d[i, ] == "^")
    cn = c(cn, s)
}
sum(cn)+1
# 1690

## Part 2.
for (i in 1:nrow(d)) {
    if(i == 1){
        d[i, d[i, ] == "S"] <- 1
    } else {
        pos <- which(d[i, ] == "^")
        for(j in pos){
            # if(i == 8) browser()
            val = sum(as.numeric(d[i - 1, (j - 1):(j)]), na.rm = T)
            val2 = sum(as.numeric(d[i - 1, (j):(j + 1)]), na.rm = T)
            val3 = sum(as.numeric(d[i - 1, (j-1):(j + 1)]), na.rm = T)
            d[i, c((j - 1))] <- fifelse(as.numeric(d[i, c((j - 1))]) %in% NA, 0 + val, as.numeric(d[i, c((j - 1))]) + val)
            d[i, c((j + 1))] <- fifelse(as.numeric(d[i, c((j + 1))]) %in% NA, 0 + val2, as.numeric(d[i, c((j + 1))]) + val2)
        }
        for (k in 2:(NCOL(d) - 1)) {
            d[i - 1, k]
            if (all(d[i - 1, c(k - 1, k, k + 1)] %plike% "\\d+|\\.") & all((d[i, c(k - 1, k + 1)]) %in% "^")) {
                d[i, k] <- as.numeric(d[i, k]) - as.numeric(d[i-1, k])
            }
            if (d[i, k] %in% NA & d[i-1, k-1] %plike% "\\d+" & d[i-1, k+1] %plike% "\\d+" & d[i, k + 1] %in% "^" & d[i, k - 1]  %in% "^") {
                d[i, k] <- as.numeric(d[i - 1, k - 1]) + as.numeric(d[i - 1, k + 1])
            }
        }
        pos <- which(d[i, ] == "|")
        d[i, pos] <- d[i - 1, pos]
    }
}
sum(as.numeric(d[nrow(d), ]), na.rm = T)
# 221371496188107