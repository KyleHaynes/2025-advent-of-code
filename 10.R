# 10.R
# Day 10 of Advent of Code 2026.
# Like day 1:9, only using R as a resource (so R help if needed / no gen ai or s/o etc)

## Setup.
renv::restore(prompt = FALSE)
options(scipen = 999)
# Required packages.
require(data.table)
require(stringr)
# Read input.
d = data.table(input = readLines("input/10"))
d[, p := gsub("\\[|\\].*", "", input)]
d

d[, com := str_extract_all(gsub(" \\{.*", "", input), "(\\d|,)+")]
d[, numb := NA]
fun = function(x = c("0,2,3,4", "2,3", "0,4", "0,1,2", "1,2,3,4"), yy = "...#."){
    combo <- data.table()
    for(i in 1:length(x)){
        combo <- rbind(combo, as.data.table(t(combn(x, i))), fill = T)
    }
    # browser()
    for(i in 1:nrow(combo)){
        y = strsplit(yy, "")[[1]]
        # print(i)
        cnt = 0
        for(j in 1:ncol(combo)){
            # if(i == 25) browser()
            # if(i == 8) browser()
            vec = eval(parse(text=paste0("c(", combo[i, ..j], ")"))) + 1
            ans = y[vec]
            ans = fifelse(ans == "#", ".", "#")
            y[vec] = ans
            # if(i == 2) browser()
            ni = j + 1
            cnt = cnt + 1
            if(all(y %in% ".")) {
                break
            }
            print(cnt)
            if(j == ncol(combo) || combo[i, ..ni] %in% NA) {
                # print("dun")
                break
            }
        }
        if(all(y %in% ".")){
            xa = cnt
            break
        }
    }
print("xxxxxx")
return(xa)
}
# (fun())

d[, x := fun(com[[1]], p), 1:nrow(d)]
sum(d$x)
# d[, {.SD; browser()}, 1:nrow(d)]


# fun(d[3]$com[[1]], d$p[3])
# 