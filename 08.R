# 08.R
# Day 8 of Advent of Code 2026.
# Like day 1:7, only using R as a resource (so R help if needed / no gen ai or s/o etc)

## Setup.
renv::restore(prompt = FALSE)
options(scipen = 999)
# Required packages.
require(data.table)
require(igraph)
# Read impute and munge.
v = readLines("input/08")
lv = length(v)
v = expand.grid(v, v)
v1 = lapply(strsplit(as.character(v$Var1), ","), as.numeric)
v2 = lapply(strsplit(as.character(v$Var2), ","), as.numeric)
d = data.table(t(as.data.table(v1)), t(as.data.table(v2)))
setnames(d, c("x", "y", "z", "x1", "y1", "z1"))
fun = function(a, b, c, x, y, z) {
  sqrt((a - x)^2 +
       (b - y)^2 +
       (c -z)^2)
}
d[, score := fun(x, y, z, x1, y1, z1)]
d = d[score > 0]
d = d[order(score)]
d = d[!duplicated(score)]
d[, a := paste(x, y, z, sep = "~")]
d[, b := paste(x1, y1, z1, sep = "~")]
d = d[, .(a, b, score)]
d[, id := as.character(1:.N)]
d[, cat := ""]
D = copy(d)
range = 1:1000


# Part 1.
g = graph_from_data_frame(d[range], directed = F, vertices = NULL)
look = data.table(a = components(g)$membership, b = names(components(g)$membership))
d[, cat := look$a[match(a, look$b)]]
d[cat %in% c("", NA), cat := look$a[match(b, look$b)]]
x = d[range, length(unique(unlist(.SD[, .(a, b)]))), cat][order(-V1)]
prod(head(x, 3)$V1)
# 54180


## Part 2.
d = copy(D)
for(i in 1:nrow(d)){
    g = graph_from_data_frame(d[1:i], directed = F, vertices = NULL)
    if(length(unique(names(components(g)$membership))) == lv & length(table(components(g)$membership)) == 1){
        d[i, stop := TRUE]
        break
    }
}
d[(stop), as.numeric(gsub("~.*", "", a)) * as.numeric(gsub("~.*", "", b))]
# 25325968