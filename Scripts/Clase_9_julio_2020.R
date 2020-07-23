rm(list = ls())
g <- function(p){pbinom(200, 1000, p) - 0.75}
uniroot(g, lower = 0, upper = 1, tol = 0.000000000001)
#resp = 0.192150