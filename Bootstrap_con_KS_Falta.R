library(boot)
library(Matching)
library(kolmim)

x <- abs(rnorm(100))
x

curve(dnorm(x), from = -3, to = 3)

xp.kt <- ks.test(x, "pexp", exact = TRUE)$p
xp.kt
p.ktimp <- ks.test.imp(x, "pexp")$p
p.ktimp
abs(xp.kt - p.ktimp)

#lenguaje desde la libreria pdf hay q profundizar
boot(data, statistic, R, sim = "ordinary", stype = c("i", "f", "w"),
     strata = rep(1,n), L = NULL, m = 0, weights = NULL,
     ran.gen = function(d, p) d, mle = NULL, simple = FALSE, ...,
     parallel = c("no", "multicore", "snow"),
     ncpus = getOption("boot.ncpus", 1L), cl = NULL)

boot.array(boot.out, indices) 