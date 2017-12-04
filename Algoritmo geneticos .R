library(GA)

# 1) one-dimensional function
f <- function(x)  abs(x)+cos(x)
curve(f, -20, 20)
fitness <- function(x) -f(x)
GA <- ga(type = "real-valued", fitness = fitness, min = -20, max = 20)
summary(GA)
plot(GA)
curve(f, -20, 20)
abline(v = GA@solution, lty = 3)
# 2) one-dimensional function
f <- function(x)  (x^2+x)*cos(x) # -10 < x < 10
curve(f, -10, 10)
# write your own tracing function
monitor <- function(obj)
{
  curve(f, -10, 10, main = paste("iteration =", obj@iter))
  points(obj@population, obj@fitness, pch = 20, col = 2)
  rug(obj@population, col = 2)
  Sys.sleep(0.2)
}
## Not run:
GA <- ga(type = "real-valued", fitness = f, min = -10, max = 10, monitor = monitor)
## End(Not run)
# or if you want to suppress the tracing
GA <- ga(type = "real-valued", fitness = f, min = -10, max = 10, monitor = NULL)
summary(GA)
monitor(GA)
abline(v = GA@solution, lty = 3)
# 3) two-dimensional Rastrigin function
Rastrigin <- function(x1, x2)
{
  20 + x1^2 + x2^2 - 10*(cos(2*pi*x1) + cos(2*pi*x2))
}
x1 <- x2 <- seq(-5.12, 5.12, by = 0.1)
f <- outer(x1, x2, Rastrigin)
persp3D(x1, x2, f, theta = 50, phi = 20)
filled.contour(x1, x2, f, color.palette = jet.colors)
GA <- ga(type = "real-valued", fitness =  function(x) -Rastrigin(x[1], x[2]),
         min = c(-5.12, -5.12), max = c(5.12, 5.12),
         popSize = 50, maxiter = 100)
summary(GA)
10
ga-class
plot(GA)
# 4) Parallel GA
# Simple example of an expensive fitness function obtained artificially by
# introducing a pause statement.
## Not run:
Rastrigin <- function(x1, x2)
{
  Sys.sleep(0.1)
  20 + x1^2 + x2^2 - 10*(cos(2*pi*x1) + cos(2*pi*x2))
}
system.time(GA1 <- ga(type = "real-valued",
                      fitness =  function(x) -Rastrigin(x[1], x[2]),
                      min = c(-5.12, -5.12), max = c(5.12, 5.12),
                      popSize = 50, maxiter = 100, monitor = FALSE,
                      seed = 12345))
system.time(GA2 <- ga(type = "real-valued",
                      fitness =  function(x) -Rastrigin(x[1], x[2]),
                      min = c(-5.12, -5.12), max = c(5.12, 5.12),
                      popSize = 50, maxiter = 100, monitor = FALSE,
                      seed = 12345, parallel = TRUE))
## End(Not run)
# 5) Hybrid GA
# Example of GA with local search
Rastrigin <- function(x1, x2)
{
  20 + x1^2 + x2^2 - 10*(cos(2*pi*x1) + cos(2*pi*x2))
}
GA <- ga(type = "real-valued",
         fitness =  function(x) -Rastrigin(x[1], x[2]),
         min = c(-5.12, -5.12), max = c(5.12, 5.12),
         popSize = 50, maxiter = 100,
         optim = TRUE)
summary(GA)
