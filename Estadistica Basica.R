### Estadistica Basica

## muestra aleatoria para muestrear

x <- sample(1000)
x
mean(x)
sd(x)
summary(x)
median(x)
str(x)
sapply(x, class)
mode(x)
sort(x)
length(x)
quantile(x)
quantile(x, probs = c(0.25, 0.75))
