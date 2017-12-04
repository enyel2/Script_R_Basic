## Distribucion Binomial

## para simular el numero de exito de 10 eventos con probabilidad de exito 0.4 con n = 1, 
## nosotros ponemos los siguientes comandos:

rbinom (n = 1, size = 10, prob = 0.4)

rbinom (n = 10000, size = 3, prob = 0.4)

n <- 2000
k <- seq(0, n, by = 20)
plot (k, dbinom(k, n, pi/10, log = TRUE), type = "l", ylab = "log density",
      main = "dbinom(*, log=TRUE) is better than  log(dbinom(*))")
lines(k, log(dbinom(k, n, pi/10)), col = "red", lwd = 2)

## Otro ejemplo para determinar 

binomData <- data.frame(successes = rbinom(n=10000, size = 10, 
                                            prob = 0.3))
require(ggplot2)
ggplot(binomData, aes(x = successes)) + geom_histogram(binwidth = 1)

###############################################################################
###### Distribucion Poisson 

## -> Ecuacion de funcion de probabilidad de masas y distribucion acumulada tb es otra
## ecuacion, lamda es el promedio y varianza 

## para generar aleatoriamente contadores, la densidad, la distribucion y quatiles usar
## rpois, dpois, ppois y gpois. respectivamente.

## si lambda tiene una gran crecimiento, la distribucion tiende a ser similar a la 
## distribucion normal,  ahora se puede simular 10.000 dibujos para la distribucion
## de poisson 

###  Ahora se generan 10.000 cuentas aleatorias de 5 diferentes distribuciones de poisson
### y ele ejercio es para represetar el efecto del aumento de lambda.
library(reshape2)
library(stringr)
library(ggplot2)

pois1 <- rpois(n=10000, lambda = 1)
pois1
pois2 <- rpois(n=10000, lambda = 2)
pois5 <- rpois(n=10000, lambda = 5)
pois10 <- rpois(n=10000, lambda=10)
pois20 <- rpois(n=10000, lambda=20)
pois <- data.frame(Lambda.1=pois1, Lambda.2=pois2,
                   Lambda.5=pois5, Lambda.10=pois10, Lambda.20=pois20)
require(reshape2)
pois <- melt(data=pois, variable.name="Lambda", value.name="x")
require(stringr)
pois$Lambda <- as.factor(as.numeric(str_extract(string=pois$Lambda, pattern="\\d+")))

head(pois)
tail(pois)

require(ggplot2)
ggplot(pois, aes(x=x)) + geom_histogram(binwidth=1) + facet_wrap(~ Lambda) + ggtitle("Probability Mass Function")

ggplot(pois, aes(x=x)) + geom_density(aes(group=Lambda, color=Lambda, fill=Lambda),
                                      adjust=4, alpha=1/2) +
                                        scale_color_discrete() + scale_fill_discrete() 


                                      
