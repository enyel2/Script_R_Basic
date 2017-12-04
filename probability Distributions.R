x <- rnorm(n=10)
x
sd(x)
mean(x)
y <- rnorm(n = 30000, mean = 100, sd = 20)
y
sd(y)
mean(y)
y

randNorm10 <- rnorm(10)
randNorm10

dnorm(randNorm10)
dnorm(c(-1,0,1))

randNorm <- rnorm(30000)
randNorm
randDensity <- dnorm(randNorm)
randDensity
require(ggplot2)
ggplot(data.frame(x = randNorm, y = randDensity)) + aes(x = x, y = y) + geom_point() + 
          labs(x = "Random Normal Variables", y = "Density")

## pnorm es una función que calcula la probabilidad acumulaiva, probabilidad
## que ocurra el evento.
randNorm10
pnorm(randNorm10)
sd(randNorm)
mean(randNorm)

## se puede encontrarar la probabilidad de que una variable caiga entre dos numero##
## y se representa como el area bajo la curva de la grafica que se tiene aqui ##. 
pnorm(1) - pnorm(0)

p <- ggplot(data.frame(x=randNorm, y=randDensity)) + aes(x=x, y=y) + geom_line() + 
  labs(x="x", y="Density")

## Creando un area bajo la curva 

neg1Seq <- seq(from=min(randNorm), to=-1, by=.1)

## construir una data.frame de secuencia como x
## distribucion de valores para secuecias como y

lessThanNeg1 <- data.frame(x=neg1Seq, y=dnorm(neg1Seq))

head(lessThanNeg1)

#combina estos con puntos dinales alejados de la izquiera y derecha la altura es 0.
lessThanNeg1 <- rbind(c(min(randNorm), 0),
                      lessThanNeg1,
                      c(max(lessThanNeg1$x), 0))
# usa la region achurada como un poligono
p + geom_polygon(data=lessThanNeg1, aes(x=x, y=y))

neg1Pos1Seq <- seq(from = -1, to=1, by=0.1)

## construir un data.frame de una secuencia de x
## la distribuciion de valor para esa secuencia y

neg1To1 <- data.frame(x=neg1Pos1Seq, y=dnorm(neg1Pos1Seq))

head(neg1To1)

## Combinando esto con los puntos finales lejano hacia la izquierda y hacia la derecha
## la altura es 0

neg1To1 <- rbind(c(min(neg1To1$x), 0),
                 neg1To1,
                 c(max(neg1To1$x), 0))

## usa esa region como un poligono
p + geom_polygon(data=neg1To1, aes(x=x, y=y))

##curva de probabilidad

##"pnorm" es una función de probabilidad usa en R, entonces se grafica los valores de
## randNorm v/s la probabilidad de los valoes de randNorm.... 
randProb <- pnorm(randNorm)

ggplot(data.frame(x=randNorm, y=randProb)) + aes(x=x, y=y) + 
  geom_point() + labs(x="random Normal Variable", y="Probability")

## la funcion opuesta a pnorm es qnorm, dando la probabilidad acumulativa y retornar
## en cuantil...... Ejemplo

qnorm(pnorm(randNorm10))

## de esta manera al aplicar la funcion all.equal se obtiene un valor igual por TRUE.
all.equal(randNorm10, qnorm(pnorm(randNorm10)))




