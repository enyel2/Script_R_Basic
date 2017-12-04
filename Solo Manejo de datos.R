thematrix <- matrix(1:9, nrow=3)
thematrix

apply(thematrix, 1, sum) # por defecto suma las filas 
rowSums(thematrix) # hace lo mismo que arriba
colSums(thematrix)

thematrix[2,1] <- NA
thematrix
apply(thematrix, 1, sum)
apply(thematrix, 1,sum, na.rm = TRUE) ##se elima el NA y suma solo los que quedan.

theList <- list(A = matrix(1:9,3), B = 1:5, C = matrix(1:4,2), D=2) ## en forma de lista 
## se tiene diferentes numeros, matrices y secuencias
theList

lapply(theList, sum)
sapply(theList, sum) ## Como vector ordena los resultados.  
## Otro ejemplo
theNames <- c("Jared","Deb","Paul")
lapply(theNames, nchar)
sapply(theNames, nchar)
## Otra herramienta
firstList <- list(A = matrix(1:16, 4), B=matrix(1:16, 2), C=1:5)
secondList <- list(A = matrix(1:16,4), B=matrix(1:16,8), C=15:1)
firstList
secondList
mapply(identical, firstList, secondList)# de esta manera se observa cuales de las
#matrices son identcas.

#------------------ Aggregate ---------------------# Inicio de Manipulación base de datos

library(ggplot2)
require(ggplot2)
data(diamonds) ## Aparece la serie de diamonds como data
head(diamonds)
diamonds
aggregate(price ~ cut, diamonds, mean)##Ordena y promedia cada cut con 
#su respectivo precio promedio
aggregate(price ~ cut + color, diamonds, mean)## similar pero ademas se añade el color
aggregate(cbind(price, carat) ~ cut,diamonds, mean)# cbind ordena como columnas dos 
#vectores ejemplo, al aprecer se hace debido a que al poner cbind es posible alinear 
#las otras columnas
a <- c(1,2,3)
b <- c(5,6,7)
cbind(a,b)
c <- c(3,8,4)
cbind(a,c)
cbind(a,b,c)
aggregate(cbind(price, carat) ~ cut + color,diamonds, mean)
## plyr

require(plyr)
data(baseball)
head(baseball)
#OBP en baseball es el porcentaje de carreras, es un idnicador cuando bueno es un
#jugador al parecer
## OBP = (H+BB+HBP)/(AB+BB+HBP+ SF)
# H = hits, BB = Bases on Balls, HBP = Times Hit by Pitch, AB = At Bats, SF = Sacrife
#file
##---------- Primero -------------##
#antes de 1954 HBP es considerado 0
baseball$sf[baseball$year < 1954] <- 0
#Para comprobar si resulto lo anterior
any(is.na(baseball$sf))#hay algun NA en la base de dato baseball columna sf?
#set NA en hbp a 0
baseball$hbp[is.na(baseball$hbp)] <- 0# todos los NA pasen a 0
any(is.na(baseball$hbp))
##Solo mantener los jugadores almenos 50 bate
baseball <- baseball[baseball$ab >= 50, ]# se reduce el numero total de los datos se 
#elimina todos los ab >= a 50.
baseball
## Calculo de OBP
baseball$OBP <- with(baseball, (h+bb+hbp)/(ab+bb+hbp+sf))#se añade una variable usando
#la funcion "with", pero evalua el OBP de cada un de los elementos aunque se repitan
baseball
tail(baseball)# esta función "tail" muestra que se acuño una nueva variable
##Nueva funcion para calcular obp por carrera de cada jugador  sin tomar solo el valor
## de cada fila de la data
obp <- function(data){
  c(OBP = with(data, sum(h + bb + hbp)/sum(ab + bb + hbp + sf)))
}
## usando la función "ddply" para calcular la % de carrera de OBP, calcula el promedio de
## cada elemento que se repita, gracias a que cada jugador tienen un nnombre unico y es
## identificado por su id y de esa forma se suma y se obtiene el % de OBP
careerOBP <- ddply(baseball, .variables = "id", .fun = obp)
## ordenar resultados de OBP
careerOBP <- careerOBP[order(careerOBP$OBP, decreasing = TRUE), ]
## se ven los resultados
head(careerOBP, 10)
