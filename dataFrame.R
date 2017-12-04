##Data Frame## 

x <- 10:1
x
y <- -4:5
y

q <- c("hockey", "Football", "baseball", "curling", "rugby", "lacrosse", "Basketball", "tennis", 
       "cricket", "soccer")
q

theDF <- data.frame(x,y,q)
theDF

theDF <- data.frame(First = x, second = y, sport = q)
theDF

nrow(theDF)
ncol(theDF)
dim(theDF)
names(theDF)
names(theDF)[1]
rownames(theDF) <- c("one", "two", "three", "four", "five", "six", "seven", 
                    "eight", "nine", "ten")
theDF
head(theDF)

tail(theDF)
theDF
rownames(theDF) <- c(NULL)
theDF
head(theDF)
## cuando usamos funcion Class esamos desiganando que tipo de clase es en R, ejemplo##

class(theDF)
 
## el operador $ indica la columna por cada nombre ##

theDF$sport

## se puede buscar especificamente las filas y las columnas ##

theDF [3,2]

theDF[3, 1:3]

## se quiere seleccionar la fila 2 y 5 y sólo mostrar el resultado de la columnn 3 ##
theDF[c(2,5),2]
##############################################


