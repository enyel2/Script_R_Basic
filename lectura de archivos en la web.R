##Abrir documentos desde la Internet
theUrl <- url("http://www.jaredlander.com/data/Tomato%20First.csv")
theUrl

#si no se baja se descarga directamente y luego se abre por el metodo de más abajo
tomato <- read.table("http://www.jaredlander.com/data/Tomato%20First.csv")

#df funciona perfectamente
df1 <- url("https://s3.amazonaws.com/assets.datacamp.com/blog_assets/test.txt")
df1

df <- read.table("https://s3.amazonaws.com/assets.datacamp.com/blog_assets/test.txt", 
                 header = FALSE)
df

tomato

## Abrir datos desde el escritorio donde aparece por defecto la misma separaci?n de factores
## que arriba...
enyel <- (read.csv(file.choose(), header = TRUE, sep = ","))

enyel

## hay q llamar el economia


