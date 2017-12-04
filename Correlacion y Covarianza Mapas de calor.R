require(ggplot2)
require(reshape2)
head(economics)

#busca la data y la carga separada por tabuladores
data1 <- read.table(file.choose(), sep = "\t", header = TRUE)

data1 <- economics

#Guardando bases de datos en formato txt
write.table(data1, "/home/enbelyert/Dropbox/John_Munoz/Libros de Programacion y Programación/R/base_economics.txt")

economics$psavert

cor(economics$pce, economics$psavert)

xPart <- economics$pce - mean(economics$pce)
yPart <- economics$psavert - mean(economics$psavert)
nMinusOne <- (nrow(economics)-1)
xSD <- sd(economics$pce)
ySD <- sd(economics$psavert)

sum(xPart * yPart) / (nMinusOne* xSD*ySD)

x <- cor(economics[, c(2, 4:6)])
x

require(ggplot2)
require(GGally)
require(reshape)
require(reshape2)

GGally::ggpairs(economics[, c(2, 4:6)], params = list(labelSize = 8))

require(scales)
#(para este analisis de la base economics se considera pce, psavert, uempemed, unemploy)
econCor <- cor(economics[, c(2, 4:6)])
head(econCor)

econMelt <- melt(econCor, varnames=c("x", "y"))

econMelt["Correlation"] <-  econMelt$value

names(econMelt)
econMelt <- econMelt[,-3]
 
econMelt <- econMelt[order(econMelt$value), ]

head(econMelt)

require(ggplot)
ggplot(econMelt, aes(x=x, y=y)) + 
  geom_tile(aes(fill=Correlation)) +
  scale_fill_gradient2(low=muted("red"), mid= "white",
                       high = "steelblue",
                       guide = guide_colorbar(ticks = FALSE, barheight = 10),
                       limits=c(-1,1)) + 
  theme_minimal() + labs(x=NULL, y = NULL)





