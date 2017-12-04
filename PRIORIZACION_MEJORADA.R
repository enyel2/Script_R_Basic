####EStructura de calculo de priorizacion
## calculular php o pp según lo que se requiera
## llamar documento según la carpeta ubicada
CC_TT <- ReadTable("E:/Poryectos_Enyel/CArrasco/para_priorizacion.txt",stringsAsFactors=F)
names(CC_TT)

CT_1 <- CC_TT[,c(-208,-209,-210,-211,-212)]
dim(CT_1)

names(CT_1)

library(RODBC)
channel <- odbcConnect( dsn = "DNS_SQL_TASREP", uid = "emuñoz", pwd = "dicom.2016")
data_1 <- sqlQuery(channel,"select * from CLMSDBTASREP.dbo.EVERCLEAN")
data_1["MARCA"] <- "A"
unique(data_1$FECHA_CALC)
unique(data_1$MARCA)

data_1 <- data_1[,-2]
dim(data_1)
names(data_1)

CT_2 <- merge(CT_1, data_1, "RUT", all.x = TRUE)
dim(CT_2)

names(CC_TT)

CT_2[is.na(CT_2)] <- 0

CT_2["EV"] <- ifelse(CT_2$MARCA == "A", 1, 0)

count(CT_2$EV)
count(CT_2$IND_DEFUNC)

CT_2 <- CT_2[ !CT_2$IND_DEFUNC == "D",]
dim(CT_2)

CT_2["N_ACRE"] <- CT_2$NACR_39 + CT_2$TACR_12 + CT_2$NACR_02
CT_2["Monto_TOTAL"] <- CT_2$TOT_MONT + CT_2$TMOT_39
CT_2["N_Documentos"] <- CT_2$TDOC_02 + CT_2$TDOC_12

names(CT_2)

library(plyr)
library(dplyr)
table(CT_2$EV, CT_2$EV)
table(CT_2$CLEAN2, CT_2$CLEAN2)

CT_2["Sum_Clean2_EVER"] <- CT_2$CLEAN2 + CT_2$EV

sum(CT_2$Sum_Clean2_EVER >= 2)
H <- data.frame(subset(CT_2, CT_2$Sum_Clean2_EVER >= 2))
H["MARCA"] <- "GRUPO 1"

sum(CC_TT$CLEAN2 == 1 & CC_TT$EV == 0)
I <- data.frame(subset(CC_TT, CC_TT$CLEAN2 == 1 & CC_TT$EV == 0))
I["MARCA"] <- "GRUPO 2"

## nueva tabla en base a los percentiles
## sacando a los limpios 
CT_2_1 <- CT_2[ CT_2$CLEAN2 == 0,]
## quantiles sin los limpios
quantile(CT_2_1$Monto_TOTAL, c(.2, .4, .6, .8))
  
sum(CT_2_1$Monto_TOTAL <= 4.30  & CT_2_1$N_ACRE == 1)
x  <- data.frame(subset(CT_2_1, CT_2_1$Monto_TOTAL <= 4.30 & CT_2_1$N_ACRE == 1))
x["MARCA"] <- "GRUPO 3"

dim(x)

mean(x$Monto_TOTAL)
mean(x$N_Documentos)

sum(CT_2_1$Monto_TOTAL > 4.30 & CT_2_1$Monto_TOTAL <= 9.39 & CT_2_1$N_ACRE == 1)
y <- data.frame(subset(CT_2_1, CT_2_1$Monto_TOTAL > 4.30 & CT_2_1$Monto_TOTAL <= 9.39 & CT_2_1$N_ACRE == 1))
y["MARCA"] <- "GRUPO 4"

dim(y)

mean(y$Monto_TOTAL)
mean(y$N_Documentos)

sum(CT_2_1$Monto_TOTAL > 9.39 & CT_2_1$Monto_TOTAL <= 18.07 & CT_2_1$N_ACRE == 1)
z <- data.frame(subset(CT_2_1, CT_2_1$Monto_TOTAL > 9.39 & CT_2_1$Monto_TOTAL <= 18.07 & CT_2_1$N_ACRE == 1))
z["MARCA"] <- "GRUPO 5"

dim(z)

mean(z$Monto_TOTAL)
mean(z$N_Documentos)

sum(CT_2_1$Monto_TOTAL > 18.07 & CT_2_1$Monto_TOTAL <= 36.95 & CT_2_1$N_ACRE == 1)
a <- data.frame(subset(CT_2_1, CT_2_1$Monto_TOTAL > 18.07 & CT_2_1$Monto_TOTAL <= 36.95 & CT_2_1$N_ACRE == 1))
a["MARCA"] <- "GRUPO 6"
dim(a)
mean(a$Monto_TOTAL)
mean(a$N_Documentos)

sum(CT_2_1$Monto_TOTAL > 36.95 & CT_2_1$N_ACRE == 1)
b <- data.frame(subset(CT_2_1, CT_2_1$Monto_TOTAL > 36.95 & CT_2_1$N_ACRE == 1))
b["MARCA"] <- "GRUPO 7"
dim(b)
mean(b$Monto_TOTAL)
mean(b$N_Documentos)

sum(CT_2_1$Monto_TOTAL <= 4.30 & CT_2_1$N_ACRE > 1)
c <- data.frame(subset(CT_2_1, CT_2_1$Monto_TOTAL <= 4.30 & CT_2_1$N_ACRE > 1))
c["MARCA"] <- "GRUPO 8"
dim(c)
mean(c$Monto_TOTAL)
mean(c$N_Documentos)

sum(CT_2_1$Monto_TOTAL > 4.30 & CT_2_1$Monto_TOTAL <= 9.39 & CT_2_1$N_ACRE > 1)
d <- data.frame(subset(CT_2_1, CT_2_1$Monto_TOTAL > 4.30 & CT_2_1$Monto_TOTAL <= 9.39 & CT_2_1$N_ACRE > 1))
d["MARCA"] <- "GRUPO 9"
dim(d)
mean(d$Monto_TOTAL)
mean(d$N_Documentos)

sum(CT_2_1$Monto_TOTAL > 9.39 & CT_2_1$Monto_TOTAL <= 18.07 & CT_2_1$N_ACRE > 1)
e <- data.frame(subset(CT_2_1, CT_2_1$Monto_TOTAL > 9.39 & CT_2_1$Monto_TOTAL <= 18.07 & CC_TT_1$N_Acredores > 1))
e["MARCA"] <- "GRUPO 10"
dim(e)
mean(e$Monto_TOTAL)
mean(e$N_Documentos)

sum(CT_2_1$Monto_TOTAL > 18.07 & CT_2_1$Monto_TOTAL <= 36.95 & CT_2_1$N_ACRE > 1)
f <- data.frame(subset(CT_2_1, CT_2_1$Monto_TOTAL > 18.07 & CT_2_1$Monto_TOTAL <= 36.95 & CT_2_1$N_ACRE > 1))
f["MARCA"] <- "GRUPO 11"
dim(f)
mean(f$Monto_TOTAL)
mean(f$N_Documentos)

sum(CT_2_1$Monto_TOTAL > 36.95 & CT_2_1$N_ACRE > 1)
G <- data.frame(subset(CT_2_1, CT_2_1$Monto_TOTAL > 36.95 & CT_2_1$N_ACRE > 1))
G["MARCA"] <- "GRUPO 12"
dim(G)
mean(G$Monto_TOTAL)
mean(G$N_Documentos)

##Evaluacion de los grupos
dim(x)
names(x)

## Union de los Grupos

dt_SCORE_Grupo <- rbind(H_1, I_1, x_1,
                        y_1, z_1, a_1,
                        b_1, c_1, d_1,
                        e_1, f_1, G_1)
dim(dt_SCORE_Grupo)
names(dt_SCORE_Grupo)

##Exportacion de los Datos
setwd("E:/Resultados_Bechmark")

ExportTable(dt_SCORE_Grupo,"dt_Carrasco_Priorizacion.txt")

#################################################################
## En caso de añadir nuevas tablas o menciones. llamar directamente.

