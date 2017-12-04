php_HH_CL_V <- ReadTable("E:/Poryectos_Enyel/Clinica_Valparaiso/PARA_PERFIL_CLV.txt",stringsAsFactors=F)

dim(php_HH_CL_V)

sanity_check_CL_V <- ReadTable("E:/Poryectos_Enyel/Clinica_Valparaiso/resultado_sanity_2.txt",stringsAsFactors=F)

names(sanity_check_CL_V)
sanity_check_CL_V <- sanity_check_CL_V[, -2]

dim(sanity_check_CL_V)

unique(sanity_check_CL_V$Seguimiento)

sanity_check_CL_V["DBM_MARCA"] <- ifelse(sanity_check_CL_V$Seguimiento == "Pagado", 1, 0)

count(sanity_check_CL_V$DBM_MARCA)
dim(sanity_check_CL_V)

CL_V_SPOTFIRE <- merge(php_HH_CL_V, sanity_check_CL_V, "RUT", all.x = TRUE)

CL_V_SPOTFIRE <- CL_V_SPOTFIRE[!duplicated(CL_V_SPOTFIRE$RUT),]

dim(CL_V_SPOTFIRE)
names(CL_V_SPOTFIRE)

setwd("E:/Resultados_Bechmark")

ExportTable(CL_V_SPOTFIRE,"CL_V_PARA_BENCH.txt")

#######################################################
#Verificacion 

CL_V <- ReadTable("E:/Poryectos_Enyel/Clinica_Valparaiso/CL_V_PARA_BENCH_Perfil.txt",stringsAsFactors=F)

names(CL_V)
count(CL_V$DBM_MARCA)

CL_V_1 <- ReadTable("E:/Poryectos_Enyel/Clinica_Valparaiso/20161005_CL&V_2_SCORE_PHP_2625.txt",stringsAsFactors=F)

names(CL_V_1)
count(CL_V_1$DBM_MARCA)

########################################################
CL_V <- ReadTable("E:/Poryectos_Enyel/Clinica_Valparaiso/CL_V_PARA_SPOTFIRE.txt",stringsAsFactors=F)

names(CL_V)
dim(CL_V)

CL_V_1 <- ReadTable("E:/Poryectos_Enyel/Clinica_Valparaiso/20161005_CL&V_2_SCORE_PHP_2625.txt",stringsAsFactors=F)

names(CL_V_1)
dim(CL_V_1)

#######################################################

Dimarsa <- ReadTable("E:/Poryectos_Enyel/Dimarsa/Dimarsa_SPOTFIRE.txt",stringsAsFactors=F)

dim(Dimarsa)

Dimarsa_1 <- merge(Dimarsa, data_2, "RUT", all.x = TRUE)

dim(Dimarsa_1)
names(Dimarsa_1)

Dimarsa_1 <- Dimarsa_1[,c(-208, -209, -210, -211)]

setwd("E:/Resultados_Bechmark")

ExportTable(Dimarsa_1,"Dimarsa_PHP_HH.txt")

###############################################
ggplot(Dimarsa_1, aes(x=Dimarsa_1$CLEAN2)) + geom_histogram(binwidth = 0.5,
                                                    alpha=1)

box(Dimarsa_1$RENTA_TOTAL_HH)

count(Dimarsa_1$REGION_IND_RM)

###############################################

dim(Dimarsa)
hist(Dimarsa$RENTA_TOTAL_HH)
count(Dimarsa$SEXO_DESC)

Dimarsa["DBM_MARCA"] <- ifelse(Dimarsa$SEXO_DESC == "F", 1, 0)

dt_2 <- Dimarsa[, !sapply(Dimarsa, is.character)]

dim(Dimarsa)
dim(dt_2)

library(randomForest)
count(dt_2$DBM_MARCA)
dt_2[is.na(dt_2)] <- 0

names(dt_2)
dt_2 <- dt_2[,c(-1,-9,-153, -116, -4)]

R_f <- randomForest(as.factor(dt_2$DBM_MARCA)~., data = dt_2, ntree = 250)

varImpPlot(R_f)

require(rpart)
creditTree <- rpart(DBM_MARCA ~ SEXO_IND_F + CODIGO_ACTI + VEHI_AVALUO_TOTAL_HH + 
                      RENTA_TOTAL_HH, data = dt_2)
require(rpart.plot)
rpart.plot(creditTree)


##########################################

CL_V_SPOTFIRE <- ReadTable("E:/Poryectos_Enyel/Clinica_Valparaiso/CL_V_PARA_SPOTFIRE.txt",stringsAsFactors=F)
names(CL_V_SPOTFIRE)
unique(CL_V_SPOTFIRE$TIPO)
count(CL_V_SPOTFIRE$TIPO)
CL_V_SPOTFIRE_1 <- CL_V_SPOTFIRE[CL_V_SPOTFIRE$TIPO == "NAT",]

unique(CL_V_SPOTFIRE_1$TIPO)
dim(CL_V_SPOTFIRE_1)
dim(CL_V_SPOTFIRE)

sum(CL_V_SPOTFIRE_1$EDAD_CALC == 0)

ef_1 <- CL_V_SPOTFIRE[CL_V_SPOTFIRE$TIPO == "JUR",]

####################################################

CLV_1 <- ReadTable("E:/Poryectos_Enyel/Clinica_Valparaiso/20161011/PHP_ecm4.txt",stringsAsFactors=F)

dim(CLV_1)
dim(CL_V_SPOTFIRE)
names(data_2)

CLV_2 <- merge(CLV_1, data_2, "RUT", all.x = TRUE)

dim(CLV_2)

sanity_check_CL_V <- read.table(file.choose(), header = TRUE, sep = "\t")

names(sanity_check_CL_V)
dim(sanity_check_CL_V)
unique(sanity_check_CL_V$Prevision)

sanity_check_CL_V_1 <- sanity_check_CL_V[!duplicated(sanity_check_CL_V),]
dim(sanity_check_CL_V_1)

CLV_3 <- merge(CLV_2, sanity_check_CL_V_1, "RUT", all.x = "TRUE")
dim(CLV_2)
dim(CLV_3)

CLV_4 <- CLV_3[!duplicated(CLV_3$RUT),]
dim(CLV_4)

unique(CLV_4$Seguimiento)

CLV_4["DBM_MARCA"] <- ifelse(CLV_4$Seguimiento == "Pagado", 1, 0)

unique(CLV_4$TIPO)
count(CLV_4$TIPO)

ef_1 <- CLV_4 [CLV_4$TIPO == "JUR",]
unique(ef_1$RUT)

CLV_5 <- CLV_4[CLV_4$TIPO == "NAT",]
dim(CLV_5)

CLV_6 <- CLV_5[!CLV_5$EDAD_CALC == 0,]
dim(CLV_6)

count(CLV_5$EDAD_CALC == 0)

setwd("E:/Resultados_Bechmark")

ExportTable(CLV_6,"NB_SPOTFIRE_CLV.txt")

#########################################
CLV_BEN <- ReadTable("E:/Poryectos_Enyel/Clinica_Valparaiso/20161011/20161011_SCORES_CL&V3_2_2580.txt",stringsAsFactors=F)

head(CLV_BEN$SCORE_HITES)

CLV_BEN <- CLV_BEN[,-20]

setwd("E:/Resultados_Bechmark")

ExportTable(CLV_BEN,"SCORE_BEN_CLV.txt")

#######################################

CLV_1 <- ReadTable("E:/Poryectos_Enyel/Clinica_Valparaiso/20161011/PHP_HH_CLV_SCORE.txt",stringsAsFactors=F)

colnames(CLV_1)[colnames(CLV_1) == "IND_DEFUNC.x"] <- "IND_DEFUNC"
names(CLV_1)

CLV_1 <- CLV_1[, c(-254, -255, -256,-259,
                   -260,-262,-269, -270,
                   -271, -272, -273,-276,
                   -277, -278, -279, -280, 
                   -281, -282, -283, -284,
                   -290, -291)]

names(CLV_1)

setwd("E:/Resultados_Bechmark")

ExportTable(CLV_1,"PHP_HH_CLV_SCORE_Selecionado.txt")

########################################

library(RODBC)
channel <- odbcConnect(dsn = "DNS_SQL_TAS", uid = "TAS_PUBLIC", pwd = "tas.2016")
data_2 <- sqlQuery(channel,"select * from DBMKS.dbo.SBIF_2015
                   WHERE periodo = 201506")

dim(data_2)
names(data_2)
unique(data_2$periodo)
unique(data_2$Instrumentos_deudas_adquiridas)

sapply(data_2, class)

head(data_2)

count(data_2$Institucion_09_registra_deuda)

data_2["MARCA_MOROSO"] <- data_2$Cred_directos_al_dia_impagos_menos_30_dias +
                          data_2$Cred_directos_impagos_30_90_dias +
                          data_2$Creditos_indirectos_al_dia_impagos_menos_30_dias +
                          data_2$Creditos_directos_impagos_90_dias_menos_180_dias +
                          data_2$Creditos_Leasing_impagos

data_NV <- data_2[data_2$MARCA_MOROSO > 0,]

dim(data_NV)

data_NV <- data_NV[!duplicated(data_NV$rutid),]

head(data_NV)

count(data_2$Cred_directos_impagos_30_90_dias == 0)

data_NV <- data_NV[,c(-20:-33)]

names(data_NV)

data_NV <- data_NV[, -23]
names(data_NV)
head(data_NV)

base_1 <- data_NV[,c(1,2)]
names(base_1)
base_1["FECHA_CALC"] <- "20161012"
names(base_1)

colnames(data_NV)[colnames(data_NV) == "rutid"] <- "RUT"
names(data_NV)
head(base_1)

base_1 <- base_1[,-2]
dim(base_1)
names(base_1)

setwd("E:/Resultados_Bechmark")

ExportTable(data_NV,"BASE.txt")

#####################################

data_NV_1 <- data_NV[ data_NV$Cred_directos_al_dia_impagos_menos_30_dias == 0,]
dim(data_NV_1)

colnames(data_NV_1)[colnames(data_NV_1) == "rutid"] <- "RUT"

dt_1 <- data_NV_1[, c(1,2)]

dim(dt_1)
names(dt_1)

#########################################
names(data_NV)
dim(data_NV)

library(RODBC)
channel <- odbcConnect( dsn = "DNS_SQL_TASREP", uid = "emuñoz", pwd = "dicom.2016")
dt_2 <- sqlQuery(channel,"select * from CLMSDBTASREP.dbo.SVK_ENYEL_20161012_PHP")

#########################################
                 
FORUM <- ReadTable("E:/Poryectos_Enyel/FORUM/20161012/php_marcaefx_20161012.txt",stringsAsFactors=F)

dim(FORUM)
names(FORUM)
unique(FORUM$GB_6M)
count(FORUM$GB_6M)

data_2 <- data_2[, -c(20:33)]

data_2 <- data_2[,-23]

names(data_2)
names(FORUM)

Base_1 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161012/BASE.txt",stringsAsFactors=F)

names(Base_1)
unique(FORUM$FECHA_CALC)
Base_2 <- Base_1[,c(1,23)]
FORUM_1 <- FORUM[,c(2, 44:53)]


NW_BASE <- merge(FORUM_1, Base_2, "RUT", all.x = TRUE)

names(NW_BASE)

setwd("E:/Resultados_Bechmark")

ExportTable(NW_BASE,"BASE_NW.txt")

##################################

Base_3 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161012/BASE_NW_PHP_total_informacion.txt",stringsAsFactors=F)

dim(Base_3)
names(Base_3)
count(Base_3$GB_6M)
count(Base_3$GB_12M)
count(Base_3$GB_18M)
count(Base_3$GB_30_6M)
count(Base_3$GB_90_6M)
count(Base_3$GB_60_12M)

Base_4 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161012/BASE_NW.txt",stringsAsFactors=F)

dim(Base_4)
count(Base_4$MARCA_MOROSO)

Base_4["FECHA_CALC"] <- 20150601 


setwd("E:/Resultados_Bechmark")

ExportTable(Base_4_1,"BASE_NeW.txt")

names(Base_4_1)
unique(Base_4_1$FECHA_CALC)

mode(Base_4_1$FECHA_CALC)

Base_4_1 <- Base_4[, c(1, 13)]

names(Base_4_1)
unique(Base_4_1$FECHA_CALC)



count(Base_4$GB_6M)
count(Base_4$GB_12M)
count(Base_4$GB_18M)
count(Base_4$GB_30_6M)
count(Base_4$GB_90_6M)
count(Base_4$GB_60_12M)
##
count(Base_4$GB_60_6M)
count(Base_4$GB_60_12M)
count(Base_4$GB_60_12M)
count(Base_4$GB_60_18M)
count(Base_4$GB_90_18M)

###
names(data_2)
######################################################

Base_4 <- ReadTable("E:/Poryectos_Enyel/FORUM/cacho_enyerlll.txt",stringsAsFactors=F)

names(Base_4)

count(Base_4$GB_6M)
count(Base_4$GB_12M)
count(Base_4$GB_18M)
count(Base_4$GB_30_6M)
count(Base_4$GB_60_6M)
count(Base_4$GB_60_12M)
count(Base_4$GB_60_18M)
count(Base_4$GB_90_6M)
count(Base_4$GB_90_12M)
count(Base_4$GB_90_18M)

Base_5 <- ReadTable("E:/Poryectos_Enyel/FORUM/Forum_benchmark.txt",stringsAsFactors=F)

rm(Base_4)
rm(Base_5)

###########################################

Base_6 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161020_SCORES_FORUM_267833.txt",stringsAsFactors=F)

names(Base_6)
head(Base_6)
Base_6 <- Base_6[, -23]

colnames(Base_6)[colnames(Base_6) == "GB_6M"] <- "DBM_GB_6M"
colnames(Base_6)[colnames(Base_6) == "GB_12M"] <- "DBM_GB_12M"
colnames(Base_6)[colnames(Base_6) == "GB_18M"] <- "DBM_GB_18M"
colnames(Base_6)[colnames(Base_6) == "GB_30_6M"] <- "DBM_GB_30_6M"
colnames(Base_6)[colnames(Base_6) == "GB_60_6M"] <- "DBM_GB_60_6M"
colnames(Base_6)[colnames(Base_6) == "GB_60_12M"] <- "DBM_GB_60_12M"
colnames(Base_6)[colnames(Base_6) == "GB_60_18M"] <- "DBM_GB_60_18M"
colnames(Base_6)[colnames(Base_6) == "GB_90_6M"] <- "DBM_GB_90_6M"
colnames(Base_6)[colnames(Base_6) == "GB_90_12M"] <- "DBM_GB_90_12M"
colnames(Base_6)[colnames(Base_6) == "GB_90_18M"] <- "DBM_GB_90_18M"

Base_6[is.na(Base_6)] <- 0

setwd("E:/Resultados_Bechmark")

ExportTable(Base_6,"BASE_FORUM_2.txt")

########################################################

Base_6 <- ReadTable("E:/Poryectos_Enyel/Clinica_Valparaiso/PHP_HH_CLV_SCORE_Selecionado.txt",stringsAsFactors=F)

names(Base_6)
Base_6 <- Base_6[,c(1,2)]
names(Base_6)
unique(Base_6)

setwd("E:/Resultados_Bechmark")

ExportTable(Base_6,"CLV_SUMC.txt")

########################################################

Base_7 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161012/cacho_enyerlll.txt",stringsAsFactors=F)
Base_8 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161020_FORUM_267833.txt",stringsAsFactors=F)
Base_9 <- ReadTable("E:/Poryectos_Enyel/FORUM/BASE_FORUM_2.txt",stringsAsFactors=F)
Base_10 <- ReadTable("E:/Poryectos_Enyel/FORUM/BASE_FORUM_3.txt",stringsAsFactors=F)

names(Base_7)
names(Base_8)
names(Base_9)
names(Base_10)

count(Base_10$DBM_GB_6M)
count(Base_9$DBM_GB_6M)



base_7 <- Base_7[,c(2,46:55)]
names(base_7)

base_8 <- Base_9[,-c(27:36)]
names(base_8)

base_9 <- merge(base_8, base_7, "RUT", all = TRUE)
names(base_9)

colnames(base_9)[colnames(base_9) == "GB_6M"] <- "DBM_GB_6M"
colnames(base_9)[colnames(base_9) == "GB_12M"] <- "DBM_GB_12M"
colnames(base_9)[colnames(base_9) == "GB_18M"] <- "DBM_GB_18M"
colnames(base_9)[colnames(base_9) == "GB_30_6M"] <- "DBM_GB_30_6M"
colnames(base_9)[colnames(base_9) == "GB_60_6M"] <- "DBM_GB_60_6M"
colnames(base_9)[colnames(base_9) == "GB_60_12M"] <- "DBM_GB_60_12M"
colnames(base_9)[colnames(base_9) == "GB_60_18M"] <- "DBM_GB_60_18M"
colnames(base_9)[colnames(base_9) == "GB_90_6M"] <- "DBM_GB_90_6M"
colnames(base_9)[colnames(base_9) == "GB_90_12M"] <- "DBM_GB_90_12M"
colnames(base_9)[colnames(base_9) == "GB_90_18M"] <- "DBM_GB_90_18M"

count(base_9$DBM_GB_12M)

setwd("E:/Resultados_Bechmark")

ExportTable(base_9,"BASE_FORUM_3.txt")

###################################################

base_C <- read.table(file.choose(), header = TRUE, sep = "\t")

count(base_C$SUMC)
count(base_C$CLEAN2)

base_C1 <- read.table(file.choose(), header = TRUE, sep = "\t")

count(base_C1$SUMC)
count(base_C1$CLEAN2)

####################################################

Base <- ReadTable("E:/Poryectos_Enyel/FORUM/20161024/forum_para_benchmark2.txt",stringsAsFactors=F)

names(Base)
dim(Base)

Base_1 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161024/BASE_FORUM_3.txt",stringsAsFactors=F)

names(Base_1)
Base_1 <- Base_1[, -c(27:36)]

Base_2 <- merge(Base, Base_1, "RUT", all.x =TRUE)
dim(Base_2)
names(Base_2)

colnames(Base_2)[colnames(Base_2) == "GB3M"] <- "DBM_GB_3M"
colnames(Base_2)[colnames(Base_2) == "GB6M"] <- "DBM_GB_6M"
colnames(Base_2)[colnames(Base_2) == "GB9M"] <- "DBM_GB_9M"
colnames(Base_2)[colnames(Base_2) == "GB12M"] <- "DBM_GB_12M"
colnames(Base_2)[colnames(Base_2) == "GB15M"] <- "DBM_GB_15M"
names(Base_2)
dim(Base_2)
setwd("E:/Resultados_Bechmark")

ExportTable(Base_2,"BASE_FORUM_5.txt")

########################################################

TODOS_1 <- ReadTable("E:/Poryectos_Enyel/CArrasco/Priorización_20161024/NN/TODOS.txt",stringsAsFactors=F)
PHP_1 <- ReadTable("E:/Poryectos_Enyel/CArrasco/Priorización_20161024/NN/del_PHP.txt",stringsAsFactors=F)

### Manera rapida de poder encontrar los rut que no hicieron MATCH
subset(data_3, !(data_3$RUT %in% CC_TT$RUT))

#######################################################

library(RODBC)
channel <- odbcConnect( dsn = "DNS_SQL_TASREP", uid = "emuñoz", pwd = "dicom.2012")
data_5 <- sqlQuery(channel,"select * from CLMSDBTASREP.dbo.FNFCS")

names(data_5)
unique(data_5$IND_INTERD)
unique(data_5$ISE_EQUIFAX)
head(data_5)

data_5[is.na(data_5)] <- 0


data_5 <- data_5[, c(1, 2, 4, 3)]
names(data_5)


head(data_5)


data_5["NewCol"] <- paste(data_5$ISE_EQUIFAX,data_5$RENTA_AJUST, data_5$IND_INTERD, sep = ",")

data_6 <- data_5[sample(nrow(data_5),100),]
 
data_6["MARCA_1"] <- "I"
data_6["MARCA_2"] <- "0766006329"
data_6["MARCA_3"] <- "FAMILYCARDSHOP_ISE_RENTA_IND                                "
names(data_6)

data_6["NEW"] <- paste(data_6$MARCA_1, data_6$RUT,
                       data_6$MARCA_2, data_6$MARCA_3, sep = "")

head(data_6) 

data_6["NEW_2"] <- paste(data_6$NEW, data_6$ISE_EQUIFAX, sep = "")
data_6["NEW_3"] <- paste(data_6$NEW_2, data_6$RENTA_AJUST,
                         data_6$IND_INTERD, sep = ",")
data_6["MARCA_4"] <- " "


data_6["NEW_4"] <- paste(data_6$NEW_3, data_6$MARCA_4, sep = ",")

head(data_6)
names(data_6)
data_6 <- data_6[,c(-9,-10)]

data_7 <- data_6[,12]

setwd("E:/Resultados_Bechmark")

ExportTable(data_7,"FILE_NEG.txt")

#############################################################################

data_5["MARCA_1"] <- "I"
data_5["MARCA_2"] <- "0766006329"
data_5["MARCA_3"] <- "FAMILYCARDSHOP_ISE_RENTA_IND                                "


data_5["NEW"] <- paste(data_5$MARCA_1, data_5$RUT,
                       data_5$MARCA_2, data_5$MARCA_3, sep = "")

names(data_5)

data_5 <- data_5[,c(-1,-5, -6, -7)]


data_5["NEW_2"] <- paste(data_5$NEW, data_5$ISE_EQUIFAX, sep = "")

data_5 <- data_5[,c(-1,-4)]

data_5["NEW_3"] <- paste(data_5$NEW_2, data_5$RENTA_AJUST,
                         data_5$IND_INTERD, sep = ",")
head(data_5)
names(data_5)

data_5["MARCA_4"] <- " "

names(data_5)
data_5 <- data_5[,c(-1,-2,-3)]

data_5["NEW_4"] <- paste(data_5$NEW_3, data_5$MARCA_4, sep = ",")

head(data_5)
names(data_6)
data_6 <- data_6[,c(-9,-10)]

data_7 <- data_5[,4]

setwd("E:/Resultados_Bechmark")

ExportTable(data_7,"FILE_NEG_TODOS_2.txt")

###############################################

T_1 <- ReadTable("E:/Poryectos_Enyel/Clinica_Valparaiso/CLV_20161028/RUT_MARCA_BN.txt",stringsAsFactors=F)
PHP_1 <- ReadTable("E:/Poryectos_Enyel/Clinica_Valparaiso/CLV_20161028/PHP_20161028.txt",stringsAsFactors=F)

count(PHP_1$SUMC)
names(T_1)
T_1 <- T_1[,-2]

PHP_1 <- PHP_1[!duplicated(PHP_1$RUT),]

NEW_CLV <- merge(PHP_1, T_1, "RUT", all.x = TRUE)

NEW_CLV <- NEW_CLV[!duplicated(NEW_CLV$RUT),]

NEW_CLV["DBM_MARCA"] <- ifelse(NEW_CLV$Seguimiento == "Pagado", 1, 0)
count(NEW_CLV$DBM_MARCA)

setwd("E:/Resultados_Bechmark")

ExportTable(NEW_CLV,"NEW_DATA_CLV.txt")

####################################

data_1 <- ReadTable("E:/Poryectos_Enyel/Clinica_Valparaiso/CLV_20161028/20161028_SCORES_CLV2_2623.txt",stringsAsFactors=F)
head(data_1)
names(data_1)

data_1 <- data_1[,-23]

data_1[is.na(data_1)] <- 0

setwd("E:/Resultados_Bechmark")

ExportTable(data_1,"NEW_DATA_CLV_2.txt")

########################################

data_1 <- ReadTable("E:/Poryectos_Enyel/Josefina/ClonesCyber/Base.txt",stringsAsFactors=F)

names(data_1)

head(data_2)

data_3 <- ReadTable("E:/Poryectos_Enyel/jeru/infSocDemoEco_clientes_201610121549.csv",stringsAsFactors=F)

table(data_2$ciudad)
table(data_2$tipo_persona)

min(data_2$edad)
max(data_2$edad)

library(data.table)

Test <- fread("infSocDemoEco_clientes_201610121549CB.csv", header = T, sep = ";",
              encoding = 'UTF-8')

data_2 <- fread("E:/Poryectos_Enyel/jeru/abertura/infSocDemoEco_clientes_201610121549CB.csv", header = T, sep = ";",
                encoding = 'UTF-8')


data_3 <- read.csv(file = "E:/Poryectos_Enyel/jeru/abertura/infSocDemoEco_clientes_201610121549CB.csv", sep = ";",
                    header = TRUE, strip.white = TRUE) 

rm(data_3)

count(Test$tipo_persona)
count(Test$ciudad)

help("read.csv")

data_3 <- data_2[sample(nrow(data_2), 100000),]

data_6 <- data_5[sample(nrow(data_5),100),]

setwd("E:/Resultados_Bechmark")

ExportTable(data_3,"data_prueba_100000.txt")

count(data_2$ciudad)
count(data_2$tipo_persona)
count(data_2$region)

#####################################################################

data_3 <- data_2[data_2$Monto_Linea_Credito_Disponible > 500 & data_2$MARCA_MOROSO > 0, ]
summary(data_2$Monto_Linea_Credito_Disponible)
names(data_3)
count(data_3$N_registra_Creditos_Consumo)
count(data_3$N_registra_Creditos_Comerciales)

#####################################################################

names(data_3)

data_4 <- data_3[,c(1,2)]
data_5 <- data_3[,c(1,2)] 
data_6 <- data_3[,c(1,2)]
data_7 <- data_3[,c(1,2)]
data_8 <- data_3[,c(1,2)]
data_9 <- data_3[,c(1,2)]

data_4["FECHA_CALC"] <- 20141215
data_5["FECHA_CALC"] <- 20150115
data_6["FECHA_CALC"] <- 20150215
data_7["FECHA_CALC"] <- 20150315
data_8["FECHA_CALC"] <- 20150415
data_9["FECHA_CALC"] <- 20150515


data_4 <- data_4[,-2]
data_5 <- data_5[,-2]
data_6 <- data_6[,-2]
data_7 <- data_7[,-2]
data_8 <- data_8[,-2]
data_9 <- data_9[,-2]

colnames(data_4)[colnames(data_4) == "rutid"] <- "RUT"
colnames(data_5)[colnames(data_5) == "rutid"] <- "RUT"
colnames(data_6)[colnames(data_6) == "rutid"] <- "RUT"
colnames(data_7)[colnames(data_7) == "rutid"] <- "RUT"
colnames(data_8)[colnames(data_8) == "rutid"] <- "RUT"
colnames(data_9)[colnames(data_9) == "rutid"] <- "RUT"

names(data_4)

data_FORUM <- rbind(data_4, data_5,
                    data_6, data_7,
                    data_8, data_9)
unique(data_FORUM$FECHA_CALC)

setwd("E:/Resultados_Bechmark")

ExportTable(data_FORUM,"FORUM_ecm4.txt")

##############################################

names(data_2)
head(data_2)

dt_1 <- read.table(file.choose(), header = TRUE, sep = "\t") 
names(dt_1)
names(data_2)
data_3 <- merge(dt_1, data_2, "id_cliente", all.y = TRUE)  

head(data_2$id_cliente)
  
data_3 <- data_2[ !data_2$id_cliente == "056477375-4"&
                    !data_2$id_cliente == "087958437-5"&
                    !data_2$id_cliente == "003494013-8"&
                    !data_2$id_cliente == "003386464-8"&
                    !data_2$id_cliente == "005595208-2"&
                    !data_2$id_cliente == "007668830-5"&
                    !data_2$id_cliente == "007706065-2"&
                    !data_2$id_cliente == "007717831-8"&
                    !data_2$id_cliente == "007910754-6"&
                    !data_2$id_cliente == "008472891-3"&
                    !data_2$id_cliente == "008555217-7"&
                    !data_2$id_cliente == "008701054-7"&
                    !data_2$id_cliente == "005659455-6"&
                    !data_2$id_cliente == "007094113-0"&
                    !data_2$id_cliente == "007097556-K"&
                    !data_2$id_cliente == "007440348-1"&
                    !data_2$id_cliente == "007162569-5"&
                    !data_2$id_cliente == "007246382-0"&
                    !data_2$id_cliente == "006530293-1"&
                    !data_2$id_cliente == "080891734-9"&
                    !data_2$id_cliente == "087318111-8"&
                    !data_2$id_cliente == "069115553-3"&
                    !data_2$id_cliente == "085850874-7"&
                    !data_2$id_cliente == "089110945-1"&
                    !data_2$id_cliente == "070975569-K"&
                    !data_2$id_cliente == "074647163-5"&
                    !data_2$id_cliente == "009179141-3"&
                    !data_2$id_cliente == "009184054-6"&
                    !data_2$id_cliente == "009556657-7"&
                    !data_2$id_cliente == "009466302-0"&
                    !data_2$id_cliente == "010168107-9"&
                    !data_2$id_cliente == "009952113-0"&
                    !data_2$id_cliente == "010425966-1"&
                    !data_2$id_cliente == "011328610-K"&
                    !data_2$id_cliente == "010889443-3"&
                    !data_2$id_cliente == "011920525-K"&
                    !data_2$id_cliente == "011731230-8"&
                    !data_2$id_cliente == "011989142-4"&
                    !data_2$id_cliente == "011999500-4"&
                    !data_2$id_cliente == "012025052-6"&
                    !data_2$id_cliente == "012295451-4"&
                    !data_2$id_cliente == "012296753-K"&
                    !data_2$id_cliente == "012328164-1"&
                    !data_2$id_cliente == "013305308-7"&
                    !data_2$id_cliente == "012587546-2"&
                    !data_2$id_cliente == "013612681-K"&
                    !data_2$id_cliente == "013646320-3"&
                    !data_2$id_cliente == "014343921-0"&
                    !data_2$id_cliente == "013278679-0"&
                    !data_2$id_cliente == "011438785-7"&
                    !data_2$id_cliente == "011562038-4"&
                    !data_2$id_cliente == "012436997-4", ]

count(data_3$ciudad)

#################################################################################

dt_1 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161114/FORUM_NW_BASE_COMPLETA.txt",stringsAsFactors=F)

###################################################################################
dt4_4 <- dt_1[sample(nrow(dt_1),50000),]

data_4 <- dt4_4
data_5 <- dt4_4
data_6 <- dt4_4
data_7 <- dt4_4
data_8 <- dt4_4
data_9 <- dt4_4

data_4["FECHA_CALC"] <- 20141215
data_5["FECHA_CALC"] <- 20150115
data_6["FECHA_CALC"] <- 20150215
data_7["FECHA_CALC"] <- 20150315
data_8["FECHA_CALC"] <- 20150415
data_9["FECHA_CALC"] <- 20150515

data_FORUM1 <- rbind(data_4, data_5,
                    data_6, data_7,
                    data_8, data_9)

data_FORUM1 <- data_FORUM1[,-2]

setwd("E:/Resultados_Bechmark")

ExportTable(data_FORUM1,"FORUM8_ecm4.txt")

#############################################
data_FORUM1["Marca1"] <- "H"

data_FORUM1 <- data_FORUM1[!duplicated(data_FORUM1$RUT),]

data_FORUM1 <- data_FORUM1[,-2]

setwd("E:/Resultados_Bechmark")

ExportTable(data_FORUM1,"FORUM_NW8.txt")

d3_3 <- merge(dt_1, data_FORUM1, "RUT", all.x = TRUE)

d3_3[is.na(d3_3)] <- 0

d3_3 <- d3_3[d3_3$Marca1 == 0 & d3_3$Marca == "A",]

dt_1 <- d3_3[, -3]

rm(data_4, data_5, data_6, data_7, data_8, data_9, data_FORUM1)
rm(dt4_4, d3_3)
##############################################
dt_2 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161114/Salida/PHP_ecm4_7.txt",stringsAsFactors=F)

dt_3 <- dt_2[,c(1,2, 152)]

library(ggplot2)

dt_4  <- aggregate(dt_3$CLEAN2~dt_3$RUT, dt_3, mean)

colnames(dt_4)[colnames(dt_4) == "dt_3$RUT"] <- "RUT"
colnames(dt_4)[colnames(dt_4) == "dt_3$CLEAN2"] <- "Sumatoria_CLean"

dt_4 <- dt_4[dt_4$Sumatoria_CLean == 1, ]

setwd("E:/Resultados_Bechmark")

ExportTable(dt_4,"SELECT_7.txt")

#######################################################

dt_1 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161114/Selecion_FORUM/SELECT_1.txt",stringsAsFactors=F)
dt_2 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161114/Selecion_FORUM/SELECT_2.txt",stringsAsFactors=F)
dt_3 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161114/Selecion_FORUM/SELECT_3.txt",stringsAsFactors=F)
dt_4 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161114/Selecion_FORUM/SELECT_4.txt",stringsAsFactors=F)
dt_5 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161114/Selecion_FORUM/SELECT_5.txt",stringsAsFactors=F)
dt_6 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161114/Selecion_FORUM/SELECT_6.txt",stringsAsFactors=F)
dt_7 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161114/Selecion_FORUM/SELECT_7.txt",stringsAsFactors=F)

dt_2["Marca1"] <- "B"
dt_2 <- dt_2[,-2]
dt_3["Marca1"] <- "C" 
dt_4["Marca1"] <- "D"
dt_4 <- dt_4[,-2]
dt_5["Marca1"] <- "E"
dt_6["Marca1"] <- "F"
dt_7["Marca1"] <- "G"
dt_8["Marca1"] <- "H"
dt_9["Marca1"] <- "I"

data_FORUM1 <- rbind(dt_2, dt_3, dt_4, dt_5, dt_6, dt_7, dt_8, dt_9)

############################################################

dt_FORUM_WORK <- rbind(dt_1, dt_2, dt_3, dt_4, dt_5, dt_6, dt_7)

##Para el PHP
dt_FORUM_WORK["FECHA_CALC"] <- 20160901

#Prueba de rut repetidos 
#dt_FORUM_WORK <- dt_FORUM_WORK[!duplicated(dt_FORUM_WORK$RUT),]

setwd("E:/Resultados_Bechmark")

ExportTable(dt_FORUM_WORK,"FORUM201609_ecm4.txt")

#######
#######
dt_2 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161114/construccion_marca_php/PHP_201507.txt",stringsAsFactors=F)
dt_3 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161114/construccion_marca_php/PHP_201508.txt",stringsAsFactors=F)
dt_4 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161114/construccion_marca_php/PHP_201509.txt",stringsAsFactors=F)
dt_5 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161114/construccion_marca_php/PHP_201510.txt",stringsAsFactors=F)
dt_6 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161114/construccion_marca_php/PHP_201511.txt",stringsAsFactors=F)
dt_7 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161114/construccion_marca_php/PHP_201512.txt",stringsAsFactors=F)
dt_8 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161114/construccion_marca_php/PHP_201601.txt",stringsAsFactors=F)
dt_9 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161114/construccion_marca_php/PHP_201602.txt",stringsAsFactors=F)
dt_10 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161114/construccion_marca_php/PHP_201603.txt",stringsAsFactors=F)
dt_11 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161114/construccion_marca_php/PHP_201604.txt",stringsAsFactors=F)
dt_12 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161114/construccion_marca_php/PHP_201605.txt",stringsAsFactors=F)
dt_13 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161114/construccion_marca_php/PHP_201606.txt",stringsAsFactors=F)
dt_14 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161114/construccion_marca_php/PHP_201607.txt",stringsAsFactors=F)
dt_15 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161114/construccion_marca_php/PHP_201608.txt",stringsAsFactors=F)
dt_16 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161114/construccion_marca_php/PHP_201609.txt",stringsAsFactors=F)

dt_16 <- dt_16[,c(1,2, 152,154, 74, 29)]
dt_16["Monto_TOTAL"] <- dt_16$TOT_MONT + dt_16$TMOT_39
dt_16["Dias_MORA"] <- dt_16$FVEN_02*30.25
dt_16 <- dt_16[,c(1,2,3, 6,7,8)]


dt_17 <- rbind(dt_2, dt_3, dt_4, dt_5, dt_6, dt_7, dt_8, dt_9, dt_10, dt_11, dt_12,
               dt_13, dt_14, dt_15, dt_16)

dt_18 <- aggregate(CLEAN2~RUT, dt_17, FUN=sum)
dt_19 <- dt_17[dt_17$FECHA_CALC == 20160901, ]
dt_19 <- dt_19[, c(-1, -3)]

dt_20 <- merge(dt_19, dt_18, "RUT")

dt_20["DBM_GM3_MALO"] <- ifelse(dt_20$Monto_TOTAL > 1.9 & dt_20$Dias_MORA > 90, "M", "I")
count(dt_20$DBM_GM3_MALO)
dt_20["DBM_GM3_BUENO"] <- ifelse(dt_20$CLEAN2 == 15, "B", "I")
count(dt_20$DBM_GM3_BUENO)

table(dt_20$DBM_GM3_MALO, dt_20$DBM_GM3_BUENO)

dt_21 <- dt_20[dt_20$DBM_GM3_MALO == "M",] 
dt_22 <- dt_20[dt_20$DBM_GM3_BUENO == "B",]

dt_20 <- rbind(dt_21, dt_22)

dt_20["DBM_G3"] <- ifelse(dt_20$DBM_GM3_BUENO == "B", 1,0)

setwd("E:/Resultados_Bechmark")

ExportTable(dt_20,"FORUM_BM_15M_1.txt")

############################################################
rm(dt_10, dt_11, dt_12, dt_13, dt_14)
rm(dt_8, dt_9)

dt_2 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161114/construccion_marca_php/PHP_201607.txt",stringsAsFactors=F)
dt2_2 <- dt_2[,c(1,2, 152, 52, 53, 153,154, 74)]

setwd("E:/Resultados_Bechmark")

ExportTable(dt2_2,"SELECION_201507.txt")

###########################################################
dt_1 <- ReadTable("E:/Poryectos_Enyel/FORUM/BASE_TOTAL_FORUM_20161117.txt",stringsAsFactors=F)

dt_2 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161114/Nuevas_MARCAS/FORUM_BM_3M_1.txt",stringsAsFactors=F)

dt_2_1 <- dt_2[dt_2$DBM_GM3_MALO == "M",]
dt_2_2 <- dt_2[dt_2$DBM_GM3_BUENO == "B",]
dt_2 <- rbind(dt_2_1, dt_2_2)

dt_2["DBM_G3"] <- ifelse(dt_2$DBM_GM3_BUENO == "B",1,0)
count(dt_2$DBM_G3)
dt_2 <- dt_2[,c(1, 8)]

dt_2["MARCA"] <- "A"

setwd("E:/Resultados_Bechmark")

ExportTable(dt_2,"DBM_G3.txt")

dt_3 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161114/Nuevas_MARCAS/FORUM_BM_6M_1.txt",stringsAsFactors=F)

dt_3_1 <- dt_3[dt_3$DBM_GM3_MALO == "M",]
dt_3_2 <- dt_3[dt_3$DBM_GM3_BUENO == "B",]

dt_3 <- rbind(dt_3_1, dt_3_2)
dt_3["DBM_G6"] <- ifelse(dt_3$DBM_GM3_BUENO =="B", 1, 0)
count(dt_3$DBM_G6)
dt_3 <- dt_3[,c(1,8)]

dt_3["MARCA"] <- "B"

setwd("E:/Resultados_Bechmark")

ExportTable(dt_3,"DBM_G6.txt")

dt_4 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161114/Nuevas_MARCAS/FORUM_BM_9M_1.txt",stringsAsFactors=F)

dt_4_1 <- dt_4[dt_4$DBM_GM3_MALO == "M",]
dt_4_2 <- dt_4[dt_4$DBM_GM3_BUENO == "B",]

dt_4 <- rbind(dt_4_1, dt_4_2)

dt_4["DBM_G9"] <- ifelse(dt_4$DBM_GM3_BUENO =="B", 1, 0)
count(dt_4$DBM_G9)
dt_4 <- dt_4[,c(1,8)]
dt_4["MARCA"] <- "C"

setwd("E:/Resultados_Bechmark")

ExportTable(dt_4,"DBM_G9.txt")

dt_5 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161114/Nuevas_MARCAS/FORUM_BM_12M_1.txt",stringsAsFactors=F)

dt_5_1 <- dt_5[dt_5$DBM_GM3_MALO == "M",]
dt_5_2 <- dt_5[dt_5$DBM_GM3_BUENO == "B",]

dt_5 <- rbind(dt_5_1, dt_5_2)

dt_5["DBM_G12"] <- ifelse(dt_5$DBM_GM3_BUENO =="B", 1, 0)
count(dt_5$DBM_G12)
dt_5 <- dt_5[,c(1,8)]

dt_5["MARCA"] <- "D"

setwd("E:/Resultados_Bechmark")

ExportTable(dt_5,"DBM_G12.txt")


dt_6 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161114/Nuevas_MARCAS/FORUM_BM_15M_1.txt",stringsAsFactors=F)

dt_6_1 <- dt_6[dt_6$DBM_GM3_MALO == "M",]
dt_6_2 <- dt_6[dt_6$DBM_GM3_BUENO == "B",]

dt_6 <- rbind(dt_6_1, dt_6_2)

dt_6["DBM_G15"] <- ifelse(dt_6$DBM_GM3_BUENO =="B", 1, 0)
count(dt_6$DBM_G15)
dt_6 <- dt_6[,c(1,8)]

dt_6["MARCA"] <- "E"

setwd("E:/Resultados_Bechmark")

ExportTable(dt_6,"DBM_G15.txt")

##########################################################################

dt_1 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161114/Nuevas_MARCAS/DBM_G3.txt",stringsAsFactors=F)
dt_2 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161114/Nuevas_MARCAS/DBM_G6.txt",stringsAsFactors=F)
dt_3 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161114/Nuevas_MARCAS/DBM_G9.txt",stringsAsFactors=F)
dt_4 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161114/Nuevas_MARCAS/DBM_G12.txt",stringsAsFactors=F)
dt_5 <- ReadTable("E:/Poryectos_Enyel/FORUM/20161114/Nuevas_MARCAS/DBM_G15.txt",stringsAsFactors=F)

PHP <- ReadTable("E:/20150504 - reportes benchmark/PHP_Y_PP_Por_fechas/PHP_FORUM201506.txt",stringsAsFactors=F)

unique(PHP$FECHA_CALC)
rm(PHP_GM12)
PHP_GM15 <- merge(PHP, dt_5, "RUT", all.x = TRUE)

PHP_GM15[is.na(PHP_GM15)] <- 0

count(PHP_GM15$MARCA)
PHP_GM15 <- PHP_GM15[PHP_GM15$MARCA == "E", ]
PHP_GM15 <- PHP_GM15[, -209]

setwd("E:/Resultados_Bechmark")

ExportTable(PHP_GM15,"PHP_GM15.txt")

################################################
dt_1 <- ReadTable("E:/Poryectos_Enyel/Patricia_La_Polar/20161122/20161111_LA_POLAR_SCORE_ESTRATEGIA_PREV.txt",stringsAsFactors=F)
names(dt_1)

dt_2 <- read.table("E:/Poryectos_Enyel/Patricia_La_Polar/20161122/20161111_LA_POLAR_COBRANZA_TRAMOS_AMARILLOS_PREV.txt", sep = ",", header = TRUE)
names(dt_2)
head(dt_2)

count(dt_2$TRAMO_PD)
max(dt_2$CIUDAD_DM)

dt_3 <- ReadTable("E:/Poryectos_Enyel/Patricia_La_Polar/20161122/20161111_LA_POLAR_SCORE_ESTRA_REAC.txt",stringsAsFactors=F)
names(dt_3)

dt_4 <- read.table("E:/Poryectos_Enyel/Patricia_La_Polar/20161122/20161111_LA_POLAR_COBRANZA_TRAMOS_AMARILLOS_REAC.txt", sep = ",", header =TRUE)

names(dt_4)
max(dt_4$SCORE_REAC1)
count(dt_4$TRAMO_PD)

dt_5 <- read.table("E:/Poryectos_Enyel/Patricia_La_Polar/20161122/20161111_LA_POLAR_PREV_104716_PD.txt", sep = "\t", header =TRUE)

names(dt_5)
mode(dt_5$valor_contable_foco20)
count(dt_5$valor_contable_foco20)
max(dt_5$Factor)
summary(dt_5$Factor)

dt_6 <- read.table("E:/Poryectos_Enyel/Patricia_La_Polar/20161122/20161111_LA_POLAR_REAC_73751_PD.txt", sep = "\t", header =TRUE)
names(dt_6)
mode(dt_6$valor_contable)

count(dt_6$valor_contable)
max(dt_6$valor_contable)
head(dt_6$RUT)
summary(dt_6$valor_contable)
count(dt_6$tipo_cartera)
dt_6["MARCA"] <- "A"
dt_7 <- merge(dt_3, dt_6, "RUT")
identical(dt_7$NIVEL.x, dt_7$NIVEL.y)
count(dt_7$tipo_cartera)

##################################################################

dt_1 <- ReadTable("E:/Poryectos_Enyel/Javer/Priorizacion_20161123/PHP_1sql.txt",stringsAsFactors=F)
names(dt_1)
dt_2 <- ReadTable("E:/Poryectos_Enyel/Javer/Priorizacion_20161123/BASE_1.txt",stringsAsFactors=F)

dt_4 <- ReadTable("E:/Poryectos_Enyel/Javer/Priorizacion_20161123/PHP_ecm4.txt",stringsAsFactors=F)
##################################################################

### Manera rapida de poder encontrar los rut que no hicieron MATCH
subset(dt_3, !(dt_3$RUT %in% dt_1$RUT))

dt_2 <- dt_2[!duplicated(dt_2$RUT),]
names(dt_2)

colnames(dt_2)[colnames(dt_2) == "NOMBRE"] <- "NOMBRE_CLIENTE"

dt_3 <- merge(dt_1, dt_2, "RUT", all.x = TRUE)

unique(dt_3$FECHA_CALC)
count(dt_3$SUMC)
count(dt_1$CLEAN2)
count(dt_4$CLEAN2)
setwd("E:/Resultados_Bechmark")

ExportTable(dt_3,"JAVER_2.txt")

rm(dt_1, dt_2, dt_3, dt_4)
