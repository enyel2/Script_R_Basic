rm(list=ls())

library (CellularAutomaton)

###############################
# para 7 

N <- 7
pell  <- expand.grid(rep(list(c(0, 1)), N))
pell <- data.matrix(pell)
pell
m <- matrix(NA, nrow=2^N, ncol=N)
m

for (i in 1:2^N){
  det <- sum(pell[i,])
  if (det == 5)
    for (j in 1:N){
      m[i,j] <- pell[i,j]
    }
}

m2 <- m[rowSums(is.na(m)) == 0,]
m2

S3 <- function(x) {
  if (!(i %in% c(1:3,7:9,13:15,19:21,25:27))) { # Impar R132
    if ((!x[1] & x[2] & !x[3]) | (sum(x) == 3)) return(1)
    else return(0)
  }
  else {         # Par R 222
    if ((!sum(x)) | (x[1] & !x[2] & x[3])) return(0)
    else return(1)
  }
}

# Porcentajes de 15% suma = 1
CellularAutomaton(t=3, fun=S3, seed=c(m2[1,]), bg=-1)$plot()  
CellularAutomaton(t=3, fun=S3, seed=c(m2[2,]), bg=-1)$plot()
CellularAutomaton(t=3, fun=S3, seed=c(m2[4,]), bg=-1)$plot()
CellularAutomaton(t=3, fun=S3, seed=c(m2[5,]), bg=-1)$plot()
CellularAutomaton(t=3, fun=S3, seed=c(m2[6,]), bg=-1)$plot()
CellularAutomaton(t=3, fun=S3, seed=c(m2[7,]), bg=-1)$plot()

# Porcentajes de 50% suma = 3

CellularAutomaton(t=2, fun=S3, seed=c(m2[1,]), bg=-1)$plot()  
CellularAutomaton(t=9, fun=S3, seed=c(m2[2,]), bg=-1)$plot()
CellularAutomaton(t=2, fun=S3, seed=c(m2[4,]), bg=-1)$plot()
CellularAutomaton(t=1, fun=S3, seed=c(m2[5,]), bg=-1)$plot()
CellularAutomaton(t=9, fun=S3, seed=c(m2[6,]), bg=-1)$plot()
CellularAutomaton(t=9, fun=S3, seed=c(m2[7,]), bg=-1)$plot()
CellularAutomaton(t=1, fun=S3, seed=c(m2[8,]), bg=-1)$plot()  
CellularAutomaton(t=9, fun=S3, seed=c(m2[9,]), bg=-1)$plot()
CellularAutomaton(t=2, fun=S3, seed=c(m2[10,]), bg=-1)$plot()
CellularAutomaton(t=9, fun=S3, seed=c(m2[11,]), bg=-1)$plot()
CellularAutomaton(t=9, fun=S3, seed=c(m2[12,]), bg=-1)$plot()
CellularAutomaton(t=1, fun=S3, seed=c(m2[13,]), bg=-1)$plot()
CellularAutomaton(t=9, fun=S3, seed=c(m2[14,]), bg=-1)$plot()  
CellularAutomaton(t=9, fun=S3, seed=c(m2[15,]), bg=-1)$plot()
CellularAutomaton(t=9, fun=S3, seed=c(m2[16,]), bg=-1)$plot()
CellularAutomaton(t=9, fun=S3, seed=c(m2[17,]), bg=-1)$plot()
CellularAutomaton(t=1, fun=S3, seed=c(m2[18,]), bg=-1)$plot()
CellularAutomaton(t=9, fun=S3, seed=c(m2[19,]), bg=-1)$plot()
CellularAutomaton(t=2, fun=S3, seed=c(m2[20,]), bg=-1)$plot()  
CellularAutomaton(t=2, fun=S3, seed=c(m2[21,]), bg=-1)$plot()
CellularAutomaton(t=9, fun=S3, seed=c(m2[22,]), bg=-1)$plot()
CellularAutomaton(t=9, fun=S3, seed=c(m2[23,]), bg=-1)$plot()
CellularAutomaton(t=1, fun=S3, seed=c(m2[24,]), bg=-1)$plot()
CellularAutomaton(t=9, fun=S3, seed=c(m2[25,]), bg=-1)$plot()
CellularAutomaton(t=1, fun=S3, seed=c(m2[26,]), bg=-1)$plot()
CellularAutomaton(t=9, fun=S3, seed=c(m2[27,]), bg=-1)$plot()
CellularAutomaton(t=9, fun=S3, seed=c(m2[28,]), bg=-1)$plot()
CellularAutomaton(t=9, fun=S3, seed=c(m2[29,]), bg=-1)$plot()
CellularAutomaton(t=9, fun=S3, seed=c(m2[30,]), bg=-1)$plot()
CellularAutomaton(t=2, fun=S3, seed=c(m2[31,]), bg=-1)$plot()
CellularAutomaton(t=9, fun=S3, seed=c(m2[32,]), bg=-1)$plot()
CellularAutomaton(t=1, fun=S3, seed=c(m2[33,]), bg=-1)$plot()
CellularAutomaton(t=9, fun=S3, seed=c(m2[34,]), bg=-1)$plot()
CellularAutomaton(t=2, fun=S3, seed=c(m2[35,]), bg=-1)$plot()
xper1 <- c(2,9,2,1,9,9,1,9,2,9,9,1,9,9,9,9,1,9,2,2,9,9,1,9,1,9,9,9,9,2,9,1,9,2)
mean(xper)

# Porcentajes de 75% suma = 5

CellularAutomaton(t=tiempo[1,], fun=S3, seed=c(m2[1,]), bg=-1)$plot()  
CellularAutomaton(t=9, fun=S3, seed=c(m2[2,]), bg=-1)$plot()
CellularAutomaton(t=9, fun=S3, seed=c(m2[4,]), bg=-1)$plot()
CellularAutomaton(t=9, fun=S3, seed=c(m2[5,]), bg=-1)$plot()
CellularAutomaton(t=1, fun=S3, seed=c(m2[6,]), bg=-1)$plot()
CellularAutomaton(t=1, fun=S3, seed=c(m2[7,]), bg=-1)$plot()
CellularAutomaton(t=9, fun=S3, seed=c(m2[8,]), bg=-1)$plot()  
CellularAutomaton(t=9, fun=S3, seed=c(m2[9,]), bg=-1)$plot()
CellularAutomaton(t=9, fun=S3, seed=c(m2[10,]), bg=-1)$plot()
CellularAutomaton(t=9, fun=S3, seed=c(m2[11,]), bg=-1)$plot()
CellularAutomaton(t=1, fun=S3, seed=c(m2[12,]), bg=-1)$plot()
CellularAutomaton(t=9, fun=S3, seed=c(m2[13,]), bg=-1)$plot()
CellularAutomaton(t=9, fun=S3, seed=c(m2[14,]), bg=-1)$plot()  
CellularAutomaton(t=9, fun=S3, seed=c(m2[15,]), bg=-1)$plot()
CellularAutomaton(t=1, fun=S3, seed=c(m2[16,]), bg=-1)$plot()
CellularAutomaton(t=9, fun=S3, seed=c(m2[17,]), bg=-1)$plot()
CellularAutomaton(t=9, fun=S3, seed=c(m2[18,]), bg=-1)$plot()
CellularAutomaton(t=1, fun=S3, seed=c(m2[19,]), bg=-1)$plot()
CellularAutomaton(t=9, fun=S3, seed=c(m2[20,]), bg=-1)$plot()  
CellularAutomaton(t=1, fun=S3, seed=c(m2[21,]), bg=-1)$plot()

tiempo <- matrix(NA, nrow=21, ncol=1)
tiempo [1,] <- 1
tiempo

xper2 <- c(1,9,9,9,1,1,9,9,9,9,1,9,9,9,1,9,9,9,1,9,9,1,9,1)
mean(xper2)

