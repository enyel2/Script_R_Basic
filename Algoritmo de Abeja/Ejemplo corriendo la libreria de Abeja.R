library (ABCoptim)

fun <- function(x){-cos(x[1])*cos(x[2])*exp(-((x[1]-pi)^2+(x[2]-pi)^2))
  
}

abc_optim(rep(0,2), fun, lb=-20, ub=20, criter=100) 


fw <- function(x){
  10*sin(0.3*x)*sin(1.3*x^2)+0.00001*x^4+0.2*x+80
}

abc_optim(50, fw, lb=-100, ub=100, criter=100)

