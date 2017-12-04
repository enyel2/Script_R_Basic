###Funciones simples
x <- function(x){
  x^2 + 3
}

x(6)

id <- c(1,45,76,8)
id

rl <- function(p){
  p < 10
}

rl(6)

5+5

### No olvida los for

for (i in 1:10)
{
  print(i)
}

as.numeric(TRUE)
as.numeric(FALSE)

toCheck <- 1

if (toCheck == 1)
{
  print("hello")
}
  
toCheck

###Primer ejercicio de if
check.bool <- function(x)
{
  if (x == 1)
  {
    print("hello")
  } else
  {
    print("goodbye")
  }
}
  
check.bool(4) 
check.bool(1)

## segundo ejercios de if
check.bool <- function(x)
{
  if (x ==1)
  {
    print("hello")
  } else if (x == 0)
  {
    print("goodbye")
  }else
  {
    print ("confused")
  }
}

check.bool(1)
check.bool(0)
check.bool(32)

### Una forma de agregar valores chequeables, uso de switch

use.switch <- function(x)
{
  switch(x,
         "a"="first",
         "b"="second",
         "c"="last",
         "z"="third",
         "other")
}  
  
use.switch("a")
use.switch(1)

# ifelse, lo bueno es que el segundo arguemento nos dice si es "true" y el tercero
# si es "false"

ifelse (1==1, "Yes", "NO")
ifelse (1==0, "Yes", "NO")
toTest <- c(1,1,0,0,1,0,1,0)
ifelse (toTest==1, "Yes", "NO")
