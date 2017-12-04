for (i in 1:10){
  print (i*2)
}

fruit <- c("apple","banana","pomegranate")
fruitLength <- rep(NA, length(fruit))
fruitLength
names(fruitLength) <- fruit
fruitLength

for (a in fruit){
  fruitLength[a] <- nchar(a)
}
fruitLength
### apple = numero de letras (5); banana = 6 letras; pomegranate = 22 letras

fruitLength2 <- nchar(fruit)
fruitLength2
names(fruitLength2) <- fruit
fruitLength2

identical(fruitLength, fruitLength2)

### while esta funcion se de tiene cuando llega a la condicion que se le otorga

x <- 1
while (x <= 5)
{
  print(x)
  x <- x + 1
}

### controlando Loops

for (i in 1:10)
{
  if (i ==3)
  {
    next
  }
  print(i)
}

for (i in 1:10)
{
  if (i == 4)
  {
    break
  }
  print(i)
}

