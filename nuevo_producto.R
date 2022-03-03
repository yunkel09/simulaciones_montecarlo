
##  leer archivo

datos <- read.csv("datoshist.csv",header=T)

##  explorar variables

str(datos)

##  explorar visualmente la variable de ventas

hist(datos$ventas,col="blue")

##  calcular parametros de la distribución

ventasmedia <- mean(datos$ventas)

ventasmedia

desventas <- sd(datos$ventas)

desventas

##  explorar visualmente la variable precio

hist(datos$precio,col="red")

#  obtener los parámetros de la distribución

preciomin <- min(datos$precio)
preciomin

preciomax <- max(datos$precio)
preciomax

preciomp <- 255


## explorar visualmente la variable costomat

hist(datos$costomat,col="green")

# obtener los parámetros de la distribución

cmmin <- min(datos$costomat)
cmmin

cmmax <- max(datos$costomat)
cmmax

## explorar visualmente la variable costomo

hist(datos$costomo,col="pink")

# obtener los parámetros de la distribución

momin <- min(datos$costomo)
momin

momax <- max(datos$costomo)
momax


### Ejercicio de Simulación de Monte Carlo

##   Definir el # de simulaciones a realizar

numsim <-  5000

##  generar datos aleatorios para las ventas

ventasim <- rnorm(numsim, ventasmedia, desventas)

##  generar datos aleatorios para el precio unitario

install.packages("triangle")
library(triangle)

preciosim  <- rtriangle(numsim, preciomin, preciomax, preciomp) 

## generar datos aleatorios para el costo de materiales

costomatsim <- runif(numsim, cmmin, cmmax)

##  generar datos aleatorios para el costo de mano de obra

costomosim <- runif(numsim, momin, momax)

##  costos fijos

costoadmin <- 400000

costopub <- 600000

##  modelo financiero

utilidad <- ventasim*(preciosim - costomatsim - costomosim) - costoadmin - costopub

hist(utilidad,col="brown")

mean(utilidad)

sd(utilidad)

quantile(utilidad,0.135)








