library(triangle)

## prob Motores

numrep = 5000

GastosAdmin = 400000

Publicidad = 600000

Precio = rtriangle(numrep, 250, 270, 255)

CostoMateriales = runif(numrep, 80, 100)

Demanda = rnorm(numrep, 15000, 4500)

ManoDeObra = rnorm(numrep, 50, 3)

Ingresos =  Precio * Demanda

CostosVariables =  CostoMateriales * Demanda + ManoDeObra * Demanda

CostosFijos =  GastosAdmin + Publicidad

Costos = CostosFijos + CostosVariables

Utilidad =  Ingresos - Costos

hist(Utilidad)

summary(Utilidad)

abline(v=0, col="red")

media = mean(Utilidad)
desv = sd(Utilidad)

pnorm(0, media, desv)

1 - pnorm(200000, media, desv)

CoefVar = desv/media*100
CoefVar

IQR(Utilidad)

int95 = c(-2,2)*desv + media

int95

caja = boxplot(Utilidad)

caja$out

df1 = data.frame(Precio, Demanda, ManoDeObra, CostoMateriales, Utilidad)

alertas = df1[df1$Utilidad < 200000,]

head(alertas)

dim(alertas)



## Problema  Juguete


numrep = 25000

CostoFijo = 100000

CostoVariable = runif(numrep, 30, 38)

Precio = sample(c(42,48), numrep, rep=T,prob=c(0.35, 0.65))

Produccion = 70000

Demanda = rnorm(numrep, 60000, 15000)

ValorSalvamento = 10

Costos =  CostoFijo + Produccion*CostoVariable

Ingresos = ifelse(Demanda < Produccion, Demanda*Precio + (Produccion-Demanda)*ValorSalvamento, Produccion*Precio)

Ingresos2 = (Demanda<Produccion)*(Demanda*Precio + (Produccion-Demanda)*ValorSalvamento) + (Demanda >= Produccion)*(Produccion*Precio)

summary(Ingresos)
summary(Ingresos2)

Utilidad = Ingresos - Costos

hist(Utilidad)

quantile(Utilidad,0.27)


perc = seq(0.01,0.99,0.01)
util  = quantile(Utilidad, perc)

plot(perc,util, type="l", xaxp=c(0,1,20))

x = perc[24:28]
y = util[24:28]

abline(h=0, col="green")

f = approxfun(x, y)

sol = uniroot(f,c(0.27,0.28))

abline(v=sol$root, col="red")


























