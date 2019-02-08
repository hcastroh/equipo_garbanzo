# TAREA 1. MACROECONOMIA II
# EL COLEGIO DE MEXICO
# MAESTRIA EN ECONOMIA

# JUAN PABLO HERNANDEZ, ANA SILVIA ROMERO, TANIA ROJAS, HUGO CASTRO

library(readxl)
library(ggplot2)
library(dplyr)
library(reshape2)
library(scales)


# EJERCICIO 2 ####

# a) 
sigma.p <- 1
Yp <- rnorm(20, 10, sigmap)    #Define los ingresos permanentes de las 20 personas
Yps <- matrix(Yp[1], ncol = 1, nrow = 100)   #100 el ingreso permanente que le tocó a 1

for(i in 2:20){
  Ypi <- rep(Yp[i], 100)
  Yps <- cbind(Yps, Ypi)
  rm(Ypi)
}

agentes <- c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10", 
             "A11", "A12", "A13", "A14", "A15", "A16", "A17", "A18", "A19", "A20")
names(Yps) <- agentes
View(Yps)

plot(Yp, main = "Ingresos Permanentes",
     xlab = "Agente #",
     ylab = "Ingreso Permanente")


# b) Crear 20 vectores de 100 ingresos transitorios
sigma.t <- 1
Yt <- matrix(rnorm(2000, 0, sigma.t), ncol = 20, nrow = 100)

names(Yt) <- agentes

# c) Crear 20 vectores de 100 ingresos totales como la suma de permanentes y transitorios


# d) Crear 20 vectores de 100 errores de medicion


# e) Crear 20 vectores de 100 consumos cada uno 


# f) Estimar relación lineal


# g) Incrementar la varianza del ingreso permanente y disminuir la 
#    varianza del ingreso transitorio y repetir la relación


# h) Disminuir la varianza del ingreso permantente y aumentar la del transitorio y 
#    repetir el ejercicio


# EJERCICIO 3 ####
setwd("C:/Users/Pablo/Documents/MAESTRIA/SEMESTRE2/MACROECONOMIA II/Tareas/01 Consumo")


# a) Conseguir Datos

# Fuente de los datos: http://www.beta.inegi.org.mx/temas/ofyd/
# Son cifras desestacionalizadas a precios constantes de 2013


demanda <- read_excel("demanda2013.xlsx", sheet = "Hoja1")


fecha <- seq(as.Date("1980/01/01"), length.out = length(demanda$Y), by = "quarter")
demanda <- demanda / 1000
demanda <- cbind(fecha, demanda)


# calculando logaritmos
l.demanda <- log(demanda[,-1])
l.demanda <- cbind(fecha, l.demanda)


# Calculando tasas
d.demanda <- demanda[-1,]
d.demanda <- d.demanda[,-c(3, 7, 8)]

attach(l.demanda)
d.demanda$Y <- diff(Y)
d.demanda$C <- diff(C)
d.demanda$I <- diff(I)
d.demanda$G <- diff(G)
detach(l.demanda)

# for para calcular las tasas de crecimiento de NX
d.demanda$NX <- rep(0, length(demanda$NX)-1)
for(i in 2:length(demanda$NX)){
  d.demanda$NX[i-1] <- (demanda$NX[i]-demanda$NX[i-1])/demanda$NX[i]
}
rm(i)




# b) Graficando niveles y logaritmos
# Grafica de los Niveles
melt.n <- demanda[,!colnames(demanda) == "X" & !colnames(demanda) == "M"]
melt.n <- melt(meltd, id="fecha")

ggplot(meltd, aes(x=fecha, y=value, colour=variable, group=variable)) + 
  geom_line(size = 1) +
  labs(y = "", x = "Fecha", title = "Variables Macroeconómicas de México (Niveles) \n 1980 - 2018", 
       caption = "Fuente: Inegi") +
  scale_color_manual(values=c("black", "red", "blue3", "green4", "darkorange")) +
  scale_y_continuous(labels = dollar) +
  theme_gray()
rm(melt.n)


# Gáfica de los Logaritmos
melt.l <- l.demanda[,!colnames(demanda) == "X" & !colnames(demanda) == "M" & !colnames(demanda) == "NX"]
melt.l <- melt(melt.l, id="fecha")

ggplot(melt.l, aes(x=fecha, y=value, colour=variable, group=variable)) + 
  geom_line(size = 1) +
  labs(y = "", x = "Fecha", title = "Variables Macroeconómicas de México (Logaritmos) \n 1980 - 2018", 
       caption = "Fuente: Inegi \n Nota: Se omite la variable NX dado que, al tener valores negativos, 
       no es posible utilizar logaritmos") +
  scale_color_manual(values=c("black", "red", "blue3", "green4", "darkorange")) +
  theme_gray()
rm(melt.l)



# c) Graficando tasas
melt.d <- d.demanda[,!colnames(d.demanda) == "X" & !colnames(d.demanda) == "M"]
melt.d <- melt(melt.d, id = "fecha")

ggplot(melt.d, aes(x=fecha, y=value, colour=variable, group=variable)) + 
  geom_line(size = 1) +
  labs(y = "", x = "Fecha", title = "Variables Macroeconómicas de México (Tasas de Crecimiento) \n 1980 - 2018", 
       caption = "Fuente: Inegi") +
  scale_color_manual(values=c("black", "red", "blue3", "green4", "darkorange")) +
  theme_gray()
rm(melt.d)


# d) Graficar tasas de consumo e ingreso
melt.d <- d.demanda[,!colnames(d.demanda) == "G" & !colnames(d.demanda) == "I"]
melt.d <- melt(melt.d, id = "fecha")

ggplot(melt.d, aes(x=fecha, y=value, colour=variable, group=variable)) + 
  geom_line(size = 1) +
  labs(y = "", x = "Fecha", title = "Consumo e Ingreso (Tasas de Crecimiento) \n 1980 - 2018", 
       caption = "Fuente: Inegi") +
  scale_color_manual(values=c("black", "red")) +
  theme_gray()
rm(melt.d)



# e) Calcular la volatilidad de las tasas de crecimiento 
varY <- var(d.demanda$Y)
varC <- var(d.demanda$C)

varianzas <- c(varY, varC)
base::rownames(varianzas) <- c("VarY", "VarC")
varianzas


# f) Estimar modelos lineales

# NOTA: Se usan los datos a partir de 1993 por razones metologicas
demanda <- filter(demanda, fecha > "1992-10-01")
l.demanda <- filter(l.demanda, fecha > "1992-10-01")
d.demanda <- filter(d.demanda, fecha > "1992-10-01")

modelo.1 <- lm(demanda$C ~ demanda$Y)
summary(modelo.1)

modelo.2 <- lm(d.demanda$C ~ d.demanda$Y)
summary(modelo.2)

modelo.3 <- lm(d.demanda$C ~ lag(d.demanda$Y))
summary(modelo.3)

modelo.4 <- lm(l.demanda$C ~ l.demanda$Y)
summary(modelo.4)

stargazer(list(modelo.1, modelo.2, modelo.3, modelo.4), type="latex")

