install.packages("ggplot2")
install.packages("dplyr")
install.packages("patchwork")
install.packages("hrbrthemes")

library(ggplot2)
library(dplyr)
library(patchwork)
library(hrbrthemes)

datos <- longley # Toma los datos del dataset de R
datos$Tasa.ocupacion <- (datos$Employed/datos$Population)*100 # se añade una la tasa de ocupacion al dataframe

#Para poder aplicar la formula de variacion del PIB
#Se necesita obtner una columna con el PIB referido al año anterio

for (i in 1:nrow(datos)){
  pib.anterior.15<- datos[1:15,2] #selecciona los datos del pib del año anterior
                                  # y los coloca en la fila del año de interes
}

#Se 3crea un valor 0 para unirlo al anterior vector y que den 
# 16 observaciones y asi poder unirlo como una nueva columna del dataframe

base <- c(0)
pib.anterior.16 <- c(base, pib.anterior.15)

#Añade nueva columna al dataframe
datos$pib.anterior <- pib.anterior.16

#Se hace un bucle que permita aplicar la fórmula del crecimiento del pib a todo las filas
for(i in 1:nrow(datos)){
  datos$Crecimiento.pib <- ((datos$GNP-datos$pib.anterior)/datos$pib.anterior)*100
}

datos.2 <- datos[-1,]


#plot
x <- 1
x11()
par(bg="white")
ggplot(datos.2, aes(x=Year, name="año")) + 
  
  geom_bar( aes(y=Population), stat = "identity", size=.1, color= "#69b3a2") +
  geom_line( aes(y=Crecimiento.pib), size=1.5) +
  
  scale_y_continuous(
    name= "Crecimiento interanual PIB", 
    sec.axis = sec_axis(~.x, name="Población")
  ) + 
  
  theme_ipsum()
    

# Se creará una tabla con los principales datos estadísticos de la base de
# datos iris
library(plyr)

#kable Permite generar tablas sobre data.frames
tabla <- knitr::kable((ddply(iris, .(Species), summarize,
                             "Anchura Pétalo"= mean(Petal.Width),
                             "Largo Pétalo"= mean(Petal.Length),
                             "Anchura Sépalo"= mean(Sepal.Width),
                             "Largo Sépalo" = mean(Sepal.Length)
)), align='c',  
caption="Largo y ancho medio de los pétalos y 
             sépalos por especies")

