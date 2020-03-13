install.packages("dismo")
install.packages("XML")
install.packages("maptools")
install.packages("dplyr")
library(dismo) # contiene la base de datos gbif
library(XML)
library(maptools)
library(dplyr)


## Descargar ocurriencia de especies del gbif

?gbif
lince <- gbif("Lynx", "pardinus", download=FALSE)  #te ensela el numero de ocurriencias. 
head(lince)
str(lince)

lince <- gbif("Lynx", "pardinus", download=TRUE) 
head(lince)
glimpse(lince)

write.csv(lince, "//bloque3/lince_ocurrencia.csv") # guarda los datos en un csv
plot(lince$lat ~ lince$lon, pch=16, cex=0.6, col=red)
summary(lince[,c("lat","lon")])

x11()
data(wrld_simpl)
plot(wrld_simpl, xlim = c(-8,2), ylim=c(35,43), axez = TRUE, col= "light yellow")
box()  
points(lince$lat ~ lince$lon, pch=20, cex=0.6, col ="red")   

#leer los datos de toda una carpeta
getwd()

#carpeta donde tengo los datos
path <- "C:/Users/Isabel/Documents/MASTER/PROGRAMACION AVANZADA/prog_avnz/bloque3/bioclim"
files <- list.files(path, pattern =  "tif$", full.names = TRUE)

#crep un raster stack

predictores <- stack(files) 
predictores
names(predictores)
x11()

#hacemos un clip con la extension de la distribucion del lince
range(na.omit(lince[, c("lon")]))
range(na.omit(lince[, c("lat")]))

e <- extent(-12, 5,35, 45)
pred <- crop(predictores, e)
dev.off()
plot(pred, 9)
points(lince$lat ~ lince$lon, pch= 20, cex = 1.5, col= "red")

library(raster)

#extrae las coordenadas donde esta el lince del archivo raster
coord <- lince[, c("lon", "lat")]
values <- extract(pred, coord)
glimpse(values)       

x11()
pairs(values, [, c(1:10)])
data <- as.data.frame(values)
data <- na.omit(data)

##model fitting
data <- subset(pred, c(1,12,15))
model1 <- bioclim(data, coord)
class(model1)


##model prediction idoneidad del habitat
map <-  predict(data, model1)
class(map)
plot(map)

##pasamos de idoneada del habita a presencia/ausencia
presp <- reclassify(map, c(0,0.75, 0.76, 1,1))

par(mfrow=c(1,2))
plot(map)
plot(presp)                    

