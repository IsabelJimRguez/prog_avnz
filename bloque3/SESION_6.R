########################################
            ### SESION 7 ###
########################################

## Rsaptial
install.packages("raster")
install.packages("sf")
install.packages("ggplot2")
install.packages("tmap")
install.packages("dplyr")
install.packages("mapview")
install.packages("leaflet")

library(sf)
library(dplyr)
library(tmap)  
library(mapview)

## EJEMPLO 1: DATOS VECTORIALES TIPO POLIGONO
# vamos a leer unos datos vectoriales
bayarea <- read_sf("bloque3/backup-counties-bayarea.shp")
str(bayarea)
glimpse(bayarea) # basicamente lo mismo que str pero más ordenado.

# vamos a hacer nuestro primer grafico de un mapa

x11()
plot(bayarea)
names(bayarea)

# hacemos un gráfico sencillo con tmap. 
tm_shape(bayarea) +
  tm_polygons(col =  "county", lwd = 2, lty = 3)+
  tm_layout("Mexico")

# Calculamos los centorides del poligono
bayarea_cent <- st_centroid(bayarea)

mapview(list(bayarea, bayarea_cent)) # Crea mapas interativos

## EJERCICIO 2
provincias <-  read_sf("bloque3/ll_provinciales_inspire_peninbal_etrs89.shp")
glimpse(provincias)

#Guardar datos vectoriales
getwd()
st_write(provincias, "bloque3//provincias.shp")

#explorar datos
class(provincias)
data.class(provincias)
head(provincias)
summary(provincias) 

provincias$geometry # da resultado de los metadatos
is.list(provincias$geometry)
str(provincias$geometry)

#creamos el mapa
x11()
plot(provincias, max.plot = 10) #el maxplot sirve para poder poner mas datos de los permitidos por defecto

#sacar la geometría del archivo vectorial
geo <- st_geometry(provincias)
plot(geo)
class(geo)
str(geo)
geo

#obtenos las proyeccion
st_crs(provincias)

# definir una proyeccion
st_crs(provincias) <- "4258"
st_crs(provincias) <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"

#Reproyectar 
prov_reproj <- st_transform(provincias, crs=32618)

## Ejemplo 3:  con datos vectoriales tipo puntos###
#leemos nuestro archivo de cmapo en que gemos estado tomando datos especiales 

madrid <- read.table("bloque3//Plotcode_madrid.txt", sep="\t")
data.class(madrid) 
View(madrid) #Observamos que esta incluyendo el nombre de las columnas como una fila nueva

#solucion 
madrid <- read.table("bloque3//Plotcode_madrid.txt", sep="\t", header = TRUE)
View(madrid)

#Convertir 
names(madrid)
summary(madrid)
madrid$FCCARB[is.na(madrid$FACCARB)] <- 0 #Todos los datos NA de la columna los cambia a 0
madrid <- na.omit(madrid) #Elimino observaciones con cualquier valor como na ¡MUCHO CUIDAD0!!


#vamos a seleccionar todos las parcelas donde la cobertura se mayor a 10% (i.e. definidas coo bosque)
x11()
boxplot(madrid$FCCARB)

bosque <- madrid[madrid$FCCARB > 10,]
dim(bosque)
dim(madrid)
nrow(bosque)/nrow(madrid)*100


##ahora pasamos de una data frame a un archivo vectorial para ello tenemos que saber que CY=latitud y CX=longitud
## Ctrl + Shift + M
sbosque <-bosque%>%  #primero pongo el nombre de la tabla. 
            st_as_sf(coords = c("CX","CY"), crs = 25830)


st_geometry(sbosque) %>% plot(col= "blue")

st_write(sbosque, "bloque3//bosque_madrid_10_fc.shp")

##Ejemplo 4: datos tipo poligono
#install.packages("devtools")
#install.packages("naturalearth")
#install.packages("usethis")
install.packages("sp")
install.packages("Rtools")

library(usethis)
library(devtools)
library(sp)


devtools::install_github("ropensci/rnaturalearth", force = TRUE)

library(rnaturalearth)

countries <-  ne_countries(returnclass = "sf")

x11()
tm_shape(shp = countries) + 
  tm_polygons(col = "name", border.col = "grey",
              title = "paises", lwd=2, lty =3) + 
  tm_layout(main.title = "Mapa del mundo", main.title.position = "center")

##states, admin levels####

devtools::install_github("ropensci/rnaturalearthhires",force=TRUE)
library(rnaturalearthhires)

sp_c <- ne_countries(returnclass = "sf",country="spain",scale = "large")
tm_shape(shp=sp_c) +
  tm_polygons(col="red")

##coast lines####
devtools::install_github("ropensci/rnaturalearthdata",force=TRUE)
library(rnaturalearthdata)

coast <- ne_coastline(returnclass = "sf")

st_geometry(coast) %>% plot(col="blue")

##countries: ejemplos de uso de ddply con un vectorial####

library(dplyr)
countries <- ne_countries(returnclass = "sf")
glimpse(countries)
names(countries)

europa <- countries %>% 
  select("name_sort","subregion","continent", "lastcensus","geometry") %>% 
  filter(continent=="Europe")
plot(europa)

##si quisiera eliminar la geometria tendria que usar st_drop_geometry

europa

st_geometry(europa) %>% plot()
plot(europa$geometry)

##hacemos una agrupacion por subregion####

europa_sr <- europa %>% 
  group_by(subregion) %>% 
  summarize(lastcensus=mean(lastcensus))

##mapa con diferentes colores para el lastcensus####

tm_shape(shp=europa_sr) +
  tm_polygons(col="lastcensus",border.col = "grey") +
  tm_layout(main.title = "Subregions Europa")

##extraer el sistema de coordenadas####

st_crs(europa_sr)

##obtener las coordenadas de la extension del mapa####

europa_sr %>% st_bbox()
st_bbox(europa_sr)

##convertir la extension en un poligono####

poly <- st_as_sfc(st_bbox(europa_sr))

poly

##obtener las coordenadas####

europa_sr %>% 
  st_coordinates() %>% 
  head()


##Parte 2: trabajando con datos raster####

library(sp)
library(raster)

##leer los datos raster de una unica banda con la funcion raster####

single <- raster("bloque3//PNOA_MDT200_ETRS89_HU30_Madrid.asc")

single

##los multibanda los leo con la funcion brick####

multi <- brick("data//LT05_L1TP_201032_20080820_20180116_01_T1_MB_7BANDAS.TIF")

multi

##escribir el archivo de salida####

writeRaster(single,"output//elevacion.tif")
writeRaster(multi,"output//landsat.tif")

plot(single)

##tamaño de archivo vs. tamaño de objetivo####

disk <- file.size("data//LT05_L1TP_201032_20080820_20180116_01_T1_MB_7BANDAS.TIF")

objeto <- object.size(multi)

disk
objeto
disk/objeto

##hemos hecho ya una prueba de grafico
plot(single)

tm_shape(single) + tm_raster()

##vamos a ver la extension del poligono####

extent(multi)
#extension <- c(xmin,xmax,ymin,ymax)
ext.ras <- c(304185,547215,4356885,4573515)
ext.pol <- c(385000,437215,4445000,4473515)

plot(ext.ras,col="red")
points(ext.pol,col="blue",add=TRUE)

new <- crop(multi,ext.pol)

plot(new)

tm_shape(new) + tm_raster()



