########################################
            ### SESION 4 ###
########################################

## ANALISIS DE DATOS EN RMARKDOWN ##
dir.create("C:/Users/Isabel/Documents/MASTER/PROGRAMACIÓN AVANZADA/PRACTICA2/SCRIPTS")
setwd("C:/Users/Isabel/Documents/MASTER/PROGRAMACIÓN AVANZADA/PRACTICA2/SCRIPTS")
getwd()

## Introduccion a GIT ##
## Primero debes instalar el paquete "usethis"hh

install.packages("usethis")

## Se debe invocar el paquete
## Seguidamente usar la funcion use_git_config.
## el user name puede ser disntinto al de github, per
## pero el correo debe ser el mismo. 

library(usethis)
use_git_config(user.name= "IsabelJimRguez",
               user.email="isabel.jimenezr@edu.uah.es")

