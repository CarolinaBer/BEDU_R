#Postwork Sesión 1. Liga española de fútbol.



# 1. Importa los datos de soccer de la temporada 2019/2020 de la primera división
#de la liga española a R, los datos los puedes encontrar en el siguiente 
#enlace: https://www.football-data.co.uk/spainm.php

#---- Solución ------

#Utilizaremos el paquete instalado dplyr
suppressWarnings(suppressMessages(library(dplyr)))

#A continuación guardamos los archivos de los enlaces que nos interesan
D1_19.20 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"
  
# Descargamos nuestros datos en nuestro directorio de trabajo establecido.
#setwd("C:/Users/Carolina/Desktop/Parte 1.R/Sesión 1/Postwork/Soccer")

download.file(url = D1_19.20, destfile = "D1_19.20.csv", mode = "wb")
Fut_1920 <- read.csv("D1_19.20.csv") #Leyendo el archivo .csv


#2. Del data frame que resulta de importar los datos a R, 
#extrae las columnas que contienen los números de goles anotados por los equipos 
#que jugaron en casa (FTHG) y los goles anotados por los equipos que jugaron 
#como visitante (FTAG).

#------- Solución -------- 

FTHG <- Fut_1920$FTHG # Columna de goles anotados como equipo local
FTAG <- Fut_1920$FTAG # Columna de goles anotados como equipo visitante

Goles <- data.frame(FTHG,FTAG) # uniendo en un único dataframe



#3. Consulta cómo funciona la función table en R al ejecutar en la consola ?table

#Posteriormente elabora tablas de frecuencias relativas para estimar las siguientes probabilidades:

#------- Solución -------- 

?table
dimen <- dim(Goles)[1]

dimen # número de renglones de la tabla 

#  La probabilidad (marginal) de que el equipo que juega en casa anote 
#x goles (x = 0, 1, 2, ...)


##------------Solución------------------
#La solución más rápida la podemos obtener con table:
## table() returns a contingency table, an object of class "table",
## an array of integer values. 
### Esto quiere decir que aparecen los valores posibles así como las repeticiones.

table.FTHG <- table(FTHG)

## Para la probabilidad utilizamos prop.table() la cual:
#Returns conditional proportions given margins, i.e. entries of x, 
#divided by the appropriate marginal sums

proba.FTHG <- prop.table(table.FTHG) #(otra forma puede ser table.FTHG/dimen)

#Así, la probabilidad marginal de goles como local es:
proba.FTHG 


#La probabilidad (marginal) de que el equipo que juega como visitante 
#anote y goles (y = 0, 1, 2, ...)

#--- Solución-----
#Análogo al procedimiento anterior 

table.FTAG <- table(FTAG) #obteniendo la tabla 
proba.FTAG <- prop.table(table.FTAG) # obteniendo la probabilidad
#Así, la probabilidad marginal de goles como visitante es:
proba.FTAG

#La probabilidad (conjunta) de que el equipo que juega en casa anote x goles 
#y el equipo que juega como visitante anote y goles (x = 0, 1, 2, ..., y = 0, 1, 2, ...)

## ----Solución ------
# ?xtabs : for cross tabulation of data frames with a formula interface. (contingency table)
table.conj <- xtabs(~FTHG+FTAG, Fut_1920)
proba.conj <- table.conj/dimen #(opcionalmente se puede usar prop.table(table.conj))
# La probabilidad conjunta es:
proba.conj
