## Postwork Sesión 2. Manipulación de datos en R. 
Desarrollo


1.    Importa los datos de soccer de las temporadas 2017/2018, 2018/2019 y 2019/2020 de la primera división de la liga española a R, los datos los puedes encontrar en el siguiente enlace: https://www.football-data.co.uk/spainm.php

 2.   Obten una mejor idea de las características de los data frames al usar las funciones: str, head, View y summary

3.    Con la función select del paquete dplyr selecciona únicamente las columnas Date, HomeTeam, AwayTeam, FTHG, FTAG y FTR; esto para cada uno de los data frames. (Hint: también puedes usar lapply).

4.    Asegúrate de que los elementos de las columnas correspondientes de los nuevos data frames sean del mismo tipo (Hint 1: usa as.Date y mutate para arreglar las fechas). Con ayuda de la función rbind forma un único data frame que contenga las seis columnas mencionadas en el punto 3 (Hint 2: la función do.call podría ser utilizada).

## :pushpin: Solución:
```R
# Postwork sesión 2 : 

#Cargamos el paquete dplyr
suppressWarnings(suppressMessages(library(dplyr)))

#1. Importa los datos de soccer de las temporadas 2017/2018, 2018/2019 y 2019/2020
##de la primera división de la liga española a R, los datos los puedes encontrar 
#en el siguiente enlace: https://www.football-data.co.uk/spainm.php

# ---- Solución --------

#Importamos los archivos de la url a .csv
fut.17_18 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
fut.18_19 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
fut.19_20 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"

#El archivo se puede descargar o leer directamente de la url para importarlo a R
# Al descargarlo tenemos que estar en el directorio de trabajo deseado 
setwd("C:/Users/Carolina/Desktop/Parte 1. R/Sesión 2/Postwork/Liga.esp")

download.file(url = fut.17_18, destfile = "D1_17.18.csv", mode = "wb")
Fut.1718 <- read.csv("D1_17.18.csv") #Leyendo el archivo .csv


download.file(url = fut.18_19, destfile = "D1_18.19.csv", mode = "wb")
Fut.1819 <- read.csv("D1_18.19.csv") #Leyendo el archivo .csv


download.file(url = fut.19_20, destfile = "D1_19.20.csv", mode = "wb")
Fut.1920 <- read.csv("D1_19.20.csv") #Leyendo el archivo .csv

dir() #para corroborar que se encuentren los archivos descargados

#-----------------
# 2. Obten una mejor idea de las características de los data frames
#al usar las funciones: str, head, View y summary

str(Fut.1718) ; head(Fut.1718) ; View(Fut.1718) ; summary(Fut.1718)
str(Fut.1819) ; head(Fut.1819) ; View(Fut.1819) ; summary(Fut.1819)
str(Fut.1920) ; head(Fut.1920) ; View(Fut.1920) ; summary(Fut.1920)

#-----------------
# 3. Con la función select del paquete dplyr selecciona únicamente las columnas 
#Date, HomeTeam, AwayTeam, FTHG, FTAG y FTR; esto para cada uno de los data frames. 
#(Hint: también puedes usar lapply).

fut.list <- list(Fut.1718,Fut.1819,Fut.1920) # Almacenamos los 3 df en una lista
sel.fut <- lapply(fut.list, select, Date, HomeTeam:FTR) # con lapply aplicamos una función a una lista
## Para ver la estructura 
lapply(sel.fut,str)
lapply(sel.fut,names) # para ver el nombre de las columnas



#-----------
# 4. Asegúrate de que los elementos de las columnas correspondientes de los 
# nuevos data frames sean del mismo tipo (Hint 1: usa as.Date y mutate para 
#arreglar las fechas). Con ayuda de la función rbind forma un único data frame 
# que contenga las seis columnas mencionadas en el punto 3
# (Hint 2: la función do.call podría ser utilizada).

str(sel.fut[[1]]) # con esto sabemos que Date es de tipo char 
?as.Date # Functions to convert between character representations and objects of class "Date"
?mutate #adds new variables and preserves existing ones, new var. overwrite existing ones. 

sel.fut[[1]] <- mutate(sel.fut[[1]], Date = as.Date(Date, "%d/%m/%y"))
sel.fut[[2]] <- mutate(sel.fut[[2]], Date = as.Date(Date, "%d/%m/%y"))
sel.fut[[3]] <- mutate(sel.fut[[3]], Date = as.Date(Date, "%d/%m/%y"))

#Corroborando
str(sel.fut[[1]])
lapply(sel.fut, str)

# Para formar un único df 
df.fut <- do.call(rbind, sel.fut) #combinando por renglones
dim(df.fut)
```
Con ```R View(df.fut) ``` podemos visualizar el nuevo dataframe creado
