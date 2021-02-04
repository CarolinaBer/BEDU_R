## Postwork Sesión 7. Alojar el fichero a un local host de MongoDB.

Utilizando el manejador de BDD Mongodb Compass (previamente instalado), deberás de realizar las siguientes acciones:

-Alojar el fichero data.csv en una base de datos llamada match_games, nombrando al collection como match

-Una vez hecho esto, realizar un count para conocer el número de registros que se tiene en la base

-Realiza una consulta utilizando la sintaxis de Mongodb, en la base de datos para conocer el número de goles que metió el Real Madrid el 20 de diciembre de 2015 y contra que equipo jugó, ¿perdió ó fue goleada?

-Por último, no olvides cerrar la conexión con la BDD.

## Solución
Es útil recordar lo que aprendimos en el módulo pasado del uso de MongoDB Compass.La consulta de los goles del Real Madrid se muestra como:
![imagen](img/query.png)

Adicionalmente podemos corroborar mediante la conexión a MongoDB Compass que lo que relizamos en R se ve reflejado. 

La creación de la bd match_test y la colección match.Además de ya haber alojado el fichero.

![imagen](img/match_mongo.png)

La actualización de los datos con las jugadas faltantes:

![imagen](img/match_actual.png)
![imagen](img/match_2015.png)


El código utilizado fue el siguiente:
```R
#Postwork 7. Alojar el fichero a un local host de MongoDB.



#Instalamos los paquetes para conectarnos a MongoDB. 
install.packages("mongolite")
install.packages("data.table")

#Utilizaremos las librerías mongolite y dply (previamente instalada)
suppressWarnings(suppressMessages(library(mongolite)))
suppressWarnings(suppressMessages(library(dplyr)))


#Utilizando el manejador de BDD Mongodb Compass (previamente instalado), deberás de realizar las siguientes acciones:

  
#1. Alojar el fichero data.csv en una base de datos llamada match_games, nombrando al collection como match

#----Sol.---

#Importando el archivo .csv a R
data.url <- "https://raw.githubusercontent.com/beduExpert/Programacion-con-R-Santander/master/Sesion-07/Postwork/data.csv"

data <- read.csv(data.url) #Leer el archivo del link

#También podemos descargarlo a nuestro directorio de trabajo
setwd("C:/Users/Carolina/Desktop/Postwork/Sesión 7")

download.file(url = data.url, destfile = "data.csv", mode = "wb")
data.r <- read.csv("data.csv") #Leyendo el archivo .csv

# Ahora tenemos que conocer cómo utilizar MongoDB con R. Por ejemplo: Using MongoDB with R : https://datascienceplus.com/using-mongodb-with-r/
# o de la documentación de la paquetería mongolite: https://jeroen.github.io/mongolite/query-data.html 

#De la paquetería data.table utilizamos la función fread para leer el archivo, fread stands for Fast and friendly file finagler
#Similar to read.table but faster and more convenient. fread is for regular delimited files; i.e., where every row has the same number of columns.

match = data.table::fread(data.url) #aquí ya está el archivo .csv listo para exportar a MongoDB
names(match)

#Otra forma de hacerlo es la siguiente:
# library(data.table)
# m <- fread(data.url)

#mediante mongo() de la paquetería mongolite tenemos la conección a MongoDB

# Por ejemplo, si nos podemos conectar a una base de datos disponibles en MongoDB, por default  url = "mongodb://localhost"
#En este caso el url proporcionado es la liga que hemos usado en la primera fase del curso, en ***** se coloca la contraseña que es parte de las credenciales
#db <- mongo(collection = "listingsAndReviews", db = "sample_airbnb", url = "mongodb+srv://CaroBer:********@clusterbedu.v2ujr.mongodb.net/test?authSource=admin&replicaSet=atlas-b1gsnf-shard-0&connectTimeoutMS=600000&socketTimeoutMS=6000000&readPreference=primary&appname=MongoDB%20Compass&ssl=true")
#db$count() #contamos el número de documentos o registros
#db$find(query='{"beds":12}',fields = '{"name":true}') #con find podemos hacer consultas

#Hay que recordar que la sintaxis para las consultas en MongoDB se realizan como llave-valor.



#Creamos la base de datos match_test y dentro de esta la colección match
match_collection = mongo(collection = "match", db = "match_test",url="mongodb+srv://CaroBer:********@clusterbedu.v2ujr.mongodb.net/test?authSource=admin&replicaSet=atlas-b1gsnf-shard-0&connectTimeoutMS=600000&socketTimeoutMS=6000000&readPreference=primary&appname=MongoDB%20Compass&ssl=true")

match_collection$insert(match) #Alojamos el fichero en dicha colección mediante insert



#2. Una vez hecho esto, realizar un count para conocer el número de registros que se tiene en la base
cuenta <- match_collection$count()

paste("El número de registros de la base de datos es: ",cuenta )
#Hay 1140 registros 


#3. Realiza una consulta utilizando la sintaxis de Mongodb, en la base de datos para conocer el número
#de goles que metió el Real Madrid el 20 de diciembre de 2015 y contra que equipo jugó, ¿perdió ó fue goleada?


#Como en la base de datos no se encuentra la fecha solicitada, al ser la menor fecha en 2017
#Entonces para obtener los datos faltantes hay que examinar esta base de datos y darnos cuenta de que 
#precisamente se trata de los archivos que hemos estado utilizando desde los primeros Postwork.
#Los cuale son los partidos jugados por la liga española de la 1era división, las cuales se encuentran en:
#https://www.football-data.co.uk/spainm.php, en este enlace podemos buscar los datos correspondientes a la temporada 2015-2016


#Nota: También nos podemos dar cuenta que el archivo match.data.csv se encuentra en el postwork 6, en el cual
#se encuentran partidos desde 2010.

#Dicho lo anterior, agragamos los datos faltantes

url.2015 <- "https://www.football-data.co.uk/mmz4281/1516/SP1.csv"
data.2015 <- read.csv(url.2015)
head(data.2015)

#Podemos descardar el archivo a nuestro dir de trabajo 
download.file(url = url.2015, destfile = "data_2015.csv", mode = "wb")
data_2015 <- read.csv("data_2015.csv") #Leyendo el archivo .csv

#Cambiamos el formato de fecha de caracter a fecha
data.2015<- mutate(data.2015, Date = as.Date(Date, "%d/%m/%y"))
data.2015 <- select(data.2015,Date,HomeTeam:FTR) #Seleccionamos las columnas que nos interesan

setwd("C:/Users/Carolina/Desktop/Postwork/Sesión 7")

fut.2015 <- write.csv(data.2015, file="fut_2015.csv") #Ya está guardado el .csv en el dir de trabajo 
match.2015 = data.table::fread("fut_2015.csv")
names(match.2015)

#Podemos crear una nueva colección o guardarla dentro de la misma creada anteriormente, para recordarlo:
#La llamaremos collection_2015 para que no se arroje error por parte de R, ya que es un "Environment" 

#En la misma base de datos y la misma colección alojamos los nuevos datos
collection_2015 = mongo(collection = "match", db = "match_test",url="mongodb+srv://CaroBer:********@clusterbedu.v2ujr.mongodb.net/test?authSource=admin&replicaSet=atlas-b1gsnf-shard-0&connectTimeoutMS=600000&socketTimeoutMS=6000000&readPreference=primary&appname=MongoDB%20Compass&ssl=true")
collection_2015$insert(match.2015)  

collection_2015$find() #Podemos visualizar ahora todo el fichero 

#Realizando la consulta con la sintaxis de MongoDB (llave-valor)
query<- collection_2015$find(query = '{"Date":"2015-12-20","HomeTeam" : "Real Madrid"}')
query

#fue goleada por parte del Real Madrid 10-2.
paste("Fue goleada por parte del Real Madrid quien anotó", query$FTHG ,"goles contra el", query$AwayTeam)

#Por último, no olvides cerrar la conexión con la BDD
rm(collection_2015) #remove objects from a specified environment
rm(match_collection)

#remove and rm can be used to remove objects. These can be specified successively as character strings, or in the character vector list, or through a combination of both.
#All objects thus specified will be removed.
```
