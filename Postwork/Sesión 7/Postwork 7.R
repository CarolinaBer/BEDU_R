#Postwork 7. Alojar el fichero a un local host de MongoDB.



#Instalamos los paquetes para conectarnos a MongoDB. 
install.packages("mongolite")
install.packages("data.table")

#Utilizaremos las librer�as mongolite y dply (previamente instalada)
suppressWarnings(suppressMessages(library(mongolite)))
suppressWarnings(suppressMessages(library(dplyr)))


#Utilizando el manejador de BDD Mongodb Compass (previamente instalado), deber�s de realizar las siguientes acciones:

  
#1. Alojar el fichero data.csv en una base de datos llamada match_games, nombrando al collection como match

#----Sol.---

#Importando el archivo .csv a R
data.url <- "https://raw.githubusercontent.com/beduExpert/Programacion-con-R-Santander/master/Sesion-07/Postwork/data.csv"

data <- read.csv(data.url) #Leer el archivo del link

#Tambi�n podemos descargarlo a nuestro directorio de trabajo
setwd("C:/Users/Carolina/Desktop/Postwork/Sesi�n 7")

download.file(url = data.url, destfile = "data.csv", mode = "wb")
data.r <- read.csv("data.csv") #Leyendo el archivo .csv

# Ahora tenemos que conocer c�mo utilizar MongoDB con R. Por ejemplo: Using MongoDB with R : https://datascienceplus.com/using-mongodb-with-r/
# o de la documentaci�n de la paqueter�a mongolite: https://jeroen.github.io/mongolite/query-data.html 

#De la paqueter�a data.table utilizamos la funci�n fread para leer el archivo, fread stands for Fast and friendly file finagler
#Similar to read.table but faster and more convenient. fread is for regular delimited files; i.e., where every row has the same number of columns.

match = data.table::fread(data.url) #aqu� ya est� el archivo .csv listo para exportar a MongoDB
names(match)

#Otra forma de hacerlo es la siguiente:
# library(data.table)
# m <- fread(data.url)

#mediante mongo() de la paqueter�a mongolite tenemos la conecci�n a MongoDB

# Por ejemplo, si nos podemos conectar a una base de datos disponibles en MongoDB, por default  url = "mongodb://localhost"
#En este caso el url proporcionado es la liga que hemos usado en la primera fase del curso, en ***** se coloca la contrase�a que es parte de las credenciales
#db <- mongo(collection = "listingsAndReviews", db = "sample_airbnb", url = "mongodb+srv://CaroBer:********@clusterbedu.v2ujr.mongodb.net/test?authSource=admin&replicaSet=atlas-b1gsnf-shard-0&connectTimeoutMS=600000&socketTimeoutMS=6000000&readPreference=primary&appname=MongoDB%20Compass&ssl=true")
#db$count() #contamos el n�mero de documentos o registros
#db$find(query='{"beds":12}',fields = '{"name":true}') #con find podemos hacer consultas

#Hay que recordar que la sintaxis para las consultas en MongoDB se realizan como llave-valor.



#Creamos la base de datos match_test y dentro de esta la colecci�n match
match_collection = mongo(collection = "match", db = "match_test",url="mongodb+srv://CaroBer:********@clusterbedu.v2ujr.mongodb.net/test?authSource=admin&replicaSet=atlas-b1gsnf-shard-0&connectTimeoutMS=600000&socketTimeoutMS=6000000&readPreference=primary&appname=MongoDB%20Compass&ssl=true")

match_collection$insert(match) #Alojamos el fichero en dicha colecci�n mediante insert



#2. Una vez hecho esto, realizar un count para conocer el n�mero de registros que se tiene en la base
cuenta <- match_collection$count()

paste("El n�mero de registros de la base de datos es: ",cuenta )
#Hay 1140 registros 


#3. Realiza una consulta utilizando la sintaxis de Mongodb, en la base de datos para conocer el n�mero
#de goles que meti� el Real Madrid el 20 de diciembre de 2015 y contra que equipo jug�, �perdi� � fue goleada?


#Como en la base de datos no se encuentra la fecha solicitada, al ser la menor fecha en 2017
#Entonces para obtener los datos faltantes hay que examinar esta base de datos y darnos cuenta de que 
#precisamente se trata de los archivos que hemos estado utilizando desde los primeros Postwork.
#Los cuale son los partidos jugados por la liga espa�ola de la 1era divisi�n, las cuales se encuentran en:
#https://www.football-data.co.uk/spainm.php, en este enlace podemos buscar los datos correspondientes a la temporada 2015-2016


#Nota: Tambi�n nos podemos dar cuenta que el archivo match.data.csv se encuentra en el postwork 6, en el cual
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

setwd("C:/Users/Carolina/Desktop/Postwork/Sesi�n 7")

fut.2015 <- write.csv(data.2015, file="fut_2015.csv") #Ya est� guardado el .csv en el dir de trabajo 
match.2015 = data.table::fread("fut_2015.csv")
names(match.2015)

#Podemos crear una nueva colecci�n o guardarla dentro de la misma creada anteriormente, para recordarlo:
#La llamaremos collection_2015 para que no se arroje error por parte de R, ya que es un "Environment" 

#En la misma base de datos y la misma colecci�n alojamos los nuevos datos
collection_2015 = mongo(collection = "match", db = "match_test",url="mongodb+srv://CaroBer:********@clusterbedu.v2ujr.mongodb.net/test?authSource=admin&replicaSet=atlas-b1gsnf-shard-0&connectTimeoutMS=600000&socketTimeoutMS=6000000&readPreference=primary&appname=MongoDB%20Compass&ssl=true")
collection_2015$insert(match.2015)  

collection_2015$find() #Podemos visualizar ahora todo el fichero 

#Realizando la consulta con la sintaxis de MongoDB (llave-valor)
query<- collection_2015$find(query = '{"Date":"2015-12-20","HomeTeam" : "Real Madrid"}')
query

#fue goleada por parte del Real Madrid 10-2.
paste("Fue goleada por parte del Real Madrid quien anot�", query$FTHG ,"goles contra el", query$AwayTeam)

#Por �ltimo, no olvides cerrar la conexi�n con la BDD
rm(collection_2015) #remove objects from a specified environment
rm(match_collection)

#remove and rm can be used to remove objects. These can be specified successively as character strings, or in the character vector list, or through a combination of both.
#All objects thus specified will be removed.