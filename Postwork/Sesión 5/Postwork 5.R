#Postwork 5. Regresión lineal y clasificación. 

#Cargando las librerias necesiarias
suppressWarnings(suppressMessages(library(dplyr)))


#A partir del conjunto de datos de soccer de la liga española de las temporadas 2017/2018, 2018/2019 y 2019/2020, 
#crea el data frame SmallData, que contenga las columnas date, home.team, home.score, away.team y away.score; 
#esto lo puede hacer con ayuda de la función select del paquete dplyr. Luego establece un directorio de trabajo 
#y con ayuda de la función write.csv guarda el data frame como un archivo csv con nombre soccer.csv. 
#Puedes colocar como argumento row.names = FALSE en write.csv.


#Obtenemos los url
fut.17_18 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
fut.18_19 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
fut.19_20 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"

#Importamos a R
Fut.1718 <- read.csv(fut.17_18)
Fut.1819 <- read.csv(fut.18_19)
Fut.1920 <- read.csv(fut.19_20)

#Creamos una lista que contenga los archivos y con lapply seleccionamos las columnas deseadas
flist <- list(Fut.1718,Fut.1819,Fut.1920)
list.f<- lapply(flist, select, Date, HomeTeam:FTR)

#Cambiamos el tipo de dato Date para que R lo tome en formato de fecha 
list.f[[1]] <- mutate(list.f[[1]], Date = as.Date(Date, "%d/%m/%y"))
list.f[[2]] <- mutate(list.f[[2]], Date = as.Date(Date, "%d/%m/%y"))
list.f[[3]] <- mutate(list.f[[3]], Date = as.Date(Date, "%d/%m/%y"))

#Combinamos los dataframe por renglones en un solo dataframe 
df.f <- do.call(rbind, list.f)
str(df.f)
#-------

#Creamos el DF "SmallData" con las columnas y nombres deseados 
SmallData <- select(df.f, date =Date, home.team = HomeTeam, home.score = FTHG, away.team  =AwayTeam, away.score = FTAG )

#Establecemos el directorio de trabajo para guardar el csv
setwd("C:/Users/Carolina/Desktop/Postwork/Sesión 5")
write.csv(SmallData, file = "soccer.csv", row.names = FALSE)

#Con la función create.fbRanks.dataframes del paquete fbRanks importe el archivo soccer.csv a R y al mismo tiempo 
#asignelo a una variable llamada listasoccer. Se creará una lista con los elementos scores y teams que son 
#data frames listos para la función rank.teams. Asigna estos data frames a variables llamadas anotaciones y equipos.

install.packages("fbRanks")
suppressWarnings(suppressMessages(library(fbRanks)))
?create.fbRanks.dataframes  #Helper function. Reads in .csv files to create the scores, team.resolver, 
#and teams data.frames.

listasoccer <- create.fbRanks.dataframes("soccer.csv")
names(listasoccer) ; str(listasoccer) #Para conocer la estructura y los títulos 

anotaciones <- listasoccer$scores #dataframe de la columna scores
equipos <- listasoccer$teams #dataframe de la columna teams   

#Con ayuda de la función unique crea un vector de fechas (fecha) que no se repitan y que correspondan a las fechas
#en las que se jugaron partidos. Crea una variable llamada n que contenga el número de fechas diferentes.
#Posteriormente, con la función rank.teams y usando como argumentos los data frames anotaciones y equipos, 
#crea un ranking de equipos usando unicamente datos desde la fecha inicial y hasta la penúltima fecha en la 
#que se jugaron partidos, estas fechas las deberá especificar en max.date y min.date. Guarda los resultados 
#con el nombre ranking.


?unique #duplicated elements/rows are removed

fecha <- unique(anotaciones$date) #fechas sin repetición 
n <- length(fecha) #núm. de fechas distintas 

?rank.teams #Creates ranks using a dataframe of match records.

#ranking de equipos con los datos desde la fecha inicial (fecha[1]) - penúltima fecha (fecha[n-1])

ranking <- rank.teams(scores =  anotaciones, teams = equipos, max.date = fecha[n-1], min.date = fecha[1])

#Finalmente estima las probabilidades de los eventos, el equipo de casa gana, el equipo visitante gana o el 
#resultado es un empate para los partidos que se jugaron en la última fecha del vector de fechas fecha.
#Esto lo puedes hacer con ayuda de la función predict y usando como argumentos ranking y fecha[n] que deberá 
#especificar en date.

?predict #predict is a generic function for predictions from the results of various model fitting functions.

estimacion <- predict(ranking, date = fecha[n])
