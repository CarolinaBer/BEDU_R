# Postwork sesión 8. Dashboard

#Crea un dashboard donde se muestren los resultados con 4 pestañas:
    
# 1. Gráficas de barras, donde en el eje  x -goles de local y visitante con un menu de selección, 
#con una geometria de tipo barras además de un facet_wrap con el equipo visitante

# 2. Agregar las imágenes de las gráficas del postwork 3

# 3. En otra pestaña coloca el data table del fichero match.data.csv

# 4. Agrega las imágenes de las gráficas de los factores de ganancia mínimo y máximo


#...Sol....

#Cargamos las librerías
suppressWarnings(suppressMessages(library(fbRanks)))
suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(shiny)))
suppressWarnings(suppressMessages(library(shinydashboard)))
suppressWarnings(suppressMessages(library(shinythemes)))

#utilizamos el directorio de trabajo en donde están almacenadas las imágenes 
# setwd("C:/Users/Carolina/Desktop/Postwork/Sesión 8") 


#......... ui ........

# Shinydasboard UI
ui <- fluidPage(
    dashboardPage(
        dashboardHeader(title = "Postwork8 Dashboard"),
        #Sidebar content
        dashboardSidebar(
            sidebarMenu(
                menuItem("Goles local y visitante", tabName = "Dashboard", icon = icon("dashboard")),
                menuItem("Proba marginal y conjunta", tabName = "Prob", icon = icon("area-chart")),
                menuItem("Data Table", tabName = "Datatable", icon = icon("table")),
                menuItem("Gráfica de Factores de ganancia", tabName = "Factores", icon =icon("file-picture-o") )
            )
        ),
        #Body content      
        dashboardBody(
            tabItems(
                #Primer pestaña- Gráfico de barras
                tabItem(tabName = "Dashboard",
                        fluidRow(
                            titlePanel("Gráfico de barras de goles local-visitante"), 
                            selectInput("x", "Seleccione el valor de X",
                                        choices = c("home.score", "away.score")),
                            plotOutput("plot1", height = 450, width = 750)
                        )
                ),
                #Segunda Pestaña - Probas postwork 3 
                tabItem(tabName = "Prob", 
                        fluidRow(
                            titlePanel(h3("Gráficas de probabilidades de goles")),
                            selectInput("hist", "Seleccione la gráfica deseada: ", 
                                        
                            choices = c("Probabilidad marginal goles local", "Probabilidad marginal goles local mejorada","Probabilidad marginal goles visitante","Probabilidad marginal goles visitante mejorada","Heatmap de probabilidades conjuntas")),
                            h6("Nota:En este caso mejorada hace referencia a que el gráfico de barras se colorea
                              según el gradiente de probabilidad"),
                            imageOutput("image1")
                            #h3("Gráfica de probabilidades marginales goles local"),
                            #img(src = "local1.png") ,
                            #h2("Gráfica de probabilidades marginales goles visitante"),
                            #img(src = "vis1.png") ,
                            #h2("Gráfica de probabilidades conjuntas"),
                            #img(src = "heatmap1.png")
                            
                        )
                ),
                #Tercer pestaña. data table del archivo match.data.csv
                tabItem(tabName = "Datatable",
                        fluidRow(        
                            titlePanel(h3("Data Table del archivo match.data.csv")),
                            dataTableOutput ("Match_Data")
                        )
                ), 
                #Cuarta pestaña:gráfica de factores del archivo momios.R
                tabItem(tabName = "Factores",
                        fluidRow(
                            titlePanel(h3("Factores de ganacia del archivo momios.R")),
                            
                            radioButtons("graf", "Seleccione el gráfico deseado:",
                                         c("Factores de ganancia máximo", "Factores de ganancia promedio")),
                           imageOutput("image2", height = 50)
                           
                            
                        )
                )
            )
        )
    )
)

######################################################################

### ........... server.r .....................


server <- function(input, output) {
    output$plot1 <- renderPlot({
        data <-  read.csv("match.data.csv", header = T)
        
        data <- mutate(data, FTR = ifelse(home.score > away.score, "H", ifelse(home.score < away.score, "A", "D")))
        x <- data[,input$x]
        #FTR es el resultado si gana local H, visitante A o si es empate D
        data %>% ggplot(aes(x, fill = FTR)) + 
            geom_bar() + 
            facet_wrap("away.team") +
            labs(x =input$x, y = "Goles") + 
            ylim(0,50)
    }) #con facet_wrap colocamos una barra sobre la otra 
    
    #Data Table match.data.csv, el cual ya está dentro de nuesto dir de trabajo 
    output$Match_Data <- renderDataTable( {read.csv("match.data.csv")}, 
                                          options = list(aLengthMenu = c(10,25,50),
                                                         iDisplayLength = 10)
    )
    
    #Imágenes de factores de momios
    outfile <- tempfile(fileext = ".png")
    output$image2 <- renderImage({
        if (is.null(input$graf))
            return(NULL)
        
        if (input$graf == "Factores de ganancia máximo") {
            return(list(
                src = "www/momios_max.png",
                contentType = "image/png",
                alt = "Face"
            ))
        } else if (input$graf == "Factores de ganancia promedio") {
            return(list(
                src = "www/momios.prom.png",
                filetype = "image/png",
                alt = "This is a chainring"
            ))
        }
        
    }, deleteFile = FALSE)
    
    
    #imágenes de las gráficas de probabilidad del postwork 3
    output$image1 <- renderImage({
        if (is.null(input$hist))
            return(NULL)
        
        if (input$hist == "Probabilidad marginal goles local") {
            return(list(
                src = "www/local1.png",
                contentType = "image/png",
                alt = "Face"
            ))
        } else if (input$hist == "Probabilidad marginal goles local mejorada") {
            return(list(
                src = "www/local_sof.png",
                filetype = "image/png",
                alt = "This is a chainring"
            ))
        }
        else if (input$hist == "Probabilidad marginal goles visitante") {
            return(list(
                src = "www/vis1.png",
                filetype = "image/png",
                alt = "This is a chainring"
            ))
        }
        
        else if (input$hist == "Probabilidad marginal goles visitante mejorada") {
            return(list(
                src = "www/vis_sof.png",
                filetype = "image/png",
                alt = "This is a chainring"
            ))
        }
        
        else if (input$hist == "Heatmap de probabilidades conjuntas") {
            return(list(
                src = "www/heatmap1.png",
                filetype = "image/png",
                alt = "This is a chainring"
            ))
        }
    }, deleteFile = FALSE)
    
    
    
}

shinyApp(ui, server)





