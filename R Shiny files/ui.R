# INSERT Libraries HERE
Sys.setenv(TZ='America/Cancun')
rm(list = ls())

if (!require(RColorBrewer)) install.packages('RColorBrewer') ;library(RColorBrewer)
if (!require(highcharter)) install.packages('highcharter') ;library(highcharter)
if (!require(data.table)) install.packages('data.table') ;library(data.table)
if (!require(visNetwork)) install.packages('visNetwork') ;library(visNetwork)
if (!require(tidyverse)) install.packages('tidyverse') ;library(tidyverse)
if (!require(arulesViz)) install.packages('arulesViz') ;library(arulesViz)
if (!require(forecast)) install.packages('forecast') ;library(forecast)
if (!require(shinyBS)) install.packages('shinyBS') ;library(shinyBS)
if (!require(shinyjs)) install.packages('shinyjs') ;library(shinyjs)
if (!require(arules)) install.packages('arules') ;library(arules)
if (!require(plotly)) install.packages('plotly') ;library(plotly)
if (!require(tidyr)) install.packages('tidyr') ;library(tidyr)
if (!require(useful)) install.packages('useful') ;library(useful)
if (!require(cluster)) install.packages('cluster') ;library(cluster)
if (!require(aod)) install.packages('aod') ;library(aod)
if (!require(factoextra)) install.packages('factoextra') ;library(factoextra)
if (!require(xts)) install.packages('xts') ;library(xts)
if (!require(ggplot2)) install.packages('ggplot2') ;library(ggplot2)
if (!require(plyr)) install.packages('plyr') ;library(plyr)
if (!require(zoo)) install.packages('zoo') ;library(zoo)
if (!require(dplyr)) install.packages('dplyr') ;library(dplyr)
if (!require(lubridate)) install.packages('lubridate') ;library(lubridate)
if (!require(reshape2)) install.packages('reshape2') ;library(reshape2)
if (!require(shiny)) install.packages('shiny') ;library(shiny)
if (!require(shinythemes)) install.packages('shinythemes') ;library(shinythemes)
if (!require(shinyWidgets)) install.packages('shinyWidgets') ;library(shinyWidgets)
if (!require(shinydashboard)) install.packages('shinydashboard') ;library(shinydashboard)
if (!require(tseries)) install.packages('tseries') ;library('tseries')
if (!require(gridExtra)) install.packages('gridExtra') ;library('gridExtra')

nums <- c(5,10,15,20,25,30)

ui <- shinyUI(
  tagList(
    tags$head(
      tags$style(HTML(
        "html {
             position: relative;
             min-height: 100%;
             background-color: #ECF0F1;
           }
           body {
             margin-bottom: 10px; /* Margin bottom by footer height */
           }
           .footer {
             position: relative;
             bottom: 0;
             width: 100%;
             left: 0;
             height: 80px; 
             background-color: #2C3D50;
             color: #ffffff;
             text-align: center;
           }
           
          #sidebar {
            margin: -20px 0 240px 0;
            background-color: #ECF0F1;
          }
          
          .mainpanel {
            background-color: #ECF0F1;
            padding-left: 0px;
            padding-right: 0px;
          }
          
          "))),
    navbarPage(id="navbar",
               tags$script(HTML("var header = $('.navbar > .container-fluid');
      header.append('<div style=\"float:right\"><a href=\"https://www.unicaribe.mx/posgrados?p=analitica\"><img src=\"uni.png\" alt=\"alt\" style=\"float:right;width:45px;height:58px;padding-top:5px;\"> </a>`</div>');
                               console.log(header)")
               ),
               title = div("", img(src = "eduAnaLytiX.png", id = "logo", height = "58px",width = "80px",style = "position: relative; margin:-19px -25px 0px 3px; display:left-align;")), 
               theme = shinytheme("flatly"),
               tabPanel(class="mainpanel",
                        "Inicio",
                        sidebarLayout(
                          mainPanel(
                          ),
                          sidebarPanel(id="sidebar", class="sidebar",
                                       h2("Selecciona el modelo analítco que quieres explorar:",style = "padding-bottom: 20px; text-align: center;font-family: 'Trebuchet MS', sans-serif;"),
                                       img(id = "basket", src="basket.png", title = "Análisis del carrito de compra", style = "width: 15%; cursor:pointer; margin: 30px 40px 70px 10px;"),
                                       img(id = "forecast", src="forecast.png", title = "Proyección de ventas", style = "width: 15%; cursor:pointer; margin: 30px 40px 70px 0;"),
                                       img(id = "customers", src="customers.png", title = "Segmentación de Clientes", style = "width: 15%; cursor:pointer; margin: 30px 40px 70px 0;"),
                                       img(id = "price", src="pricing.png", title = "Optimización de precios", style = "width: 15%; cursor:pointer; margin: 30px 40px 70px 0;"),
                                       width="100%",align = "center"
                          )
                        )
               ),
               navbarMenu("Modelos",
                          tabPanel("Carrito",
                                   titlePanel(h1("Modelo del análisis del carrito de compra (Basket Market)", style = "padding-bottom: 20px")),
                                   sidebarLayout(
                                     sidebarPanel(
                                       fileInput("file", label = "Carga un archivo:", accept = c(".csv", ".tsv")),
                                       uiOutput("rules"),
                                       uiOutput("sliderS"),
                                       uiOutput("sliderC"),
                                       bsTooltip(id = "SliderS", title = "Soporte"),
                                       bsTooltip(id = "SliderC", title = "Confianza")
                                     ),
                                     mainPanel(
                                       uiOutput("mb")
                                     )
                                   )
                          ),
                          tabPanel("Forecasting",
                                   titlePanel(h1("Modelos ETS, ARIMA y SARIMA para Proyección (Forecasting)", style = "padding-bottom: 20px")),
                                   sidebarLayout(
                                     sidebarPanel(
                                       fileInput("file_fc", label = "Carga un archivo:", accept = c(".csv", ".tsv")),
                                       uiOutput("periodo"),
                                       uiOutput("varsPred"),
                                       uiOutput("observ"),
                                       uiOutput("logbut")
                                     ),
                                     mainPanel(
                                       uiOutput("fc")
                                     )
                                   )
                          ),
                          tabPanel("Customers",
                                   titlePanel(h1("Modelo de Segmentación de Clientes (Clustering)", style = "padding-bottom: 20px")),
                                   sidebarLayout(
                                     sidebarPanel(
                                       fileInput("file_cs", label = "Carga un archivo:", accept = c(".csv", ".tsv")),
                                       uiOutput("numK"),
                                       bsTooltip(id = "numK", title = "Clusters"),
                                       uiOutput("varsCheck"),
                                       bsTooltip(id = "varsCheck", title = "Variables")
                                     ),
                                     
                                     mainPanel(
                                       uiOutput("cs")
                                     )
                                   )
                          ),
                          tabPanel("Pricing",
                                   titlePanel(h3("Modelo de optimización del precio (Pricing) para maximizar ingresos en función de la demanda", style = "padding-bottom: 20px")),
                                   sidebarLayout(
                                     sidebarPanel(
                                       fileInput("file_op", label = "Carga un archivo:", accept = c(".csv", ".tsv")),
                                       uiOutput("producto"),
                                       uiOutput("mode")
                                     ),
                                     mainPanel(
                                       uiOutput("op")
                                     )
                                   )
                          )
               ),
               tabPanel(
                 "Acerca de",
                 sidebarLayout(
                   mainPanel(
                     div(img(src="logo_eduanalytix.png",width="25%"),align = "center"), br(),
                     h3("Aplicación didáctica para la exploración de modelos usados en analítica de negocios"),
                     h5("Esta aplicación se desarrolló con el fin de brindar apoyo académico para alumnos que cursan la licenciatura de Ingeniería en Datos, las maestría de Analitica e Inteligencia de Negocios y la maestría de E-commerce."),
                     h5("Estos son los modelos que se desarrollaron usando R como lenguaje de programación y Shiny que es su interfaz gráfica para la creación de las siguientes aplicaciones:"),
                     tags$li("Análisis del carrito de compra (Apriori)"),
                     tags$li("Proyección de Ventas (Forecasting)"),
                     tags$li("Segmentación de Clientes (Clustering)"),
                     tags$li("Optimización de Precios (Pricing)"),br(),
                     a(href="https://github.com/KennethLeyva/eduAnaLytix", "Haz clic aquí para descargar el repositorio de datos usado para los modelos."),
                     br(),br()
                   ),
                   sidebarPanel(
                     img(src="kentooncir.png",width="60%"),
                     h5("Kenneth Leyva Quijano",style = "padding-bottom: 20px;text-align: center;"),align = "center"
                   )
                 )
               )
               ,tags$footer(class = "footer",
                            HTML("<footer><br>
                       <div>Aplicación didáctica para la exploración de modelos usados en analítica de negocios
                        <br><a href='https://www.unicaribe.mx'>Universidad del Caribe</a>
                        </div>
                    </footer>")
               )
               ,useShinyjs()
    )
  )
)