server <- function(input, output, session){
  
  
  # Insert the slider for the support after load the data
  output$sliderS <- renderUI({
    req(input$file)
    sliderInput("supp","Soporte", min = 0, max = 0.2, value = 0.05)
  })
  
  # Insert the slider for the confidence after load the data
  output$sliderC <- renderUI({
    req(input$file)
    sliderInput("conf","Confianza", min = 0, max = 0.5, value = 0.25)
  })
  
  # Insert the select input for the rules after load the data
  output$rules <- renderUI({
    req(input$file)
    selectInput('Rules','Reglas:',nums, selected = 20, multiple = FALSE, selectize = TRUE)
  })
  
  output$mode <- renderUI({
    req(input$file_op)
    radioButtons("mod", "Tipo de Modelo:",
                 c("Lineal" = "line",
                   "Logit" = "logit",
                   "Probit" = "probit",
                   "Elasticidad Constante" = "elast"))
  }) 
  
  output$periodo <- renderUI({
    req(input$file_fc)
    radioButtons("period", "Periodo:",
                 c("Diario" = "365",
                   "Semanal" = "52",
                   "Mensual" = "12"))
  })
  
  output$observ <- renderUI({
    req(input$file_fc)
    numericInput("obsfc", "No. Periodos a proyectar", 5, min = 10, max = 360)
  })
  
  output$logbut <- renderUI({
    req(input$file_fc)
    materialSwitch(inputId = "log", label = "Ajuste logarítmico", status = "primary")
  })
  
  output$diff <- renderUI({
    req(input$file_fc)
    materialSwitch(inputId = "opt", label = "Aplicar Diferenciación", status = "primary")
  })
  
  output$sliderDiff <- renderUI({
    req(input$opt)
    sliderInput("diff","Diferencias", min = 1, max = 5, value = 1)
  })
  
  output$arimaon <- renderUI({
    req(input$file_fc)
    materialSwitch(inputId = "arima", label = "Aplicar ARIMA", status = "primary")
  })
  
  output$seasonal <- renderUI({
    req(input$arima)
    materialSwitch(inputId = "season", label = "Aplicar SARIMA", status = "primary")
  })
  
  output$ariman <- renderUI({
    req(input$arima)
    materialSwitch(inputId = "ari", label = "Ajustar ARIMA manualmete", status = "primary")
  })
  
  output$sariman <- renderUI({
    req(input$season)
    materialSwitch(inputId = "sari", label = "Ajustar SARIMA manualmete", status = "primary")
  })
  
  output$ar <- renderUI({
    req(input$ari)
    numericInput("ar1", "AR", 0, min = 0, max = 10)
  })
  
  output$i <- renderUI({
    req(input$ari)
    numericInput("i2", "I", 0, min = 0, max = 10)
  })
  
  output$ma <- renderUI({
    req(input$ari)
    numericInput("ma3", "MA", 0, min = 0, max = 10)
  })
  
  output$visuals <- renderUI({
    req(input$arima)
    radioGroupButtons(
      inputId = "changev",
      # label = "Selecciona:",
      choices = c(
        `<i class='fa fa-line-chart'>Forecast</i>` = "fore",
        `<i class='fa fa-bar-chart'>Residuales</i>` = "resi"
      ),
      checkIcon = list(yes = tags$i(class = "fa fa-circle", style = "color: steelblue"),
                       no = tags$i(class = "fa fa-circle-o", style = "color: steelblue")),
      justified = TRUE,
      selected = "fore"
    )
  })
  
  # Add the definitions as mouse hover display on the sliders
  addTooltip(session=session,id="sliderS",title="Mide la popularidad de un ítem, es decir, la probabilidad de que un cliente lleve el producto A en el carrito de compra. Por ejemplo: Si el café es muy popular y tiene soporte de 0.74, el 74% de los clientes compró café.")
  addTooltip(session=session,id="sliderC",title="Mide la posibilidad de un cliente lleve un producto como consecuencia de llevar otro producto, es decir, es la probabilidad de llevar el producto B si se decidió llevar el producto A. Por ejemplo: si el azúcar tiene confianza de 0.68 respecto del café, el 68% de los clientes que compró café, también decidió comprar azúcar.")
  addTooltip(session=session,id="basket",title="Análisis del carrito de compra")
  addTooltip(session=session,id="forecast",title="Proyección de Ventas")
  addTooltip(session=session,id="customers",title="Segmentación de Clientes")
  addTooltip(session=session,id="price",title="Optimización de Precios")
  
  ################# Add the content output here #####################
  
  ###### Market Basket section #########
  # Read the data from the .csv file
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()} 
    dataSet <- read.csv(file1$datapath, header = TRUE)
    dataSet
  })
  
  # Insert the text definition of the model
  output$definition <- renderText({
    HTML(paste("<b>El análisis de las cestas de mercado es una de las aplicaciones clave del aprendizaje automático, 
    porque ayuda por ejemplo a conocer el  comportamiento de compra de los clientes. <br/><br/>Para entender como 
    utilizar Apriori, primero tenemos que entender cómo funciona. Para Apriori: Todos los subconjuntos 
    de un conjunto de elementos frecuente deben ser frecuentes (apriori propertry).<br/><br/>Si un conjunto de 
    elementos es poco frecuente, todos sus superconjuntos serán poco frecuentes. Tres conceptos muy 
    importantes para Apriori son: Soporte, Confianza y Elevación.</b>"))
  })
  
  
  # Insert the sample data into a table
  output$sample <- renderTable({
    if(is.null(data())){return ()}
    head(data(),n = 15) 
  })
  
  # Insert a histogram of the market basket size
  output$histogram <- renderPlot({
    x <- data()
    datos_matriz <- x %>%
      as.data.frame() %>%
      mutate(valor = 1) %>%
      spread(key = item, value = valor, fill = 0) %>%
      column_to_rownames(var = "id_compra") %>%
      as.matrix()
    
    transacciones <- as(datos_matriz, Class = "transactions")
    tamanyos <- size(transacciones)
    
    if (!is.numeric(tamanyos)) {
      createAlert(session, "alarm", alertId = "Peligro", 
                  title = "Error: ",
                  content = "El histograma toma solo valores numéricos", 
                  style = "danger", dismiss = TRUE, append = TRUE)
    }
    if (is.numeric(tamanyos)) {
      closeAlert(session, "Peligro")
    }
    
    data.frame(tamanyos) %>%
      ggplot(aes(x = tamanyos, fill=..x..)) +
      geom_histogram() +
      scale_fill_gradient(low='blue', high='red') +
      labs(title = "Cantidad de carritos de compra por tamaño",
           x = "Tamaño",
           y = "Frecuencia") +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#000000", size = 16, face = "bold"),
        plot.subtitle = element_text(size = 10, face = "bold"),
        plot.caption = element_text(face = "italic")
      )
    
  })
  
  # Insert a plot of the TOP sales products
  output$topN <- renderPlot({
    x    <- data()
    
    datos_matriz <- x %>%
      as.data.frame %>%
      mutate(valor = 1) %>%
      spread(key = item, value = valor, fill = 0) %>%
      column_to_rownames(var = "id_compra") %>%
      as.matrix()
    
    transactions <- as(datos_matriz, Class = "transactions")
    
    arules::itemFrequencyPlot(transactions,
                              topN=20,
                              col=brewer.pal(8,'Set2'),
                              main='Frecuencia por producto',
                              type="relative",
                              ylab="Frequencia relativa") 
    
  })
  
  output$model <- renderTable({
    if(is.null(data())){return ()}
    datos_matriz <- data() %>%
      as.data.frame() %>%
      mutate(valor = 1) %>%
      spread(key = item, value = valor, fill = 0) %>%
      column_to_rownames(var = "id_compra") %>%
      as.matrix()
    
    transacciones <- as(datos_matriz, Class = "transactions")
    tamanyos <- size(transacciones)
    
    soporte <- input$supp
    confianza <- input$conf
    
    # Implementación del algoritmo Apriori
    rules <- apriori(transacciones, parameter = list(support = soporte, confidence = confianza))
    
    # Remueve las reglas redundantes   
    rules <- rules[!is.redundant(rules)]
    rules_dt <- data.table( lhs = labels( lhs(rules) ), 
                            rhs = labels( rhs(rules) ), 
                            quality(rules) )[ order(-lift), ]
    head(rules_dt,15)
  })
  
  # Insert the Network plot with the result of the model apriori
  output$network <- renderVisNetwork({
    x <- data()
    soporte <- input$supp
    confianza <- input$conf
    reglas <- as.numeric(input$Rules)
    
    datos_matriz <- x %>%
      as.data.frame %>%
      mutate(valor = 1) %>%
      spread(key = item, value = valor, fill = 0) %>%
      column_to_rownames(var = "id_compra") %>%
      as.matrix()
    
    transacciones <- as(datos_matriz, Class = "transactions")
    rules <- apriori(transacciones, parameter = list(support = soporte, confidence = confianza))
    
    subrules2 <- head(sort(rules, by="confidence"),reglas)
    ig <- plot(subrules2, method = "graph",  engine = "htmlwidget")
    
    ig %>%
      visNodes(size = 10) %>%
      #visLegend() %>%
      visEdges(smooth = FALSE) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visInteraction(navigationButtons = TRUE) %>%
      visEdges(arrows = 'from') %>%
      visPhysics(
        solver = "barnesHut",
        maxVelocity = 35,
        forceAtlas2Based = list(gravitationalConstant = -6000)
      )
    
  })
  
  # Add here the navigation tabls of the sub-page
  output$mb <- renderUI({
    if(is.null(data()))
      h4("Carga un archivo tipo CSV, la columna del identificador deberá llamarse 'id_compra' y la columna de producto 'item'.")
    else
      tabsetPanel(tabPanel("Definición", htmlOutput("definition")),
                  tabPanel("Datos", tableOutput("sample")),
                  tabPanel("Tamaño de carritos", 
                           bsAlert("alarm"),
                           plotOutput("histogram")),
                  tabPanel("Top Productos comprados", 
                           plotOutput("topN")),
                  tabPanel("Resultado del Modelo", tableOutput("model")),
                  tabPanel("Grafico de nodos", visNetworkOutput("network",height="100vh"))
      )
  })
  
  ###### Pricing optimization section #########
  # Read the data from the .csv file
  data_op <- reactive({
    file1 <- input$file_op
    if(is.null(file1)){return()} 
    dataSetOp <- read.csv(file1$datapath, header = TRUE, stringsAsFactors = FALSE)
    dataSetOp
  })
  
  # Insert the Selection product input
  output$producto <- renderUI({
    x <- data_op()
    req(input$file_op)
    selectInput("prod", "Seleccione un Producto:", choices = unique(x$Producto))
  })
  
  # Insert the text definition of the model
  output$definition_op <- renderText({
    HTML(paste("<b>La optimización de precios es un proceso que consiste en fijar el precio adecuado de un producto o servicio para maximizar los ingresos y los beneficios. ¿Cómo asegurarse de que no es demasiado caro para que los clientes no lo compren, pero no demasiado barato para perder dinero? Así se hace.<br/><br/>
  Podemos decir que la optimización de precios consiste en averiguar el precio ideal de un producto o servicio, y hay que tener en cuenta muchos factores. Por ejemplo, hay que tener en cuenta cuánto cuesta producir el producto o prestar el servicio, cuánto cobran los competidores y cuánto están dispuestos a pagar los clientes.<br/><br/>
  Para facilitar este proceso, las empresas pueden utilizar herramientas que les ayuden a analizar la demanda del mercado y el comportamiento de los clientes para averiguar cuál es el precio óptimo que maximizará sus beneficios. Gracias a estas herramientas, podemos eliminar algunas conjeturas sobre la fijación de precios y tomar decisiones más informadas.<br/><br/>
  Por supuesto, conviene recordar que la optimización de precios no es algo que se haga una sola vez. Los precios y las condiciones del mercado pueden cambiar con el tiempo, por lo que tendrás que estar atento y ajustar tu estrategia de precios según sea necesario. 
  </b>"))
  })
  
  # Insert the sample data into a table
  output$sample_op <- renderTable({
    if(is.null(data_op())){return ()}
    x <- data_op()
    x <- x %>% filter(Producto == input$prod)
    head(x,n = 15) 
  })
  
  
  
  output$price_trend <- renderHighchart({
    x <- data_op()
    x <- x %>% filter(Producto == input$prod)
    x$Periodo <- as.Date(x$Periodo, format = '%d/%m/%Y')
    hc <- highchart()%>%
      hc_xAxis(type = "datetime", labels = list(format = '{value:%d/%m/%Y}')) %>%
      hc_yAxis_multiples(list(title = list(text = "Demanda"),labels=list(format = '{value}'),min=0,
                              max=100,showFirstLabel = TRUE,showLastLabel=TRUE,opposite = FALSE),
                         list(title = list(text = "Precio"),min=0,max = max(x$Precio),
                              labels = list(format = "${value}"),showLastLabel = FALSE, opposite = TRUE)) %>%
      hc_plotOptions(column = list(stacking = "normal")) %>%
      hc_add_series(x,type="line",hcaes(x=Periodo,y=Precio),name = 'Precio',yAxis=1) %>%
      hc_add_series(x,type="line",hcaes(x=Periodo,y=Cantidad), name = 'Demanda') %>%
      hc_add_theme(hc_theme_538())
    
    hc
  })
  
  output$model_detail <- renderPrint({
    x <- data_op()
    x <- x %>% filter(Producto == input$prod)
    maxPrecio <- max(x$Precio)+10
    # Generación del modelo
    # modelo_lineal =   lm(x$Cantidad ~ x$Precio)
    modelo <- if(input$mod == 'line'){
      lm(Cantidad ~ Precio, data = x) # linear model for demanda
    } else {
      if(input$mod == 'probit'){
        glm(as.factor(Cantidad) ~ Precio, data = x, family = binomial(link = "probit"))
      } else {
        if(input$mod == 'logit'){
          glm(as.factor(Cantidad) ~ Precio, data = x, family = binomial(link = "logit"))
        } else {
          if(input$mod == 'elast'){
            lm(log(Cantidad) ~ Precio, data = x)
          } else {
            0
          }
        }
      }
    }
    
    summary(modelo)
    
  })
  
  output$price_model <- renderPlot({
    x <- data_op()
    x <- x %>% filter(Producto == input$prod)
    maxPrecio <- max(x$Precio)+10
    # Generación del modelo
    # modelo_lineal =   lm(x$Cantidad ~ x$Precio)
    modelo <- if(input$mod == 'line'){
      lm(Cantidad ~ Precio, data = x) # linear model for demanda
    } else {
      if(input$mod == 'probit'){
        glm(as.factor(Cantidad) ~ Precio, data = x, family = binomial(link = "probit"))
      } else {
        if(input$mod == 'logit'){
          glm(as.factor(Cantidad) ~ Precio, data = x, family = binomial(link = "logit"))
        } else {
          if(input$mod == 'elast'){
            lm(log(Cantidad) ~ Precio, data = x)
          } else {
            0
          }
        }
      }
    }
    
    summary(modelo)
    # Estimación de parametros
    beta = modelo$coefficients[1]
    alpha = modelo$coefficients[2]  
    # Maximización del beneficio (Derivada del  beneficio con respecto al precio)
    p.ingresos = -beta/(2*alpha) 
    # Representación de como evolucionan los ingresos con respecto al precio
    modelado_ingresos = function(p) p*(alpha*p + beta)
    # Estimación del punto donde se maximizan los ingresos para el precio óptimo calculado
    max_ingresos = modelado_ingresos(p.ingresos) 
    df_ingresos = data.frame(p.ingresos, max_ingresos)
    
    # Visualizar el modelo
    ggplot(data = data.frame(precio = 0)) +
      stat_function(fun = modelado_ingresos, mapping = aes(x = precio, color = 'Modelado del precio')) +
      geom_segment(aes(x = p.ingresos, y = max_ingresos, xend = p.ingresos, yend = 0), data = df_ingresos) +
      geom_point(aes(x = p.ingresos, y = max_ingresos, xend = p.ingresos, yend = 0), data = df_ingresos, color = 'darkblue', position = "dodge") +
      scale_x_continuous(limits = c(0,p.ingresos * 2)) +
      labs(title = 'Optimizacion del precio en función de la demanda', colour = '') +
      ylab('Resultados') +
      theme_classic() +
      geom_text(
        aes(label = paste("$", round(p.ingresos, 2), sep = ""), x = p.ingresos, y = (max_ingresos * 1.03)),
        position = position_dodge(0.9),
        vjust = 0
      )
  })
  
  output$op <- renderUI({
    if(is.null(data_op()))
      h4("Carga un archivo tipo CSV, deberá tener las columnas de Fecha o Periodo, Demanda o Cantidad y Precio.")
    else
      tabsetPanel(tabPanel("Definición", htmlOutput("definition_op")),
                  tabPanel("Datos", tableOutput("sample_op")),
                  tabPanel("Tendencia de Demanda e Ingresos",
                           highchartOutput("price_trend") 
                  ),
                  tabPanel("Optimización del Precio",
                           plotOutput("price_model", height="30vh"),verbatimTextOutput("model_detail")
                  )
      )
  })
  
  
  ###### Customer Clustering section #########
  
  # Insert the text definition of the model
  output$definition_cs <- renderText({
    HTML(paste("<b>El análisis de clientes también se puede utilizar para identificar diferentes grupos de clientes en función de su comportamiento.<br/><br/>
    La segmentación de clientes es un buen ejemplo y resultado del análisis de clientes. Al identificar subgrupos de clientes similares, puede comprender mejor 
    las poblaciones objetivo.<br/><br/>Por ejemplo, la estrategia de marketing para clientes con bajo compromiso debe ser diferente de la estrategia de marketing para clientes 
    con alto compromiso.<br/><br/>Al segmentar eficazmente la base de clientes según el nivel de participación, puede tener una comprensión más profunda de cómo se comportan y 
    reaccionan los diferentes grupos de clientes ante diferentes estrategias de marketing.<br/><br/>Esto le ayuda aún más a dirigirse mejor a ciertos subgrupos de clientes.
  </b>"))
  })
  
  
  # Read the data from the .csv file
  data_cs <- reactive({
    file2 <- input$file_cs
    if(is.null(file2)){return()} 
    dataSetCS <- read.csv(file2$datapath, header = TRUE)
    dataSetCS
  })
  
  # Insert the checkbox list of variables
  output$varsCheck <- renderUI({
    req(input$file_cs)
    x <- data_cs()
    variables <- colnames(x[,-1])
    checkboxGroupInput("VarsCheck", "Variables:",c(variables), selected = c(variables[1:4]))
  })
  
  # Insert the sample data into a table
  output$sample_cs <- renderTable({
    # if(is.null(data_cs())){return ()}
    x <- data_cs()
    xn <- colnames(x[1])
    y <- input$VarsCheck
    xy <- x %>% select(xn,y)
    head(xy,n = 15) 
    # x$Periodo <- as.Date(x$Periodo, "%Y-%m-%d")
  })
  
  output$heatmap <- renderHighchart({
    x <- data_cs()
    y <- input$VarsCheck
    xy <- x %>% select(y)
    
    hchart.cor <- function(object, ...) {
      
      df <- as.data.frame(object)
      is.num <- sapply(df, is.numeric)
      df[is.num] <- lapply(df[is.num], round, 2)
      dist <- NULL
      
      x <- y <- names(df)
      
      df <- tbl_df(cbind(x = y, df)) %>% 
        gather(y, dist, -x) %>% 
        mutate(x = as.character(x),
               y = as.character(y)) %>% 
        left_join(data_frame(x = y,
                             xid = seq(length(y)) - 1), by = "x") %>% 
        left_join(data_frame(y = y,
                             yid = seq(length(y)) - 1), by = "y")
      
      ds <- df %>% 
        select_("xid", "yid", "dist") %>% 
        list_parse2()
      
      fntltp <- JS("function(){
                  return this.series.xAxis.categories[this.point.x] + ' ~ ' +
                         this.series.yAxis.categories[this.point.y] + ': <b>' +
                         Highcharts.numberFormat(this.point.value, 2)+'</b>';
               ; }")
      cor_colr <- list( list(0, '#FF5733'),
                        list(0.5, '#F8F5F5'),
                        list(1, '#2E86C1')
      )
      highchart() %>% 
        hc_chart(type = "heatmap") %>% 
        hc_xAxis(categories = y, title = NULL) %>% 
        hc_yAxis(categories = y, title = NULL) %>% 
        hc_add_series(data = ds) %>% 
        hc_plotOptions(
          series = list(
            boderWidth = 0,
            dataLabels = list(enabled = TRUE)
          )) %>% 
        hc_tooltip(formatter = fntltp) %>% 
        hc_legend(align = "right", layout = "vertical",
                  margin = 0, verticalAlign = "top",
                  y = 25, symbolHeight = 280) %>% 
        hc_colorAxis(  stops= cor_colr,min=-1,max=1)
    }
    
    z <- cor(xy)
    hchart.cor(z)
    
  })
  
  # Insert the slider for the number of cluster
  output$numK <- renderUI({
    req(input$file_cs)
    numericInput("clust", "Número de Clusters", 4, min = 2, max = 10)
  })
  
  output$kmeans <- renderPrint({
    clusters <- input$clust
    x <- data_cs()
    y <- input$VarsCheck
    dataMKT <- x %>% select(y)
    dataMKT[is.na(dataMKT)] = 0
    dataMKTs <- scale(dataMKT)
    set.seed(1004)
    csK <- kmeans(x=dataMKTs, centers = clusters)
    csK
  })
  
  output$clusters <- renderPlot({
    clusters <- input$clust
    x <- data_cs()
    y <- input$VarsCheck
    dataMKT <- x %>% select(y)
    dataMKT[is.na(dataMKT)] = 0
    dataMKTs <- scale(dataMKT)
    set.seed(1004)
    csK <- kmeans(x=dataMKTs, centers = clusters)
    fviz_cluster(csK, data = dataMKTs,
                 # palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
                 geom = "point",
                 ellipse.type = "convex", 
                 ggtheme = theme_minimal()
    )
  })
  
  output$elbow <- renderPlot({
    x <- data_cs()
    y <- input$VarsCheck
    dataMKT <- x %>% select(y)
    dataMKT[is.na(dataMKT)] = 0
    set.seed(1004)
    K_data <- dataMKT[, sapply(dataMKT, is.numeric)]
    scale_data <- as.data.frame(scale(K_data))
    set.seed(123)
    k.max <- 15
    wss <- sapply(1:k.max, 
                  function(k){kmeans(scale_data, k, nstart=50,
                                     iter.max = 15)$tot.withinss})
    plot(1:k.max, wss,
         type="b", pch = 19, frame = FALSE, 
         xlab="Número de Clusters",
         ylab="Suma total de cuadrados dentro de los grupos")
  })
  
  output$silhoutte <- renderPlot({
    clusters <- input$clust
    x <- data_cs()
    y <- input$VarsCheck
    dataMKT <- x %>% select(y)
    dataMKT[is.na(dataMKT)] = 0
    dataMKTs <- scale(dataMKT)
    c6 <- c("tomato", "forest green", "dark blue", "purple2", "goldenrod4", "gray20")
    op <- par(mfrow= c(3,2), oma= c(0,0, 3, 0),
              mgp= c(1.6,.8,0), mar= .1+c(4,2,2,2))
    for(k in 2:clusters)
      plot(silhouette(pam(dataMKTs, k=k)), main = paste("k = ",k), do.n.k=FALSE, col = c6[1:k])
    par(op)
  })
  
  
  output$cs <- renderUI({
    if(is.null(data_cs()))
      h4("Carga un archivo tipo CSV, deberá tener mas de 2 columnas tipo numericas que esten describiendo un comportamiento.")
    else
      tabsetPanel(
        tabPanel("Definición", htmlOutput("definition_cs")),
        tabPanel("Datos", tableOutput("sample_cs")),
        tabPanel("Gráfico del codo", plotOutput("elbow")),
        tabPanel("Métrica Silhoutte", plotOutput("silhoutte",height="100vh")),
        tabPanel("K-means model", verbatimTextOutput("kmeans")),
        tabPanel("Clusters", plotOutput("clusters",height="70vh")),
        tabPanel("Correlación de Variables", highchartOutput("heatmap"))
      )
  })
  
  ###### Forecasting section #########
  
  # Insert the text definition of the model
  output$definition_fc <- renderText({
    HTML(paste("<b>Un pronóstico o proyección es una predicción realizada mediante el estudio de datos históricos y patrones pasados. Las empresas utilizan herramientas y sistemas de software para analizar grandes cantidades de datos recopilados durante un largo período. Luego, el software predice la demanda y las tendencias futuras para ayudar a las empresas a tomar decisiones financieras, operativas y de marketing más precisas.</br></br>
    La previsión actúa como una herramienta de planificación para ayudar a las empresas a prepararse para la incertidumbre que puede ocurrir en el futuro. Ayuda a los gerentes a responder con confianza a los cambios, controlar las operaciones comerciales y tomar decisiones estratégicas que impulsen el crecimiento futuro. Por ejemplo, las empresas utilizan la previsión para hacer lo siguiente:
    </br><br><ul>
    <li>Utilizar los recursos de manera más eficiente</li>
    <li>Visualizar el rendimiento empresarial</li>
    <li>Cronometrar el lanzamiento de nuevos productos o servicios.</li>
    <li>Estimar costos recurrentes.</li>
    <li>Predecir eventos futuros como volúmenes de ventas y ganancias.</li>
    <li>Revisar las decisiones de gestión.</li>
    </ul>
  </b>"))
  })
  
  
  # Read the data from the .csv file
  data_fc <- reactive({
    file2 <- input$file_fc
    if(is.null(file2)){return()} 
    dataSetFC <- read.csv(file2$datapath, header = TRUE)
    dataSetFC
  })
  
  # Insert the radio buttons list of variables
  output$varsPred <- renderUI({
    req(input$file_fc)
    x <- data_fc()
    variables <- colnames(x[,-1])
    cols <- ncol(x)
    if(cols<3){
      disabled(textInput("varsPred", label = "Variable:", value=colnames(x[2])))
    }
    else{
      radioButtons("varsPred", "Selecciona la variable a pronosticar:",c(variables), choiceValues = c(variables))
    }
  })
  
  # Insert the sample data into a table
  output$sample_fc <- renderTable({
    x <- data_fc()
    xn <- colnames(x[1])
    y <- input$varsPred
    xy <- x %>% select(xn,y)
    head(x,n = 15)
  })
  
  # TimeSeries descomposition
  output$desc_fc <- renderPlot({
    x <- data_fc()
    xn <- colnames(x[1])
    y <- input$varsPred
    z <- as.numeric(input$period)
    if(nchar(x[1,1])==7){
      x[,1] <- as.Date.yearmon(as.yearmon(x[,1]), format="%Y-%m")
    } else {
      x[,1] <- as.Date(x[,1],format = "%d/%m/%Y")
    }
    year <- as.numeric(format(min(x[,1]), format="%Y"))
    month <- as.numeric(format(min(x[,1]), format="%m"))
    dataFcst <- x %>% select(y)
    tsdata <- ts(na.omit(dataFcst[,1]), start= c(year,month), frequency = z)
    rec <- window(tsdata, start= c(year,month))
    reclog <- window(log(tsdata), start= c(year,month))
    if(input$log == FALSE){
      fit <- stl(rec, s.window = "periodic")
    } else {
      fit <- stl(reclog, s.window = "periodic")
    }
    autoplot(fit, ts.colour = 'blue') + theme_minimal()
  })
  
  output$adf <- renderPrint({
    x <- data_fc()
    xn <- colnames(x[1])
    y <- input$varsPred
    z <- as.numeric(input$period)
    if(nchar(x[1,1])==7){
      x[,1] <- as.Date.yearmon(as.yearmon(x[,1]), format="%Y-%m")
    } else {
      x[,1] <- as.Date(x[,1],format = "%d/%m/%Y")
    }
    year <- as.numeric(format(min(x[,1]), format="%Y"))
    month <- as.numeric(format(min(x[,1]), format="%m"))
    dataFcst <- x %>% select(y)
    tsdata <- ts(na.omit(dataFcst[,1]), start= c(year,month), frequency = z)
    adf.test(tsdata, alternative = "stationary")
  })
  
  output$adf_dif <- renderPrint({
    x <- data_fc()
    xn <- colnames(x[1])
    y <- input$varsPred
    z <- as.numeric(input$period)
    ndif <- input$diff
    if(nchar(x[1,1])==7){
      x[,1] <- as.Date.yearmon(as.yearmon(x[,1]), format="%Y-%m")
    } else {
      x[,1] <- as.Date(x[,1],format = "%d/%m/%Y")
    }
    year <- as.numeric(format(min(x[,1]), format="%Y"))
    month <- as.numeric(format(min(x[,1]), format="%m"))
    dataFcst <- x %>% select(y)
    tsdata <- ts(na.omit(dataFcst[,1]), start= c(year,month), frequency = z)
    if(input$log == FALSE){
      rec <- window(tsdata, start= c(year,month))
    } else {
      rec <- window(log(tsdata), start= c(year,month))
    }
    
    # fit <- stl(rec, s.window = "periodic")
    # deseasonal <- seasadj(fit)
    
    if(input$opt == FALSE){
      ts_diff <- rec
    } else {
      if(ndif == 1){
        ts_diff <- diff(rec,lag = 1,differences = 1)
      } else {
        if(ndif == 2){
          ts_diff_1 <- diff(rec,lag = 1,differences = 1)
          ts_diff <- diff(ts_diff_1,lag = 1,differences = 1)
        } else {
          if(ndif == 3){
            ts_diff_1 <- diff(rec,lag = 1,differences = 1)
            ts_diff_2 <- diff(ts_diff_1,lag = 1,differences = 1)
            ts_diff <- diff(ts_diff_2,lag = 1,differences = 1)
          } else {
            if(ndif == 4){
              ts_diff_1 <- diff(rec,lag = 1,differences = 1)
              ts_diff_2 <- diff(ts_diff_1,lag = 1,differences = 1)
              ts_diff_3 <- diff(ts_diff_2,lag = 1,differences = 1)
              ts_diff <- diff(ts_diff_3,lag = 1,differences = 1)
            } else {
              if(ndif == 5){
                ts_diff_1 <- diff(rec,lag = 1,differences = 1)
                ts_diff_2 <- diff(ts_diff_1,lag = 1,differences = 1)
                ts_diff_3 <- diff(ts_diff_2,lag = 1,differences = 1)
                ts_diff_4 <- diff(ts_diff_3,lag = 1,differences = 1)
                ts_diff <- diff(ts_diff_4,lag = 1,differences = 1)
              } else {
                ts_diff <- diff(rec,lag = 1,differences = 1)
              }
            }
          }
        }
      }
    }
    
    adf.test(ts_diff, alternative = "stationary")
    
  })
  
  
  output$adf_plot <- renderPlot({
    x <- data_fc()
    xn <- colnames(x[1])
    y <- input$varsPred
    z <- as.numeric(input$period)
    ndif <- input$diff
    if(nchar(x[1,1])==7){
      x[,1] <- as.Date.yearmon(as.yearmon(x[,1]), format="%Y-%m")
    } else {
      x[,1] <- as.Date(x[,1],format = "%d/%m/%Y")
    }
    year <- as.numeric(format(min(x[,1]), format="%Y"))
    month <- as.numeric(format(min(x[,1]), format="%m"))
    dataFcst <- x %>% select(y)
    tsdata <- ts(na.omit(dataFcst[,1]), start= c(year,month), frequency = z)
    if(input$log == FALSE){
      rec <- window(tsdata, start= c(year,month))
    } else {
      rec <- window(log(tsdata), start= c(year,month))
    }
    # fit <- stl(rec, s.window = "periodic")
    # deseasonal <- seasadj(fit)
    
    if(input$opt == FALSE){
      ts_diff <- rec
    } else {
      if(ndif == 1){
        ts_diff <- diff(rec,lag = 1,differences = 1)
      } else {
        if(ndif == 2){
          ts_diff_1 <- diff(rec,lag = 1,differences = 1)
          ts_diff <- diff(ts_diff_1,lag = 1,differences = 1)
        } else {
          if(ndif == 3){
            ts_diff_1 <- diff(rec,lag = 1,differences = 1)
            ts_diff_2 <- diff(ts_diff_1,lag = 1,differences = 1)
            ts_diff <- diff(ts_diff_2,lag = 1,differences = 1)
          } else {
            if(ndif == 4){
              ts_diff_1 <- diff(rec,lag = 1,differences = 1)
              ts_diff_2 <- diff(ts_diff_1,lag = 1,differences = 1)
              ts_diff_3 <- diff(ts_diff_2,lag = 1,differences = 1)
              ts_diff <- diff(ts_diff_3,lag = 1,differences = 1)
            } else {
              if(ndif == 5){
                ts_diff_1 <- diff(rec,lag = 1,differences = 1)
                ts_diff_2 <- diff(ts_diff_1,lag = 1,differences = 1)
                ts_diff_3 <- diff(ts_diff_2,lag = 1,differences = 1)
                ts_diff_4 <- diff(ts_diff_3,lag = 1,differences = 1)
                ts_diff <- diff(ts_diff_4,lag = 1,differences = 1)
              } else {
                ts_diff <- diff(rec,lag = 1,differences = 1)
              }
            }
          }
        }
      }
    }
    
    autoplot(ts_diff, ts.colour = "turquoise",ts.size = 1.2) + 
      theme(plot.title = element_text(hjust = 0.5,face="bold",size=15))
  })
  
  output$residuals <- renderPlot({
    x <- data_fc()
    xn <- colnames(x[1])
    y <- input$varsPred
    z <- as.numeric(input$period)
    if(nchar(x[1,1])==7){
      x[,1] <- as.Date.yearmon(as.yearmon(x[,1]), format="%Y-%m")
    } else {
      x[,1] <- as.Date(x[,1],format = "%d/%m/%Y")
    }
    year <- as.numeric(format(min(x[,1]), format="%Y"))
    month <- as.numeric(format(min(x[,1]), format="%m"))
    dataFcst <- x %>% select(y)
    tsdata <- ts(na.omit(dataFcst[,1]), start= c(year,month), frequency = z)
    
    if(input$log == FALSE){
      rec <- window(tsdata, start= c(year,month))
    } else {
      rec <- window(log(tsdata), start= c(year,month))
    }
    
    fit <- stl(rec, s.window = "periodic")
    plot(forecast(rec, h = p))
  })
  
  output$adjust <- renderPrint({
    x <- data_fc()
    xn <- colnames(x[1])
    y <- input$varsPred
    z <- as.numeric(input$period)
    d <- input$diff
    a <- input$arima
    f <- input$ari
    ar <- input$ar1
    i <- input$i2
    ma <- input$ma3
    sea <- input$season
    if(nchar(x[1,1])==7){
      x[,1] <- as.Date.yearmon(as.yearmon(x[,1]), format="%Y-%m")
    } else {
      x[,1] <- as.Date(x[,1],format = "%d/%m/%Y")
    }
    year <- as.numeric(format(min(x[,1]), format="%Y"))
    month <- as.numeric(format(min(x[,1]), format="%m"))
    dataFcst <- x %>% select(y)
    tsdata <- ts(na.omit(dataFcst[,1]), start= c(year,month), frequency = z)
    
    if(input$log == FALSE){
      rec <- window(tsdata, start= c(year,month))
    } else {
      rec <- window(log(tsdata), start= c(year,month))
    }
    fit <- stl(rec, s.window = "periodic")
    deseasonal <- seasadj(fit)
    
    if(a == TRUE){
      if(f == TRUE){
        arima(deseasonal, order=c(ar,i,ma))
      } else {
        auto.arima(deseasonal, seasonal=sea)
      }
    } else {
      "Modelo ETS (Suavizado Exponencial con base en la tendencia) por defecto, selecciona 'Aplicar ARIMA' para cambiar el modelo"
    }
  })
  
  output$fcst <- renderPlot({
    x <- data_fc()
    xn <- colnames(x[1])
    y <- input$varsPred
    z <- as.numeric(input$period)
    p <- input$obsfc
    a <- input$arima
    f <- input$ari
    ar <- input$ar1
    i <- input$i2
    ma <- input$ma3
    sea <- input$season
    v <- input$changev
    if(nchar(x[1,1])==7){
      x[,1] <- as.Date.yearmon(as.yearmon(x[,1]), format="%Y-%m")
    } else {
      x[,1] <- as.Date(x[,1],format = "%d/%m/%Y")
    }
    year <- as.numeric(format(min(x[,1]), format="%Y"))
    month <- as.numeric(format(min(x[,1]), format="%m"))
    dataFcst <- x %>% select(y)
    tsdata <- ts(na.omit(dataFcst[,1]), start= c(year,month), frequency = z)
    
    if(input$log == FALSE){
      rec <- window(tsdata, start= c(year,month))
    } else {
      rec <- window(log(tsdata), start= c(year,month))
    }
    
    fit <- stl(rec, s.window = "periodic")
    deseasonal <- seasadj(fit)
    
    if(a == TRUE){
      if(f == TRUE){
        modeloarima <- arima(rec, order=c(ar,i,ma))
        prediccion <- forecast(modeloarima, h=p)
        if(v %in% "fore"){
          plot(prediccion)
        } else {
          checkresiduals(prediccion)
        }
      } else {
        modeloarima <- auto.arima(rec, seasonal=sea)
        prediccion <- forecast(modeloarima, h=p)
        if(v %in% "fore"){
          plot(prediccion)
        } else {
          checkresiduals(prediccion)
        }
      }
    } else {
      plot(forecast(rec, h = p))
    }
    
  })
  
  output$fc <- renderUI({
    if(is.null(data_fc()))
      h4("Carga un archivo tipo CSV, deberá tener 1 columna de tipo fecha y al menos 1 de tipo numerica que esten describiendo un comportomaiento.")
    else
      tabsetPanel(
        tabPanel("Definición", htmlOutput("definition_fc")),
        tabPanel("Datos", tableOutput("sample_fc")),
        tabPanel("Descomposición", plotOutput("desc_fc",height="50vh")),
        tabPanel("ADF Test", br(), uiOutput("diff"), uiOutput("sliderDiff"), verbatimTextOutput("adf_dif"),plotOutput("adf_plot")),
        tabPanel("Forecast",br(),fluidRow(
          column(4,uiOutput("arimaon")),
          column(4,uiOutput("ariman")),
          column(4,uiOutput("seasonal"))
        ),
        fluidRow(
          column(2,uiOutput("ar")),
          column(2,uiOutput("i")),
          column(2,uiOutput("ma"))
        ),verbatimTextOutput("adjust"),uiOutput("visuals"),plotOutput("fcst",height="50vh"))
      )
  })
  
  shinyjs::onclick("logo", updateTabsetPanel(session, inputId="navbar", selected="Inicio"))
  shinyjs::onclick("basket", updateTabsetPanel(session, inputId="navbar", selected="Carrito"))
  shinyjs::onclick("forecast", updateTabsetPanel(session, inputId="navbar", selected="Forecasting"))
  shinyjs::onclick("customers", updateTabsetPanel(session, inputId="navbar", selected="Customers"))
  shinyjs::onclick("price", updateTabsetPanel(session, inputId="navbar", selected="Pricing"))
  
}