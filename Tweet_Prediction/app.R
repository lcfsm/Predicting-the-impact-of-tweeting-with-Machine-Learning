# This is a Shiny web application. 
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(reshape2)
library(stringr)
library(RCurl)
library(RJSONIO)
library(tm)
library(ggplot2)
library(wordcloud)

# Define UI for application
library(shinythemes)
ui <- fluidPage(theme = shinytheme("darkly"),
                # Application title
                br(),
                br(),
                tags$head(
                  tags$style(
                    HTML(".bird {margin-right:20px;margin-bottom:13px}")
                  )
                ),
                titlePanel(title=div(img(src="twitter.png",width="60",class="bird"), "PREDICTING THE IMPACT OF TWEETING")),
                # Make a text input box
                textInput("text", width=900,label = h4(""), value = "Insert your tweet here...."),
                br(),
                tags$head(
                  tags$style(HTML('#predecir{background-color:#337ab7}'))
                ),
                actionButton("predecir","PREDICT"),
                hr(),
                p("1 - Very few impressions"),
                p("2 - Few impressions"),
                p("3 - Acceptable number of impressions"),
                p("4 - High number of impressions"),
                p("5 - Very high number of impressions"),
                p("6 - Number of impressions level influencer"),
                sliderInput("slider1", width=900,label = h3(""), min = 0, max = 6, value = 0),
                fluidRow(column(3, verbatimTextOutput("value"))),
                hr(),
                ################################ TABLE #####
                # Create a new row for the table.
                #tags$style(HTML(".dataTables_filter {display: none }")),
                tags$style(HTML(".irs-min, .irs-max, .irs-grid-text {font-size: 10pt; }"))
                # Show table
                #mainPanel(
                #fluidRow(
                #DT::dataTableOutput("table")
                #)
                #),
                # Show Word Cloud
                #mainPanel(
                #plotOutput("plot")
                #)
)

# Define server logic
server <- shinyServer(function(input, output, session) {
  datos = read.csv("prueba.csv")
  datos<-datos[1,] #Nos quedamos solo con la primera fila
  datos[,1] = NULL
  #datos[1:42]=0
  datos$impresionesdiscret <- as.factor(datos$impresionesdiscret)
  datos$impresionesdiscret=0
  #print(sapply(datos, typeof))
  # You can access the value of the widget with input$text, e.g.
  output$value <- renderPrint({ input$text })
  output$value  <- eventReactive(input$go, {
    "El impacto sera de: "
  })
  observeEvent(input$predecir, {
    ############### (PART 1)  - Prepare Text ###############    
    #All text is lowercase   
    texto = input$text
    texto = tolower(texto)
    library(stringi)
    texto<-stri_trans_general(texto, "Latin-ASCII")
    #We Replace Direct Messages, Links and Mentions
    texto = gsub(" http[a-zA-Z0-9:/.]*"," LINK",texto)
    texto = gsub(" @[a-zA-Z0-9_]*"," MENCTION",texto)
    texto = gsub("@[a-zA-Z0-9_]*[ ]","DIRECTTOPRIV ",texto)
    texto = gsub(".@[a-zA-Z0-9_]*[ ]","DIRECTTOPUB ",texto)
    texto = gsub(" #[a-zA-Z0-9_]*"," TAG",texto)
    #We remove special characters and numbers
    texto = gsub("[^a-zA-Z ñ]"," ",texto)
    ############### (PART 2)  - COUNT LINKS, MENTIONS, LABELS ###############
    for (i in 1:length(texto)){
      datos$nLinks[i]=length(grep("LINK", texto[i]))
      datos$nTags[i]=length(grep("TAG", texto[i]))
      datos$nMenctions[i]=length(grep("MENCTION", texto[i]))
    }
    #We eliminate special words
    texto = gsub("LINK"," ",texto)
    texto = gsub("TAG"," ",texto)
    texto = gsub("MENCTION"," ",texto)
    texto = gsub("DIRECTTOPRIV"," ",texto)
    texto = gsub("DIRECTTOPUB"," ",texto)
    datos$Texto.del.Tweet=texto
    ################################## (PART 3) - PROCESSING OF THE DATE ######################
    tiempo = Sys.time()
    # We calculate the day of the week
    diasemana = weekdays(as.Date(tiempo))
    # We divide the time in all its components
    tiempo = as.character(tiempo)
    tiempo = colsplit(string=tiempo, pattern="[-: ]", names=c("anio","mes","dia","hora","min","zz"))
    # Add the day of the week
    tiempo["diasemana"] = diasemana
    # We create a column findesemana, from the previous one
    tiempo["findesemana"] = ((diasemana=="Saturday") | (diasemana=="Sunday") | (diasemana=="Friday"))
    # We created a horariolaboral column (9:00 a.m. to 5:00 p.m.)
    tiempo["horariolaboral"] = ((diasemana=="Monday") | (diasemana=="Tuesday") | (diasemana=="Wednesday") | (diasemana=="Thursday") | (diasemana=="Friday")) & (tiempo$hora>=9 & tiempo$hora<=17)
    # We add all the variables created (minus the zz)
    datos["anio"] = tiempo$anio
    datos["mes"] = tiempo$mes
    datos["dia"] = tiempo$dia
    datos["hora"] = tiempo$hora
    paste(tiempo$diasemana, datos$diasemana, sep="")
    datos["findesemana"] = tiempo$findesemana
    datos["horariolaboral"] = tiempo$horariolaboral
    ################################# (PART 4) WORDS PROCESSING, LANGUAGE ############
    texto = datos$Texto.del.Tweet
    # We count the number of words and add it to the original dataset    
    Nterminos = sapply(gregexpr("\\W+", texto), length)
    datos["Nterminos"] = Nterminos
    # We do the detection of words through dictionary    
    # We cut the text and remove columns with all NA
    prueba=colsplit(texto," ",1:140)
    prueba=prueba[,colSums(is.na(prueba))<nrow(prueba)]
    #We read the dictionary of words in Spanish
    #From: http://corpus.rae.es/lfrecuencias.html 
    #setwd('C:\\Users\\LAPTOP\\Desktop\\PFM\\src')
    palabrasESP = read.csv("PalabrasESP_v2.csv")
    #We create the attribute and add the words of the tweet that are in the dictionary
    datos["NpalESP"]=0
    for (i in 1:nrow(palabrasESP)){
      datos$NpalESP = datos$NpalESP + rowSums(prueba[,]==as.character(palabrasESP[i,1]))
    }
    #We read the dictionary of words in English
    #From: https://github.com/first20hours/google-10000-english
    palabrasENG = read.csv("PalabrasENG.csv")
    #We create the attribute and add the words of the tweet that are in the dictionary
    datos["NpalENG"]=0
    for (i in 1:nrow(palabrasESP)){
      datos$NpalENG = datos$NpalENG + rowSums(prueba[,]==as.character(palabrasENG[i,1]))
    }
    #We define the language according to the number of words found in each dictionary
    for (i in 1:nrow(datos)){
      if(datos$NpalESP[i]>datos$NpalENG[i]){
        datos$idioma[i]= "spanish"
      }else{
        datos$idioma[i]= "english"
      }
    } 
    datos$NpalENG=NULL
    datos$NpalESP=NULL
    ################################ API DATUMBOX ##############################################
    #FUNCION GETSENTIMENT 
    getSentiment <- function (text, key){
      text <- URLencode(text);
      #save all the spaces, then get rid of the weird characters that break the API, then convert back the URL-encoded spaces.
      text <- str_replace_all(text, "%20", " ");
      text <- str_replace_all(text, "%\\d\\d", "");
      text <- str_replace_all(text, " ", "%20");
      if (str_length(text) > 360){
        text <- substr(text, 0, 359);
      }
      ##########################################
      data <- getURL(paste("http://api.datumbox.com/1.0/TwitterSentimentAnalysis.json?api_key=", key, "&text=",text, sep=""))
      js <- fromJSON(data, asText=TRUE);
      # get mood probability
      sentiment = js$output$result
      ###################################
      data <- getURL(paste("http://api.datumbox.com/1.0/SubjectivityAnalysis.json?api_key=", key, "&text=",text, sep=""))
      js <- fromJSON(data, asText=TRUE);
      # get mood probability
      subject = js$output$result
      ##################################
      data <- getURL(paste("http://api.datumbox.com/1.0/TopicClassification.json?api_key=", key, "&text=",text, sep=""))
      js <- fromJSON(data, asText=TRUE);
      # get mood probability
      topic = js$output$result
      ##################################
      data <- getURL(paste("http://api.datumbox.com/1.0/LanguageDetection.json?api_key=", key, "&text=",text, sep=""))
      js <- fromJSON(data, asText=TRUE);
      # get mood probability
      language = js$output$result
      ##################################
      data <- getURL(paste("http://api.datumbox.com/1.0/GenderDetection.json?api_key=", key, "&text=",text, sep=""))
      js <- fromJSON(data, asText=TRUE);
      # get mood probability
      gender = js$output$result
      ##################################
      data <- getURL(paste("http://api.datumbox.com/1.0/ReadabilityAssessment.json?api_key=", key, "&text=",text, sep=""))
      js <- fromJSON(data, asText=TRUE);
      # get mood probability
      readability = js$output$result
      ##################################
      return(list(sentiment=sentiment,subject=subject,topic=topic,language=language,gender=gender,readability=readability))
    }
    ############################ CALL THE FUNCTION ######################################
    # how many tweets
    tweet_num = length(datos$Texto.del.Tweet)
    # data frame (text, sentiment, score)
    tweet_df = data.frame(text=datos$Texto.del.Tweet, sentiment=rep("", tweet_num),
                          subject=1:tweet_num, topic=1:tweet_num, language=1:tweet_num,gender=1:tweet_num,readability=1:tweet_num, stringsAsFactors=FALSE)
    # apply function getSentiment
    sentiment = rep(0, tweet_num)
    for (i in 1:tweet_num)
    {
      tmp = getSentiment(tweet_df$text[i], "9681e24763c070707b7ac6e96190cXXX")
      #tweet_df$sentiment[i] = tmp$sentiment
      datos$sentiment[i] = tmp$sentiment
      datos$subject[i] = tmp$subject
      datos$topic[i] = tmp$topic
      #tweet_df$language[i] = tmp$language
      #tweet_df$gender[i] = tmp$gender
      datos$readability[i] = tmp$readability
    }  
    myCorpus <-Corpus(VectorSource(texto))
    myStopwordseng <- c(stopwords("english"),"rt","via")
    myStopwordsesp <- c(stopwords("spanish"),"s","p","via")
    myCorpus <- tm_map(myCorpus, removeWords, myStopwordseng)
    myCorpus <- tm_map(myCorpus, removeWords, myStopwordsesp)
    tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))
    m<- as.matrix(tdm)
    palabraencontrada=0
    for (i in 18:42) {
      for(x in 1:nrow(m)){
        if(names(datos[i])==rownames(m)[x]){
          palabraencontrada=palabraencontrada+1
          datos[i]=1
          datos[i] <- as.integer( datos[i])
        }
      }
    }
    
    #----------------------------- WORDCLOUD -----------------------#
    #m <- as.matrix(tdm)
    #m <-m[!rownames(m)=="que", ] #eliminacion de filas que tengn determinadas palabras
    #m <-m[!rownames(m) %in% c("co"), ]
    # calculate the frequency of words and sort it by frequency
    word.freq <- sort(rowSums(m), decreasing = F)
    #wordcloud(words = names(word.freq), freq = word.freq, min.freq = 2,max.words=100, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
    # Make the wordcloud drawing predictable during a session
    #wordcloud_rep <- repeatable(wordcloud)
    
    #output$plot <- renderPlot({
      #wordcloud_rep(names(word.freq), word.freq, scale=c(4,0.5),
                    #min.freq = 1, max.words=100,
                    #colors=brewer.pal(8, "Set1"))
    #})
    datos$Texto.del.Tweet=NULL
  
    # LOAD THE MODEL
    # -----------------------------------------------------
    library(randomForest)
    modelo <- readRDS("randomforest.rda")
    #library(ipred)
    #modelo <- readRDS("Bootstrapped.rda")
    prediccion <- predict(modelo, datos, type="class")
    mat=as.data.frame(prediccion)
    value=as.character(mat[1])
    datos$impresionesdiscret=value
    value=paste(" El valor de la prediccion es: ",value)
    #output$value <- renderPrint({ value }) #Mostrar el valor de la prediccion en un texto
    observe({
      updateSliderInput(session, "slider1", value = datos$impresionesdiscret)
    })
    ##### Mostramos la tabla despues de predecir
    #output$table <- DT::renderDataTable(DT::datatable(datos,options = list(paging = FALSE,searching = FALSE)))
  })
  #################### FIN REACTIVE PREDECIR ########################  
  # Filter data based on selections
  #datosvacio=datos#Mostramos una tabla en blanco
  #datosvacio[1:42]=0
  #output$table <- DT::renderDataTable(DT::datatable({
    #datosvacio
  #}))

})

# Run the application 
shinyApp(ui = ui, server = server)

