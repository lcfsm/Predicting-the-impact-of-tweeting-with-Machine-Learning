######################### (PARTE 0) - PREPARING THE ENVIRONMENT ############################### 

library(reshape2)
library(qdap)
library(plyr)
library(SnowballC)
library(dplyr)
library(twitteR)
library(RCurl)
library(RJSONIO)
library(stringr)
library(ggplot2)
library(XML)
library(tm)

#Set the PATH where the Dataset are
rm(list = ls())
setwd('C:\\Users\\LAPTOP\\Desktop\\PFM\\src\\Tweets')

#Read the CSV's that contain the tweets
    datapath = "..//Tweets"
    files = list.files(path=datapath, pattern=".csv")
    files = paste(datapath,files,sep="//")
    datos = lapply(files,read.csv,encoding = "UTF-8")
    datos <- do.call(rbind,datos)
    
######################################################################################
    #Se leen los CSV's que contienen los tweets de la cuenta de ALEX
    #datapathalex = "..//Tweets//datos Alex"
    #filesalex = list.files(path=datapathalex, pattern=".csv")
    #filesalex = paste(datapathalex,filesalex,sep="//")
    #datosalex = lapply(filesalex,read.csv,encoding = "UTF-8")
    #datosalex <- do.call(rbind,datosalex)
    #cambiamos los nombres de las columnas por los de Enrique
    #for (i in 1:length(datos)){
    #colnames(datosalex)[i] <- colnames(datos[i])
    #}
    #datos=datosalex

######################### (PARTE 1) - ELIMINATION OF NON-RELEVANT ATTRIBUTES ################

#".promoted" attributes removed, no working with promoted tweets
    datos=datos[,1:22]              
#Remove the Tweet ID and its permanent link
    datos=datos[,3:ncol(datos)]     
    datosoriginales = datos
 
######################### (PARTE 2) - TEXT PROCESSING #################################

#All text is lowercase   
texto = datos$Texto.del.Tweet
texto = tolower(texto) 

###################################################
##### Inicio limpieza de datos con mi formula#####
# remueve retweets
#txtclean = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", texto)
# remove @otragente
#txtclean = gsub("@\\w+", "", txtclean)
# remueve simbolos de puntuación
#txtclean = gsub("[[:punct:]]", "", txtclean)
# remove números
#txtclean = gsub("[[:digit:]]", "", txtclean)
# remueve links
#txtclean = gsub("http\\w+", "", txtclean)

# Acabamos con el procesado de texto
#datos$Texto.del.Tweet=txtclean
#compruebo el idioma con la libreria textcat (no funciona muy bien)
#texto = datos$Texto.del.Tweet
#text_tweets  = str_trim(unlist(texto))
#datos$idioma = textcat(text_tweets)

#check=1-textcat(text_tweets) %in% c("spanish","speranto ")

#compruebo el idioma con la libreria franc (no funciona muy bien)
#library(franc)
#for (i in 1:nrow(datos)){
#print(franc(datos$Texto.del.Tweet[i]))
#}

###################################### FIN DE MI PRUEBA

# Se quitan acentos
    #texto = gsub("Ã¡", "a", texto)
    #texto = gsub("Ã©", "e", texto)
    #texto = gsub("Ã???", "i", texto)
    #texto = gsub("Ã³", "o", texto)
    #texto = gsub("Ãº", "u", texto)
   
  library(stringi)
  texto<-stri_trans_general(texto, "Latin-ASCII")
    
#We Replace Direct Messages, Links and Mentions
    texto = gsub(" http[a-zA-Z0-9:/.]*"," LINK",texto)
    texto = gsub(" @[a-zA-Z0-9_]*"," MENCTION",texto)
    texto = gsub("@[a-zA-Z0-9_]*[ ]","DIRECTTOPRIV ",texto)
    texto = gsub(".@[a-zA-Z0-9_]*[ ]","DIRECTTOPUB ",texto)
    texto = gsub(" #[a-zA-Z0-9_]*"," TAG",texto)

#We remove special characters and numbers
    texto = gsub("[^a-zA-Z Ã±]"," ",texto)

#We finished with the text processing
    datos$Texto.del.Tweet=texto
           
############### (PARTE 3)  - COUNT LINKS, MENTIONS, LABELS ###############
    
datos["nLinks"] = 0
datos["nTags"] = 0
datos["nMenctions"] = 0
datos["nDirecttoprivs"] = 0
datos["nDirecttopubs"] = 0
  for (i in 1:length(texto)){
      datos$nLinks[i]=length(grep("LINK", texto[i]))
      datos$nTags[i]=length(grep("TAG", texto[i]))
      datos$nMenctions[i]=length(grep("MENCTION", texto[i]))
      datos$nDirecttoprivs[i]=length(grep("DIRECTTOPRIV", texto[i]))
      datos$nDirecttopubs[i]=length(grep("DIRECTTOPUB", texto[i]))
  }
    
#We eliminate special words
    texto = gsub("LINK"," ",texto)
    texto = gsub("TAG"," ",texto)
    texto = gsub("MENCTION"," ",texto)
    texto = gsub("DIRECTTOPRIV"," ",texto)
    texto = gsub("DIRECTTOPUB"," ",texto)
    datos$Texto.del.Tweet=texto
    
############### NORMALIZACION DE ATRIBUTOS #########################
# ------------------------------------------------------ #
  #doit <- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) - min(x, na.rm=TRUE))*1}
  #datosnormal=datos
  #datosnormal[1:2]=NULL #Eliminamos las 2 primeras columnas para dejar solo valores numericos
  #datosnormal <- as.data.frame(lapply(datosnormal, doit))
# ------------------------------------------------------- #
    
################################## (PARTE 4) - PROCESSING OF THE DATE ######################
    
    tiempo = datos$tiempo
    
    
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
    datos["min"] = tiempo$min
    datos["diasemana"] = tiempo$diasemana
    datos["findesemana"] = tiempo$findesemana
    datos["horariolaboral"] = tiempo$horariolaboral
    
################################# (PARTE 5) WORDS PROCESSING, LANGUAGE ############
    
  texto = datos$Texto.del.Tweet

# We count the number of words and add it to the original dataset    
    #Nterminos = sapply(gregexpr("\\W+", texto), length) + 1
    Nterminos = sapply(gregexpr("\\W+", texto), length)
    datos["Nterminos"] = Nterminos
    
# Detectamos el idioma mediante la libreria textcat (No funciona del todo bien)
    #library("textcat")
    #library("rvest")
    #library("stringr")
    #text_tweets  = str_trim(unlist(texto))
    #datos$idioma = textcat(text_tweets)

# We do the detection of words through dictionary    
    # We cut the text and remove columns with all NA
    prueba=colsplit(texto," ",1:140)
    prueba=prueba[,colSums(is.na(prueba))<nrow(prueba)]
    
    #We read the dictionary of words in Spanish
    #From: http://corpus.rae.es/lfrecuencias.html 
    setwd('C:\\Users\\LAPTOP\\Desktop\\PFM\\src')
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
   
datprueba<-data.frame(datos$tiempo,datos$anio,datos$mes,datos$dia,datos$hora,datos$min,datos$diasemana,datos$findesemana,datos$horariolaboral)
    
################ (PARTE 5) - CORPUS #######################################################################
    
    #How many tweets
    tweet_num = length(datos$Texto.del.Tweet)
    # data frame (text, sentiment, score)
    tweet_df = data.frame(text=datos$Texto.del.Tweet, sentiment=rep("", tweet_num),
    subject=1:tweet_num, topic=1:tweet_num, language=1:tweet_num,gender=1:tweet_num,readability=1:tweet_num, stringsAsFactors=FALSE)    

    myCorpus <- Corpus(VectorSource(tweet_df$text))
    #Add two extra stop words: 'available' and 'via'
    myStopwordseng <- c(stopwords("english"),"via","rt")#podemos añadir mas palabras como available o via --- myStopwordseng <- c(stopwords("en"), "available", "via") 
    myStopwordsesp <- c(stopwords("spanish"),"s","p")
    #Remove 'r' and 'big' from stopwords
    #myStopwords <- setdiff(myStopwordseng, c("r", "big"))
    #Remove stopwords from corpus, english and spanish
    myCorpus <- tm_map(myCorpus, removeWords, myStopwordseng)
    myCorpus <- tm_map(myCorpus, removeWords, myStopwordsesp)
    
    #Keep a copy of corpus to use later as a dictionary for stemcompletion
    myCorpusCopy <- myCorpus
    #Stem words
    #myCorpus <- tm_map(myCorpus, stemDocument) # TALLO DE LA PALABRA
   
    #Inspect the first 5 documents (tweets) inspect(myCorpus[1:5]) 
    for (i in 1:10) {
      cat(paste("[[", i, "]] ", sep = ""))
      #cat(myCorpus[[i]]$content)
      writeLines(as.character(myCorpus[[i]]))
    }
    
    #library(tm)
    #We create an array of terms (occurrences of each word)
    #Example: Changing the value 1 to 3 for example we remove the words of 3 letters
    tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))
    tdm
    
    #How many characters per tweet?
    chars_per_tweet = sapply(datos$Texto.del.Tweet, nchar)
    summary(chars_per_tweet)
    
    #How many words per tweets, split words
    words_list = strsplit(datos$Texto.del.Tweet, " ")
    
    #Words per tweet
    words_per_tweet = sapply(words_list, length)
    #Barplot
    barplot(table(words_per_tweet), border=NA, main="Distribution of words per tweet", cex.main=1)
    
    #Length of words per tweet
    wsize_per_tweet = sapply(words_list, function(x) mean(nchar(x)))
    #Barplot
    barplot(table(round(wsize_per_tweet)), border=NA,
            xlab = "word length in number of characters",
            main="Distribution of words length per tweet", cex.main=1)
    
    #How many unique words per tweet
    uniq_words_per_tweet = sapply(words_list, function(x) length(unique(x)))
    #Barplot
    barplot(table(uniq_words_per_tweet), border=NA,
            main="Distribution of unique words per tweet", cex.main=1)
    
    #Let's create a data frame with all the calculated stuff and make some plots
    stuff = data.frame(
       chars=chars_per_tweet,
       words = words_per_tweet,
       lengths = wsize_per_tweet,
       uniqs = uniq_words_per_tweet,
       hashs = datos$nTags,
       ats = datos$nMenctions,
       links = datos$nLinks
    )
    
    #The more words in a tweet, the more characters per word
    #Words -vs- chars
    ggplot(stuff, aes(x=words, y=chars)) + geom_point(colour="gray20", alpha=0.4) + stat_smooth(method="lm") + labs(x="number of words per tweet", y="number of characters per tweet") 
        
    #The more words in a tweet, the shorter the words
    #Words -vs- word length
    ggplot(stuff, aes(x=words, y=lengths)) + geom_point(colour="gray20", alpha=0.4) +stat_smooth(method="lm") +labs(x="number of words per tweet", y="size of words per tweet")
  
    #Lexical diversity: number of unique tokens / number of total tokens
    #Unique words in total
    uniq_words = unique(unlist(words_list))

    #The lexical diversity reflects the range of diversity in vocabulary.
    length(uniq_words) / length(unlist(words_list))

    #Freqency words and Association
    idx <- which(dimnames(tdm)$Terms == "p") #Starts with p
    inspect(tdm[idx + (0:5), 1:length(myCorpus)])
    
    #Inspect frequent words (more than 5 times)
    (freq.terms <- findFreqTerms(tdm, lowfreq=5))
    term.freq <- rowSums(as.matrix(tdm))
    term.freq <- subset(term.freq, term.freq >5)
    df <- data.frame(term = names(term.freq), freq = term.freq)
    #Sort by frecuency
    df$term <- factor(df$term, levels = df$term[order(df$freq)])
    df$term  # notice the changed order of factor levels
    ggplot(df, aes(x=term, y=freq)) + geom_bar(stat = "identity") + xlab("Most frequent terms") + ylab("Count") 
    
    #Which words are associated with 'data'?
    findAssocs(tdm, "r", 0.2)
    #Which words are associated with 'big'?
    findAssocs(tdm, "mining", 0.25)
  
    #source("https://bioconductor.org/biocLite.R")
    #biocLite("graph")
    #biocLite("Rgraphviz")
    library(graph)
    library(Rgraphviz)
    plot(tdm, term = freq.terms, corThreshold = 0.12, weighting = T)
    
    ### Word Cloud ###
    
    library(wordcloud)
    m <- as.matrix(tdm)
    #m <-m[!rownames(m)=="que", ] #eliminacion de filas que tengn determinadas palabras
    #m <-m[!rownames(m) %in% c("co"), ]
    # calculate the frequency of words and sort it by frequency
    word.freq <- sort(rowSums(m), decreasing = F)
    wordcloud(words = names(word.freq), freq = word.freq, min.freq = 2,max.words=100, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
    
    ##### Clustering ###
    
    # Poniendo esto no funciona el codigo de mas abajo
    #tdm$dimnames$Terms
    #Convierte a una matriz de Nº palabras filas y Nº tweets columnas
    #tdm = as.matrix(tdm)
    #Trasponemos la matriz 
    #tdm = t(tdm)
    #dim(tdm)
    #apply(tdm,2,function(x) length(x[x>0]))
    
    tdm2 <- removeSparseTerms(tdm, sparse = 0.984) #Cambiando este valor obtenemos mas o menos palabras
    m2 <- as.matrix(tdm2)
    #Cluster terms
    distMatrix <- dist(scale(m2))
    fit <- hclust(distMatrix, method = "ward.D")
    plot(fit)
    rect.hclust(fit, k = 3) # cut tree into 3 clusters 
    
    ### KMEANS ###
    
    m3 <- t(m2) #Transpose the matrix to cluster documents (tweets)
    set.seed(122) #Set a fixed random seed
    k <- 3 #Number of clusters
    kmeansResult <- kmeans(m3, k)
    round(kmeansResult$centers, digits = 3) # cluster centers
    
    library(fpc)
    #Partitioning around medoids with estimation of number of clusters
    pamResult <- pamk(m3, metric="manhattan")
    k <- pamResult$nc # number of clusters identified
    pamResult <- pamResult$pamobject
    #Print cluster medoids
    for (i in 1:k) {
      cat("cluster", i, ": ",
          colnames(pamResult$medoids)[which(pamResult$medoids[i,]==1)], "\n")
    }
    
    ### Topic Model ###
    
    dtm <- as.DocumentTermMatrix(tdm)
    library(topicmodels)
    lda <- LDA(dtm, k = 3) #Find 3 topics
    term <- terms(lda, 4) #First 4 terms of every topic
    term
    term <- apply(term, MARGIN = 2, paste, collapse = ", ")
    
    #First topic identified for every document (tweet)
    require(data.table) #fore IDate
    topic <- topics(lda, 1)
    topics <- data.frame(date=as.IDate(datos$tiempo), topic)
    qplot(date, ..count.., data=topics, geom="density",fill=term[topic],alpha=I(.5))
    
################# detect people and places (no termina de funcionar del todo bien, revisarlo) #################
    library(NLP)
    library(openNLP)
    library(magrittr)
    
    word_ann <- Maxent_Word_Token_Annotator()
    sent_ann <- Maxent_Sent_Token_Annotator()
    bio_annotations <- annotate(texto, list(sent_ann, word_ann))
    class(bio_annotations)
    head(bio_annotations)
    
    bio_doc <- AnnotatedPlainTextDocument(texto, bio_annotations)
    sents(bio_doc) %>% head(2)
    words(bio_doc) %>% head(10)

    library(rJava)
    #install.packages(c("NLP", "openNLP", "RWeka", "qdap"))
    #install.packages("openNLPmodels.es",repos = "http://datacube.wu.ac.at/",type = "source")
    library(openNLP)
    library(NLP)
    person_ann <- Maxent_Entity_Annotator(kind = "person")
    location_ann <- Maxent_Entity_Annotator(kind = "location")
    organization_ann <- Maxent_Entity_Annotator(kind = "organization")
    pipeline <- list(sent_ann,
                     word_ann,
                     person_ann,
                     location_ann,
                     organization_ann)
    bio_annotations <- annotate(texto, pipeline)
    bio_doc <- AnnotatedPlainTextDocument(texto, bio_annotations)  
    
    #Extract entities from an AnnotatedPlainTextDocument
    entities <- function(doc, kind) { 
      s <- doc$content
      a <- annotations(doc)[[1]]
      if(hasArg(kind)) {
        k <- sapply(a$features, `[[`, "kind")
        s[a[k == kind]]
      } else {
        s[a[a$type == "entity"]]
      }
    }
    
    entities(bio_doc, kind = "person")
    entities(bio_doc, kind = "location")
    entities(bio_doc, kind = "organization")
#############################################################################################   
    
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

############################ Llamamos a la funcion ######################################
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
      tweet_df$sentiment[i] = tmp$sentiment
      tweet_df$subject[i] = tmp$subject
      tweet_df$topic[i] = tmp$topic
      tweet_df$language[i] = tmp$language
      tweet_df$gender[i] = tmp$gender
      tweet_df$readability[i] = tmp$readability
    }

##########################################################################################
    
  ########## DISCRETIZACION DE VARIABLES###########
  library(arules)
  library(arulesViz)
  
  #1ª FORMA
  hist(datos$impresiones, right = F, freq = T, main = "Histograma Frecuencias Absolutas")
  hist(datos$impresiones, right = F, freq = F, main = "Histograma Frequencias Relativas")
  
  datos$interaccionesdiscret = discretize(datos$interacciones,method="frequency",categories=6)
  #datos$impresionesdiscret = discretize(datos$impresiones,method="frequency",categories=5)
  
  #anadir las nuevas columnas discretizadas de forma numerica, no factorial
  datos$impresionesdiscret = as.numeric(discretize(datos$impresiones,method="frequency",categories=6))
  discretizacion <-cbind(datos$impresiones,datos$impresionesdiscret)
  colnames(discretizacion) <- c("impresiones", "impresiones_discret")
  
  table(datos$interaccionesdiscret)
  table(datos$impresionesdiscret)
  
  #2ª FORMA
  #0-	Ninguna impresion
  #datos$interdiscret2[datos$interacciones==0] = 0
  #1-	Muy pocas impresiones
  #datos$interdiscret2[datos$interacciones>0 & datos$interacciones<=3] = 1
  #2-	Pocas impresiones
  #datos$interdiscret2[datos$interacciones>3 & datos$interacciones<=8] = 2
  #3-	Numero de impresiones normal
  #datos$interdiscret2[datos$interacciones>8 & datos$interacciones<=15] = 3
  #4-	Número de impresiones alto
  #datos$interdiscret2[datos$interacciones>15 & datos$interacciones<=25] = 4
  #5-	Número de impresiones muy alto
  #datos$interdiscret2[datos$interacciones>25] = 5
  
  
  #3ª FORMA
  #min<-min(datos$impresiones)
  #max<-max(datos$impresiones)
  #cut(datos$impresiones, seq(0, 2655, by = 525), right = FALSE)
  #table(cut(datos$interacciones, seq(0, 2655, by = 525), right = FALSE))
  #n <- nrow(datos)
  #table(cut(datos$impresiones, seq(0, 2655, by = 525), right = FALSE))/n
  #hist(datos$impresiones, right = T, main = "Histograma (c,c]")
  #hist(datos$impresiones, right = F, freq = T, main = "Histograma Frecuencias Absolutas")
  #hist(datos$impresiones, right = F, freq = F, main = "Histograma Frequencias Relativas")
  
##########################################################################################

  
#Eliminamos la columna texto del dataset getsentiment para unirlo a datos
tweet_df[,1]=NULL
#Eliminamos la columna idioma del dataset getsentiment para unirlo a datos
tweet_df[,5]=NULL
#Unimos los dos datasets (getsentiment y datos)   
datosfinal=cbind(datos,tweet_df,m3)   

# Guardamos el dataset resultante        
write.csv(datosfinal,file="DataSet_Final.csv")

### PRUEBAS FINALES ###

setwd('C:\\Users\\LAPTOP\\Desktop\\PFM\\src')
DatasetFinal = read.csv("DataSet_Final.csv")  # read csv file 
#Eliminamos las columnas que no son utiles para el proceso de mineria
DatasetFinal<-DatasetFinal[, colSums(DatasetFinal != 0) > 0] #Eliminamos las que tengan todos los valores a 0
DatasetFinal$X <- NULL
DatasetFinal$Texto.del.Tweet <- NULL
DatasetFinal$tiempo <- NULL
DatasetFinal$interacciones <- NULL
DatasetFinal$impresiones <- NULL
DatasetFinal$language <- NULL
DatasetFinal$min <- NULL
DatasetFinal$NpalENG <- NULL
DatasetFinal$NpalESP <- NULL
DatasetFinal$nDirecttoprivs <- NULL
DatasetFinal$interaccionesdiscret <- NULL
#Nos quedamos con las 25 palabras mas representativas
#Para eso eliminamos las siguientes
DatasetFinal$si <- NULL
DatasetFinal$mas <- NULL

# Guardamos el dataset resultante        
write.csv(DatasetFinal,file="Ultimate1.csv")

# Guardamos el dataset resultante eliminando atributos incluidos del tweet
DatasetFinal[1:11] <- NULL
write.csv(DatasetFinal,file="Ultimate2.csv")

#sapply(DatasetFinal, class)
#Final = read.csv("Ultimate.csv")  # read csv file 
