limpiador <- function(texto, idioma, cuerpo = TRUE, graph = TRUE,
                      palabras = 10, color = "Set1", ncolores = 8){
  library(tm); library(wordcloud); library(wordcloud);
  if(cuerpo == TRUE && graph == TRUE){
    texto1 = gsub("[[:cntrl:]]", " ", texto)#elimina saltos de línea o \r
    texto2 = tolower(texto1)
    texto3 = removeWords(texto2, words = stopwords(idioma))
    texto4 = removePunctuation(texto3)
    texto5 = removeNumbers(texto4)
    texto6 = stripWhitespace(texto5)
    cuerpo = Corpus(VectorSource(texto6))
    cuerpo2 = tm_map(cuerpo, PlainTextDocument)
    x11(); wordcloud(cuerpo2, max.words = palabras,
                     random.order = FALSE, colors = brewer.pal(name = color, ncolores))
    return(list(texto6, cuerpo2))
  }
  else if(cuerpo == TRUE && graph == FALSE){
    texto1 = gsub("[[:cntrl:]]", " ", texto)
    texto2 = tolower(texto1)
    texto3 = removeWords(texto2, words = stopwords(idioma))
    texto4 = removePunctuation(texto3)
    texto5 = removeNumbers(texto4)
    texto6 = stripWhitespace(texto5)
    cuerpo = Corpus(VectorSource(texto6))
    cuerpo2 = tm_map(cuerpo, PlainTextDocument)
    return(list(texto6, cuerpo2))
  } else{
    texto1 = gsub("[[:cntrl:]]", " ", texto)
    texto2 = tolower(texto1)
    texto3 = removeWords(texto2, words = stopwords(idioma))
    texto4 = removePunctuation(texto3)
    texto5 = removeNumbers(texto4)
    texto6 = stripWhitespace(texto5)
    return(list(texto6))
  }
}