#' @param text vector of text. 
#' @param language language of text.
#' @param bagWord construct bag of word (Sparse Matrix) for model. Default is "TRUE".

cleanText <- function(text, language = "english", bagWord = TRUE){
  
  # Load library
  suppressMessages(suppressWarnings(library(tm)))
  
  # Corpus text
  textCorpus = VCorpus(VectorSource(text))
  
  # Simple clean text
  textCorpus = tm_map(x = textCorpus, FUN = content_transformer(tolower))
  textCorpus = tm_map(x = textCorpus, FUN = content_transformer(removePunctuation))
  textCorpus = tm_map(x = textCorpus, FUN = content_transformer(removeNumbers))
  textCorpus = tm_map(x = textCorpus, FUN = content_transformer(stripWhitespace))
  textCorpus = tm_map(x = textCorpus, FUN = removeWords, stopwords(language))
  textCorpus = tm_map(x = textCorpus, FUN = stemDocument, language = language)
  
  # Bag word model --> Sparse matrix for model
  if(bagWord == TRUE){
    bagWordMatrix = DocumentTermMatrix(textCorpus)
  }
  
  return(list(textCorpus = textCorpus,
              bagWordMatrix = bagWordMatrix))
}