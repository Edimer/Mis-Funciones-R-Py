# #--------------- Argumentos de la función --------------------------- # #

#' @param modelo porcentaje de la base de datos para train, por defecto utiliza el 80% para train y el 20% para test. 
#' @param dataPredict posición de la variable respuesta u objetivo. Por defecto es igual a 45.

# #--------------------- FUNCIÓN MODELADOR ---------------------------- # #
predictor <- function(modelo, dataPredict){
  
  suppressMessages(library(lightgbm))
  modelo = modelo
  dataTest = dataPredict
  predicciones <- predict(modelo, data.matrix(dataTest))
  
  # Retorno
  return(predicciones)
  
} 