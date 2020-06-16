# #--------------- Argumentos de la función --------------------------- # #

#' @param ptrain porcentaje de la base de datos para train, por defecto utiliza el 80% para train y el 20% para test. 
#' @param posTarget posición de la variable respuesta u objetivo. Por defecto es igual a 45.
#' @param boosting tipo de árbol a utilizar. Por defecto es Gradient Boosting Decision Tree - gbdt.
#' @param feature_fraction proporción de variables utilizadas para la construcción de árboles. Por defecto igual a 1.
#' @param bagging_fraction proporción de filas muestreadas para la consrucción de árboles. Por defecto es igual a 1.
#' @param max_depth máxima profundidad del árbol. Por defecto es igual a -1, que indica que es sin límite.
#' @param is_unbalance booleano que determina si el problema de clasificación es inbalanceado. Por defecto es TRUE.
#' @param min_data_in_leaf mínimo número de datos en cada hoja del árbol construido. Por defecto es igual a 100.
#' @param num_leaves número de hojas para cada árbol construido. Por defecto es igual a 255.
#' @param data dataframe con variables (predictoras y respuesta) para construcción de modelos.
#' @param semilla entero con número que garantiza replicabilidad de resultados. Por defecto es igual a 123.
#' @param iter número de iteraciones (árboles) construidos por el algoritmo. Por defecto es igual a 30 mil.
#' @param earlyStop número de ejecuciones para detener el algoritmo luego de no presentar mejoras en el entrenamiento. Por defecto es 500.


# #--------------------- FUNCIÓN MODELADOR ---------------------------- # #
modelador <- function(ptrain = 0.8, posTarget = 45, boosting = "gbdt",
                      learningRate = 0.05, feature_fraction = 1,
                      bagging_fraction = 1, max_depth = -1, is_unbalance = TRUE,
                      min_data_in_leaf = 100, num_leaves = 255,
                      data, semilla = 123, iter = 30000, earlyStop = 500){
  
  # Bibliotecas
  suppressMessages(library(caret))
  suppressMessages(library(lightgbm))
  suppressMessages(library(dplyr))
  
  # Data train
  dataTrain = data
  
  # Partition data
  set.seed(semilla)
  indx <- createDataPartition(y = dataTrain$target, times = 1, p = ptrain, list = FALSE)
  dfTrain <- dataTrain[indx, ]
  dfTest <- dataTrain[-indx, ]
  
  # Categorical features
  catFeatures <- names(
    dataTrain %>% select_if(is.factor)
  )
  
  # Data for lightgbm
  dataTrain_lgbm <- lgb.Dataset(data = data.matrix(dfTrain[, -45]), 
                                label = dfTrain[, 45],
                                categorical_feature = catFeatures)
  dataTest_lgbm <- lgb.Dataset(data = data.matrix(dfTest[, -45]),
                               label = dfTest[, 45],
                               categorical_feature = catFeatures)
  
  # Parameters for lightgbm
  myParams <- list(
    boosting = boosting,
    objective = "binary",
    metric = "binary_logloss",
    learning_rate = learningRate,
    feature_fraction = feature_fraction,
    bagging_fraction = bagging_fraction,
    max_depth = max_depth,
    is_unbalance = is_unbalance,
    min_data_in_leaf = min_data_in_leaf,
    num_leaves = num_leaves
  )
  
  # Train model
  modelo <- lgb.train(params = myParams,
                      data = dataTrain_lgbm,
                      nrounds = iter,
                      valids = list(test = dataTest_lgbm),
                      early_stopping_rounds = earlyStop)
  
  # Return
  return(list(modeloLGBM = modelo))
}