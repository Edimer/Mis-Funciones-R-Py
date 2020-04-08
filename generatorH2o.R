# ================= Función generatorH2o ==================================== #

#' @param cores número de núcleos a utilizar. Por defecto es -1.
#' @param puerto puerto para conexión de h2o. Por defecto es 54321 (localhost).
#' @param memRAM máximo de memoria ram a utilizar. Por defecto es igual a 2g.
#' @param progreso activar/desactivar progreso de h2o. Por defecto es FALSE.
#' @param data_train datos para entrenamiento de modelos. Debe ser un dataframe.
#' @param data_test datos para prueba de modelos. Debe ser un dataframe. 
#' @param ptrain porcentaje (0-1) de la base de datos para entrenamiento.
#' @param pval porcentaje (0-1) de la base de datos para validación.
#' @param rta string con variable respuesta.
#' @param semilla entrero que permite establecer semilla para replicar resultados.
#' por defecto es 73.
generatorH2o <- function(cores = -1, puerto = 54321, memRAM = "2g",
                         progreso = FALSE, data_train = NULL, data_test = NULL,
                         ptrain = 0.7, pval = 0.10, rta = NULL, semilla = 73){
  
  # Bibliotecas
  library(h2o)
  
  # Progreso h2o
  if (progreso == TRUE) {
    h2o.show_progress()
  } else{
    h2o.no_progress()
  }
  
  # Iniciando H2o
  h2o.init(nthreads = cores, port = puerto, max_mem_size = memRAM)
  
  # =========================== Train ======================================= #
    
  # Datos h2o
  datah2o = as.h2o(x = data_train, destination_frame = "data_h2o")
  particiones = h2o.splitFrame(data = datah2o,
                               ratios = c(ptrain, pval),
                               seed = semilla)
  datatrain_h2o = h2o.assign(data = particiones[[1]], key = "datatrain_h2o")
  dataval_h2o   = h2o.assign(data = particiones[[2]], key = "dataval_h2o")
  datatest_h2o  = h2o.assign(data = particiones[[3]], key = "datatest_h2o")
  
  # Variable respuesta y predictores
  target = rta
  predictors = setdiff(h2o.colnames(datatrain_h2o), target)
  
  # ============================= Test =====================================
  if(is.null(data_test)){
    resultados = list(target = target,
                      predictors = predictors,
                      train = datatrain_h2o,
                      validation = dataval_h2o,
                      testTrain = datatest_h2o,
                      datah2o = datah2o)
  } else{
    test_h2o = as.h2o(x = data_test, destination_frame = "test_h2o")
    resultados = list(target = target,
                      predictors = predictors,
                      train = datatrain_h2o,
                      validation = dataval_h2o,
                      testTrain = datatest_h2o,
                      datah2o = datah2o,
                      test = test_h2o)
  }
   
  # Resultados
  return(resultados)
}