# ================= Función generatorH2o ======================================== #

#' @param cores número de núcleos a utilizar. Por defecto es -1.
#' @param puerto puerto para conexión de h2o. Por defecto es 54321 (localhost).
#' @param memRAM máximo de memoria ram a utilizar. Por defecto es igual a 2g.
#' @ 
generatorH2o <- function(cores = -1, puerto = 54321, memRAM = "2g"){
  
  # Bibliotecas
  library(h2o)
  
  # Iniciando H2o
  h2o.init(nthreads = cores, port = puerto, max_mem_size = memRAM)
} 