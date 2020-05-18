modeladorR1 <- function(df_train, n_mod = 10, giga_ram = "2g", n_cores = -1,
                        particiones = c(0.6, 0.20), n_cv = 10, tiempo_secs = 3600,
                        semilla = 123, ...){
  
  # Bibliotecas
  suppressWarnings(suppressMessages(library(h2o)))
  suppressWarnings(suppressMessages(library(tidyverse)))
  
  # Inicia h2o
  h2o.init(nthreads = n_cores, port = 54321, max_mem_size = giga_ram)
  
  #Ocultar progreso de h2o
  h2o.no_progress()
  
  # Editando df_train para volver a h2oFrame
  df_train = df_train %>% 
    mutate_if(is.character, as.factor) %>% 
    mutate_if(is.logical, as.factor) %>% 
    select(-id_app)
  
  # Datos h2o
  datos_h2o = as.h2o(x = df_train, destination_frame = "datos_h2o")
  
  particiones = h2o.splitFrame(data = datos_h2o, ratios = particiones,
                                seed = semilla)
  datos_train_h2o = h2o.assign(data = particiones[[1]], key = "datos_train_H2O")
  datos_val_h2o   = h2o.assign(data = particiones[[2]], key = "datos_val_H2O")
  datos_test_h2o  = h2o.assign(data = particiones[[3]], key = "datos_test_H2O")
  
  # A tipo factor la variable respuesta
  datos_train_h2o$resultado = h2o.asfactor(datos_train_h2o$resultado)
  datos_test_h2o$resultado  = h2o.asfactor(datos_test_h2o$resultado)
  
  # Se define la variable respuesta y los predictores.
  var_respuesta = "resultado"
  
  # Para este modelo se emplean todos los predictores disponibles.
  predictores   = setdiff(h2o.colnames(datos_train_h2o), var_respuesta)
  
  # Ajuste de modelos
  mod_aml = h2o.automl(
    x = predictores,
    y = var_respuesta,
    training_frame = datos_train_h2o,
    validation_frame = datos_val_h2o,
    nfolds = n_cv, 
    leaderboard_frame = datos_val_h2o,
    balance_classes = TRUE,
    stopping_metric = "AUC",
    sort_metric = "AUC",
    max_models = n_mod,
    seed = semilla,
    max_runtime_secs = tiempo_secs,
    ...)
  
  return(mod_aml) }