predictorR1 <- function(mod_h2o, n_cores = -1, giga_ram = "2g", import = FALSE,
                        df_test){
  
  # Datos
  datos = df_test
  
  # Bibliotecas
  suppressWarnings(suppressMessages(library(h2o)))
  suppressWarnings(suppressMessages(library(tidyverse)))
  suppressWarnings(suppressMessages(library(data.table)))
  
  # Inicio de H2o
  h2o.init(nthreads = n_cores, port = 54321, max_mem_size = giga_ram)
  h2o.no_progress()
  
  if(import == FALSE) {
    # Coercionando clases at?micas para compatibilidad con h2o
    df_test = datos %>%
      mutate_if(is.logical, as.factor)
    
    # Datos a h2o
    df_test_h2o = as.h2o(df_test)
    
    # Predicciones
    predicciones = h2o.predict(mod_h2o, newdata = df_test_h2o)
    predicciones = as.data.frame(predicciones) %>%
      select(predict, p1)
    predicciones = bind_cols(df_test, predicciones) %>%
      select(id_app, porcentaje_afinidad = p1,
             Supera_postulantes_y_atraccion_digital = predict) %>%
      mutate(porcentaje_afinidad = round(porcentaje_afinidad, digits = 5))
    
    return(predicciones)
  } else{
    # Importando datos
    df_test = fread(input = df_test, encoding = "UTF-8", sep = ",")
    
    # Coercionando clases at?micas para compatibilidad con h2o
    df_test = df_test %>%
      mutate_if(is.logical, as.factor)
    
    # Datos a h2o
    df_test_h2o = as.h2o(df_test)
    
    # Predicciones
    predicciones = h2o.predict(mod_h2o, newdata = df_test_h2o)
    predicciones = as.data.frame(predicciones) %>%
      select(predict, p1)
    predicciones = bind_cols(df_test, predicciones) %>%
      select(id_app, porcentaje_afinidad = p1,
             Supera_postulantes_y_atraccion_digital = predict) %>%
      mutate(porcentaje_afinidad = round(porcentaje_afinidad, digits = 5))
    
    return(predicciones)
  } }