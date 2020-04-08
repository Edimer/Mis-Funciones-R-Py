depuradorR3 <- function(train, test, n_cores = -1, giga_ram = "2g",
                        particiones = c(0.6, 0.2), semilla = 123){
  # Bibliotecas
  suppressWarnings(suppressMessages(library(data.table)))
  suppressWarnings(suppressMessages(library(tidyverse)))
  suppressWarnings(suppressMessages(library(h2o)))
  suppressWarnings(suppressMessages(library(Hmisc)))
  
  # Vector de nombres
  nombres = c("id_candidato", "fecha_ingreso", "fecha_retiro", "tipo_contrato",
              "clasification", "causa_retiro", "motivo_retiro", "RESPA",
              "RESPC", "RESPE", "RESPF", "RESPH", "RESPQ", "RESPS", "EscalaA",
              "EscalaC", "EscalaE", "EscalaF", "EscalaH", "EscalaQ",
              "EscalaS", "ITPC")
  
  # #----------------------- Train -------------------------------------------- #
  
  # Importando datos y editando nombre
  df_train = fread(train, sep = ",", encoding = "UTF-8")
  names(df_train) = nombres
  
  # Depuración y estructuración train
  df_train = df_train %>% 
    select(-c(causa_retiro, motivo_retiro)) %>%
    mutate(tiempo = as.Date(fecha_retiro, format = "%d/%m/%Y") - as.Date(fecha_ingreso, format = "%d/%m/%Y"),
           tiempo = as.numeric(tiempo),
           tipo_contrato = recode(tipo_contrato,
                                  "Termino Fijo < 3 Meses" = "TFijo_Menos3M",
                                  "Termino fijo >= 3 Meses" = "TFijo_Mayor3M"),
           RESPC = fct_recode(RESPC, "No indica" = ""),
           RESPF = fct_recode(RESPF, "No indica" = ""),
           RESPQ = fct_recode(RESPQ, "No indica" = ""),
           EscalaA = fct_recode(EscalaA, "No indica" = ""),
           EscalaE = fct_recode(EscalaE, "No indica" = ""),
           EscalaH = fct_recode(EscalaH, "No indica" = ""),
           EscalaS = fct_recode(EscalaS, "No indica" = ""),
           ITPC = fct_recode(ITPC, "No indica" = ""),
           RESPA = as.numeric(impute(RESPA, median)),
           RESPE = as.numeric(impute(RESPE, median)),
           RESPH = as.numeric(impute(RESPH, median)),
           RESPS = as.numeric(impute(RESPS, median)),
           EscalaC = as.numeric(impute(EscalaC, median)),
           EscalaF = as.numeric(impute(EscalaF, median)),
           EscalaQ = as.numeric(impute(EscalaQ, median)),
           tipo_contrato = factor(tipo_contrato)) %>% 
    mutate(resultado = if_else(tiempo >= 90 | clasification == "Activo",
                               true = 0, false = 1)) %>% 
    select(-c(fecha_ingreso, fecha_retiro, clasification, tiempo, id_candidato))
  
  # #----------------------- Test -------------------------------------------- #
  
  # Importando datos y editando nombre
  df_test= fread(test, sep = ",", encoding = "UTF-8")
  names(df_test) = nombres
  
  # Depuración y estructuración test
  df_test = df_test %>% 
    select(-c(causa_retiro, motivo_retiro)) %>%
    mutate(tipo_contrato = recode(tipo_contrato,
                                  "Termino Fijo < 3 Meses" = "TFijo_Menos3M",
                                  "Termino fijo >= 3 Meses" = "TFijo_Mayor3M"),
           RESPC = fct_recode(RESPC, "No indica" = ""),
           RESPF = fct_recode(RESPF, "No indica" = ""),
           RESPQ = fct_recode(RESPQ, "No indica" = ""),
           EscalaA = fct_recode(EscalaA, "No indica" = ""),
           EscalaE = fct_recode(EscalaE, "No indica" = ""),
           EscalaH = fct_recode(EscalaH, "No indica" = ""),
           EscalaS = fct_recode(EscalaS, "No indica" = ""),
           ITPC = fct_recode(ITPC, "No indica" = ""),
           RESPA = as.numeric(impute(RESPA, median)),
           RESPE = as.numeric(impute(RESPE, median)),
           RESPH = as.numeric(impute(RESPH, median)),
           RESPS = as.numeric(impute(RESPS, median)),
           EscalaC = as.numeric(impute(EscalaC, median)),
           EscalaF = as.numeric(impute(EscalaF, median)),
           EscalaQ = as.numeric(impute(EscalaQ, median)),
           tipo_contrato = factor(tipo_contrato)) %>% 
    select(-c(fecha_ingreso, fecha_retiro, clasification))
  
  # #---------------------- H2o --------------------------------------- # #
  
  # Iniciando h2o
  h2o.init(nthreads = n_cores, port = 54321, max_mem_size = giga_ram)
  
  # Ocultar progreso de h2o
  h2o.no_progress()
  
  # Datos a h2oFrame
  datos_h2o = as.h2o(x = df_train, destination_frame = "datos_h2o")
  datos_nuevos = as.h2o(x = df_test, destination_frame = "datos_nuevos_h2o")
  
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
  
  return(list(df_train = df_train, df_test = df_test,
              df_h2o_train = datos_train_h2o,
              df_h2o_val = datos_val_h2o,
              df_h2o_test = datos_test_h2o,
              df_h2o_nuevos = datos_nuevos,
              respuesta = var_respuesta,
              predictores = predictores))
}