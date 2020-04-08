depuradorR1 <- function(data_candidate, data_vacant, data_stage, data_app,
                        data_appS1 = FALSE, data_appS2){
  
  # Bibliotecas
  suppressMessages(library(data.table))
  suppressMessages(library(tidyverse))
  suppressMessages(library(tm))
  suppressMessages(library(Hmisc))
  
  # Etiquetas del nivel de educación en "candidates" y "vacants"
  profesional = c("Profesional", "Especialización/ Maestría", "Doctorado")
  tecnico     = c("Tecnólogo", "Técnico")
  secundaria  = c("Bachillerato (grados 6°, 7° u 8°)",
                  "Bachillerato (grados 9°, 10° y 11°)",
                  "Bachillerato completo")
  
  
  # #--------------- Candidatos -------------------------------------- # #
  
  ## Nombres de datasets "candidates"
  nombres_candidatos = c("id_candidato", "email", "nombre", "apellido", "phone",
                          "fecha_nac", "gender", "tipo_id", "num_id",
                          "pais_nac", "city_resid", "education_level",
                          "salario", "perfil", "reg_exper", "reg_estud",
                          "profession", "viajar", "est_civil", "video",
                          "estudios", "experiencia", "psy_test")
  
  ## Lectura de datos
  df_candidatos = fread(data_candidate, sep = ",", encoding = "UTF-8",
                         verbose = FALSE)
  
  ## Edición de nombres
  names(df_candidatos) = nombres_candidatos
  
  ## Mediana (exponencial) del logaritmo de la aspiración salarial
  mediana_salary =  df_candidatos %>%
    filter(!is.na(salario)) %>% 
    pull(salario) %>% 
    log() %>% 
    median(na.rm = TRUE) %>% 
    exp()
  
  ## Depuración y Estructuración de Datos
  df_candidatos = df_candidatos %>%
    select(-c(num_id, -phone, -video)) %>% 
    mutate(id_candidato = paste0("C", id_candidato),
           perfil = trimws(trimws(x = perfil, which = "l"), which = "r"),
           n_letras = nchar(perfil),
           education = if_else(education_level %in% profesional, true = "Profesional",
                               false = if_else(
                                 education_level %in% tecnico, true = "Tecnology",
                                 false = if_else(
                                   education_level %in% secundaria, true = "Secundaria",
                                   false = "Otro"))),
           ingles = grepl("Inglés|Ingles", perfil, ignore.case = TRUE),
           frances = grepl("Francés|Frances", perfil, ignore.case = TRUE),
           aleman = grepl("Alemán|Aleman", perfil, ignore.case = TRUE),
           portugues = grepl("Portugués|Portugues", perfil, ignore.case = TRUE),
           italiano = grepl("Italiano", perfil, ignore.case = TRUE),
           idiomas = ingles+frances+aleman+portugues+italiano,
           estudios = tolower(estudios),
           gender = if_else(gender %in% c("", "other_gender", "unkown"),
                            true = "Otro", false = gender),
           pais_nac = if_else(pais_nac == "Colombia",
                            true = "Colombia", false = "Extranjero"),
           est_civil = if_else(est_civil %in% c("", "divorced", "windower"),
                            true = "Otro", false = est_civil)) %>%
    replace_na(list(viajar = "No indica",
                    salario = mediana_salary)) %>%  
    group_by(id_candidato) %>% 
    mutate(instituciones = length(str_extract_all(estudios, "institute")[[1]]),
           cursos = length(str_extract_all(estudios, "curso")[[1]]),
           diplomados = length(str_extract_all(estudios, "diplomado")[[1]]),
           universidad = length(str_extract_all(estudios, "universidad")[[1]]),
           edu_informal = length(str_extract_all(estudios, "informal_education")[[1]]),
           manejo_excel = length(str_extract_all(estudios, "excel")[[1]]),
           manejo_word = length(str_extract_all(estudios, "word")[[1]]),
           manejo_office = length(str_extract_all(estudios, "office")[[1]]),
           estudio_sena = length(str_extract_all(estudios, "sena")[[1]])) %>% 
    select(-estudios) %>% 
    ungroup() %>% 
    mutate(edu_informal2 = if_else(edu_informal > 0, true = "Si", false = "No"),
           manejo_excel = if_else(manejo_excel > 0, true = "Si", false = "No"),
           manejo_word = if_else(manejo_word > 0, true = "Si", false = "No"),
           manejo_office = if_else(manejo_office > 0, true = "Si", false = "No"),
           estudio_sena2 = if_else(estudio_sena > 0, true = "Si", false = "No")) %>% 
    select(-c(email, nombre, apellido, phone, profession, video,
              psy_test, tipo_id, city_resid, education_level, perfil))
  
  ## Experiencia laboral registrada
  df_exper1 = df_candidatos %>% 
    select(id_candidato, experiencia) %>% 
    filter(experiencia != "[]") %>% 
    mutate(empresas = str_count(experiencia, "company")) %>% 
    group_by(id_candidato) %>%
    mutate(trabajo_actual1 = removePunctuation(experiencia,
                                               preserve_intra_word_dashes = TRUE),
           trabajo_actual2 = if_else(grepl("atpresentfalse", trabajo_actual1),
                                     true = "No",
                                     false = if_else(
                                       grepl("atpresenttrue", trabajo_actual1),
                                       true = "Si",
                                       false = "Otro")),
           fechas = str_extract_all(experiencia, "\\d{4}-\\d{2}-\\d{2}"),
           fechas2 = as.character(fechas),
           fechas3 = removePunctuation(fechas2, preserve_intra_word_dashes = TRUE),
           fechas4 = trimws(gsub("c", "", fechas3), "l")) %>% 
    ungroup() %>% 
    separate(fechas4, into = paste0("F", 1:20), sep = " ") %>% 
    select(-c(fechas, fechas2, fechas3)) %>% 
    replace_na(list(empresa = 0,
                    trabajo_actual2 = "No indica"))
  
  df_exper2 = df_exper1 %>% 
    select(-c(experiencia, trabajo_actual1)) %>% 
    gather(key = "fecha_id", value = "fecha", -c(id_candidato, empresas, trabajo_actual2)) %>% 
    mutate(fecha = as.Date(fecha)) %>% 
    group_by(id_candidato) %>% 
    summarise(max_fecha = max(fecha, na.rm = TRUE),
              min_fecha = min(fecha, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(exper_year = as.numeric(max_fecha - min_fecha)/365.5) %>% 
    select(-max_fecha, -min_fecha)
  
  df_exper3 = df_exper1 %>% 
    select(id_candidato, empresas, trabajo_actual2) %>% 
    inner_join(df_exper2, by = "id_candidato") %>% 
    mutate(tiempo_promedio = round(exper_year/empresas, digits = 1)) %>% 
    rename(trabaja_actual = trabajo_actual2)
  
  # Candidatos + Experiencia
  df_candidatos = left_join(df_candidatos, df_exper3)
  
  # #--------------- Vacantes -------------------------------------- # #
  
  ## Nombres de datasets "vacantes"
  nombres_vacantes = c("id_vacante", "nombre", "description", "tipo_salario", "min_salary",
                       "max_salary", "estado_vacante", "fecha_crea", "empresa", "edu_min",
                       "acuerdo_salary", "requisitos", "fecha_publi", "confiden",
                       "fecha_expira", "cargos_simil", "habilidades", "estudios",
                       "cupos")
  
  ## Lectura de datos
  df_vacantes = fread(data_vacant, sep = ",", encoding = "UTF-8",
                       verbose = FALSE)
  
  ## Editando nombres de vacantes
  names(df_vacantes) = nombres_vacantes
  
  ## Empresas con menos de 30 registros (vacantes)
  otras_empresas = df_vacantes %>% 
    group_by(empresa) %>% 
    count() %>% 
    ungroup() %>% 
    filter(n <= 30) %>% 
    pull(empresa)
  
  # Depuración y estructuración de datos
  df_vacantes = df_vacantes %>% 
    select(id_vacante, tipo_salario, min_salary, fecha_publi, empresa,
           edu_min, cargos_simil, requisitos, habilidades, estudios) %>% 
    mutate(id_vacante = paste0("V", id_vacante),
           edu_min = if_else(edu_min == "",
                             true = "No indica",
                             false = edu_min),
           edu_min = if_else(edu_min %in% profesional, true = "Profesional",
                             false = if_else(
                               edu_min %in% tecnico, true = "Tecnology",
                               false = if_else(
                                 edu_min %in% secundaria, true = "Secundaria",
                                 false = "Otro"))),
           empresa = if_else(empresa %in% otras_empresas,
                             true = "Otra",
                             false = empresa),
           cargos_simil = if_else(cargos_simil == "",
                                  true = "No indica",
                                  false = "Si"),
           requisitos = if_else(requisitos == "",
                                true = "No indica",
                                false = "Si"),
           habilidades = if_else(habilidades == "",
                                 true = "No indica",
                                 false = "Si"),
           estudios = if_else(estudios == "",
                              true = "No indica",
                              false = "Si"))
  
  # # ---------------------- Stages ------------------------------- # #
  
  ## Nombres de variables en el datasets "Stages"
  nombres_stage = c("id_etapa", "nombre", "envia_sms", "envia_email",
                     "envia_phone", "tipo_etapa", "id_vacante", "stage_order")
  
  ## Importando datos
  df_stage = fread(data_stage, sep = ",", encoding = "UTF-8",
                    verbose = FALSE)
  
  ## Editando nombres
  names(df_stage) = nombres_stage
  
  ## Depuración y estructuración
  df_stage = df_stage %>% 
    mutate(id_etapa = paste0("E", id_etapa),
           id_vacante = paste0("V", id_vacante)) %>% 
    select(id_etapa, id_vacante, tipo_etapa, nombre, stage_order)
  
  # #-------------------------------- Application ------------------- # #
  # Cambiando nombres
  nombres_app <- c("id_app", "id_vacante", "id_candidato",
                   "fecha_creaApp", "estado_app", "tipo_descarto")
  
  # Importando datos
  df_app = fread(data_app, sep = ",", encoding = "UTF-8",
                  verbose = FALSE)
  
  # Editando nombres
  names(df_app) = nombres_app
  
  # Depuración y estructuración de datos
  df_app = df_app %>% 
    #slice(1:10) %>% 
    select(-fecha_creaApp, -estado_app, -tipo_descarto) %>% 
    mutate(id_app = paste0("A", id_app),
           id_vacante = paste0("V", id_vacante),
           id_candidato = paste0("C", id_candidato))
  
  if (data_appS1 == TRUE) {
    # #------------------------ ApplicationStage ----------------------- # #
    
    # Cambiando nombres
    nombres_appS <- c("id_app_stage", "id_app", 
                      "id_etapa", "fecha_crea_appS", "estado_appS")
    
    # Importando datos
    df_app_stage <- fread(data_appS2, sep = ",", encoding = "UTF-8",
                          verbose = FALSE)
    
    # Editando nombres
    names(df_app_stage) = nombres_appS
    
    # Depuración y estructuración de datos
    df_app_stage = df_app_stage %>% 
      mutate(id_app = paste0("A", id_app),
             id_etapa = paste0("E", id_etapa)) %>% 
      select(id_app, id_app, estado_appS, id_etapa)
    
    # #------------ Unión de datos -------------------------------------# #
    
    ## Juntando app y candidatos por "id_candidato" 
    ## Juntando df_vacantes por "id_vacante"
    datos1 = inner_join(df_app, df_candidatos) %>% 
      inner_join(df_vacantes)
    
    # Jutando stage2 con df_app_stage2 por "id_etapa" y datos2
    datos2 = inner_join(df_app_stage, df_stage) %>% 
      inner_join(datos1)
    
    # #------------ Datos para modelos ---------------------------------# #
    
    ## Datos completos train
    datos3 = datos2 %>% 
      filter(tipo_etapa %in% c(0, 1)) %>% 
      mutate(edad = as.numeric(as.Date(fecha_publi) - as.Date(fecha_nac))/365.5,
             edad = round(edad, digits = 1)) %>% 
      group_by(id_candidato) %>% 
      mutate(max_etapa = max(stage_order, na.rm = TRUE)) %>% 
      ungroup() %>% 
      filter(stage_order == max_etapa,
             edad >= 15 & edad <= 60 | is.na(edad),
             exper_year <= 30) %>% 
      distinct(id_candidato, .keep_all = TRUE) %>% 
      mutate(resultado = if_else(estado_appS == "accepted",
                                 true = 1,
                                 false = 0),
             dif_salario = min_salary - salario,
             expectativa = if_else(dif_salario < 0, true = "Subestima",
                                   false = if_else(
                                     dif_salario == 0, true = "Cumple",
                                     false = "Sobreestima")),
             edad = impute(edad, mean),
             salario = if_else(salario <= 0, true = 0, false = salario),
             idiomas = if_else(idiomas == 0, true = "Nativo",
                               false = if_else(
                                 idiomas == 1, true = "1 Idioma",
                                 false = "2-4 Idiomas")),
             edad = as.numeric(edad)) %>% 
      replace_na(list(empresas = 0,
                      trabaja_actual = "Otro",
                      exper_year = 0,
                      tiempo_promedio = 0))
    
    # Datos con variables para modelos train
    datos4 = datos3 %>% 
      select(-c(max_etapa, estado_appS, tipo_etapa, stage_order,
                min_salary, fecha_publi, experiencia, id_etapa, id_vacante,
                nombre, id_candidato, fecha_nac, ingles:italiano))
    
    return(list(candidatos = df_candidatos, vacantes = df_vacantes,
                etapa = df_stage, app = df_app, appS = df_app_stage,
                df_total = datos3, df_train = datos4))
  }
  else {
    # #------------ Unión de datos -------------------------------------# #
    
    ## Juntando app y candidatos por "id_candidato" 
    ## Juntando df_vacantes por "id_vacante"
    datos1 = inner_join(df_app, df_candidatos) %>% 
      inner_join(df_vacantes)
    
    # Juntando stage2 con df_app_stage2 por "id_etapa" y datos2
    datos2 = inner_join(df_stage, datos1)
    
    # #------------ Datos para modelos ---------------------------------# #
    
    ## Datos completos test
    datos3 = datos2 %>% 
      mutate(edad = as.numeric(as.Date(fecha_publi) - as.Date(fecha_nac))/365.5,
             edad = round(edad, digits = 1)) %>% 
      group_by(id_candidato) %>% 
      mutate(max_etapa = max(stage_order, na.rm = TRUE)) %>% 
      ungroup() %>% 
      distinct(id_candidato, .keep_all = TRUE) %>% 
      mutate(dif_salario = min_salary - salario,
             expectativa = if_else(dif_salario < 0, true = "Subestima",
                                   false = if_else(
                                     dif_salario == 0, true = "Cumple",
                                     false = "Sobreestima")),
             edad = impute(edad, mean),
             salario = if_else(salario <= 0, true = 0, false = salario),
             idiomas = if_else(idiomas == 0, true = "Nativo",
                               false = if_else(
                                 idiomas == 1, true = "1 Idioma",
                                 false = "2-4 Idiomas")),
             edad = as.numeric(edad)) %>% 
      replace_na(list(empresas = 0,
                      trabaja_actual = "Otro",
                      exper_year = 0,
                      tiempo_promedio = 0))
    
    # Datos con variables para modelos test
    datos4 = datos3 %>% 
      select(-c(max_etapa, tipo_etapa, stage_order,
                min_salary, fecha_publi, experiencia, id_etapa, id_vacante,
                nombre, id_candidato, fecha_nac, ingles:italiano))
    
    return(list(candidatos = df_candidatos, vacantes = df_vacantes,
                etapa = df_stage, app = df_app, df_total = datos3,
                df_test = datos4))
  }
}

