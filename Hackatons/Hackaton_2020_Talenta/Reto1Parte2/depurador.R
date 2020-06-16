# #--------------- Argumentos de la función --------------------------- # #

#' @param data_candidate directorio donde se encuentra la base de datos candidatos. 
#' @param data_vacant directorio donde se encuentra la base de datos vacantes.
#' @param data_app directorio donde se encuentra la base de datos application.
#' @param data_target directorio donde se encuentra la nueva base de datos con target.

# #--------------------- FUNCIÓN DEPURADOR ---------------------------- # #

depurador <- function(data_candidate, data_vacant, data_app, data_target){
  
  # Bibliotecas
  suppressMessages(library(data.table))
  suppressMessages(library(tidyverse))
  suppressMessages(library(tm))
  
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
                        verbose = FALSE, na.strings=c("","NA", "[]"))
  
  ## Edición de nombres
  names(df_candidatos) = nombres_candidatos
  
  ## Depuración y Estructuración de Datos
  df_candidatos = df_candidatos %>%
    select(-c(num_id, email, phone, video, nombre, apellido, )) %>% 
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
           estudio_sena2 = if_else(estudio_sena > 0, true = "Si", false = "No"))
  
  ## Experiencia laboral registrada
  df_exper1 = df_candidatos %>% 
    select(id_candidato, experiencia) %>% 
    filter(!is.na(experiencia)) %>% 
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
    mutate(tiempo_promedio = round(exper_year/empresas, digits = 1),
           id_candidato = paste0("C", id_candidato)) %>% 
    rename(trabaja_actual = trabajo_actual2)
  
  ## Candidatos + Experiencia
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
  
  ## Depuración y estructuración de datos
  df_vacantes = df_vacantes %>% 
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
                              false = "Si")) %>% 
    select(-c(nombre, description, max_salary, estado_vacante,
              fecha_crea, fecha_expira))
  
  # # ---------------------- Stages ------------------------------- # #
  
  ## Ya no es necesario tener en cuenta esta base de datos.
  
  
  # #-------------------------------- Application ------------------- # #
  
  ## Cambiando nombres
  nombres_app <- c("id_app", "id_vacante", "id_candidato",
                   "fecha_creaApp", "estado_app", "tipo_descarto")
  
  ## Importando datos
  df_app = fread(data_app, sep = ",", encoding = "UTF-8",
                 verbose = FALSE)
  
  ## Editando nombres
  names(df_app) = nombres_app
  
  ## Depuración y estructuración de datos
  df_app = df_app %>% 
    select(-fecha_creaApp, -estado_app, -tipo_descarto) %>% 
    mutate(id_app = paste0("A", id_app),
           id_vacante = paste0("V", id_vacante),
           id_candidato = paste0("C", id_candidato))
  
  # #------------------------ ApplicationStage ----------------------- # #
  
  ## Ya no es necesario tener en cuenta esta base de datos.
  
  # #------------ Unión de datos -------------------------------------# #
  
  ## Juntando app y candidatos por "id_candidato" 
  ## Juntando df_vacantes por "id_vacante"
  datos1 = inner_join(df_app, df_candidatos) %>% 
    inner_join(df_vacantes)
  
  # #------------------------ Datos Train -------------------------- # #
  
  datos2 = datos1 %>% 
    mutate(edad = as.numeric(as.Date(fecha_publi) - as.Date(fecha_nac))/365.5,
           edad = round(edad, digits = 1),
           dif_salario = min_salary - salario,
           expectativa = if_else(dif_salario < 0, true = "Subestima",
                                 false = if_else(
                                   dif_salario == 0, true = "Cumple",
                                   false = "Sobreestima")),
           idiomas = if_else(idiomas == 0, true = "Nativo",
                             false = if_else(
                               idiomas == 1, true = "1 Idioma",
                               false = "2-4 Idiomas")),
           edad = as.numeric(edad)) 
  
  # #------------------------ Datos Predict  -------------------------- # #
  
  dataPredict = datos2 %>% 
    select(-c(id_vacante, id_candidato, fecha_nac, city_resid,
              perfil, profession, experiencia, psy_test, edu_informal,
              min_salary, fecha_publi)) %>% 
    mutate_if(is.character, as.factor) %>% 
    mutate(tipo_id = as.factor(tipo_id))
  
  dataPredict[dataPredict$cupos > 150] <- NA
  dataPredict[dataPredict$dif_salario > 5e+06] <- NA
  dataPredict[dataPredict$dif_salario < -5e+06] <- NA
  
  # #------------------------ Nuevos Datos: target ----------------- # #
  
  ## Importando datos
  newData <- fread(data_target,  sep = ",", encoding = "UTF-8")
  
  ## Cambiando nombres
  names(newData) <- c("id_app", "target")
  
  ## Editando variable id_app
  newData <- newData %>% 
    mutate(id_app = paste0("A", id_app)) %>% 
    select(id_app, target)
  
  ## Juntando datos (350337 registros)
  datos3 <- left_join(datos2, newData) %>% 
    filter(!is.na(target))
  
  ## Editando datos finales para train
  dataTrain <- datos3 %>% 
    select(-c(id_vacante, id_candidato, fecha_nac, city_resid,
              perfil, profession, experiencia, psy_test, edu_informal,
              min_salary, fecha_publi)) %>% 
    mutate_if(is.character, as.factor) %>% 
    mutate(tipo_id = as.factor(tipo_id))
  
  dataTrain[dataTrain$cupos > 150] <- NA
  dataTrain[dataTrain$dif_salario > 5e+06] <- NA
  dataTrain[dataTrain$dif_salario < -5e+06] <- NA
  
  # #---------------------- Retorno de la función ----------------- # #
  
  return(list(candidatos = df_candidatos, vacantes = df_vacantes,
              app = df_app, df_train = dataTrain, df_predict = dataPredict))
  
}