modeladorR3 <- function(df_train, df_val, n_mod = 10, giga_ram = "4g", n_cores = -1,
                        particiones = c(0.6, 0.20), n_cv = 10, semilla = 123){
  
  # Datos
  datos_train_h2o = df_train
  datos_valid_h2o = df_val
  
  # ------------- GLM -------------------------- #
  
  # Ajuste del modelo y validación mediente 10-CV para estimar su error.
  modelo_glm <- h2o.glm(
    y = respuesta,
    x = predictores,
    training_frame = datos_train_h2o,
    validation_frame = datos_valid_h2o,
    family = "binomial",
    link = "logit",
    standardize = TRUE,
    ignore_const_cols = TRUE,
    missing_values_handling = "Skip",
    lambda = 0,
    solver = "AUTO",
    alpha = 0,
    seed = 123,
    nfolds = 10,
    fold_assignment = "Stratified",
    keep_cross_validation_predictions = FALSE,
    model_id = "modelo_glm"
  )
  
  # ------------------- GLM Grid: Lasso es el mejor -------------------------#
  
  ### GLM Grid
  hiper_grid_glm <- list(alpha = c(0, 1, 0.3, 0.5, 0.7, 0.9))
  
  grid_glm <- h2o.grid(
    algorithm = "glm",
    family = "binomial",
    link = "logit",
    y = respuesta,
    x = predictores,
    training_frame = datos_train_h2o,
    standardize = TRUE,
    missing_values_handling = "Skip",
    ignore_const_cols = TRUE,
    hyper_params = hiper_grid_glm,
    search_criteria = list(strategy = "Cartesian"),
    lambda_search = TRUE,
    solver = "AUTO",
    seed = 123,
    nfolds = 10,
    fold_assignment = "Stratified",
    keep_cross_validation_predictions = FALSE,
    grid_id = "grid_glm"
  )
  
  # Modelos ordenados de mayor a menor AUC
  resultados_grid_glm <- h2o.getGrid(grid_id = "grid_glm",
                                     sort_by = "auc",
                                     decreasing = TRUE)
  mod_grid_glm <- h2o.getModel(resultados_grid_glm@model_ids[[1]])
  
  # --------------------------- GBM ---------------------------- #
  modelo_gbm <- h2o.gbm(
    distribution = "bernoulli",
    y = respuesta,
    x = predictores,
    training_frame = datos_train_h2o,
    validation_frame = datos_valid_h2o,
    ntrees = 500,
    max_depth = 3,
    min_rows = 10,
    learn_rate = 0.01,
    score_tree_interval = 5,
    stopping_rounds = 3,
    stopping_metric = "AUC",
    stopping_tolerance = 0.001,
    model_id = "modelo_gbm",
    seed = 123,
    fold_assignment = "Stratified",
    keep_cross_validation_predictions = FALSE,
    nfolds = 10)
  
  # ------------------------ GBM Grid (48 modelos diferentes) ------------------------------------- #
  hyper_gbm <- list(max_depth   = c(5, 8, 10),
                    ntrees = c(100, 500, 1000, 2500),
                    learn_rate = c(0.001, 0.05),
                    min_rows = c(30, 60))

  grid_gbm <- h2o.grid(
    algorithm = "gbm",
    distribution = "bernoulli",
    y = respuesta,
    x = predictores,
    training_frame = datos_train_h2o,
    validation_frame = datos_valid_h2o,
    ignore_const_cols = TRUE,
    score_tree_interval = 10,
    stopping_rounds = 3,
    stopping_metric = "AUC",
    stopping_tolerance = 0.001,
    hyper_params = hyper_gbm,
    search_criteria = list(strategy = "Cartesian"),
    seed = 123,
    grid_id = "grid_gbm",
    fold_assignment = "Stratified",
    keep_cross_validation_predictions = FALSE,
    nfolds = 10)

  
  resultados_grid_gbm <- h2o.getGrid(grid_id = "grid_gbm",
                                     sort_by = "auc",
                                     decreasing = TRUE)
  
  modelo_gbm_grid <- h2o.getModel(resultados_grid_gbm@model_ids[[1]])
  
  # ------------ Random Forest Grid ---------------------------------------- #
  hyper_rf <- list(ntrees = seq(100, 1000, by = 250),
                   mtries = seq(3, 5, by = 1),
                   max_depth = seq(3, 6, by = 2))
  grid_rf <- h2o.grid(x = predictores, y = respuesta,
                      algorithm = "randomForest",
                      grid_id = "grid_rf",
                      nfolds = 5,
                      training_frame = datos_train_h2o,
                      validation_frame = datos_valid_h2o,
                      stopping_metric = "AUC",
                      hyper_params = hyper_rf,
                      search_criteria = list(strategy = "Cartesian"),
                      seed = 123)
  resultados_grid_rf <- h2o.getGrid(grid_id = "grid_rf",
                                    sort_by = "auc",
                                    decreasing = TRUE)
  
  modelo_rf_grid <- h2o.getModel(resultados_grid_rf@model_ids[[1]])
  
  return(list(mod_glm = modelo_glm,
              mod_gbm = modelo_gbm,
              mod_grid_glm = mod_grid_glm,
              mod_gid_gbm = modelo_gbm_grid,
              mod_grid_rf = modelo_rf_grid))
}