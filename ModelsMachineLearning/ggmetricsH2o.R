ggmetrics_h2o <- function(modelo_h2o, df_test, title_mod){
  
  # Bibliotecas
  suppressWarnings(suppressMessages(library(tidyverse)))
  suppressWarnings(suppressMessages(library(tibble)))
  suppressWarnings(suppressMessages(library(caret)))
  suppressMessages(suppressWarnings(library(pROC)))
  
  # Tema ggplot
  tema_gg1 = theme_light() +
    theme(strip.background = element_rect(fill = "black"),
          axis.text.x = element_text(color = "black"),
          axis.text.y = element_text(color = "black"),
          title = element_text(color = "black"),
          legend.position = "top")
  
  # Función para curva ROC
  auc_test <- function(modelo_h2o) {
    real_class = df_test %>% as.data.frame() %>% pull(resultado)
    prob_p1 = h2o.predict(modelo_h2o, df_test) %>% as.data.frame() %>% pull(p1)
    my_auc = roc(real_class, prob_p1)
    return(my_auc)
  }
  
  if (is.null(df_test)) {
    
    # Gráfico
    g = rownames_to_column(modelo_h2o@model$cross_validation_metrics_summary, "Metric") %>% 
      select(-mean, -sd) %>% 
      gather(key = "CrossV", value = "Value", -Metric) %>% 
      filter(Metric %in% c("accuracy", "auc", "logloss", "recall", "specificity",
                           "precision")) %>% 
      mutate(Value = as.numeric(Value),
             Metric = gsub("accuracy", "Accuracy", Metric),
             Metric = gsub("auc", "AUC", Metric),
             Metric = gsub("logloss", "Logloss", Metric),
             Metric = gsub("recall", "Recall", Metric),
             Metric = gsub("specificity", "specificity", Metric),
             Metric = gsub("precision", "Precision", Metric)) %>% 
      ggplot(data = ., aes(x = "", y = Value)) +
      facet_wrap(~Metric, scales = "free", ncol = 6) + 
      scale_y_continuous(labels = scales::percent) + 
      geom_boxplot(color = "black", fill = "#660066", alpha = 0.65) +
      labs(x = "", y = "Porcentaje",
           title = paste0("Performance en Train del modelo ", title_mod)) +
      tema_gg1
    
    # Retorno
    return(g)
    
  } else {
    
    # Matriz de confusión: 
    pred_class = h2o.predict(modelo_h2o, df_test) %>% as.data.frame() %>% pull(predict)
    real_class = df_test %>% as.data.frame() %>% pull(resultado)
    mtx_conf = confusionMatrix(pred_class, real_class, positive = "1")
    
    # ROC y AUC
    my_auc = auc_test(modelo_h2o)
    
    prueba = data_frame(TPR = my_auc$sensitivities, FPR = 1 - my_auc$specificities)
    
    gg = prueba %>%  
      ggplot(aes(x = FPR, ymin = 0, ymax = TPR))+
      geom_polygon(aes(y = TPR), fill = "#660066", alpha = 0.7)+
      geom_path(aes(y = TPR), col = "#660066", size = 1.3) +
      geom_abline(intercept = 0, slope = 1, color = "gray37",
                  size = 1, linetype = "dashed") + 
      theme_light() +
      coord_equal() +
      labs(x = "FPR (1 - Specificity)", 
           y = "TPR (Sensitivity)", 
           title = paste0("Performance en Test del modelo ", title_mod), 
           subtitle = paste0("AUC Value: ", my_auc$auc %>% round(2)))
    
    # Retorno
    return(list(confusion_mtx = mtx_conf,
                gg_roc = gg))
  }
  
}
