modeladorTRM <- function(ts_trm, frecuencia = 365, fecha1 = "2020-03-21", fecha2 = "2020-04-21"){
  
  
  # Bibliotecas
  suppressWarnings(suppressMessages(library(tidyverse)))
  suppressWarnings(suppressMessages(library(RColorBrewer)))
  suppressWarnings(suppressMessages(library(forecast)))
  suppressWarnings(suppressMessages(library(tseries)))
  suppressWarnings(suppressMessages(library(prophet)))
  suppressWarnings(suppressMessages(library(lubridate)))
  suppressWarnings(suppressMessages(library(readxl)))
  suppressWarnings(suppressMessages(library(openxlsx)))
  suppressWarnings(suppressMessages(library(plotly)))
  
  # Tema ggplot
  tema_gg1 = theme_light() +
    theme(strip.background = element_rect(fill = "black"),
          axis.text.x = element_text(color = "black"),
          axis.text.y = element_text(color = "black"),
          title = element_text(color = "black"),
          legend.position = "top")
  
  # Fechas a pronosticar
  fechas = seq.Date(from = as.Date(fecha1), to = as.Date(fecha2), by = "day")
  
  # Serie TRM
  trm = read_xlsx(ts_trm, skip = 7)
  trm = trm %>% slice(1:10347)
  names(trm) = c("Fecha", "TRM")
  trm = na.omit(trm)
  serie_ts = ts(trm$TRM, frequency = frecuencia, start = c(1991, 11))
  
  # Ajuste de modelos
  mod_hw = HoltWinters(x = serie_ts)
  mod_pro = prophet(trm %>% rename(ds = Fecha, y = TRM), changepoint.prior.scale = 0.001,
                    interval.width = 0.95)
  mod_arima = auto.arima(y = serie_ts, stepwise = FALSE, approximation = FALSE)
  mod_naive = snaive(y = serie_ts)
  mod_nnet = nnetar(y = serie_ts, size = 10, scale.inputs = TRUE)
  
  # Forecasting
  forecast_hw <- as.data.frame(forecast(mod_hw, h = length(fechas))) %>% 
    select(`Point Forecast`) %>% pull(`Point Forecast`)
  
  forecast_pro <- predict(object = mod_pro,
                          df = make_future_dataframe(mod_pro, periods = length(fechas),
                                                     include_history = FALSE)) %>% 
    select(trend) %>% pull(trend)
  
  forecast_arima <-  as.data.frame(forecast(mod_arima, h = length(fechas))) %>% 
    select(`Point Forecast`) %>% pull(`Point Forecast`)
  
  forecast_naive <- as.data.frame(forecast(mod_naive, h = length(fechas))) %>% 
    select(`Point Forecast`) %>% pull(`Point Forecast`) 
  
  forecast_nnet <- as.data.frame(forecast(mod_nnet, h = length(fechas))) %>% 
    select(`Point Forecast`) %>% pull(`Point Forecast`)
  
  # Dataframe
  pronosticos = data.frame(
    Fecha = fechas,
    HoltWinters = forecast_hw,
    Prophet = forecast_pro,
    Arima = forecast_arima,
    Naive = forecast_naive,
    NNet = forecast_nnet
  )
  
  # Promedios de modelos
  promedio = apply(pronosticos[, -1], 1, mean)
  pronosticos$Stack = promedio
  
  # Gráfico con pronósticos
  gg <- ggplotly(
    pronosticos %>% 
      gather(key = "variable", value = "valor", -Fecha) %>% 
      ggplot(data = ., aes(x = Fecha, y = valor, color = variable)) +
      facet_wrap(~variable, scales = "free") +
      geom_line() +
      scale_color_brewer(palette = "Set1") +
      labs(title = "Pronósticos de 6 modelos para TRM",
           subtitle = "21-03-2020 a 21-04-2020") +
      tema_gg1
  )
  
  # Retorno
  return(list(pronosticos = pronosticos,
              forecast_gg = gg))}