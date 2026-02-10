#Grafici Prophet----------------------------------------------------------------
prophet_decomp <- function(InputCollect, date_origin = InputCollect$dt_input$Date[1]){
  dt_plotProphet <- InputCollect$dt_mod %>%
    select(c("ds", "dep_var", InputCollect$prophet_vars, InputCollect$factor_vars)) %>%
    tidyr::gather("variable", "value", -ds) %>%
    mutate(ds = as.Date(.data$ds, origin = date_origin))
  
  ggplot(
    dt_plotProphet, aes(x = .data$ds, y = .data$value)) +
    geom_line(color = "steelblue", size = 1) +
    facet_wrap(~ .data$variable, scales = "free", ncol = 1) +
    labs(title = "Prophet decomposition", x = NULL, y = NULL) +
    theme_lares(background = "white", ) +
    scale_y_abbr()
}

prophet_vs_depvar <- function(InputCollect, variabili_prophet, date_origin = InputCollect$dt_input$Date[1], giornalieri = T){
  # Calcola la somma delle variabili presenti nel dataset
  InputCollect$dt_mod <- InputCollect$dt_mod %>% 
    mutate(decomp_var = rowSums(select(., one_of(variabili_prophet)))) %>% 
    rename(lead = dep_var,
           prophet_var = decomp_var)
  # Crea il grafico
  InputCollect$dt_mod %>%
    select(ds, lead, prophet_var) %>%
    tidyr::gather("variable", "value", -ds) %>%
    mutate(ds = as.Date(.data$ds, origin = date_origin)) %>%
    ggplot(aes(x = ds, y = value, color = variable, size = variable)) + # Mappatura aggiunta
    geom_line() +
    scale_color_manual(values = c("lead" = "#f0076fff", "prophet_var" = "#073763ff")) +
    scale_size_manual(values = c("lead" = 1, "prophet_var" = 0.5)) +
    labs(title = "Lead vs prophet_var",
         subtitle = ifelse(giornalieri, 
                           "Daily Data", 
                           "Weekly Data"), 
         x = "Data", y = "Valore") +
    theme_lares(background = "white") +
    theme(
      plot.title = element_text(
        family = "sans",
        face = "bold",  
        color = "black",
        size = 14
      ),
      plot.subtitle = element_text(
        family = "sans",
        face = "italic",
        color = "gray30",
        size = 13
      )
    )
}

#Metriche modello---------------------------------------------------------------
metriche <- function(file_path){
  mmm_model = robyn_read(json=file_path)
  InputCollect = mmm_model$InputCollect
  OutputModel = robyn_run(json=file_path, quiet = TRUE, export = FALSE)
  temp = OutputModel$xDecompAgg %>% 
    slice(1)
  model_metrics <- paste0(
    "Adj.R²: train = ", temp$rsq_train, 
    ", val = ", temp$rsq_val, 
    ", test = ", temp$rsq_test, 
    " | NRMSE: train = ", temp$nrmse_train,
    ", val = ", temp$nrmse_val,
    ", test = ", temp$nrmse_test
  )
  print(model_metrics)
}

#Actual vs Predicted------------------------------------------------------------
actual_vs_pred <- function(file_path, date_origin = InputCollect$dt_input$Date[1], giornalieri = T){
  OutputModel = robyn_run(json=file_path, quiet = TRUE, export = FALSE)
  temp = OutputModel$allPareto$plotDataCollect
  dynamic_name <- names(temp)[1]
  
  xDecompVecPlotMelted <- temp[[dynamic_name]]$plot5data$xDecompVecPlotMelted %>%
    mutate(
      linetype = ifelse(.data$variable != "actual", "solid", "dotted"),
      variable = stringr::str_to_title(.data$variable),
      ds = as.Date(.data$ds, origin = date_origin)
    )
  ggplot(
    xDecompVecPlotMelted,
    aes(x = .data$ds, y = .data$value, color = .data$variable, linetype = .data$linetype)) +
    scale_color_manual(values = c("Actual" = "deeppink2", "Predicted" = "#073763ff")) +
    geom_path(linewidth = 0.6) +
    labs(title = "Actual vs. Predicted Response",
         subtitle = ifelse(giornalieri, 
                           "Daily Data", 
                           "Weekly Data"),
         x = "Data", y = "Valore") +
    theme_lares(background = "white", legend = "top") +
    theme(
      plot.title = element_text(
        family = "sans",
        face = "bold",  
        color = "black",
        size = 14,      
      ),
      plot.subtitle = element_text(
        family = "sans",
        face = "italic",
        color = "gray30",
        size = 13,      
      )
    ) +
    guides(linetype = "none")
}

#Adstock Plot-------------------------------------------------------------------
adstock_plots <- function(InputCollect, file_path){
  OutputModel = robyn_run(json=file_path, quiet = TRUE, export = FALSE)
  temp = OutputModel$allPareto$plotDataCollect
  weibullCollect <- temp[[select_model]]$plot3data$weibullCollect
  wb_type <- temp[[select_model]]$plot3data$wb_type
  ggplot(weibullCollect, aes(x = .data$x, y = .data$decay_accumulated)) +
    geom_line(aes(color = .data$channel)) +
    facet_wrap(~ .data$channel) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
    geom_text(aes(x = max(.data$x), y = 0.5, vjust = -0.5, hjust = 1, label = "Halflife"), colour = "gray") +
    theme_lares(background = "white", legend = "none", grid = "Xx") +
    labs(
      title = paste("Weibull", wb_type, "Adstock: Flexible Rate Over Time"),
      x = sprintf("Time unit [%ss]", InputCollect$intervalType), y = NULL
    )
}


#Curve si saturazione-----------------------------------------------------------
hill_funs <- function(InputCollect, file_path){
  json_data = fromJSON(file_path)
  raw_data = json_data$Extras$raw_data
  OutputModel = robyn_run(json=file_path, quiet = TRUE, export = FALSE)
  temp = OutputModel$allPareto$plotDataCollect
  dynamic_name <- names(temp)[1] 
  dt_scurvePlot <- temp[[dynamic_name]]$plot4data$dt_scurvePlot
  dt_scurvePlotMean <- temp[[dynamic_name]]$plot4data$dt_scurvePlotMean
  trim_rate <- 1.3 #Parametro per lo zoom (aumentare per zommare)
  dt_scurvePlot <- dt_scurvePlot %>%
    filter(
      .data$spend < max(dt_scurvePlotMean$mean_spend_adstocked) * trim_rate,
      .data$response < max(dt_scurvePlotMean$mean_response) * trim_rate,
      .data$channel %in% InputCollect$paid_media_spends) %>%
    left_join(
      dt_scurvePlotMean[, c("channel", "mean_carryover")], "channel")
  
  if (!"channel" %in% colnames(dt_scurvePlotMean)) {
    dt_scurvePlotMean$channel <- dt_scurvePlotMean$rn
  }
  ggplot(
    dt_scurvePlot, aes(x = .data$spend, y = .data$response, color = .data$channel)) +
    geom_line() +
    geom_area(
      data = group_by(dt_scurvePlot, .data$channel) %>% filter(.data$spend <= .data$mean_carryover),
      aes(x = .data$spend, y = .data$response, color = .data$channel),
      stat = "identity", position = "stack", linewidth = 0.1,
      fill = "grey50", alpha = 0.4, show.legend = FALSE) +
    geom_point(data = dt_scurvePlotMean, aes(
      x = .data$mean_spend_adstocked, y = .data$mean_response, color = .data$channel)) +
    geom_text(
      data = dt_scurvePlotMean, aes(
        x = .data$mean_spend_adstocked, y = .data$mean_response, color = .data$channel,
        label = formatNum(.data$mean_spend_adstocked, 2, abbr = TRUE)),
      show.legend = FALSE, hjust = -0.2) +
    theme_lares(background = "white", pal = 2) +
    theme(
      legend.position.inside = c(0.9, 0.2),
      legend.background = element_rect(fill = alpha("grey98", 0.6), color = "grey90")) +
    labs(
      title = "Response Curves and Mean Spends by Channel",
      x = "Spend (carryover + immediate)", y = "Response", color = NULL) +
    scale_y_abbr() +
    scale_x_abbr()
}


#Grafico cumulativo-------------------------------------------------------------
grafico_cumulativo <- function(file_decomp_matrix, select_model, giornalieri = T, 
                                altre_variabili = NULL, #Vettore stringa con nome di variabili (contenute in decomp_matrix)
                                baseline_level = FALSE #Se TRUE somma le variabili prophet
                                ){
  variabili_prophet <- c("trend", "season", "weekday", "monthly", "holiday")
  data_model_output <- read_csv(file_decomp_matrix) %>%
    select(-top_sol) %>% 
    filter(solID == select_model)
  
  if (!is.null(altre_variabili)) {
    data_model_output <- data_model_output %>%
      select(ds, matches("_S"), matches(variabili_prophet), matches(altre_variabili))
  } else {
    data_model_output <- data_model_output %>%
      select(ds, matches("_S"), matches(variabili_prophet))
  }
  
  if (baseline_level) {
    data_model_output <- data_model_output %>%
      mutate(baseline = rowSums(across(matches(variabili_prophet), ~ .))) %>%
      pivot_longer(
        cols = -c(ds, matches(variabili_prophet)), 
        names_to = "cost_type", 
        values_to = "cost_value")
  } else {
    data_model_output <- data_model_output %>%
      pivot_longer(
        cols = -ds, 
        names_to = "cost_type", 
        values_to = "cost_value")
  }
  
    ggplot(data_model_output, aes(x = ds, y = cost_value, fill = cost_type)) +
    geom_bar(stat = "identity", width = 7, position = "stack") +
    scale_fill_manual(values = c(colorRampPalette(c("#f0076fff", "#073763ff"))(14))) +
    labs(title = "Lead e decomp_var",
         subtitle = ifelse(giornalieri, 
                           "Daily Data", 
                           "Weekly Data"), 
         x = "Date", y = "Cost Value", fill = "Cost Type") +
    theme_lares(background = "white", grid = TRUE) +
    theme(
      plot.title = element_text(
        family = "sans",
        face = "bold",  
        color = "black",
        size = 14,      
      ),
      plot.subtitle = element_text(
        family = "sans",
        face = "italic",
        color = "gray30",
        size = 13,      
      )
    )
}
