###Librerie---------------------------------------------------------------------
Sys.setenv(RETICULATE_PYTHON = "C:/Users/...") #Inserire il path per reticulate
library(Robyn)
library(reticulate)
Sys.setenv(R_FUTURE_FORK_ENABLE = "true")
options(future.fork.enable = TRUE)
library(readxl)
library(tidyverse)
library(timeDate)
library(here)
library(jsonlite)
library(lares)
library(parallel)
library(janitor)

### DATA DIRECTORY--------------------------------------------------------------
setwd("C:/Users/...")
i_am("scripts/MMM.R")

read_datasets_dir <- purrr::partial(here, "data/datasets")
input_dati_g <- "dati_giornalieri.csv"
input_dati_sett <- "dati_settimanali.csv"

####Festività------------------------------------------------------------------
data("dt_prophet_holidays")
force(dt_prophet_holidays)
get_holidays <- function(paese, start, end){
  data <- subset(dt_prophet_holidays, country == paese & year >= start & year <= end)
  return(data)
}
prophet_holidays <- as_tibble(get_holidays("IT", 2021, 2024))


###DATI GIORNALIERI-------------------------------------------------------------
robyn_directory_daily = paste0(here(), '/robyn_results/daily')

###DATI SETTIMANALI-------------------------------------------------------------
robyn_directory_weekly <- paste0(here(), '/robyn_results/weekly')
### Ultime modifiche del dataset (eventuali)------------------------------------
#Impostare: dati_sett_agg <- dati_sett se non si vuole cambiare il dataset
dati_sett <- read_csv(read_datasets_dir(input_dati_sett))
dati_sett_agg <- dati_sett %>%
  mutate(across(c(lead, vendite_auto, matches("_S"), matches("_I")), ~ {
    x_std <- scale(.)
    x_std + abs(min(x_std, na.rm = TRUE))
  }))

####Adstock geometrica----------------------------------------------------------
######Input---------------------------------------------------------------------
Input_geom <- robyn_inputs(
  dt_input = dati_sett_agg,
  #dt_holidays = prophet_holidays,
  date_var = "Date",
  dep_var = "lead",
  dep_var_type = "revenue",
  prophet_vars = c("trend", "season", "monthly"),
  context_vars = c("tasso_interesse", "unemp_rate", "vendite_auto"),
  paid_media_spends = c("bing_S", "criteo_S", "meta_S_nuovo", "meta_S_usato", "meta_S_awareness", "google_S_nuovo", "google_S_usato", "google_S_awareness"),
  paid_media_vars = c("bing_S", "criteo_I", "meta_I_nuovo", "meta_I_usato", "meta_I_awareness", "google_I_nuovo", "google_I_usato", "google_I_awareness"),
  window_start = "2021-12-26",
  window_end = "2024-10-20",
  adstock = "geometric"
)

#######Hyperparametri-----------------------------------------------------------
hyperpar_geom <- list(
  meta_S_nuovo_alphas = c(0.5, 3),
  meta_S_nuovo_gammas = c(0.3, 1),
  meta_S_nuovo_thetas = c(0, 0.4),
  meta_S_awareness_alphas = c(0.5, 3),
  meta_S_awareness_gammas = c(0.3, 1),
  meta_S_awareness_thetas = c(0, 0.4),
  meta_S_usato_alphas = c(0.5, 3),
  meta_S_usato_gammas = c(0.3, 1),
  meta_S_usato_thetas = c(0, 0.4),
  criteo_S_alphas = c(0.5, 3),
  criteo_S_gammas = c(0.3, 1),
  criteo_S_thetas = c(0, 0.4),
  google_S_nuovo_alphas = c(0.5, 3),
  google_S_nuovo_gammas = c(0.3, 1),
  google_S_nuovo_thetas = c(0, 0.4),
  google_S_usato_alphas = c(0.5, 3),
  google_S_usato_gammas = c(0.3, 1),
  google_S_usato_thetas = c(0, 0.4),
  google_S_awareness_alphas = c(0.5, 3),
  google_S_awareness_gammas = c(0.3, 1),
  google_S_awareness_thetas = c(0, 0.4),
  bing_S_alphas = c(0.5, 3),
  bing_S_gammas = c(0.3, 1),
  bing_S_thetas = c(0, 0.4),
  #organic_alphas = c(0.5, 3),
  #organic_gammas = c(0.3, 1),
  #organic_thetas = c(0, 0.4),
  train_size = c(0.69, 0.71)
)
Input_geom <- robyn_inputs(InputCollect = Input_geom, hyperparameters = hyperpar_geom)

########Decomposizione Prophet--------------------------------------------------
prophet_decomp(Input_geom, date_origin = Input_geom$dt_input$Date[1])
prophet_vs_depvar(Input_geom, variabili_prophet = c("trend", "season", "monthly"), 
                  date_origin = Input_geom$dt_input$Date[1], giornalieri = F)

######Run modello---------------------------------------------------------------
Models_geom <- robyn_run(
  InputCollect = Input_geom,
  cores = 6,
  iterations = 10000,
  trials = 10,
  ts_validation = TRUE
  #add_penalty_factor = TRUE
)

######Output modello------------------------------------------------------------
OutputCollect_geom <- robyn_outputs(
  Input_geom, Models_geom,
  pareto_fronts = "auto",
  min_candidates = 150,
  csv_out = "pareto",
  clusters = TRUE,
  export = TRUE,
  plot_folder = paste0(robyn_directory_weekly, '/', Input_geom$adstock),
  plot_pareto = TRUE,
  cluster_by = "hyperparameters",
  k = "auto",
  #max_clusters = 10,
  limit = 1,
  weights = c(1, 1, 0),
  dim_red = "tSNE", 
  baseline_level = 4
)

######Modello scelto------------------------------------------------------------
select_model <- "3_527_4" 
ExportedModel <- robyn_write(Input_geom, OutputCollect_geom, select_model, export = T)
print(ExportedModel)
file_path = paste0(OutputCollect_geom[["plot_folder"]], '/RobynModel-', select_model, '.json')
metriche(file_path = file_path)

#######Grafici modello----------------------------------------------------------
pareto_alldecomp_matrix = paste0(OutputCollect_geom[["plot_folder"]], '/pareto_alldecomp_matrix.csv')
actual_vs_pred(file_path = file_path,
               date_origin = Input_geom$dt_input$Date[1], giornalieri = F)
grafico_cumulativo(file_decomp_matrix =  pareto_alldecomp_matrix,
                    select_model, giornalieri = F, variabili_prophet = c("trend", "season", "holiday"), variabili_organic = "organic")
adstock_plots(file_path = file_path)
hill_funs(file_path = file_path)

######Budget allocator-----------------------------------------------------------
geom_allocator <- robyn_allocator(
  InputCollect = Input_geom,
  OutputCollect = OutputCollect,
  select_model = select_model,
  channel_constr_up = 3,
  scenario = "max_response",
  export = create_files
)

####Adstock Weibull CDF---------------------------------------------------------
######Input---------------------------------------------------------------------
Input_wcdf <- robyn_inputs(
  dt_input = dati_sett_agg,
  dt_holidays = prophet_holidays,
  date_var = "Date",
  dep_var = "lead",
  dep_var_type = "revenue",
  prophet_vars = c("season", "monthly"),
  context_vars = c("vendite_auto", "tasso_inflazione", "unemp_rate"),
  #organic_vars = c("organic"),
  paid_media_spends = c("google_S_nuovo", "google_S_usato", "google_S_awareness", "meta_S_nuovo", "meta_S_usato", "meta_S_awareness", "criteo_S", "bing_S"),
  paid_media_vars = c("google_I_nuovo","google_I_usato", "google_I_awareness", "meta_I_nuovo", "meta_I_usato", "meta_I_awareness", "criteo_I", "bing_I"),
  window_start = "2021-12-26",
  window_end = "2024-10-20",
  adstock = "weibull_cdf"
)

#######Hyperparametri-----------------------------------------------------------
hyperpar_weibcdf <- list(
  meta_S_nuovo_alphas = c(0.5, 3),
  meta_S_nuovo_gammas = c(0.3, 1),
  meta_S_nuovo_shapes = c(0, 3),
  meta_S_nuovo_scales = c(0, 0.1),
  meta_S_awareness_alphas = c(0.5, 3),
  meta_S_awareness_gammas = c(0.3, 1),
  meta_S_awareness_shapes = c(0, 3),
  meta_S_awareness_scales = c(0, 0.1),
  meta_S_usato_alphas = c(0.5, 3),
  meta_S_usato_gammas = c(0.3, 1),
  meta_S_usato_shapes = c(0, 3),
  meta_S_usato_scales = c(0, 0.1),
  google_S_nuovo_alphas = c(0.5, 3),
  google_S_nuovo_gammas = c(0.3, 1),
  google_S_nuovo_shapes = c(0, 3),
  google_S_nuovo_scales = c(0, 0.1),
  google_S_awareness_alphas = c(0.5, 3),
  google_S_awareness_gammas = c(0.3, 1),
  google_S_awareness_shapes = c(0, 3),
  google_S_awareness_scales = c(0, 0.1),
  google_S_usato_alphas = c(0.5, 3),
  google_S_usato_gammas = c(0.3, 1),
  google_S_usato_shapes = c(0, 3),
  google_S_usato_scales = c(0, 0.1),
  criteo_S_alphas = c(0.5, 3),
  criteo_S_gammas = c(0.3, 1),
  criteo_S_shapes = c(0, 3),
  criteo_S_scales = c(0, 0.1),
  bing_S_alphas = c(0.5, 3),
  bing_S_gammas = c(0.3, 1),
  bing_S_shapes = c(0, 3),
  bing_S_scales = c(0, 0.1),
  #organic_alphas = c(0.5, 3),
  #organic_gammas = c(0.3, 1),
  #organic_shapes = c(0, 3),
  #organic_scales = c(0, 0.1),
  train_size = c(0.69, 0.71)
)
Input_wcdf <- robyn_inputs(InputCollect = Input_wcdf, hyperparameters = hyperpar_weibcdf)

########Decomposizione Prophet--------------------------------------------------
prophet_decomp(Input_wcdf, date_origin = Input_wcdf$dt_input$Date[1])
prophet_vs_depvar(Input_wcdf, variabili_prophet = c("trend", "season", "monthly"), 
                  date_origin = Input_wcdf$dt_input$Date[1], giornalieri = F)

######Run modello---------------------------------------------------------------
Models_wcdf <- robyn_run(
  InputCollect = Input_wcdf,
  cores = 6,
  iterations = 8000,
  trials = 7,
  ts_validation = TRUE,
  add_penalty_factor = TRUE,
  #objective_weights = c(2, 1)
)
Models_wcdf$convergence$moo_distrb_plot

######Output modello------------------------------------------------------------
OutputCollect_cdf <- robyn_outputs(
  Input_wcdf, Models_wcdf,
  pareto_fronts = "auto",
  min_candidates = 150,
  csv_out = "pareto",
  clusters = TRUE,
  export = TRUE,
  plot_folder = paste0(robyn_directory_weekly, '/', Input_wcdf$adstock),
  plot_pareto = TRUE,
  cluster_by = "hyperparameters",
  k = "auto",
  #max_clusters = 10,
  limit = 1,
  weights = c(1, 1, 0),
  dim_red = "PCA", 
  baseline_level = 4
)

######Modello scelto------------------------------------------------------------
select_model <- "5_996_5" 
ExportedModel <- robyn_write(Input_wcdf, OutputCollect_cdf, select_model, export = T)
print(ExportedModel)
file_path = paste0(OutputCollect_cdf[["plot_folder"]], '/RobynModel-', select_model, '.json')
metriche(file_path = file_path)

#######Grafici modello----------------------------------------------------------
pareto_alldecomp_matrix = paste0(OutputCollect_cdf[["plot_folder"]], '/pareto_alldecomp_matrix.csv')
actual_vs_pred(file_path = file_path,
               date_origin = Input_wcdf$dt_input$Date[1], giornalieri = F)
grafico_cumulativo(file_decomp_matrix =  pareto_alldecomp_matrix,
                    select_model, giornalieri = F, baseline_level = T)
adstock_plots(file_path = file_path)
hill_funs(file_path = file_path)

######Budget allocator----------------------------------------------------------
wcdf_allocator(
  InputCollect = Input_wcdf,
  OutputCollect = OutputCollect_cdf,
  select_model = select_model,
  channel_constr_up = 3,
  scenario = "max_response",
  export = create_files
)

####Adstock Weibull PDF---------------------------------------------------------
######Input---------------------------------------------------------------------
Input_wpdf <- robyn_inputs(
  dt_input = dati_sett_agg,
  dt_holidays = prophet_holidays,
  date_var = "Date",
  dep_var = "lead",
  dep_var_type = "revenue",
  prophet_vars = c("season", "monthly"),
  context_vars = c("tasso_interesse", "unemp_rate", "vendite_auto"),
  context_signs = c("default", "negative", "positive"),
  paid_media_spends = c("criteo_S", "meta_S_nuovo", "meta_S_usato", "meta_S_awareness", "google_S_nuovo", "google_S_usato", "google_S_awareness"),
  paid_media_vars = c("criteo_I", "meta_I_nuovo", "meta_I_usato", "meta_I_awareness", "google_I_nuovo", "google_I_usato", "google_I_awareness"),
  window_start = "2021-12-26",
  window_end = "2024-10-20",
  adstock = "weibull_pdf"
)
#######Hyperparametri-----------------------------------------------------------
hyperpar_weibpdf <- list(
  meta_S_nuovo_alphas = c(0.5, 3),
  meta_S_nuovo_gammas = c(0.3, 1),
  meta_S_nuovo_shapes = c(0.0001, 10),
  meta_S_nuovo_scales = c(0, 0.1),
  meta_S_awareness_alphas = c(0.5, 3),
  meta_S_awareness_gammas = c(0.3, 1),
  meta_S_awareness_shapes = c(0.0001, 10),
  meta_S_awareness_scales = c(0, 0.1),
  meta_S_usato_alphas = c(0.5, 3),
  meta_S_usato_gammas = c(0.3, 1),
  meta_S_usato_shapes = c(0.0001, 10),
  meta_S_usato_scales = c(0, 0.1),
  google_S_nuovo_alphas = c(0.5, 3),
  google_S_nuovo_gammas = c(0.3, 1),
  google_S_nuovo_shapes = c(0.0001, 10),
  google_S_nuovo_scales = c(0, 0.1),
  google_S_awareness_alphas = c(0.5, 3),
  google_S_awareness_gammas = c(0.3, 1),
  google_S_awareness_shapes = c(0.0001, 10),
  google_S_awareness_scales = c(0, 0.1),
  google_S_usato_alphas = c(0.5, 3),
  google_S_usato_gammas = c(0.3, 1),
  google_S_usato_shapes = c(0.0001, 10),
  google_S_usato_scales = c(0, 0.1),
  criteo_S_alphas = c(0.5, 3),
  criteo_S_gammas = c(0.3, 1),
  criteo_S_shapes = c(0.0001, 10),
  criteo_S_scales = c(0, 0.1),
  #bing_S_alphas = c(0.5, 3),
  #bing_S_gammas = c(0.3, 1),
  #bing_S_shapes = c(0.0001, 10),
  #bing_S_scales = c(0, 0.1),
  #organic_alphas = c(0.5, 3),
  #organic_gammas = c(0.3, 1),
  #organic_shapes = c(0.0001, 10),
  #organic_scales = c(0, 0.1),
  train_size = c(0.83, 0.85)
)
Input_wpdf <- robyn_inputs(InputCollect = Input_wpdf, hyperparameters = hyperpar_weibpdf)

########Decomposizione Prophet--------------------------------------------------
prophet_decomp(Input_wpdf)
prophet_vs_depvar(Input_wpdf, variabili_prophet = c("season", "monthly"), giornalieri = F)

######Run modello---------------------------------------------------------------
Models_wpdf <- robyn_run(
  InputCollect = Input_wpdf,
  cores = 6,
  iterations = 10000,
  trials = 10,
  ts_validation = TRUE,
  #add_penalty_factor = TRUE,
  #objective_weights = c(3, 1)
)
Models_wpdf$convergence$moo_distrb_plot

######Output modello------------------------------------------------------------
OutputCollect_pdf <- robyn_outputs(
  Input_wpdf, Models_wpdf,
  pareto_fronts = "auto",
  min_candidates = 150,
  csv_out = "pareto",
  clusters = TRUE,
  export = TRUE,
  plot_folder = paste0(robyn_directory_weekly, '/', Input_wpdf$adstock),
  plot_pareto = TRUE,
  cluster_by = "hyperparameters",
  k = "auto",
  #max_clusters = 10,
  limit = 1,
  weights = c(1, 1, 0),
  dim_red = "PCA", 
  baseline_level = 4
)

######Modello scelto------------------------------------------------------------
select_model <- "10_9741_1"
ExportedModel <- robyn_write(Input_wpdf, OutputCollect_pdf, select_model)
print(ExportedModel)
file_path = paste0(OutputCollect_pdf[["plot_folder"]], 'RobynModel-', select_model, '.json')
metriche(file_path = file_path)

#######Grafici modello----------------------------------------------------------
pareto_alldecomp_matrix = paste0(OutputCollect_pdf[["plot_folder"]], '/pareto_alldecomp_matrix.csv')
actual_vs_pred(file_path = file_path,
               date_origin = Input_wpdf$dt_input$Date[1], giornalieri = F)
grafico_cumulativo(file_decomp_matrix =  pareto_alldecomp_matrix,
                    select_model, giornalieri = F, baseline_level = T)
adstock_plots(Input_wpdf, file_path = file_path)
hill_funs(Input_wpdf, file_path = file_path)

######Budget allocator----------------------------------------------------------
Allocator <- robyn_allocator(
  InputCollect = Input_wpdf,
  OutputCollect = OutputCollect_pdf,
  select_model = select_model,
  channel_constr_low = 0.1,
  channel_constr_up = 2,
  channel_constr_multiplier = 3,
  scenario = "max_response",
  maxeval = 150000
)
