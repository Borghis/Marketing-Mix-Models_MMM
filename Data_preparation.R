###Librerie---------------------------------------------------------------------
library(readxl)
library(tidyverse)
library(timeDate)
library(eurostat)
library(istat)
library(zoo)
library(reshape2)
library(here)
library(forecast)

### DATA DIRECTORY--------------------------------------------------------------
setwd("C:/Users/...")
i_am("scripts/Data_preparation.R")
read_datasets_dir <- purrr::partial(here, "data/datasets")
read_contextvars_dir <- purrr::partial(here, "data/context_vars")

### IMPORT DATASETS ------------------------------------------------------------
# Read files
input_file <- ".xlsx"
input_lead_agg <- "lead.xlsx"
input_pcarburanti <- "prezzi_carburanti.csv"
input_tasso_disoccupazione <- "tasso_disoccupazione.csv"
input_tasso_inflazione <- "tasso_inflazione.csv"
input_tasso_interesse <- "tasso_interesse.csv"
input_imm_auto <- "imm_auto.csv"
input_vendita_auto1 <- "Prime, usato netto e rad.ni categ.mese 2022 IT.csv"
input_vendita_auto2 <- "Prime, usato netto e rad.ni categ.mese 2023 IT.csv"
output_dati_giornalieri <- "dati_giornalieri.csv"
output_dati_settimanali <- "dati_settimanali.csv"

###Fun: get_date_interval-------------------------------------------------------
get_date_interval <- function(dati){
  intervallo_date <- data.frame(
    inizio = as.Date(min(dati$Date)),
    fine = as.Date(max(dati$Date)),
    stringsAsFactors = FALSE
  )
  return(intervallo_date)
}

### Import dati e variabili contesto--------------------------------------------
#### Dataset iniziale-----------------------------------------------------------
dati <- read_xlsx(read_contextvars_dir(input_file))
dati <- dati %>% rename(Date = Data,
                        lead = Lead)
dati <- dati %>% arrange(desc(Date))

####Variabili contesto----------------------------------------------------------
#####Immatricolazione auto------------------------------------------------------
imm_auto <- read_csv(read_contextvars_dir(input_imm_auto))

#####Tasso di disoccupazione----------------------------------------------------
get_unemrate <- function(start, end, paese = "IT", intervallo_anni = "TOTAL", sesso = "T", correzione_stag = "NSA"){
  data <- get_eurostat(id = "une_rt_m", time_format = "date")
  data <- data[data$geo == paese & data$age == intervallo_anni & data$sex == sesso & data$s_adj == correzione_stag &
                 data$unit == "PC_ACT", ]
  data <- subset(data, TIME_PERIOD >= as.Date(start) & TIME_PERIOD <= as.Date(end))
  data <- data %>% select(TIME_PERIOD, values)
  data <- data %>% rename(Date = TIME_PERIOD, unemp_rate = values)
  return(data)
}
#unempl_rate <- get_unemrate(start = "2021-01-01", end = "2024-11-01")
#write.csv(unempl_rate, "/Users/...tasso_disoccupazione.csv", row.names = FALSE)
unempl_rate <- read.csv(read_contextvars_dir(input_tasso_disoccupazione))
unempl_rate$Date <- as.Date(unempl_rate$Date)
unempl_rate$Date <- as.Date(cut(unempl_rate$Date, breaks = "month"))

#####Prezzo carburanti----------------------------------------------------------
prezzo_compb <- read.csv(read_contextvars_dir(input_pcarburanti))
str(prezzo_compb)
prezzo_compb$Date <- as.Date(prezzo_compb$Date)
prezzo_compb$p_benzina <- as.numeric(gsub(",", ".", gsub("\\.", "", prezzo_compb$p_benzina)))
prezzo_compb$p_gasolio <- as.numeric(gsub(",", ".", gsub("\\.", "", prezzo_compb$p_gasolio)))
intervallo_date <- get_date_interval(prezzo_compb)

#####Tasso di inflazione--------------------------------------------------------
tasso_inflazione <- read.csv(read_contextvars_dir(input_tasso_inflazione))
tasso_inflazione <- tasso_inflazione %>% mutate(Indice.Dal = as.numeric(str_replace(Indice.Dal, ",", ".")),
                                                Indice.Al = as.numeric(str_replace(Indice.Al, ",", ".")))
tasso_inflazione <- bind_rows(tasso_inflazione %>% 
                                select(Periodi, Indice.Dal) %>% 
                                rename(Date = Periodi, 
                                       tasso_inflazione = Indice.Dal), 
                              tasso_inflazione %>% 
                                select(Periodi, Indice.Al) %>% 
                                rename(Date = Periodi, 
                                       tasso_inflazione = Indice.Al))
tasso_inflazione <- tasso_inflazione[-c(22:30),]
tasso_inflazione <- tasso_inflazione %>% 
  mutate(Date = seq(as.Date("2022-01-01"), as.Date("2024-09-01"), by = "month"))

ts_inflazione <- ts(tasso_inflazione$tasso_inflazione, start = c(2022, 1), frequency = 12)
# Stima il modello ARIMA
modello_arima <- auto.arima(ts_inflazione)
previsione <- forecast(modello_arima, h = 1)

tasso_inflazione <- tasso_inflazione %>%
  add_row(Date = as.Date("2024-10-01"), tasso_inflazione = as.numeric(previsione$mean))
tasso_inflazione <- tasso_inflazione %>% arrange(desc(Date))

#####Vendita autovetture--------------------------------------------------------
auto1 <- read.csv(read_contextvars_dir(input_vendita_auto1))
auto2 <- read.csv(read_contextvars_dir(input_vendita_auto2))
auto2$Quantità <- as.numeric(gsub(",", "", auto2$Quantità))
vendite_auto <- bind_rows(auto1, auto2)
vendite_auto$Mese <- gsub("[^A-Za-z]", "", vendite_auto$Mese)
mesi <- c("GENNAIO", "FEBBRAIO", "MARZO", "APRILE", "MAGGIO", "GIUGNO", 
          "LUGLIO", "AGOSTO", "SETTEMBRE", "OTTOBRE", "NOVEMBRE", "DICEMBRE")
vendite_auto <- vendite_auto %>%
  filter(
    Formalità %in% c("PASSAGGI DI PROPRIETA' AL NETTO DELLE MINIVOLTURE", "PRIME ISCRIZIONI"),
    Categoria %in% c("AUTOVETTURA", "AUTOVETTURE")
  ) %>%
  mutate(
    Mese_num = match(Mese, mesi),
    Date = as.Date(paste(Anno, Mese_num, "01", sep = "-"), format = "%Y-%m-%d")
  ) %>%
  group_by(Date) %>%
  summarise(vendite_auto = sum(Quantità)) %>%
  ungroup()
vendite_auto <- vendite_auto %>% arrange(desc(Date))

#####Tasso di interesse---------------------------------------------------------
tasso_interesse <- read.csv(read_contextvars_dir(input_tasso_interesse))
str(tasso_interesse)
tasso_interesse$Date <- as.Date(tasso_interesse$Date)
intervallo_date <- bind_rows(intervallo_date, get_date_interval(tasso_interesse))

###Dataset di appoggio per le variabili contesto--------------------------------
Date <- seq(max(intervallo_date$inizio), 
            as.Date("2024-10-31"), by = "day")
app_data <- data.frame(Date)
app_data <- app_data %>% arrange(desc(Date))
app_data <- app_data %>% left_join(unempl_rate, by = "Date")
app_data <- app_data %>%
  fill(unemp_rate, .direction = "down")
app_data$unemp_rate[which(is.na(app_data$unemp_rate))] = unempl_rate$unemp_rate[length(unempl_rate$unemp_rate)]

app_data <- app_data %>% left_join(prezzo_compb %>% select(Date, p_benzina, p_gasolio), by = "Date")
app_data <- app_data %>%
  fill(c(p_benzina, p_gasolio), .direction = "down")
app_data$p_benzina[which(is.na(app_data$p_benzina))] = prezzo_compb$p_benzina[length(prezzo_compb$p_benzina)]
app_data$p_gasolio[which(is.na(app_data$p_gasolio))] = prezzo_compb$p_gasolio[length(prezzo_compb$p_gasolio)]

app_data <- app_data %>% left_join(tasso_inflazione, by = "Date")
app_data <- app_data %>%
  fill(tasso_inflazione, .direction = "down")
app_data$tasso_inflazione[is.na(app_data$tasso_inflazione)] = tasso_inflazione$tasso_inflazione[1]

app_data <- app_data %>% left_join(vendite_auto, by = "Date")
app_data <- app_data %>%
  fill(vendite_auto, .direction = "up") %>% 
  mutate(vendite_auto = round(vendite_auto / 30))

app_data <- app_data %>% left_join(tasso_interesse, by = "Date")
app_data <- app_data %>%
  fill(tasso_interesse, .direction = "up")
app_data$tasso_interesse[is.na(app_data$tasso_interesse)] = 0.00

app_data <- app_data %>% left_join(imm_auto, by = "Date")
app_data <- app_data %>%
  fill(imm_auto, .direction = "up") %>% 
  mutate(imm_auto = round(imm_auto / 30))
app_data$imm_auto[which(is.na(app_data$imm_auto))] = imm_auto$imm_auto[length(imm_auto$imm_auto)]

###Dati campagne----------------------------------------------------------------
#!!!Runnare "Data_campaigns.R" per ottenere i dataset delle campagne!!!

####Aggregazione al dataset giornaliero finale----------------------------------
dati_g <- dati
dati_g <- dati_g %>%
  left_join(organic, by = "Date")
dati_g <- dati_g %>%
  left_join(app_data, by = "Date")
dati_g <- dati_g %>%
  left_join(meta_campagna, by = "Date")%>%
  left_join(google_campagna, by = "Date")%>%
  left_join(bing_campagna, by = "Date")%>%
  left_join(criteo_campagna, by = "Date")

intervallo_date <- bind_rows(intervallo_date, get_date_interval(google_campagna))
intervallo_date <- bind_rows(intervallo_date, get_date_interval(meta_campagna))
intervallo_date <- bind_rows(intervallo_date, get_date_interval(bing_campagna))

dati_g <- dati_g %>%
  filter(Date >= as.Date(max(intervallo_date$inizio)) & Date <= as.Date(min(intervallo_date$fine)))

dati_g <- dati_g %>%
  mutate_all(~replace(., is.na(.), 0))

#Esportazione dataset giornaliero
write_csv(dati_g, read_datasets_dir(output_dati_giornalieri))
###Analisi grafica dei dati-----------------------------------------------------
####Fun: normalize & get_plots--------------------------------------------------
normalize <- function(x){ #funzione per riscalare le variabili
  return((x - min(x)) / (max(x) - min(x)))
}

get_plots <- function(dati, giornalieri = TRUE, normalizza = TRUE) {
  # Normalizzare i dati?
  if (normalizza) {
    # Normalizza solo le variabili numeriche, esclusa la variabile Date
    dati[sapply(dati, is.numeric)] <- lapply(dati[sapply(dati, is.numeric)], normalize)
  }
  # Crea il data frame per il plotting
  dts <- data.frame(Date = as.Date(sort(dati$Date)), 
                    zoo(dati %>% select(-Date) %>% as.matrix(), 
                        dati$Date))
  # Riformatta i dati per ggplot
  dts <- melt(dts, id.vars = "Date", variable.name = "Serie", value.name = "Valore")
  # Crea il grafico
  colori <- c("black", "#f0076fff", "#073763ff", "#7f7f7fff", "purple")
  
  plots <- ggplot(dts, aes(x = Date, y = Valore, color = Serie)) +
    geom_line(aes(linewidth = ifelse(Serie == "lead", 1, 0.5))) +  # Linea più spessa per "lead"
    scale_color_manual(values = colori) +  # Colori personalizzati per tutte le serie
    scale_linewidth_identity(guide = "none") +
    labs(title = ifelse(normalizza, 
                        "Confronto tra lead e variabili spesa (normalizzate)", 
                        "Confronto tra lead e variabili spesa"),
         subtitle = ifelse(giornalieri, 
                           "Dati giornalieri", 
                           "Dati settimanali"),
         x = "Data", y = "Valori") +
    theme_lares(background = "white") +
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
  return(plots)
}
####Revenue vs var. contesto----------------------------------------------------
#####Correlazione---------------------------------------------------------------
library(RColorBrewer)
corrplot::corrplot(cor(dati_g %>% select(lead, 
                                         unemp_rate,
                                         p_benzina,
                                         p_gasolio,
                                         vendite_auto,
                                         tasso_inflazione,
                                         tasso_interesse,
                                         imm_auto)),
                   col = colorRampPalette(c("#f0076fff", "white", "#073763ff"))(200),
                   type = "lower", diag = F,
                   method = "circle" , title = "\n\nCorrelazione lead vs. variabili contesto\n(dati giornalieri)", 
                   addCoef.col = "black", tl.col="black", tl.srt=90)
#####Grafico delle serie temporali----------------------------------------------
get_plots(data.frame(Date = dati_g$Date,
                                dati_g %>% #dataset fittizio per ottenere i dati normalizzati
                                  select(lead, #Inserire sotto le variabili di confronto
                                         unemp_rate, tasso_inflazione, tasso_interesse, vendite_auto)), normalizza = T)

####Revenue vs var. spesa-------------------------------------------------------
#####Correlazione---------------------------------------------------------------
corrplot::corrplot(cor(dati_g %>% select(lead, 
                                         meta_S_awareness,
                                         meta_S_nuovo,
                                         meta_S_usato,
                                         google_S_awareness,
                                         google_S_nuovo,
                                         google_S_usato,
                                         bing_S,
                                         criteo_S)), 
                   col = colorRampPalette(c("#f0076fff", "white", "#073763ff"))(200),
                   type = "lower", diag = F,
                   method = "circle" , title = "\n\nCorrelazione lead vs. variabili spesa\n(dati giornalieri)", 
                   addCoef.col = "black", tl.col="black", tl.srt=90)
#####Grafico delle serie temporali----------------------------------------------
get_plots(data.frame(Date = dati_g$Date,
                     dati_g %>%
                       select(lead,
                              google_S_nuovo, google_S_usato, google_S_awareness)), normalizza = T)
get_plots(data.frame(Date = dati_g$Date,
                     dati_g %>%
                       select(lead,
                              meta_S_nuovo, meta_S_usato, meta_S_awareness)), normalizza = T)
get_plots(data.frame(Date = dati_g$Date,
                     dati_g %>%
                       select(lead,
                              criteo_S, bing_S)), normalizza = T)

###Dati settimali---------------------------------------------------------------
dati_sett <- dati_g %>%
  group_by(Date = floor_date(Date, "week")) %>%
  summarize(
    across(c(unemp_rate, p_benzina, p_gasolio, tasso_inflazione, tasso_interesse), mean),
    across(where(is.numeric) & !c(unemp_rate, p_benzina, p_gasolio, tasso_inflazione,
                                  tasso_interesse), sum)
  )
dati_sett <- dati_sett %>% arrange(desc(Date))
dati_sett <-  dati_sett %>% mutate(across(-Date, as.double))

####Gestione ultima e prima settimana incompleta--------------------------------
ultima_settimana <- dati_sett$Date[NROW(dati_sett)]  # Settimana incompleta (prima o ultima)
giorni_ultima_settimana <- n_distinct(dati_g %>% filter(floor_date(Date, "week") == ultima_settimana) %>% pull(Date))
# Calcola il coefficiente per la settimana precedente
#k <- (7 - giorni_ultima_settimana) / 7  # Ponderazione per la settimana precedente
#dati_sett[NROW(dati_sett), 6:ncol(dati_sett)] <- dati_sett[NROW(dati_sett), 6:ncol(dati_sett)] * 1 + 
#  dati_sett[NROW(dati_sett)-1, 6:ncol(dati_sett)] * k
dati_sett[NROW(dati_sett), 6:ncol(dati_sett)] <- dati_sett[NROW(dati_sett) - 1, 6:ncol(dati_sett)]

prima_settimana <- dati_sett$Date[1]
giorni_prima_settimana <- n_distinct(dati_g %>% filter(floor_date(Date, "week") == prima_settimana) %>% pull(Date))
k <- (7 - giorni_prima_settimana) / 7
dati_sett[1, 6:ncol(dati_sett)] <- dati_sett[1, 6:ncol(dati_sett)] * 1 + 
  dati_sett[2, 6:ncol(dati_sett)] * k

#Esporto i dati settimanali
#write_csv(dati_sett, read_datasets_dir(output_dati_settimanali))
####Revenue vs var. contesto----------------------------------------------------
#####Correlazione---------------------------------------------------------------
corrplot::corrplot(cor(dati_sett %>% select(lead, 
                                            unemp_rate,
                                            p_benzina,
                                            p_gasolio,
                                            vendite_auto,
                                            tasso_inflazione,
                                            tasso_interesse,
                                            imm_auto)), 
                   col = colorRampPalette(c("#f0076fff", "white", "#073763ff"))(200),
                   type = "lower", diag = F,
                   method = "circle" , title = "\n\nCorrelazione lead vs. variabili contesto\n(dati settimanali)", 
                   addCoef.col = "black", tl.col="black", tl.srt=90)
#####Grafico delle serie temporali----------------------------------------------
get_plots(data.frame(Date = dati_sett$Date,
                                dati_sett %>%
                                  select(lead,
                                         unemp_rate, tasso_inflazione, tasso_interesse, vendite_auto)), giornalieri = F, normalizza = T)

####Revenue vs var. spesa-------------------------------------------------------
#####Correlazione---------------------------------------------------------------
corrplot::corrplot(cor(dati_sett %>% select(lead, 
                                            meta_S_awareness,
                                            meta_S_nuovo,
                                            meta_S_usato,
                                            google_S_awareness,
                                            google_S_nuovo,
                                            google_S_usato,
                                            bing_S,
                                            criteo_S)), 
                   col = colorRampPalette(c("#f0076fff", "white", "#073763ff"))(200),
                   type = "lower", diag = F,
                   method = "circle" , title = "\n\nCorrelazione lead vs. variabili spesa\n(dati settimanali)", 
                   addCoef.col = "black", tl.col="black", tl.srt=90)
#####Grafico delle serie temporali----------------------------------------------
get_plots(data.frame(Date = dati_sett$Date,
                     dati_sett %>%
                       select(lead,
                              meta_S_nuovo, google_S_nuovo, criteo_S, bing_S) %>% scale()), giornalieri = F, normalizza = F)
get_plots(data.frame(Date = dati_sett$Date,
                     dati_sett %>% 
                       select(lead,
                              meta_S_usato, google_S_usato, criteo_S, bing_S) %>% scale()), giornalieri = F, normalizza = F)
get_plots(data.frame(Date = dati_sett$Date,
                     dati_sett %>% 
                       select(lead,
                              meta_S_awareness, google_S_awareness, criteo_S, bing_S) %>% scale()), giornalieri = F, normalizza = F)

custom_colors <- c("#ed8b93", "#f08963", "#dfa950", "#f27faa", "#d2bd86", "#bfcc61",
                   "#a5d17f", "#65d7bb", "#5ec4e6", "#79a7ed","#baabec", "#e4a1da",
                   "#e298bb")

dati_g %>%
  select(c(Date, lead, matches("_S"))) %>%
  pivot_longer(cols = matches("_S"), 
               names_to = "cost_type", 
               values_to = "cost_value") %>% 
  mutate(year = year(Date)) %>%
  ggplot() +
  geom_line(aes(x = Date, y = cost_value, color = cost_type), linewidth = 0.5) +  #Colori per cost_value
  geom_line(aes(x = Date, y = lead), color = "black", linewidth = 1) +  #Colore per lead
  scale_color_manual(values = custom_colors) +
  facet_grid(vars(year), scales = 'free') +
  labs(x = "Data", y = "Valore") +
  theme_minimal()

dati_sett %>%
  select(c(Date, lead, matches("_S"))) %>%
  pivot_longer(cols = matches("_S"), 
               names_to = "cost_type", 
               values_to = "cost_value") %>% 
  mutate(year = year(Date)) %>%
  filter(year > 2021) %>% #Tolgo il 2021 in quanto c'è una sola osservazione
  ggplot() +
  geom_line(aes(x = Date, y = cost_value, color = cost_type), linewidth = 0.5) +  #Colori per cost_value
  geom_line(aes(x = Date, y = lead), color = "black", linewidth = 1) +  #Colore per lead
  scale_color_manual(values = custom_colors) +
  facet_grid(vars(year), scales = 'free') +
  labs(x = "Data", y = "Valore") +
  theme_minimal()
