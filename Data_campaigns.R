###Librerie---------------------------------------------------------------------
library(here)
library(readxl)
library(tidyverse)

### DATA DIRECTORY--------------------------------------------------------------
#setwd("C:/Users/...")
i_am("scripts/Data_campaigns.R")
read_campaigns_dir <- purrr::partial(here, "data/campaigns/raw_data")
rw_campaigns <- purrr::partial(here, "data/campaigns/clean_data")
rw_campaigns_names_dir <- purrr::partial(here, "data/campaigns/campaigns_names")

### IMPORT DATASETS-------------------------------------------------------------
input_criteo_old <- "Criteo_old.csv"
input_criteo <- "CriteoAds.xlsx"
input_google <- "GoogleAds.xlsx"
input_bing <- "Microsoft_BingAds.xlsx"
input_meta <- "MetaAds.xlsx"

### NOMI CAMPAGNE---------------------------------------------------------------
nomi_google <- "nomi_campagne_google.csv"
nomi_bing <- "nomi_campagne_bing.csv"
nomi_meta <- "nomi_campagne_meta.csv"

###Fun: clean_vars--------------------------------------------------------------
clean_vars <- function(dati, google = F, bing = F, meta = F){
  if(google){
    dati$Day <- as.Date(dati$Day) 
    dati$Cost <- as.numeric(dati$Cost)
    dati <- dati %>% rename(Date = Day,
                            Nome_campagna = Campaign,
                            spesa = Cost,
                            impressions = Impr.)
    dati <- dati %>% select(Date, Nome_campagna, spesa, impressions)
  }
  if(bing){
    dati$Data <- as.Date(dati$Date, format = "%d/md/%y")
    dati <- dati %>% rename(spesa = Spend,
                            impressions = Impressions)
    dati <- dati %>% select(Date, spesa, impressions)
  }
  if(meta){
    dati$Giorno <- as.Date(dati$Giorno)
    dati <- dati %>% rename(Date = Giorno,
                            Nome_campagna = `Nome della campagna`,
                            spesa = `Importo speso (EUR)`,
                            impressions = Impression)
    dati$spesa[which(is.na(dati$spesa))] = 0.0
    dati <- dati %>% select(Date, Nome_campagna, spesa, impressions)
  }
  return(dati)
}

###Fun: crea_campagna-----------------------------------------------------------
crea_campagna <- function(dataset, keywords_nuovo, keywords_usato, keywords_awareness, keywords_escluse = NULL){
  dataset$campagna <- sapply(dataset$Nome_campagna, function(campagna) {
    if (any(sapply(keywords_escluse, grepl, campagna))) {
      return("-")
    } else if (any(sapply(keywords_awareness, grepl, campagna))) {
      return("awareness")
    } else if (any(sapply(keywords_usato, grepl, campagna))) {
      return("usato")
    } else if (any(sapply(keywords_nuovo, grepl, campagna))) {
      return("nuovo")
    } else {
      return("-")  # Valore predefinito per nessuna corrispondenza
    }
  })
  
  return(dataset)
}

###Fun: get_dataset-------------------------------------------------------------
get_dataset <- function(dati){
  dati <- dati %>% 
    group_by(Date, campagna) %>%
    reframe(spesa = sum(spesa),
            impressions = sum(impressions))
  dati <- dati %>%
    pivot_wider(names_from = campagna, 
                values_from = c(spesa, impressions), 
                names_prefix = "", 
                values_fill = list(spesa = 0, impressions = 0))
  dati <- sort_by(dati, dati$Date, decreasing = T)
  return(dati)
}

###Fun: rename_spend------------------------------------------------------------
rename_spend <- function(dataset, prefisso){
  nuovi_nomi <- setNames(
    c("spesa_nuovo", "spesa_usato", "spesa_awareness", 
      "impressions_nuovo", "impressions_usato", "impressions_awareness"),
    c(paste0(prefisso, "_S_nuovo"),
      paste0(prefisso, "_S_usato"),
      paste0(prefisso, "_S_awareness"),
      paste0(prefisso, "_I_nuovo"),
      paste0(prefisso, "_I_usato"),
      paste0(prefisso, "_I_awareness")
    )
  )
  # Rinomina le colonne utilizzando rename
  dataset <- dataset %>% rename(!!!nuovi_nomi)
  
  return(dataset)
}

####Criteo----------------------------------------------------------------------
criteo_vecchio <- read.csv2(read_campaigns_dir(input_criteo_vecchio))
criteo_vecchio <- criteo_vecchio[-nrow(criteo_vecchio),]
criteo_vecchio$Cost <- as.numeric(gsub(",", ".", gsub("\\.", "", gsub("€", "", criteo_vecchio$Cost))))
criteo <- read_xlsx(read_campaigns_dir(input_criteo))
criteo <- criteo[-NROW(criteo), ]
criteo_campagna <- criteo %>% rename(Date = Day,
                              criteo_S = Cost,
                              criteo_I = `Exposed Users`) %>% 
  mutate(Date = as.Date(Date)) %>% 
  select(Date, criteo_S, criteo_I) %>% 
  group_by(Date) %>%
  summarise(
    criteo_S = sum(criteo_S),
    criteo_I = sum(criteo_I)
  )
criteo_campagna <- criteo_campagna %>% arrange(desc(Date))
criteo_campagna <- criteo_campagna[-NROW(criteo_campagna), ]

#####Controllo preliminare------------------------------------------------------
str(criteo_campagna)
which(is.na(criteo_campagna)) #verifico valori NA

#####Dati puliti----------------------------------------------------------------
get_criteodata <- function(dati){
  dati$Cost <- as.numeric(dati$Cost)
  dati <- dati %>% select(Day, Cost, Exposed.Users)
  dati$Day <- as.Date(dati$Day, format = "%d/%m/%y") #Formatto la data in modo da farla combaciare con quella di Robyn
  dati <- dati %>% arrange(desc(Day)) #Metto in ordine cronologico i dati
  dati <- dati %>% #Cambio il nome delle variabili
    rename(
      Date = Day,
      criteo_S = Cost,
      criteo_I = Exposed.Users
    )
  dati <- dati %>% select(Date, criteo_S, criteo_I) #Seleziono le variabili di interesse
  
  dati <- dati %>% #Ragruppo i dati per i vari giorni e per ogni giorno sommo i valori di spesa e quelli delle esposizioni
    group_by(Date) %>%
    summarise(
      criteo_S = sum(criteo_S),
      criteo_I = sum(criteo_I)
    )
  
  dati <- dati %>% arrange(desc(Date)) #Metto il dataset in ordine decrescente per data
  dati$Date <- as.Date(dati$Date)
  return(dati) #La funzione riporta il dataset
}
criteo_campagna_vecchio <- get_criteodata(criteo_vecchio)
criteo_campagna <- rbind(criteo_campagna, criteo_campagna_vecchio %>% filter(Date < criteo_campagna$Date[NROW(criteo_campagna)]))

####Google----------------------------------------------------------------------
google_orig <- read_xlsx(read_campaigns_dir(input_google))
#####Controllo preliminare------------------------------------------------------
google_orig <- clean_vars(google_orig, google = T)
#Creazione dataset spesa
google <- crea_campagna(google_orig, 
                        keywords_nuovo = c("LIST OF WORDS TO IDENTIFY EXPENSES FOR NEW CARS ADVERTISING"),
                        keywords_usato = c("LIST OF WORDS TO IDENTIFY EXPENSES FOR USED CARS ADVERTISING",
                        keywords_awareness = c("LIST OF WORDS TO IDENTIFY EXPENSES FOR AWARENESS ADVERTISING"),
                        keywords_escluse = c("LIST OF WORDS TO IDENTIFY ADVERTISING EXPENSES THAT SHOULD BE NOT CONSIDERED"))

#####Nomi delle campagne--------------------------------------------------------
write.csv(google %>% distinct(Nome_campagna, campagna), rw_nomi_camp_dir(nomi_google))

#####Creazione dataset----------------------------------------------------------
google <- google[-which(google$campagna == "-"),]
google_campagna <- get_dataset(google)

google_campagna <- rename_spend(google_campagna, prefisso = "google")
glimpse(google_campagna)
#Esporto il file finale
#write_csv(google_campagna, rw_campaigns())


####Bing-------------------------------------------------------------------
bing_orig <- read_xlsx(read_campaigns_dir(input_bing))

#####Controllo preliminare------------------------------------------------------
bing_orig <- clean_vars(bing_orig, bing = T)

bing <- crea_campagna(bing_orig,
                     keywords_nuovo = c("LIST OF WORDS TO IDENTIFY EXPENSES FOR NEW CARS ADVERTISING"),
                     keywords_usato = c("LIST OF WORDS TO IDENTIFY EXPENSES FOR USED CARS ADVERTISING",
                     keywords_awareness = c("LIST OF WORDS TO IDENTIFY EXPENSES FOR AWARENESS ADVERTISING"),
                     keywords_escluse = c("LIST OF WORDS TO IDENTIFY ADVERTISING EXPENSES THAT SHOULD BE NOT CONSIDERED"))

#####Nomi delle campagne--------------------------------------------------------
#write.csv(bing %>% distinct(Nome_campagna, campagna), rw_nomi_camp_dir(nomi_bing))

#####Creazione dataset----------------------------------------------------------
bing_campagna <- bing_orig %>% 
  group_by(Date) %>%
  summarise(
    bing_S = sum(spesa),
    bing_I = sum(impressions))
bing_campagna <- sort_by(bing_campagna, bing_campagna$Date, decreasing = T)

glimpse(bing_campagna)

####Meta------------------------------------------------------------------------
meta_orig <- read_xlsx(read_campaigns_dir(input_meta))

meta_orig <- clean_vars(meta_orig, meta = T)

meta <- crea_campagna(meta_orig,
                      keywords_nuovo = c("LIST OF WORDS TO IDENTIFY EXPENSES FOR NEW CARS ADVERTISING"),
                      keywords_usato = c("LIST OF WORDS TO IDENTIFY EXPENSES FOR USED CARS ADVERTISING",
                                         keywords_awareness = c("LIST OF WORDS TO IDENTIFY EXPENSES FOR AWARENESS ADVERTISING"),
                                         keywords_escluse = c("LIST OF WORDS TO IDENTIFY ADVERTISING EXPENSES THAT SHOULD BE NOT CONSIDERED"))



#####Nomi delle campagne--------------------------------------------------------
write.csv(meta %>% distinct(Nome_campagna, campagna), rw_nomi_camp_dir(nomi_meta))

#####Creazione dataset----------------------------------------------------------
meta <- meta[-which(meta$campagna == "-"),]
meta_campagna <- get_dataset(meta)

meta_campagna <- rename_spend(meta_campagna, prefisso = "meta")
glimpse(meta_campagna)
