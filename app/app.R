# ADP Shiny App - Main Application
# Point d'entrée de l'application.
# Charge les packages, la configuration, les données et les modules.

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(readr)
library(leaflet)
library(DT)
library(readxl)
library(jsonlite)
library(rvest)
library(lubridate)
library(tidyr)

# Chargement de la configuration
config_path <- "config/config.R"
if (!file.exists(config_path)) {
  warning("Fichier config.R manquant. Utilisation de config_example.R par défaut.")
  source("config/config_example.R")
} else {
  source(config_path)
}

# --- Chargement des Données ---
# On suppose que les fichiers sont dans app/data/
# Adapter les chemins si nécessaire

# 1. Flights (Excel)
flights_path <- "data/flights.xlsx"
if(file.exists(flights_path)) {
  flights <- read_excel(flights_path)
} else {
  warning("Fichier flights.xlsx introuvable.")
  flights <- data.frame() # Fallback vide
}

# 2. Airports (Excel)
airports_path <- "data/airports.xlsx"
if(file.exists(airports_path)) {
  airports <- read_excel(airports_path)
} else {
  warning("Fichier airports.xlsx introuvable.")
  airports <- data.frame()
}

# 3. Airlines (JSON)
airlines_path <- "data/airlines.json"
if(file.exists(airlines_path)) {
  airlines <- fromJSON(airlines_path)
  # Standardisation des colonnes
  if("code" %in% names(airlines)) {
    airlines <- airlines %>% rename(carrier = code)
  }
  if(!("carrier" %in% names(airlines))) {
    names(airlines)[1] <- "carrier"
  }
  if(!("name" %in% names(airlines))) {
    names(airlines)[2] <- "name"
  }
} else {
  warning("Fichier airlines.json introuvable.")
  airlines <- data.frame(carrier = character(), name = character())
}

# 4. Planes (HTML)
planes_path <- "data/planes.html"
if(file.exists(planes_path)) {
  planes <- read_html(planes_path) %>% 
    html_table() %>% 
    .[[1]]
} else {
  warning("Fichier planes.html introuvable.")
  planes <- data.frame()
}

# Chargement des modules
source("R/data_processing.R")
source("R/mod_exploration.R")
source("R/mod_geospatial.R")
source("R/mod_temporal.R")
source("R/mod_reliability.R")
source("R/mod_cancellations.R")

# --- Préparation des Données ---
# Utilisation de la fonction centralisée
# --- Préparation des Données ---
# Utilisation de la fonction centralisée
flights_master <- prepare_flight_data(flights, airlines, airports)
flights_filtered <- filter_flight_data(flights_master)

# Interface Utilisateur
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "ADP Data Consulting"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Exploration Générale", tabName = "exploration", icon = icon("search")),
      menuItem("Analyse Géospatiale", tabName = "geospatial", icon = icon("globe")),
      menuItem("Analyse Temporelle", tabName = "temporal", icon = icon("calendar")),
      menuItem("Retards et Fiabilité", tabName = "reliability", icon = icon("clock")),
      menuItem("Annulations", tabName = "cancellations", icon = icon("ban")),
      menuItem("Rapport Complet", tabName = "report", icon = icon("file-alt"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Onglet Exploration
      tabItem(tabName = "exploration",
              mod_exploration_ui("exploration_ui_1")
      ),
      
      # Onglet Géospatiale
      tabItem(tabName = "geospatial",
              mod_geospatial_ui("geospatial_ui_1")
      ),
      
      # Onglet Temporelle
      tabItem(tabName = "temporal",
              mod_temporal_ui("temporal_ui_1")
      ),
      
      # Onglet Fiabilité
      tabItem(tabName = "reliability",
              mod_reliability_ui("reliability_ui_1")
      ),
      
      # Onglet Annulations
      tabItem(tabName = "cancellations",
              mod_cancellations_ui("cancellations_ui_1")
      ),
      
      # Onglet Rapport
      tabItem(tabName = "report",
              tags$iframe(src = "report.html", width = "100%", height = "800px", style = "border:none;")
      )
    )
  )
)

# Serveur
server <- function(input, output, session) {
  
  # Appel des modules en passant les données
  # Note: On passe les dataframes directement. 
  # Si les données devaient changer dynamiquement, on utiliserait des reactive().
  
  # Exploration utilise flights_master pour avoir toutes les données (y compris annulés)
  mod_exploration_server("exploration_ui_1", flights_master, airports, airlines, planes)
  
  # Geospatial peut utiliser master ou filtered selon besoin, ici master pour complétude
  mod_geospatial_server("geospatial_ui_1", flights_master, airports)
  
  # Temporal utilise master (pour avoir les dates correctes)
  mod_temporal_server("temporal_ui_1", flights_master)
  
  # Reliability DOIT utiliser filtered (sans valeurs aberrantes)
  mod_reliability_server("reliability_ui_1", flights_filtered, airports)
  
  # Cancellations utilise master (pour voir les NAs)
  mod_cancellations_server("cancellations_ui_1", flights_master, airports, airlines)
  
}

# Lancement de l'application
shinyApp(ui = ui, server = server)
