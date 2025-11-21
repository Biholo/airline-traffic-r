# Module Cancellations
# Phase 3 : Analyse des Annulations et Données Manquantes
library(dplyr)
library(ggplot2)
library(tidyr)

mod_cancellations_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Phase 3 : Analyse des Annulations et Données Manquantes"),
    
    tabsetPanel(
      # --- Onglet 1: Audit Données Manquantes ---
      tabPanel("Audit Données Manquantes",
        fluidRow(
          box(title = "Proportion de NA pour variables critiques", width = 6,
              tableOutput(ns("missing_stats_table"))
          ),
          box(title = "Colonnes avec données manquantes", width = 6,
              verbatimTextOutput(ns("na_counts_text"))
          )
        )
      ),
      
      # --- Onglet 2: Analyse Annulations ---
      tabPanel("Analyse Annulations",
        fluidRow(
          valueBoxOutput(ns("total_cancelled_box"), width = 12)
        ),
        fluidRow(
          box(title = "Top 5 Destinations les plus touchées", width = 6,
              tableOutput(ns("top_cancelled_dest_table"))
          ),
          box(title = "Annulations par Compagnie", width = 6,
              plotOutput(ns("cancelled_airline_plot"))
          )
        ),
        fluidRow(
          box(title = "Tableau Annulations par Compagnie", width = 12,
              tableOutput(ns("cancelled_airline_table"))
          )
        )
      )
    )
  )
}

mod_cancellations_server <- function(id, flights, airports, airlines) {
  moduleServer(id, function(input, output, session) {
    
    # --- 1. Audit Données Manquantes ---
    
    output$missing_stats_table <- renderTable({
      req(nrow(flights) > 0)
      flights %>%
        summarise(
          na_dep_time = mean(is.na(dep_time)),
          na_dep_delay = mean(is.na(dep_delay)),
          na_arr_time = mean(is.na(arr_time)),
          na_arr_delay = mean(is.na(arr_delay))
        ) %>%
        tidyr::pivot_longer(everything(), names_to = "variable", values_to = "taux_na")
    })
    
    output$na_counts_text <- renderPrint({
      req(nrow(flights) > 0)
      col_na_counts <- colSums(is.na(flights))
      col_na_counts[col_na_counts > 0]
    })
    
    # --- 2. Analyse Annulations ---
    
    cancelled_flights <- reactive({
      req(nrow(flights) > 0)
      flights %>%
        filter(is.na(dep_time) & is.na(arr_time))
    })
    
    output$total_cancelled_box <- renderValueBox({
      nb <- nrow(cancelled_flights())
      valueBox(nb, "Nombre Total de Vols Annulés", icon = icon("ban"), color = "red")
    })
    
    output$top_cancelled_dest_table <- renderTable({
      cancelled_flights() %>%
        count(dest, sort = TRUE) %>%
        head(5) %>%
        left_join(airports, by = c("dest" = "faa")) %>%
        select(Destination = dest, Ville = name, Annulations = n)
    })
    
    output$cancelled_airline_plot <- renderPlot({
      df_plot <- cancelled_flights() %>%
        count(carrier, sort = TRUE) %>%
        left_join(airlines, by = "carrier") %>%
        mutate(name = ifelse(is.na(name), carrier, name)) # Fallback to carrier code if name is NA
      
      ggplot(df_plot, aes(x = reorder(name, n), y = n)) +
        geom_col(fill = "firebrick") +
        coord_flip() +
        labs(
          title = "Nombre de vols annulés par Compagnie",
          x = "Compagnie",
          y = "Annulations"
        ) +
        theme_minimal()
    })
    
    output$cancelled_airline_table <- renderTable({
      cancelled_flights() %>%
        count(carrier, sort = TRUE) %>%
        left_join(airlines, by = "carrier") %>%
        select(Code = carrier, Compagnie = name, Annulations = n)
    })
    
  })
}
