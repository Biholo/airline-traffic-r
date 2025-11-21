# Module Reliability
# Phase 2 : Analyse des Retards et Fiabilité
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

mod_reliability_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Phase 2C : Analyse des Retards et Fiabilité"),
    
    tabsetPanel(
      # --- Onglet 1: Vue Globale Compagnies ---
      tabPanel("Fiabilité par Compagnie",
        fluidRow(
          box(title = "Classement Fiabilité (Retard Moyen)", width = 12,
              tableOutput(ns("reliability_table"))
          )
        ),
        fluidRow(
          box(title = "Distribution des Retards (Boxplot)", width = 6,
              plotOutput(ns("delay_boxplot"))
          ),
          box(title = "Proportion Retard vs Avance", width = 6,
              plotOutput(ns("proportion_plot"))
          )
        )
      ),
      
      # --- Onglet 2: Efficacité & Rattrapage ---
      tabPanel("Efficacité & Rattrapage",
        fluidRow(
          box(title = "Gain en Vol vs Retard Initial", width = 12,
              plotOutput(ns("gain_scatter_plot"))
          )
        ),
        fluidRow(
          box(title = "Top Vols ayant rattrapé du retard", width = 12,
              tableOutput(ns("catchup_table"))
          )
        )
      ),
      
      # --- Onglet 3: Facteurs (Distance & Heure) ---
      tabPanel("Facteurs de Retard",
        fluidRow(
          box(title = "Relation Distance vs Retard", width = 6,
              plotOutput(ns("distance_delay_plot")),
              textOutput(ns("distance_cor_text"))
          ),
          box(title = "Retard par Heure de Départ", width = 6,
              plotOutput(ns("hour_delay_plot"))
          )
        )
      ),
      
      # --- Onglet 4: Aéroports ---
      tabPanel("Analyse Aéroports",
        fluidRow(
          valueBoxOutput(ns("best_airport_box"), width = 6),
          box(title = "Détails Meilleur Aéroport", width = 6,
              tableOutput(ns("best_airport_table"))
          )
        ),
        fluidRow(
          box(title = "Fiabilité par Aéroport de Destination", width = 12,
              tableOutput(ns("dest_reliability_table"))
          )
        )
      )
    )
  )
}

mod_reliability_server <- function(id, flights, airports) {
  moduleServer(id, function(input, output, session) {
    
    # --- 1. Fiabilité par Compagnie ---
    
    fiabilite_data <- reactive({
      req(nrow(flights) > 0)
      flights %>%
        group_by(carrier, name) %>%
        summarise(
          n = n(),
          prop_delayed_arr = mean(delayed_arr, na.rm = TRUE),
          prop_early_arr   = mean(early_arr, na.rm = TRUE),
          mean_arr_delay   = mean(arr_delay, na.rm = TRUE),
          sd_arr_delay     = sd(arr_delay, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(mean_arr_delay)
    })
    
    output$reliability_table <- renderTable({
      fiabilite_data() %>% 
        head(20) %>%
        select(Compagnie = name, Vols = n, "Retard Moyen" = mean_arr_delay, "% Retard" = prop_delayed_arr)
    })
    
    output$delay_boxplot <- renderPlot({
      # Top compagnies pour lisibilité (min 100 vols)
      top_companies <- fiabilite_data() %>% filter(n >= 100) %>% pull(name)
      
      df_plot <- flights %>% filter(name %in% top_companies)
      
      ggplot(df_plot, aes(x = reorder(name, arr_delay, FUN = median), y = arr_delay)) +
        geom_boxplot(outlier.colour = "red", fill = "lightblue", outlier.size = 0.5) +
        coord_flip() +
        labs(title = "Distribution des retards à l'arrivée", x = "Compagnie", y = "Retard (min)") +
        theme_minimal()
    })
    
    output$proportion_plot <- renderPlot({
      # Barplot proportions
      plot_data <- fiabilite_data() %>%
        filter(n >= 100) %>%
        select(name, prop_delayed_arr, prop_early_arr) %>%
        tidyr::pivot_longer(cols = c(prop_delayed_arr, prop_early_arr), 
                            names_to = "type", values_to = "proportion")
      
      ggplot(plot_data, aes(x = reorder(name, proportion), y = proportion, fill = type)) +
        geom_col(position = "dodge") +
        coord_flip() +
        scale_fill_manual(values = c("prop_delayed_arr" = "#d73027", "prop_early_arr" = "#1a9850"),
                          labels = c("Retard", "Avance")) +
        labs(title = "Proportion Retard vs Avance", x = "Compagnie", y = "Proportion") +
        theme_minimal()
    })
    
    # --- 2. Efficacité ---
    
    output$gain_scatter_plot <- renderPlot({
      # Scatter: Retard Moyen vs Gain Moyen
      efficacite <- flights %>%
        group_by(carrier, name) %>%
        summarise(
          mean_gain_vol = mean(gain_vol, na.rm = TRUE),
          mean_arr_delay = mean(arr_delay, na.rm = TRUE),
          .groups = "drop"
        )
      
      ggplot(efficacite, aes(x = mean_arr_delay, y = mean_gain_vol, label = carrier)) +
        geom_point(size = 4, color = "blue") +
        geom_text(nudge_y = 1.5) +
        labs(title = "Retard moyen vs Gain moyen en vol", 
             x = "Retard moyen à l'arrivée (min)", y = "Gain moyen en vol (min)") +
        theme_minimal()
    })
    
    output$catchup_table <- renderTable({
      flights %>% 
        filter(!is.na(gain_vol) & gain_vol > 0 & gain_vol < 1000) %>%
        arrange(desc(gain_vol)) %>%
        select(Date=date, Compagnie=name, "Retard Dep"=dep_delay, "Retard Arr"=arr_delay, "Gain"=gain_vol) %>%
        head(10)
    })
    
    # --- 3. Facteurs ---
    
    output$distance_delay_plot <- renderPlot({
      # Par destination
      dest_stats <- flights %>%
        group_by(dest) %>%
        summarise(
          mean_arr_delay = mean(arr_delay, na.rm = TRUE),
          mean_distance  = mean(distance, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        filter(dest != "HNL") # Exclure HNL si outlier (comme dans rapport)
      
      ggplot(dest_stats %>% filter(!is.na(mean_distance), !is.na(mean_arr_delay)), aes(x = mean_distance, y = mean_arr_delay)) +
        geom_point() +
        geom_smooth(method = "lm", formula = y ~ x) +
        labs(title = "Distance vs Retard Moyen (par dest)", x = "Distance (miles)", y = "Retard Moyen (min)") +
        theme_minimal()
    })
    
    output$distance_cor_text <- renderText({
      dest_stats <- flights %>%
        group_by(dest) %>%
        summarise(
          mean_arr_delay = mean(arr_delay, na.rm = TRUE),
          mean_distance  = mean(distance, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        filter(dest != "HNL")
      
      cor_val <- cor(dest_stats$mean_distance, dest_stats$mean_arr_delay, use = "complete.obs")
      paste("Corrélation (Pearson) :", round(cor_val, 3))
    })
    
    output$hour_delay_plot <- renderPlot({
      # Retard par heure de départ
      # Utilisation de dep_datetime pour extraire l'heure
      # On filtre d'abord les dates manquantes
      df_time <- flights %>%
        filter(!is.na(dep_datetime)) %>%
        mutate(dep_hour = hour(dep_datetime))
        
      ggplot(df_time %>% filter(!is.na(dep_hour)), aes(x = factor(dep_hour), y = arr_delay)) +
        geom_boxplot(outlier.size = 0.5) +
        labs(title = "Retard par heure de départ", x = "Heure", y = "Retard (min)") +
        theme_minimal()
    })
    
    # --- 4. Aéroports ---
    
    best_dest_reactive <- reactive({
      flights %>%
        group_by(dest) %>%
        summarise(
          n = n(),
          mean_arr_delay = mean(arr_delay, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(mean_arr_delay) %>%
        slice(1) %>%
        left_join(airports, by = c("dest" = "faa"))
    })
    
    output$best_airport_box <- renderValueBox({
      best <- best_dest_reactive()
      valueBox(best$name, "Meilleur Aéroport (Retard Min)", icon = icon("trophy"), color = "green")
    })
    
    output$best_airport_table <- renderTable({
      best_dest_reactive() %>%
        select(Code=dest, Aéroport=name, Vols=n, "Retard Moyen"=mean_arr_delay)
    })
    
    output$dest_reliability_table <- renderTable({
      flights %>%
        group_by(dest) %>%
        summarise(
          n = n(),
          mean_arr_delay = mean(arr_delay, na.rm = TRUE),
          prop_delayed_arr = mean(arr_delay > 0, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(mean_arr_delay) %>%
        head(10)
    })
    
  })
}
