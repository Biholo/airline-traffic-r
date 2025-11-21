# Module Temporal
# Phase 2B : Analyse Temporelle et Pics de Trafic
library(lubridate)
library(dplyr)
library(ggplot2)

mod_temporal_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Phase 2B : Analyse Temporelle"),
    
    tabsetPanel(
      tabPanel("Trafic Mensuel",
        fluidRow(
          box(title = "Comparaison Mensuelle par Aéroport d'Origine", width = 12,
              p("Visualisation par facette avec moyenne mensuelle."),
              plotOutput(ns("monthly_traffic_plot"))
          )
        ),
        fluidRow(
          box(title = "Taux d'Accroissement Mensuel", width = 12,
              plotOutput(ns("growth_rate_plot"))
          )
        )
      ),
      
      tabPanel("Hebdomadaire & Jours Spéciaux",
        fluidRow(
          box(title = "Semaine vs Week-end", width = 6,
              plotOutput(ns("weekly_pattern_plot"))
          ),
          box(title = "Jours Spéciaux (Fêtes)", width = 6,
              p("Comparaison trafic jours spéciaux vs moyenne annuelle"),
              tableOutput(ns("special_days_table"))
          )
        )
      ),
      
      tabPanel("Pics de Trafic",
        fluidRow(
          valueBoxOutput(ns("top1_airport"), width = 4),
          valueBoxOutput(ns("top2_airport"), width = 4),
          valueBoxOutput(ns("top3_airport"), width = 4)
        ),
        fluidRow(
          box(title = "Top 3 Aéroports (Trafic)", width = 12,
              tableOutput(ns("peaks_table"))
          )
        )
      )
    )
  )
}

mod_temporal_server <- function(id, flights) {
  moduleServer(id, function(input, output, session) {
    
    # Préparation des données temporelles (Reactive)
    flights_dt <- reactive({
      req(nrow(flights) > 0)
      flights %>% filter(!is.na(date))
    })
    
    # --- Trafic Mensuel Normalisé ---
    output$monthly_traffic_plot <- renderPlot({
      req(flights_dt())
      
      daily_traffic <- flights_dt() %>%
        group_by(origin, date) %>%
        summarise(flights_per_day = n(), .groups = "drop")
      
      monthly_traffic <- daily_traffic %>%
        mutate(month = floor_date(date, "month")) %>%
        group_by(origin, month) %>%
        summarise(
          flights_total = sum(flights_per_day),
          days_in_month = n_distinct(date),
          flights_per_day_mean = flights_total / days_in_month,
          .groups = "drop"
        )
      
      origin_monthly_mean <- monthly_traffic %>%
        group_by(origin) %>%
        summarise(origin_mean_flights_per_day = mean(flights_per_day_mean))
      
      ggplot(monthly_traffic, aes(x = month, y = flights_per_day_mean)) +
        geom_line() +
        geom_point() +
        geom_hline(
          data = origin_monthly_mean,
          aes(yintercept = origin_mean_flights_per_day),
          linetype = "dashed"
        ) +
        facet_wrap(~ origin, scales = "free_y") +
        labs(
          title = "Trafic mensuel normalisé (vols/jour) par aéroport d'origine",
          x = "Mois",
          y = "Vols moyens par jour"
        ) +
        theme_minimal()
    })
    
    # --- Taux d'Accroissement ---
    output$growth_rate_plot <- renderPlot({
      req(flights_dt())
      
      daily_traffic <- flights_dt() %>%
        group_by(origin, date) %>%
        summarise(flights_per_day = n(), .groups = "drop")
      
      monthly_traffic <- daily_traffic %>%
        mutate(month = floor_date(date, "month")) %>%
        group_by(origin, month) %>%
        summarise(
          flights_per_day_mean = sum(flights_per_day) / n_distinct(date),
          .groups = "drop"
        )
      
      monthly_growth <- monthly_traffic %>%
        arrange(origin, month) %>%
        group_by(origin) %>%
        mutate(
          growth_rate = (flights_per_day_mean - lag(flights_per_day_mean)) /
            lag(flights_per_day_mean)
        ) %>%
        ungroup()
      
      scale_factor <- 100
      
      ggplot(monthly_growth, aes(x = month)) +
        geom_line(aes(y = flights_per_day_mean, color = "Vols/Jour")) +
        geom_point(aes(y = flights_per_day_mean)) +
        geom_line(aes(y = growth_rate * scale_factor, color = "Taux Croissance"), linetype = "dotted") +
        facet_wrap(~ origin, scales = "free_y") +
        scale_y_continuous(
          name = "Vols moyens par jour",
          sec.axis = sec_axis(~ . / scale_factor, name = "Taux d'accroissement")
        ) +
        labs(title = "Trafic mensuel et taux de croissance") +
        theme_minimal() +
        theme(legend.position = "bottom")
    })
    
    # --- Semaine vs Week-end ---
    output$weekly_pattern_plot <- renderPlot({
      req(flights_dt())
      
      weekly_traffic <- flights_dt() %>%
        mutate(
          wday = wday(date, week_start = 1),
          day_type = if_else(wday %in% c(6, 7), "weekend", "weekday")
        )
      
      daily_by_daytype <- weekly_traffic %>%
        group_by(day_type, date) %>%
        summarise(flights = n(), .groups = "drop")
      
      ggplot(daily_by_daytype, aes(x = date, y = flights, colour = day_type)) +
        geom_line(alpha = 0.7) +
        theme_minimal() +
        labs(
          title = "Trafic quotidien : Weekend vs Jours ouvrés",
          x = "Date",
          y = "Nombre de vols"
        )
    })
    
    # --- Jours Spéciaux ---
    output$special_days_table <- renderTable({
      req(flights_dt())
      
      holidays_2013 <- as.Date(c(
        "2013-01-01",
        "2013-07-04",
        "2013-11-28", # Thanksgiving 2013
        "2013-12-25"
      ))
      
      flights_special <- flights_dt() %>%
        mutate(is_special_day = date %in% holidays_2013)
      
      special_daily <- flights_special %>%
        group_by(is_special_day, date) %>%
        summarise(flights = n(), .groups = "drop")
      
      special_stats <- special_daily %>%
        group_by(is_special_day) %>%
        summarise(mean_flights = mean(flights)) %>%
        mutate(Type = ifelse(is_special_day, "Jours Fériés", "Jours Normaux")) %>%
        select(Type, mean_flights)
      
      special_stats
    })
    
    # --- Pics de Trafic (Top 3) ---
    output$top1_airport <- renderValueBox({ 
      top <- flights %>% count(dest, sort = TRUE) %>% slice(1)
      valueBox(top$dest, "Top 1 Destination", color="red") 
    })
    output$top2_airport <- renderValueBox({ 
      top <- flights %>% count(dest, sort = TRUE) %>% slice(2)
      valueBox(top$dest, "Top 2 Destination", color="orange") 
    })
    output$top3_airport <- renderValueBox({ 
      top <- flights %>% count(dest, sort = TRUE) %>% slice(3)
      valueBox(top$dest, "Top 3 Destination", color="yellow") 
    })
    
    output$peaks_table <- renderTable({
      # Top 3 des aéroports par trafic total (Origin + Dest) comme dans le rapport
      origin_counts <- flights %>% count(airport = origin, name = "n_origin")
      dest_counts   <- flights %>% count(airport = dest,   name = "n_dest")
      
      airport_traffic <- full_join(origin_counts, dest_counts, by = "airport") %>%
        replace_na(list(n_origin = 0, n_dest = 0)) %>%
        mutate(total_traffic = n_origin + n_dest) %>%
        arrange(desc(total_traffic)) %>%
        head(3)
      
      airport_traffic
    })
    
  })
}
