# Module Exploration
# Phase 1A : Exploration Générale (S'approprier les Données)

mod_exploration_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Phase 1A : Exploration Générale"),
    
    tabsetPanel(
      tabPanel("Inventaire & Fuseaux",
        fluidRow(
          box(title = "Inventaire Global", status = "primary", solidHeader = TRUE, width = 6,
            valueBoxOutput(ns("nb_airports"), width = 6),
            valueBoxOutput(ns("nb_airlines"), width = 6),
            valueBoxOutput(ns("nb_planes"), width = 6),
            valueBoxOutput(ns("nb_cancelled"), width = 6)
          ),
          box(title = "Fuseaux Horaires", status = "info", solidHeader = TRUE, width = 6,
            p("Aéroports ne respectant pas le DST (23) :"),
            textOutput(ns("dst_issues")),
            p("Nombre de fuseaux horaires distincts :"),
            textOutput(ns("nb_timezones"))
          )
        )
      ),
      
      tabPanel("Trafic & Top N",
        fluidRow(
          box(title = "Trafic Principal", width = 12,
             p("Aéroport de départ le plus emprunté :"),
             textOutput(ns("top_origin"))
          )
        ),
        fluidRow(
          box(title = "Top 10 Destinations (Plus/Moins prisées)", width = 6,
              selectInput(ns("top_dest_type"), "Type :", choices = c("Plus prisées", "Moins prisées")),
              tableOutput(ns("top_dest_table"))
          ),
          box(title = "Top 10 Avions (Plus/Moins décollés)", width = 6,
              selectInput(ns("top_plane_type"), "Type :", choices = c("Plus de décollages", "Moins de décollages")),
              tableOutput(ns("top_plane_table"))
          )
        )
      ),
      
      tabPanel("Couverture Compagnies",
        fluidRow(
          box(title = "Destinations par Compagnie", width = 12,
              plotOutput(ns("dest_by_airline_plot"))
          )
        ),
        fluidRow(
          box(title = "Compagnies incomplètes", width = 12,
              p("Compagnies ne desservant pas l'ensemble des aéroports d'origine :"),
              tableOutput(ns("incomplete_airlines_table"))
          )
        )
      ),
      
      tabPanel("Filtres Spécifiques",
        fluidRow(
          box(title = "Filtres à la demande", width = 12,
              selectInput(ns("specific_filter"), "Analyse spécifique :", 
                          choices = c(
                            "Vols vers Houston (IAH/HOU)", 
                            "Liaison NYC -> Seattle", 
                            "Destinations exclusives",
                            "Focus United/American/Delta"
                          )),
              uiOutput(ns("filter_result_ui"))
          )
        )
      )
    )
  )
}

mod_exploration_server <- function(id, flights, airports, airlines, planes) {
  moduleServer(id, function(input, output, session) {
    
    # --- Inventaire ---
    output$nb_airports <- renderValueBox({ 
      nb <- n_distinct(c(flights$origin, flights$dest))
      valueBox(nb, "Aéroports (Total)", icon = icon("plane-departure"), color = "blue") 
    })
    
    output$nb_airlines <- renderValueBox({ 
      nb <- n_distinct(flights$carrier)
      valueBox(nb, "Compagnies", icon = icon("building"), color = "purple") 
    })
    
    output$nb_planes <- renderValueBox({ 
      nb <- n_distinct(flights$tailnum, na.rm = TRUE)
      valueBox(nb, "Avions", icon = icon("plane"), color = "yellow") 
    })
    
    output$nb_cancelled <- renderValueBox({ 
      # Vols annulés : dep_time ET arr_time manquants (selon rapport)
      nb <- sum(is.na(flights$dep_time) & is.na(flights$arr_time))
      valueBox(nb, "Vols Annulés", icon = icon("ban"), color = "red") 
    })
    
    output$dst_issues <- renderText({ 
      # Suppose airports a une colonne 'dst'
      if("dst" %in% names(airports)) {
        nb <- sum(airports$dst == "N", na.rm = TRUE) # Exemple, adapter selon les données réelles
        paste(nb, "aéroports")
      } else {
        "Données DST manquantes"
      }
    })
    
    output$nb_timezones <- renderText({ 
      if("tzone" %in% names(airports)) {
        nb <- n_distinct(airports$tzone, na.rm = TRUE)
        paste(nb, "fuseaux")
      } else {
        "Données Timezone manquantes"
      }
    })
    
    # --- Trafic & Top N ---
    output$top_origin <- renderText({ 
      top <- flights %>% count(origin, sort = TRUE) %>% slice(1)
      paste(top$origin, "(", top$n, "vols )")
    })
    
    output$top_dest_table <- renderTable({
      req(input$top_dest_type)
      df <- flights %>% 
        count(dest) %>% 
        mutate(Percentage = n / sum(n) * 100) %>%
        left_join(airports, by = c("dest" = "faa")) %>%
        select(dest, name, n, Percentage)
      
      if(input$top_dest_type == "Plus prisées") {
        df %>% arrange(desc(n)) %>% head(10)
      } else {
        df %>% arrange(n) %>% head(10)
      }
    })
    
    output$top_plane_table <- renderTable({
      req(input$top_plane_type)
      df <- flights %>% 
        filter(!is.na(tailnum)) %>%
        count(tailnum)
      
      if(input$top_plane_type == "Plus de décollages") {
        df %>% arrange(desc(n)) %>% head(10)
      } else {
        df %>% arrange(n) %>% head(10)
      }
    })
    
    # --- Couverture ---
    output$dest_by_airline_plot <- renderPlot({
      df <- flights %>%
        group_by(carrier) %>%
        summarise(n_dest = n_distinct(dest)) %>%
        left_join(airlines, by = "carrier")
      
      ggplot(df, aes(x = reorder(carrier, -n_dest), y = n_dest, fill = carrier)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(title = "Nombre de destinations par compagnie", x = "Compagnie", y = "Destinations")
    })
    
    output$incomplete_airlines_table <- renderTable({
      # Compagnies ne desservant pas tous les aéroports d'origine
      all_origins <- unique(flights$origin)
      
      flights %>%
        group_by(carrier) %>%
        summarise(origins_served = n_distinct(origin)) %>%
        filter(origins_served < length(all_origins)) %>%
        left_join(airlines, by = "carrier") %>%
        select(carrier, name, origins_served)
    })
    
    # --- Filtres Spécifiques ---
    output$filter_result_ui <- renderUI({
      req(input$specific_filter)
      
      if(input$specific_filter == "Vols vers Houston (IAH/HOU)") {
        nb <- flights %>% filter(dest %in% c("IAH", "HOU")) %>% nrow()
        p(paste("Nombre de vols vers Houston :", nb))
        
      } else if(input$specific_filter == "Liaison NYC -> Seattle") {
        sub <- flights %>% filter(dest == "SEA") # NYC est l'origine implicite ici (EWR, JFK, LGA)
        n_flights <- nrow(sub)
        n_carrier <- n_distinct(sub$carrier)
        n_planes <- n_distinct(sub$tailnum)
        
        tagList(
          p(paste("Vols vers Seattle :", n_flights)),
          p(paste("Compagnies uniques :", n_carrier)),
          p(paste("Avions uniques :", n_planes))
        )
        
      } else if(input$specific_filter == "Destinations exclusives") {
        # Destinations servies par une seule compagnie
        excl <- flights %>%
          group_by(dest) %>%
          distinct(dest, carrier) %>%
          group_by(dest) %>%
          filter(n() == 1) %>%
          left_join(airports, by = c("dest" = "faa")) %>%
          left_join(airlines, by = "carrier") %>%
          select(Destination = dest, Ville = name.x, Compagnie = name.y) %>%
          arrange(Destination)
        
        tagList(
          p(paste("Nombre de destinations exclusives :", nrow(excl))),
          renderTable(head(excl, 10))
        )
        
      } else if(input$specific_filter == "Focus United/American/Delta") {
        nb <- flights %>% filter(carrier %in% c("UA", "AA", "DL")) %>% nrow()
        p(paste("Vols opérés par UA, AA ou DL :", nb))
      }
    })
    
  })
}
