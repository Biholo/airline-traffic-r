# Module Geospatial
# Phase 1B : Analyse Géospatiale

mod_geospatial_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Phase 1B : Analyse Géospatiale"),
    
    fluidRow(
      box(title = "Carte des États-Unis & Lignes Aériennes", width = 12, height = "700px",
          p("Représentation des lignes Origine-Destination. Les lignes les plus empruntées sont mises en évidence."),
          leafletOutput(ns("usa_map"), height = "600px")
      )
    )
  )
}

mod_geospatial_server <- function(id, flights, airports) {
  moduleServer(id, function(input, output, session) {
    
    output$usa_map <- renderLeaflet({
      req(nrow(flights) > 0, nrow(airports) > 0)
      
      # Préparation des données : Jointure pour avoir lat/lon origine et destination
      # On agrège par route pour ne pas tracer 300k lignes
      routes <- flights %>%
        group_by(origin, dest) %>%
        summarise(count = n(), .groups = "drop") %>%
        arrange(desc(count)) %>%
        head(100) # Top 100 des routes pour la lisibilité
      
      # Jointure Origine
      routes <- routes %>%
        left_join(airports, by = c("origin" = "faa")) %>%
        rename(origin_lat = lat, origin_lon = lon, origin_name = name)
      
      # Jointure Destination
      routes <- routes %>%
        left_join(airports, by = c("dest" = "faa")) %>%
        rename(dest_lat = lat, dest_lon = lon, dest_name = name)
      
      # Filtrer les NA
      routes <- routes %>% filter(!is.na(origin_lat), !is.na(dest_lat))
      
      # Carte
      map <- leaflet() %>%
        addTiles() %>%
        setView(lng = -98.5795, lat = 39.8283, zoom = 4)
      
      # Ajout des lignes
      for(i in 1:nrow(routes)) {
        map <- map %>%
          addPolylines(
            lng = c(routes$origin_lon[i], routes$dest_lon[i]),
            lat = c(routes$origin_lat[i], routes$dest_lat[i]),
            weight = log(routes$count[i])/2, # Épaisseur selon le trafic
            color = "blue",
            opacity = 0.5,
            popup = paste(routes$origin[i], "->", routes$dest[i], ":", routes$count[i], "vols")
          )
      }
      
      map
    })
    
  })
}
