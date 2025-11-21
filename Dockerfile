FROM rocker/shiny:latest

# Installation des dépendances système si nécessaire (ex: pour leaflet/sf)
# Installation des dépendances système si nécessaire (ex: pour leaflet/sf et ragg/graphics)
RUN apt-get update && apt-get install -y \
    libgdal-dev \
    libproj-dev \
    libwebp-dev \
    libpng-dev \
    libjpeg-dev \
    libtiff-dev \
    && rm -rf /var/lib/apt/lists/*

# Installation des packages R
RUN install2.r --error \
    shiny \
    shinydashboard \
    dplyr \
    ggplot2 \
    readr \
    leaflet \
    DT \
    readxl \
    jsonlite \
    rvest \
    lubridate \
    tidyr \
    rmarkdown

# Suppression des exemples par défaut de Shiny Server
RUN rm -rf /srv/shiny-server/*

# Copie de l'application
COPY ./app /srv/shiny-server/

# Génération du rapport HTML
RUN Rscript -e "rmarkdown::render('/srv/shiny-server/Projet - Groupe 2.Rmd', output_format = 'html_document', output_file = '/srv/shiny-server/www/report.html')"

# Exposition du port
EXPOSE 3838

# Lancement du serveur
CMD ["/usr/bin/shiny-server"]
