# Configuration Example
# Ce fichier sert de modèle pour la connexion à la base de données.
# Copiez ce fichier en 'config.R' et adaptez les valeurs.

db_config <- list(
  host = "localhost",      # Adresse du serveur de base de données
  port = 5432,             # Port (ex: 5432 pour PostgreSQL)
  user = "user",           # Nom d'utilisateur
  password = "password",   # Mot de passe
  dbname = "adp_data"      # Nom de la base de données
)

# Fonction utilitaire pour tester la connexion (simulation)
test_db_connection <- function(config) {
  message("Tentative de connexion à ", config$host, ":", config$port, "...")
  # Ici viendrait le code réel de connexion (ex: DBI::dbConnect)
  return(TRUE)
}
