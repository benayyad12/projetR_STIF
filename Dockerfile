# Utiliser l'image de base R
FROM r-base

# Installer les dépendances système nécessaires
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev

# Installer les packages R nécessaires
RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cloud.r-project.org/')"

# Copier les fichiers de l'application Shiny dans l'image Docker
COPY . /srv/shiny-server/

# Exposer le port
EXPOSE 3838

# Exécuter l'application Shiny
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/', host = '0.0.0.0', port = 3838)"]
