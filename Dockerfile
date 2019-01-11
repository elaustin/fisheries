FROM rocker/r-ver:3.5.2

MAINTAINER Elena Austin "elaustin@github.com"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev

# basic shiny functionality
RUN R -e "install.packages('devtools', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('shiny', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('rmarkdown', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('pacman', repos='https://cloud.r-project.org/')"

# install dependencies of the visualization app
RUN R -e "pacman::p_load('dplyr', 'lubridate', 'scales', 'DT', 'leaflet', 'devtools', 'yaml', 'data.table', 'shinythemes', 'remotes', 'openssl', repos='https://cloud.r-project.org/')"
RUN R -e "pacman::p_load_gh('ramnathv/rCharts')"

# copy the app to the image
RUN mkdir -p /root/visualizer
COPY visualizer /root/visualizer

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

COPY Rprofile.site /usr/local/lib/R/etc/Rprofile.site

CMD ["R", "-e shiny::runApp('/root/visualizer')"]
