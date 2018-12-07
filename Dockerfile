FROM openanalytics/r-base

MAINTAINER Elena Austin "elaustin@github.com"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0

# basic shiny functionality
RUN R -e "install.packages('devtools', repos='https://cloud.r-project.org/')"
RUN R -e "devtools::install_github('rstudio/shiny')"
RUN R -e "install.packages(c('rmarkdown'), repos='https://cloud.r-project.org/')"

# install dependencies of the SYVisualization app
RUN R -e "install.packages(c('dplyr', 'lubridate', 'scales', 'DT', 'leaflet', 'devtools', 'yaml','data.table', 'shinythemes', 'remotes', 'openssl'), repos='https://cloud.r-project.org/')"

RUN R -e "devtools::install_github('ramnathv/rCharts')"

# copy the app to the image
RUN mkdir /root/visualizer
COPY visualizer /root/visualizer

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e shiny::runApp('/root/visualizer')"]
