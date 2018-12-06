# Load libraries ---------------------------------------------------------------
library(pacman)

p_load("shiny","dplyr","lubridate","scales",
       "DT","leaflet","devtools", "yaml",
       "data.table","shinythemes","remotes","openssl")

install_github('ramnathv/rCharts')
library(rCharts)

# Helper has server-side functions to read and manipulate data -----------------
source("./scripts/helper.R", local=T) 

