# Load libraries ---------------------------------------------------------------
library(pacman)

p_load("shiny","dplyr","lubridate","xlsx","scales",
       "DT","leaflet","plotly","devtools", "yaml","data.table")

p_load_gh('ramnathv/rCharts')

# Helper has server-side functions to read and manipulate data -----------------
source("./scripts/helper.R", local=T) 

