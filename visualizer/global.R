# Load libraries ---------------------------------------------------------------
library(pacman)

p_load("shiny","dplyr","lubridate","scales",
       "leaflet","devtools", "yaml",
       "data.table","shinythemes","openssl")

library(rCharts)

# Helper has server-side functions to read and manipulate data -----------------
source("./scripts/helper.R", local=T) 

west_coast_fatal <- readRDS("west_coast_fatal_state.rds")
west_coast_nonfatal <- readRDS("west_coast_non_fatal_state.rds")
non_fatal_source = readRDS("west_coast_non_fatal_source.rds")
non_fatal_event = readRDS("west_coast_non_fatal_event.rds")