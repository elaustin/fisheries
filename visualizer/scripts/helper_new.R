# library(xlsx)
# library(dplyr)
# library(lubridate)
# library(tidyr)
# library(stringr)

# Read & Manipulate Data -------------------------------------------------------


# Manipulate data for Plotly sales by state graph ------------------------------



# Render charts that are responsive to screen size ----------------
renderChart_pct <- function(expr, env = parent.frame(), quoted = FALSE) {
  func <- shiny::exprToFunction(expr, env, quoted)
  function() {
  rChart_ <- func()
    cht_style <- sprintf("<style>.rchart {width: %s; height: %s} </style>",
                         #### change these here to desired %
                         "100%","50%")
    cht <- paste(capture.output(rChart_$print()), collapse = '\n')
    HTML(paste(c(cht_style, cht), collapse = '\n'))
  }
}

        



