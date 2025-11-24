library(shiny)

wrap_server <- function(fun) {
  if (is.null(fun)) {
    return(function(input, output, session) {})
  }
  n <- length(formals(fun))
  if (n >= 3) {
    function(input, output, session) fun(input, output, session)
  } else {
    function(input, output, session) fun(input, output)
  }
}

# -------------------------
# Haoquan
# -------------------------
source("haoquan/data_prep.R")
source("haoquan/server.R")

haoquan_server_raw <- if (exists("server")) server else NULL
if (exists("server")) rm(server)

haoquan_server <- wrap_server(haoquan_server_raw)

# -------------------------
# Ruichen
# -------------------------
source("ruichen/server.R")

ruichen_server_raw <- if (exists("server")) server else NULL
if (exists("server")) rm(server)

ruichen_server <- wrap_server(ruichen_server_raw)

# -------------------------
# Yikai
# -------------------------
source("yikai/server.R")

yikai_server_raw <- if (exists("server")) server else NULL
if (exists("server")) rm(server)

yikai_server <- wrap_server(yikai_server_raw)

# -------------------------
# Eric
# -------------------------
source("eric/server.R")      # Eric's server.R with price/emissions code

eric_server_raw <- if (exists("server")) server else NULL
if (exists("server")) rm(server)

eric_server <- wrap_server(eric_server_raw)

# -------------------------
# Combined group server
# -------------------------
server <- function(input, output, session) {
  haoquan_server(input, output, session)
  ruichen_server(input, output, session)
  yikai_server(input, output, session)
  eric_server(input, output, session)
}