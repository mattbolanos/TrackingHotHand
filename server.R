# ------------------------- #
# --- Main Server Calls --- #
# ------------------------- #

shinyServer(function(input, output, session) {
  
  # Home page
  home_server(id = "home_page")
  
  # Shot chart server
  shot_chart_server("shot_chart")
  
  # Closest def server
  distance_server("distance")
  
})