# --- UI functions --- #

## Color functions
make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(x), maxColorValue = 255)
}

get_color <- make_color_pal(
  c("lightblue", "#f7f7f7", "orange"), 
  bias = 1
)

# Tooltip function
with_tooltip <- function(val, tooltip_val) {
  
  div(
    tippy(text = val, tooltip = tooltip_val, placement = "top", theme = "translucent", animation = "scale", duration = 500)
  )
  
}
