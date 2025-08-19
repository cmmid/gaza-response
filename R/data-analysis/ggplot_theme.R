#...............................................................................
### ++ MONITORING THE NUTRITIONAL STATUS OF HUMANITARIAN WORKERS IN GAZA +++ ###
#...............................................................................

#...............................................................................
## ------ LSHTM GGPLOT THEME ----- ##
#...............................................................................



lshtm_theme <- function() {
  theme(
    # add border 1)
    panel.border = element_rect(colour = "#01454f", fill = NA, size = 0.5),
    # color background 2)
    panel.background = element_rect(fill = "white"),
    # modify grid 3)
    #panel.grid.major.x = element_line(colour = "steelblue", linetype = 3, size = 0.5),
    panel.grid.minor.x = element_line(colour = "aliceblue"),
    #panel.grid.major.y =  element_line(colour = "steelblue", linetype = 3, size = 0.5),
    panel.grid.minor.y = element_line(colour = "aliceblue"),
    # modify text, axis and colour 4) and 5)
    axis.text = element_text(colour = "#01454f"),
    axis.title = element_text(colour = "#01454f"),
    axis.ticks = element_line(colour = "#01454f"),
    # legend at the bottom 6)
    #legend.position = "bottom"
    strip.text.x = element_text(colour = "white"),
    strip.text.y = element_text(colour = "white", angle = 270),
    strip.background = element_rect(
      color="#01454f", fill="#01454f", size=1.5, linetype="solid"
    ),
    legend.position = "bottom",
    legend.title = element_text(colour = "#01454f", face = "bold"),
    legend.text = element_text(colour = "#01454f")
  )
}
theme_set(lshtm_theme())

lshtm_palette <- list(
  bmi_categories = c(
    "Obese" = "#01454f",
    "Overweight" = "#007457",
    "Normal" = "#709b28",
    "Underweight" = "#ffa600")
)
