library(shiny) # 1.10.0
library(bslib) # 0.9.0
library(data.table) # 1.16.4
library(dplyr) # 1.1.4
library(dtplyr) # 1.3.1
library(sf) # 1.0.19
library(plotly) # 4.10.4

source("./view/settingsModule.R")
source("./view/tsbedModule.R")

theme_color = list(bgcolor = "white", fgcolor = "black")

ui <- page_navbar(
  title = tags$a(
    href = "https://www.yourcompany.com",  # Replace with your actual link
    target = "_blank",  # Opens the link in a new tab
    tags$img(src = "img/demo.png", height = "40px", style = "margin-right:80px;")
  ),
  window_title = "My insights", 
  nav_panel("Home", h2("Welcome to Home")),
  nav_panel("Data", plotUI("tsbed")),
  nav_panel("Maps", h2("Map review")),
  nav_panel("Settings", settingsUI("settings")),
  theme = bs_theme(version = 5, bootswatch = NULL, 
                   bg = theme_color$bgcolor, fg = theme_color$fgcolor)
)

server <- function(input, output, session){
  theme_switch <- settingsServer("settings", session)
  plotServer("tsbed", theme_switch)
}

shinyApp(ui, server)
