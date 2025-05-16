library(shiny) # 1.10.0
library(bslib) # 0.9.0
library(thematic)
library(data.table) # 1.16.4
library(dplyr) # 1.1.4
library(dtplyr) # 1.3.1
library(sf) # 1.0.19
library(plotly) # 4.10.4

source("./model/Load_data.R")

data_base_list <- data_base()

base_alt <- "This plot shows time series full bed occupancy per quarter"
theme_color = list(bgcolor = "white", fgcolor = "black")

ui <- page_navbar(
  theme = bs_theme(version = 5, bootswatch = NULL, bg = theme_color$bgcolor, fg = theme_color$fgcolor), 
  
  nav_panel("Home", h2("Welcome to Home")),
  nav_panel("Data", 
            h2("Data Section"),
            sidebarLayout(
              sidebarPanel(
                selectInput("board_name", "Choose a board name:",
                            choices = data_base_list[["hb_choices"]],
                            multiple = TRUE),
                uiOutput("selected_items")
              ),
              mainPanel(
                plotlyOutput("plot")
              )
            )
            ),
  nav_panel("Maps", h2("Map review")),
  nav_panel("Settings", 
            h2("App Settings"),
            sidebarLayout(
              sidebarPanel(h3("UI Theme")),
              mainPanel(
                materialSwitch(inputId = "dark_mode", label = "Dark Mode",
                               status = "primary")
                ))
            )
)

server <- function(input, output, session){
  thematic_shiny()
  
  observe({
    if(input$dark_mode){
      theme_color = list(bgcolor = "black", fgcolor = "white")
    }else{
      theme_color = list(bgcolor = "white", fgcolor = "black")
    }
    new_theme <- bs_theme(version = 5, bootswatch = NULL, bg = theme_color$bgcolor, fg = theme_color$fgcolor)
    session$setCurrentTheme(new_theme)
  })
  
  output$selected_items <- renderUI({
    if(is.null(input$board_name)) return(NULL)
    list_names <- data_base_list[["shapefile"]][HB %in% input$board_name]$HBName
    tags$ul(
      lapply(list_names, function(board) {
        tags$div(HTML(paste("<label style='display:none;'>", board, "</label>")))
      })
    )
  })
  
  output$plot <- renderPlotly({
    if(input$dark_mode){
      theme_color = list(bgcolor = "black", fgcolor = "white")
    }else{
      theme_color = list(bgcolor = "white", fgcolor = "black")
    }
    
    p <- plot_ly(if(!is.null(input$board_name)){data_base_list[["final"]][HB %in% input$board_name]}else{data_base_list[["final"]]},
                 x = ~Quarter, y = ~percFull, color = ~HBName,
                 type = 'scatter', mode = 'lines+markers',
                 text = ~paste(Quarter, '<br>', HBName, '<br>', percFull, '%'),
                 hoverinfo = 'text') |>
      layout(title = "Quarter Series Plot",
             xaxis = list(title = "Quarter"),
             yaxis = list(title = "Percentage full beds hosp"),
             plot_bgcolor = theme_color$bgcolor,
             paper_bgcolor = theme_color$bgcolor,
             font = list(color = theme_color$fgcolor)
             )

    if(!is.null(input$board_name)){
      list_names <- data_base_list[["shapefile"]][HB %in% input$board_name]$HBName
      final_alt <- paste0(base_alt, " for ", paste(list_names, collapse = ", "))
    }else{
      final_alt <- paste0(base_alt, " for all the health boards")
    }

    # Add dynamic alt text using htmlwidgets::onRender
    p <- htmlwidgets::onRender(p, sprintf("
      function(el, x) {
        el.setAttribute('aria-label', '%s');
      }
    ", final_alt))
    p
  })
}

shinyApp(ui, server)
