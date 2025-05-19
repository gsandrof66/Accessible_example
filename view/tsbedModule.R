source("./model/Load_data.R")

data_base_list <- data_tsbed()

plotUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h3("Time series"),
      selectInput(ns("board_name"), "Choose a board name:",
                  choices = data_base_list[["hb_choices"]],
                  multiple = TRUE),
      uiOutput(ns("selected_items"))
    ),
    mainPanel(
      plotlyOutput(ns("plot"))
    )
  )
}

plotServer <- function(id, theme_switch) {
  moduleServer(id, function(input, output, session) {
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
      if(theme_switch()){
        theme_color = list(bgcolor = "black", fgcolor = "white")
      }else{
        theme_color = list(bgcolor = "white", fgcolor = "black")
      }
      
      base_alt <- "This plot shows time series full bed occupancy per quarter"
      p <- plot_ly(if(!is.null(input$board_name)){data_base_list[["final"]][HB %in% input$board_name]}else{data_base_list[["final"]]},
                   x = ~Quarter, y = ~percFull, color = ~HBName, colors = RColorBrewer::brewer.pal(8, "Set1"),
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
      }", final_alt))
      p
    })
  })
}