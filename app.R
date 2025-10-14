library(shiny) # 1.10.0
library(bslib) # 0.9.0
library(data.table) # 1.16.4
library(dplyr) # 1.1.4
library(dtplyr) # 1.3.1
library(sf) # 1.0.19
library(plotly) # 4.10.4

shapefile <- fread("./hb_codes.csv") |> 
  arrange(HBName)

hb_choices <- setNames(shapefile$HB, shapefile$HBName)

final <- fread("./total_bed.csv") |> 
  arrange(Quarter)

bootswatch_themes <- c("flatly", "vapor")

base_alt <- "This plot shows time series full bed occupancy per quarter"

ui <- tags$html(
  lang = "en",
  fluidPage(
    theme = bs_theme(bootswatch = "vapor"),
    title = "Main Page",
    h1("Shiny website", style = "text-align: center;"),
    titlePanel("Choose your options"),
    sidebarLayout(
      sidebarPanel(
        selectInput("theme", "Choose a theme:", 
                    choices = bootswatch_themes,
                    selectize = FALSE,
                    selected = "vapor"),
        
        selectInput("board_name", "Choose a board name:",
                    choices = hb_choices,
                    multiple = TRUE),
        uiOutput("selected_items")
      ),
      mainPanel(
        plotlyOutput("plot")
      )
    )
  )  
)

server <- function(input, output, session){
  observe({
    session$setCurrentTheme(
      bs_theme(bootswatch = input$theme)
    )
  })
  
  output$selected_items <- renderUI({
    if(is.null(input$board_name)) return(NULL)
    list_names <- shapefile[HB %in% input$board_name]$HBName
    tags$ul(
      lapply(list_names, function(board) {
        tags$div(HTML(paste("<label style='display:none;'>", board, "</label>")))
      })
    )
  })
  
  output$plot <- renderPlotly({
    p <- plot_ly(if(!is.null(input$board_name)){final[HB %in% input$board_name]}else{final}, 
                 x = ~Quarter, y = ~percFull, color = ~HBName, 
                 type = 'scatter', mode = 'lines+markers',
                 text = ~paste(Quarter, '<br>', HBName, '<br>', percFull, '%'),
                 hoverinfo = 'text') |> 
      layout(title = "Quarter Series Plot",
             xaxis = list(title = "Quarter"),
             yaxis = list(title = "Percentage full beds hosp"),
             plot_bgcolor = 'black', 
             paper_bgcolor = 'black',
             font = list(color = 'white'))
    
    if(!is.null(input$board_name)){
      list_names <- shapefile[HB %in% input$board_name]$HBName
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
