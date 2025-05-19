settingsUI <- function(id) {
  ns <- NS(id)
  h2("App Settings")
  sidebarLayout(
    sidebarPanel(h3("UI Theme")),
    mainPanel(
      # materialSwitch(inputId = "dark_mode", label = "Dark Mode",
      #                status = "primary")
      
      input_switch(ns("dark_mode"), "Dark mode"),
      verbatimTextOutput(ns("theme_status"))
    )
  )
}

settingsServer <- function(id, session) {
  moduleServer(id, function(input, output, session) {
    observe({
      if(input$dark_mode){
        theme_color = list(bgcolor = "black", fgcolor = "white")
      }else{
        theme_color = list(bgcolor = "white", fgcolor = "black")
      }
      new_theme <- bs_theme(version = 5, bootswatch = NULL, bg = theme_color$bgcolor, fg = theme_color$fgcolor)
      session$setCurrentTheme(new_theme)
    })
    
    output$theme_status <- renderText({
      if(input$dark_mode) "Dark Mode Enabled" else "Light Mode Enabled"
    })
    
    return(reactive({ input$dark_mode }))
  })
}