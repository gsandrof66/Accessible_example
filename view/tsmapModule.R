source("./model/Load_data.R")

data_map_list <- data_map()

mapUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h3("Heat maps"),
      selectInput(ns("ddtype"), "Type of year:",
                  choices = c("normal", "financial")),
      selectInput(ns("ddyear"), "Year",
                  choices = data_map_list[["year_choices"]],
                  selected = "2023")
    ),
    mainPanel(
      leafletOutput(ns("mymap"))
    )
  )
}

mapServer <- function(id, theme_switch) {
  moduleServer(id, function(input, output, session) {

    map_data <- reactive({
      future({
        base_alt <- "This heatmap shows full bed occupancy % in"
        year = as.integer(input$ddyear)

        if(input$ddtype == "normal"){
          d3 <- data_map_list[["data"]] |>
            _[y == year] |>
            summarise(percFull = mean(percFull), .by= c("HB"))

        }else if(input$ddtype == "financial"){
          quarters <- glue("Q{seq(4)}")
          quarters <- c(glue("{year}{quarters[2]}"),
                        glue("{year}{quarters[3]}"),
                        glue("{year+1}{quarters[4]}"),
                        glue("{year+1}{quarters[1]}"))

          d3 <- data_map_list[["data"]] |>
            _[Quarter %in% quarters] |>
            summarise(percFull = mean(percFull), .by= c("HB"))
        }

        myfinal <- merge(data_map_list[["shapedata"]], d3,
                         by=c("HB"), all = F) |>
          arrange(desc(percFull))

        # Identify the 3 highest and 3 lowest values
        top_3 <- head(myfinal[order(-myfinal$percFull), ], 3)
        bottom_3 <- head(myfinal[order(myfinal$percFull), ], 3)
        legend_values <- c(top_3$percFull, bottom_3$percFull)
        legend_labels <- c(top_3$HBName, bottom_3$HBName)
        pal <- colorNumeric("YlOrRd", domain = myfinal$percFull)

        final <- list(myfinal=myfinal, legend_values=legend_values,
                      legend_labels=legend_labels, pal=pal)

        leaflet(final[["myfinal"]]) |>
          addProviderTiles(provider = providers$CartoDB.DarkMatter) |>
          addPolygons(
            fillColor = ~colorQuantile("YlOrRd", percFull)(percFull),
            weight = 1, opacity = 1, color = "white", dashArray = "3",
            fillOpacity = 0.7,
            label = ~glue("{HBName}: {percFull}"),
            labelOptions = labelOptions(textsize = "16px", direction = "auto")
          ) |>
          addLegend("bottomright",
                    pal = final[["pal"]],
                    values = final[["legend_values"]],
                    labels = final[["legend_labels"]],
                    title = "% full hospital",
                    opacity = 1)
      })
    })

    output$mymap <- renderLeaflet({
      map_data() %...>% identity()
    })
  })
}
