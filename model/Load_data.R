source("model/data_functions.R")

data_base <- function(){
  # Read the shapefile
  shapefile <- read_files("./data/hb_codes.csv", "csv") |> 
    arrange(HBName)
  
  # Read the total bed occupancy data
  final <- read_files("./data/total_bed.csv", "csv") |> 
    arrange(Quarter)
  
  # Create a list of choices for the select input
  hb_choices <- setNames(shapefile$HB, shapefile$HBName)
  
  # Return the data and choices
  return(list(
    shapefile = shapefile,
    final = final,
    hb_choices = hb_choices
  ))
}