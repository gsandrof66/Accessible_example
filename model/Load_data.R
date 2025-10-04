source("model/data_functions.R")

data_tsbed <- function(){
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

data_map <- function(){
  shapedata <- read_files("./data/shpfiles/SG_NHS_HealthBoards_2019.shp", "shp") |> 
    st_transform(crs = 4326) |> 
    rename(HB = HBCode)
  
  data <- read_files("./data/total_bed.csv", "csv") |> 
    # read_files("./data/beds_by_nhs_board_of_treatment_and_specialty.csv", "csv",
    #                  c("Quarter", "HB", "Location", "Specialty", "SpecialtyName", 
    #                    "TotalOccupiedBeddays", "PercentageOccupancy")) |> 
    arrange(Quarter) |> 
    _[!is.na(percFull)] |> 
    mutate(y = as.integer(unlist(tstrsplit(Quarter, "Q", fixed=T, keep=1))),
           q = as.integer(unlist(tstrsplit(Quarter, "Q", fixed=T, keep=2))))
  
  year_choices <- unique(data$y)
  
  return(list(
    shapedata = shapedata,
    data = data,
    year_choices = year_choices
  ))
}
