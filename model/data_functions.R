#' `read_files`  This function reads parquet, csv and xlsx files. It has the capability to
#' select specific columns and it uses try catch for errors and stopping the App.
#' @param file file path
#' @param extension_file parquet, csv, xlsx with no dot
#' @param columns NULL by default, you can pass your columns as a character vector
read_files <- function(file, extension_file, columns = NULL){
  tryCatch({
    switch(extension_file,
           "parquet" = {
             return(as.data.table(read_parquet(file, col_select = all_of(columns), as_data_frame = F)))
           },
           "csv" = {
             return(fread(file, select = columns, strip.white = TRUE))
           },
           "xlsx" = {
             return(as.data.table(readxl::read_excel(file)))
           }
    )
  },
  error = function(err) {
    stop(glue::glue("Error reading file: {err$message}"))
  })
}