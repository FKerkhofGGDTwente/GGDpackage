#' Title title of function
#'
#' @param data.path explain this parameter
#' @param metadata.path explain this parameter
#' @param select.columns explain this parameter
#' @param select.rows explain this parameter
#' @param set.spss.missings.to.na explain this parameter
#'
#' @return function returns x
#' @export
#'
#' @examples write one or more examples here that describe the function

ggd_loadData <- function(
    data.path, # TODO: replace with name and year, and then query meta data
    metadata.path, # TODO: this should come from meta data
    select.columns = "*", # strings with exact boolean expressions eg topic==mentale gezondheid seperated by semicolons
    select.rows,
    set.spss.missings.to.na = T
){
  # required packages and prerequisite functions
  library(haven)
  library(tidyverse)
  library(jsonlite)
  library(rrapply)

  # load data and load+shape metadata
  message("Loading data from ", data.path, "\nLoading metadata from ", metadata.path)
  local_data <- read_sav(data.path, user_na = !set.spss.missings.to.na)
  meta_data <- read_json(metadata.path)[[1]] %>% fromJSON %>% rrapply(how = "melt") # alternatief is bind

  if(select.columns == "*"){
    return(local_data)
  }

  # select columns
  selections <- strsplit(select.columns, ";")
  selections <- strsplit(selections[[1]], "==")
  selected_columns <- NULL
  for(i in 1:length(selections)){
    my_column <- which.max(colSums(meta_data == selections[[i]][1]))
    found_columns <- grepl(selections[[i]][1], meta_data[, my_column], T) & grepl(selections[[i]][2], meta_data$value, T)
    found_columns_ids <- meta_data[found_columns,]$L1
    found_varcodes <- meta_data$value[meta_data$L1 %in% found_columns_ids & meta_data$L2 == "varcode"]
    if(length(found_varcodes) > 0){
      selected_columns <- c(selected_columns, unlist(found_varcodes))
    }
  }

  # select rows
  selected_rows <- 1:nrow(local_data) # placeholder

  selected_data <- local_data[, selected_columns]

  return(selected_data)
}
