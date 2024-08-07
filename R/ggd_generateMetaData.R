#' Title title of function
#'
#' @param data.path explain this parameter
#' @param id.prefix explain this parameter
#' @param metadata.excel.path explain this parameter
#' @param target.path explain this parameter
#'
#' @return function returns x
#' @export
#'
#' @examples write one or more examples here that describe the function

ggd_generateMetadata <- function(data.path, id.prefix, metadata.excel.path = "", target.path = ""){
  #### Load packages and functions ####
  message("Load packages and functions")
  library(haven)
  library(jsonlite)
  library(readxl)
  # pre-requisite utility functions
  GetAttributeFromExcel <- function(metadata.excel, code, attribute){
    retreived_attribute <- unname(metadata.excel[metadata.excel$varcode == code, attribute])
    return(retreived_attribute)
  }

  #### Load data and metadata excel ####
  message("Loading data and metadata excel")
  local_data <- haven::read_sav(data.path)
  metadata.excel <- read_xlsx(metadata.excel.path)
  cat(" Metadata available for ", round(100 * mean(colnames(local_data) %in% metadata.excel$varcode)), "% of data\n")

  #### Initialize variables ####
  output <- NULL

  #### Retrieving data and metadata ####
  # Per column in the dataset, get relevant meta data and store in the list
  # some metadata is retrieved from the .sav file
  # other metadata is retrieved from the metadata excel file
  message("Retrieving data and metadata")
  progressbar <- txtProgressBar(1, ncol(local_data))
  for(i in 1:ncol(local_data)){
    setTxtProgressBar(progressbar, i)
    local_output <- list(
      list(
        varcode = colnames(local_data)[[i]],
        label = attr(local_data[[i]], "label"), # original question in survey
        questionset = GetAttributeFromExcel(metadata.excel, colnames(local_data)[[i]], "questionset"), # NL for nederland or lokaal (eg TW for Twente)
        topic = GetAttributeFromExcel(metadata.excel, colnames(local_data)[[i]], "topic"),
        values = unname(attr(local_data[[i]], "labels")), # values of categories, eg '1, 2, 3'
        valuelabels = names(attr(local_data[[i]], "labels")), # label that correspond to values, eg 'yes, no, other'
        missingvalues = GetAttributeFromExcel(metadata.excel, colnames(local_data)[[i]], "value_missings"), # which value if missing, eg '9'
        nvtvalues = GetAttributeFromExcel(metadata.excel, colnames(local_data)[[i]], "value_nvt"), # which value indicated not applicable; meaning respondents did not see the question. eg non-smokers are not asked how many cigs a day they smoke
        column_type = GetAttributeFromExcel(metadata.excel, colnames(local_data)[[i]], "variable_type"), # type of variable, eg proces, indicator, source
        dependency = list(
          conditional_on = GetAttributeFromExcel(metadata.excel, colnames(local_data)[[i]], "conditional_on"), # was the question only asked depending if another question was asked?
          conditional_response = GetAttributeFromExcel(metadata.excel, colnames(local_data)[[i]], "conditional_response")# was the question only asked depending if a particular answer was given to another question?
        ),
        data_type = tail(attr(local_data[[i]], "class"), 1) # technical information about type, eg string or double
      )
    )
    names(local_output) <- paste0(id.prefix, "_", colnames(local_data)[[i]])
    output <- c(output, local_output)
  }
  close(progressbar)

  #### Convert list to json file ####
  message("Converting metadata to JSON")
  output_json <- toJSON(output)

  #### Write json file ####
  output_path <- paste0(target.path, "/", id.prefix, "_metadata")
  message("Writing Metadata to ", output_path)
  write_json(output_json, output_path)
  message("Done")
}
