#' Title title of function
#'
#' @param data explain parameter
#' @param remove_zeroes_row explain parameter
#' @param remove_zeroes_column explain parameter
#'
#' @return function returns x
#' @export
#'
#' @examples write one or more examples here that describe the ggd_AddWeights function

ggd_make_frequency_table <- function(
    data, # Dataset of type x
    remove_zeroes_row = FALSE,  # Boolean, if TRUE, remove rows with zeroes from frequency table
    remove_zeroes_column = FALSE  # Boolean, if TRUE, remove rows with zeroes from frequency table
    #### Function description ####
    #TODO: write function description
){
  # TODO: check if dat is suitable to make frequency table or not, if not, return message
  # check names(data)
  # check unique(unlist(data)
  # check as.data.frame(table(data[column_name]))

  unique_data_labels <- sort(unique(unlist(data)), na.last=FALSE)  # Gets all unique data labels of all columns
  unique_data_labels[[1]] <- toString(unique_data_labels[[1]])  # Convert NA to string

  frequency_table <- matrix(0, length(unique_data_labels), length(data))  # Initiate frequency table with zeros
  colnames(frequency_table) <- names(data)  # Set columnnames of frequency table
  rownames(frequency_table) <- unique_data_labels  # Set rownames of frequency table

  for (column_name in colnames(frequency_table)) {
    frequencies_per_column <- as.data.frame(table(data[column_name], useNA="ifany"))
    names(frequencies_per_column)[is.na(names(frequencies_per_column))] <- "NA"
    result <- tryCatch({  # Try to insert frequency into frequency table
      frequency_table[names(frequencies_per_column), ] <- frequencies_per_column
    }, error = function(err) {  # If error occurs, print error message and stop running this function
      stop(paste("An error occurred in entering the frequencies in the frequency table:", conditionMessage(err)))
    })
  }

  if (remove_zeroes_row) {
    frequency_table <- frequency_table[rowSums(frequency_table) !=0,]  # Removes rows of frequency table where all elements are zero
  }

  if (remove_zeroes_column) {
    frequency_table <- frequency_table[colSums(frequency_table) !=0,]  # Removes columns of frequency table where all elements are zero
  }

  return(frequency_table)  # Returns matrix
}
