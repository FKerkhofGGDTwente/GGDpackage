# title: Recode values
# Author: Floor Kerkhof
# Date: 08-03-2024
# Last updated by author: x
# Date last updated: xx-xx-xxxx

#' Title recode values in data
#'
#' @param to_be_recoded the dataset that needs to be recoded
#' @param old_coding vector of values with the old coding
#' @param new_coding vector of values with the new coding
#'
#' @return the function returns the data with the recoded values
#' @export
#'
#' @examples nog te doen

ggd_recodeValues <- function(
    to_be_recoded, # Values to be recoded
    old_coding,  # Vector of values with the old coding
    new_coding  # Vector of values with the new coding
    #### Function description ####
    # Recode values using a vector with old_coding and a vector with new_coding
){
  #### Check if input variables are suitable for this function ####
  message("Check if input variables are suitable for this function")
  if(length(old_coding) != length(new_coding)) {
    stop("Vectors to recode do not have the same length")
  }


  #### Function code ####
  recoded_values <- new_coding[match(to_be_recoded, old_coding, nomatch = NA)]


  return(recoded_values)  # Returns vector
}
