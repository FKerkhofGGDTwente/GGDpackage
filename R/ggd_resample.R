#' Title title of function
#'
#' @param data explain parameter
#' @param weights explain parameter
#' @param desired.size explain parameter
#'
#' @return function returns x
#' @export
#'
#' @examples write one or more examples here that describe the ggd_AddWeights function

ggd_resample <- function(data, weights = NA, desired.size = 1){

  if(is.na(weights[1])){
    message("Resampling data (unweighted) to a size of ", round(desired.size * 100), "%")
    weights <- rep(1, nrow(data))
  } else{
    message("Resampling data (weighted) to a size of ", round(desired.size * 100), "%")
  }

  selected.cases <- sample(nrow(data), nrow(data) * desired.size, T, weights)
  return(data[selected.cases, ])
}
