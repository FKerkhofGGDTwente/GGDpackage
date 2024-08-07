#' Title title of function
#'
#' @param data explain parameter
#' @param specified.columns explain parameter
#'
#' @return function returns x
#' @export
#'
#' @examples write one or more examples here that describe the ggd_AddWeights function

ggd_randomImpute <- function(data, specified.columns){
  for(i in which(colnames(data) %in% specified.columns)){
    cat(" waardes random imputeren voor kolom: ", colnames(data)[i], "\n")
    nas <-is.na(data[[i]])
    n_na <- sum(nas)
    data[nas, i] <- sample(unique(data[[i]]), n_na, T)
  }
  return(data)
}
