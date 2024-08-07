#' Title title of function
#'
#' @param data explain parameter
#' @param outcome explain parameter
#' @param method explain parameter
#' @param desired.n explain parameter
#' @param desired.n.relax explain parameter
#' @param selected.variables explain parameter
#' @param glm.formula explain parameter
#' @param glm.p.value.threshold explain parameter
#' @param rf.num.trees explain parameter
#' @param rf.num.try explain parameter
#' @param rf.min.node.size explain parameter
#' @param swr.alpha.to.enter explain parameter
#' @param swr.alpha.to.remove explain parameter
#' @param swr.n.iter explain parameter
#'
#' @return function returns x
#' @export
#'
#' @examples write one or more examples here that describe the ggd_AddWeights function

ggd_getClusterVariables <- function(
    data, # whole dataset that contains outcome and independent variables
    outcome, # name or unique regexp pattern of the column in data
    method = "all", # all=all variables, select=pre-defined, glm, rf, swr=step-wise regression
    desired.n = 10, # how many vars to return, sorted by best to worst
    desired.n.relax = F, # if is ok to return less (eg when non-signifacant). else gives error
    # arguments for method 1: pre-specified vars
    selected.variables = NA, # vector of variable names to pass on
    # arguments for method 2: glm
    glm.formula = "y ~.", # standard is to use simple additive formula
    glm.p.value.threshold = .05, # how significant vars need to be to be included in clustering
    # arguments for method 3: rf
    rf.num.trees = 1e3, # how many trees are built
    rf.num.try = 1, # multiplied by sqrt(NOGWAT)
    rf.min.node.size = .1, # if <1, corresponds to % rows of data
    # arguments for method 4: stepwise regression
    swr.alpha.to.enter = .15,
    swr.alpha.to.remove = .15,
    swr.n.iter = 100
){
  # ---- General prep and inits ----
  source("T:\\GGDTwente\\GGD-STAF\\EGB\\EPI\\R\\Probeersels\\CleanDataForModels.R")
  source("T:\\GGDTwente\\GGD-STAF\\EGB\\EPI\\__R_CODE__\\GDD_functions\\data_analysis\\ggd_glm.R")
  message("Variabelen selecteren")

  # ---- Method: selected ----
  if(method == "selected"){
    message("Methode: geselecteerde variabelen")
    cat(" Checken of alle variabelen in de data zitten\n")
    selected.var.present <- selected.variables %in% colnames(data)
    if(!all(selected.var.present)){
      stop("Niet alle opgevraagde variabelen zitten in de data. Missing:", selected.variables[!selected.var.present])
    }
    message("Selectie klaar.")
    return(data[, which(colnames(data) %in% selected.variables)])
  }

  # ---- Data prep ----
  # prepare the data into x (ind vars) and y (dep var)
  message("Data voorbereiden")
  cat(" Uitkomst\n")
  y.column <- which(grepl(outcome, colnames(data), T))
  if(length(y.column) == 0){
    stop("Kan de beoogde uitkomst niet vinden: ", outcome)
  } else if(length(y.column) > 1){
    stop("Meerdere uitkomst kolommen gevonden, '", outcome, "' correspondeerd met: ", paste(colnames(data)[y.column], collapse = ";"))
  }
  y <- data[, y.column]

  cat(" Covariaten\n")
  x <- data[, -y.column]
  clean_data_check <- CleanDataForModels(x)
  x <- clean_data_check$cleaned_data
  y <- y[clean_data_check$y_to_keep, ]

  # ---- Method: GLM ----
  vars.to.output <- ggd_glm(y = y, x = x, glm.formula = glm.formula, glm.p.value.threshold = glm.p.value.threshold)

  # ---- Method: RF ----
  vars.to.output <- ggd_rf()

  # ---- Package and return data ----
  return(vars.to.output)
}
