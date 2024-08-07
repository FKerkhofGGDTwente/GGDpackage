#' Title title of function
#'
#' @param y explain parameter
#' @param x explain parameter
#' @param glm.formula explain parameter
#' @param glm.p.value.threshold explain parameter
#'
#' @return function returns x
#' @export
#'
#' @examples write one or more examples here that describe the ggd_AddWeights function

ggd_glm <- function(
    y, # outcome, ie dependent var
    x, # covariates ie indep vars
    glm.formula, # standard is to use simple additive formula
    glm.p.value.threshold # how significant vars need to be to be included in clustering)
){
  # ---- Generate formula ----
  # ---- Fit the model ----
  browser()
  model <- glm(formula = as.formula(glm.formula), data = data.frame(y, x))
  # ---- Identify the top x variables ----
  # ---- Return ----
  browser()
}
