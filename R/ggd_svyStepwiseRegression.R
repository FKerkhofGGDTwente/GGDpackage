#' Title title of function
#'
#' @param data explain parameter
#' @param outcome explain parameter
#' @param predictor.subset.xls explain parameter
#' @param alpha.to.enter explain parameter
#' @param alpha.to.remove explain parameter
#' @param n.iter explain parameter
#'
#' @return function returns x
#' @export
#'
#' @examples write one or more examples here that describe the ggd_AddWeights function

ggd_SvyStepwiseRegression <- function(
    data,
    outcome,
    predictor.subset.xls = NA,
    alpha.to.enter = .15,
    alpha.to.remove = .15,
    n.iter = 100
)
{
  message("Applying stepwise regression on ", outcome, " over ", n.iter, " simulations")
  library(survey)
  library(dplyr)
  getStatus <- function(var){x_status[[var]]}
  setStatus <- function(var, val){x_status[[var]] <<- val}

  if(is.character(outcome)){
    y <- data %>% select(outcome)
  } else {
    y <- outcome
    outcome <- "y"
  }

  if(!is.na(predictor.subset.xls)){
    x_start <- readxl::read_xlsx(predictor.subset.xls) %>%
      pull(varnames)
    x <- data %>% select(x_start)
  } else {
    x <- data
    if(is.character(outcome)){
      x <- x %>% select(-all_of(outcome))
    }
  }

  all_x_status <- matrix(0, n.iter, ncol(x))



  for(i in 1:n.iter){
    x_status <- rep(0, ncol(x))
    names(x_status) <- colnames(x)
    force_stop <- F
    counter <- 1
    # LOOP
    while(!force_stop){
      cat("=== SIM ",i,", STEP ", counter, "===\n")
      # startronde
      if(mean(x_status == 0) == 1){
        if(i == 1){
          p_values <- rep(999, ncol(x))
          # glms
          for(p in which(x_status == 0)){
            cat(colnames(x)[p], "\n")
            design <- svydesign(ids=~1, strata=data$stratum, weights=rep(1, nrow(data)), data=cbind(y, x[p]))
            model <- svyglm(as.formula(paste(outcome, "~", colnames(x)[p])), design = design, family = binomial(link = "logit"))
            if(model$converged){
              p_values[p] <- coef(summary(model))[4]
            }
          }
          if(min(p_values) <= alpha.to.enter){
            setStatus(colnames(x)[which.min(p_values)], 1)
            cat(" including as first predictor", colnames(x)[which.min(p_values)], "because it's alpha is the lowest, namely", p_values[p], "\n")
          } else {
            cat(" No predictor has alpha <=", alpha.to.enter, " in univariate model step: stopping\n")
            force_stop <- T
          }
          p_values_base <- p_values
        } else {
          p_values <- p_values_base
          if(min(p_values) <= alpha.to.enter){
            setStatus(colnames(x)[which.min(p_values)], 1)
            cat(" including as first predictor", colnames(x)[which.min(p_values)], "because it's alpha is the lowest, namely", p_values[p], "\n")
          } else {
            cat(" No predictor has alpha <=", alpha.to.enter, " in univariate model step: stopping\n")
            force_stop <- T
          }
        }

        # vervolgronde
      } else if(sum(x_status == 0) > 0){
        candidate <- sample(length(x_status), 1, F, x_status == 0)
        all_vars <- c(which(x_status == 1), candidate)
        cat(" evaluating ", colnames(x_status)[candidate], paste(outcome, "~", paste(colnames(x)[all_vars], collapse = "+")),"\n")
        # design
        design <- svydesign(
          ids = ~1,
          strata = data$stratum,
          weights = rep(1, nrow(data)),
          data = cbind(
            y,
            x[, all_vars]
          )
        )
        # glm
        model <- svyglm(as.formula(paste(outcome, "~", paste(colnames(x)[all_vars], collapse = "+"))), design = design)
        coefs <- coef(summary(model))
        p_values <- coefs[-1, 4]
        names(p_values) <- rownames(coefs)[-1]
        for(p in 1:length(p_values)){
          if(getStatus(names(p_values)[p]) == 1){
            if(p_values[p] >= alpha.to.remove){
              setStatus(names(p_values)[p], -1)
              cat(" -", names(p_values)[p], "@", round(p_values[p], 2), "\n")
            }
          } else {
            if(p_values[p] <= alpha.to.enter){
              setStatus(names(p_values)[p], 1)
              cat(" +", names(p_values)[p], "@", round(p_values[p], 2), "\n")
            } else {
              setStatus(names(p_values)[p], -1)
              cat(" -", names(p_values)[p], "@", round(p_values[p], 2), "\n")
            }
          }
        }

        # einde
      } else {
        force_stop <- T
      }
      plot(x_status)
      counter <- counter + 1
    }

    all_x_status[i, ] <- x_status

  }
  # final predictors
  x_final <- colMeans(all_x_status) > .5

  # final design
  design <- svydesign(
    ids = ~1,
    strata = data$stratum,
    weights = rep(1, nrow(data)),
    data = cbind(
      y,
      x[, x_final]
    )
  )
  # final model
  model <- svyglm(as.formula(paste(outcome, "~", paste(colnames(x)[x_final], collapse = "+"))), design = design)

  return(
    list(
      pars = list(
        alpha.to.enter = .15,
        alpha.to.remove = .15
      ),
      predictor_scores = all_x_status,
      final_design = design,
      final_model = model,
      final_model_summary = summary(model)
    )
  )
}
