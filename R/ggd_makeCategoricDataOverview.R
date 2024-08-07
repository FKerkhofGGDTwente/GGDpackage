#' Title title of function
#'
#' @param dataset explain parameter
#' @param name_dataset explain parameter
#' @param output_frequency_table explain parameter
#' @param output_scale_factor_table explain parameter
#' @param output_score_per_row explain parameter
#'
#' @return function returns x
#' @export
#'
#' @examples write one or more examples here that describe the ggd_AddWeights function

ggd_make_categoric_data_overview <- function(
    dataset, # Dataset of type x
    name_dataset = "", # str, name of the dataset
    output_frequency_table = FALSE,  # Boolean, if TRUE, return frequency table
    output_scale_factor_table = FALSE,  # Boolean, if TRUE, return factor table
    output_score_per_row = FALSE  # Boolean, if TRUE, return score per respondent
    #### Function description ####
    #TODO: write function description
){
  #### Load packages ####
  library(sjlabelled)


  #### Load data ####
  source("T:/GGDTwente/GGD-STAF/EGB/EPI/R/R_functions/ggd_make_frequency_table.R")


  #### Initialize variables ####
  na_per_column <- c()
  na_per_row <- c()
  question_labels <- as.list(get_label(dataset))  # library(sjlabelled)



  #### Initialize subplots ####
  plot_layout <- matrix(c(1,2,3,3), nrow = 2, ncol = 2, byrow = TRUE)
  layout(plot_layout)



  #### Determine amount of NA per column ####
  for (name in names(dataset)) {
    amount_na <- sum(is.na(dataset[name]))
    na_per_column <- c(na_per_column, amount_na)
  }

  barplot(na_per_column,
          main=paste("Aantal NA per kolom van", name_dataset),
          xlab="Kolom",
          ylab="Aantal NA")



  #### Determine amount of NA per row ####
  for (row in 1:length(dataset)) {
    amount_na <- sum(is.na(dataset[[row]]))
    na_per_row <- c(na_per_row, amount_na)
  }

  barplot(na_per_row,
          horiz=TRUE,
          main=paste("Aantal NA per rij van", name_dataset),
          xlab="Aantal NA",
          ylab="Rij")



  #### Frequency table all ####
  frequency_table <- ggd_make_frequency_table(dataset)



  #### Total score per row ####
  # Make table with factors of how many times an answer is given compared to other answers
  factor_table <- as.data.frame(frequency_table)

  for (name in colnames(factor_table)) {
    if(!sum(factor_table[name]) == 0){
      factor_table[name] <- factor_table[name] / sum(factor_table[name])
    }

  }

  # Determine scores per row
  scores_rows <- as.data.frame(dataset)
  for (name in colnames(factor_table)) {
    indexes <- match(dataset[[name]], rownames(factor_table))
    indexes[is.na(indexes)] <- 1
    scores_rows[name] <- factor_table[indexes, name]
  }

  # Add column with total score per row
  scores_rows["Total"] <- rowSums(scores_rows)
  hist(scores_rows[["Total"]],
       breaks=round(length(scores_rows[["Total"]])*0.06),
       main=paste("Frequentie per score of", name_dataset),
       xlab="Score per rij",
       ylab="Frequentie")



  #### Determine return value(s) ####
  if (output_frequency_table && output_scale_factor_table && output_score_per_row) {
    return(c(frequency_table, factor_table, scores_rows))
  } else if (output_frequency_table && output_scale_factor_table) {
    return(c(frequency_table, factor_table))
  } else if (output_frequency_table && output_score_per_row) {
    return(c(frequency_table, scores_rows))
  } else if (output_scale_factor_table && output_score_per_row) {
    return(c(factor_table, scores_rows))
  } else if (output_frequency_table) {
    return(frequency_table)
  } else if (output_scale_factor_table) {
    return (factor_table)
  } else if (output_score_per_row) {
    return(scores_rows)
  } else {
    return(NULL)  #If not one of the cases above
  }
}
