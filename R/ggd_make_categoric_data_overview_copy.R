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
  #TODO: check if data is suitable for function or not

  #### Load packages ####
  library(sjlabelled)


  #### Load custom functions ####
  source("T:\\GGDTwente\\GGD-STAF\\EGB\\EPI\\__R_CODE__\\GDD_functions\\data_analysis\\ggd_make_frequency_table.R")


  #### Initialize variables ####
  question_labels <- as.list(get_label(dataset))  # library(sjlabelled)
  output <- c()



  #### Initialize subplots ####
  plot_layout <- matrix(c(1,2,3,3), nrow = 2, ncol = 2, byrow = TRUE)
  layout(plot_layout)



  #### Determine amount of NA per column ####
  na_per_column <- colSums(is.na(dataset))
  na_per_row <- rowSums(is.na(dataset))

  barplot(na_per_column,
          main=paste("Aantal NA per kolom van", name_dataset),
          xlab="Kolom",
          ylab="Aantal NA")

  barplot(na_per_row,
          horiz=TRUE,
          main=paste("Aantal NA per rij van", name_dataset),
          xlab="Aantal NA",
          ylab="Rij")



  #### Frequency table all ####
  frequency_table <- ggd_make_frequency_table(dataset)



  #### Total score per row ####
  # Make table with factors of how many times an answer is given compared to other answers
  scale_factor_table <- as.data.frame(frequency_table)

  for (name in colnames(scale_factor_table)) {
    if(!sum(scale_factor_table[name]) == 0){
      scale_factor_table[name] <- scale_factor_table[name] / sum(scale_factor_table[name])
    }
  }

  # Determine scores per row
  scores_rows <- as.data.frame(dataset)
  for (name in colnames(scale_factor_table)) {
    indexes <- match(dataset[[name]], rownames(scale_factor_table))
    indexes[is.na(indexes)] <- 1
    scores_rows[name] <- scale_factor_table[indexes, name]
  }

  # Add column with total score per row
  scores_rows["Total"] <- rowSums(scores_rows)

  hist(scores_rows[["Total"]],
       breaks=round(length(scores_rows[["Total"]])*0.06),
       main=paste("Frequentie per score of", name_dataset),
       xlab="Score per rij",
       ylab="Frequentie")



  #### Determine return value(s) ####
  if (output_frequency_table) {
    output <- c(output, frequency_table)
  }

  if (output_scale_factor_table) {
    output <- c(output, scale_factor_table)
  }

  if (output_score_per_row) {
    output <- c(output, scores_rows)
  }

  return(output)
}
