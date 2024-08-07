#' Title title of function
#'
#' @param data explain parameter
#' @param n.clusters explain parameter
#' @param distance.metric explain parameter
#' @param method.in explain parameter
#' @param append.clusters explain parameter
#'
#' @return function returns x
#' @export
#'
#' @examples write one or more examples here that describe the ggd_AddWeights function

ggd_hierarchicalCluster <- function(
    data,
    n.clusters,
    distance.metric = "euclidean",
    method.in = "average",
    append.clusters = T
){
  message("Applying hierachical cluster analysis (", method.in, " method with ", distance.metric, " distance)")
  calculated_clusters <- data %>%
    dist(method = distance.metric) %>%
    hclust(method = method.in) %>%
    cutree(k = n.clusters)

  if(append.clusters){
    data <- data %>% add_column(cluster = calculated_clusters)
    return(data)
  } else {
    return(calculated_clusters)
  }
}
