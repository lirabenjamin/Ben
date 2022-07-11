#' Average Correlations with Fisher's R to Z conversion
#'
#' This function will take as input a verctor of correlations and output their mean after being transformed to fisher's Zs and back
#'
#' @param correlations
#' The vector of correlations to average
#'
#' @return
#' float: Average of correlations
#'
#' @export
#'
#' @examples
#' mtcars |> cor()

average_correlations <-
  function(correlations, na.rm = T) {
    correlations |> psych::fisherz() |> mean(na.rm = na.rm) |> psych::fisherz2r()
  }


