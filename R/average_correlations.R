#' Average Correlations with Fisher's R to Z conversion
#'
#' This function will take as input a verctor of correlations and output their mean after being transformed to fisher's Zs and back
#'
#'see https://www.tandfonline.com/doi/pdf/10.1080/00221309809595548?needAccess=true
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
#' c(.40, .04, .23, .25, .94, .01) |> average_correlations()
#' c(.40, .04, .23, .25, .94, .01) |> average_correlations(ns = c(10, 20, 40, 23, 10, 123))
#' c(.40, .04, .23, .25, .94, .01) |> average_correlations(ns = c(10, 20, 40, 23, 10, 123), use_weights = F)
#' c(.40, .04, .23, .25, .94, .01) |> average_correlations(ns = c(10, 20, 40, 23, 10, 123), use_weights = T)


average_correlations <-
  function(correlations,ns, na.rm = T, use_weights = F) {
    if(use_weights){
      r = correlations |> psych::fisherz() |> weighted.mean(w = ns, na.rm = na.rm) |> psych::fisherz2r()
      return(r)
    }
    if(!use_weights){
      r = correlations |> psych::fisherz() |> mean(na.rm = na.rm) |> psych::fisherz2r()
      return(r)
    }
  }
