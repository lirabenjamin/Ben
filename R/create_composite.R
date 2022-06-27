#' Create Composites
#'
#' This function will take as input a dataset, a selection of columns, and a name for the column to be created
#'
#' @param data
#' The data to add a composite column to
#'
#' @param selection
#' A tidy selection of the variables that are to be averaged together
#'
#' @param name
#' The name of the variable to be created
#'
#' @param na.rm
#' Set to TRUE (default), will adjust the denominator in the case of missing data. If set to false, will return NA if the row has any missing data
#'
#' @return
#' a tibble object
#'
#' @export
#'
#' @examples
#' mtcars |> create_composite(c(drat, gear), drat_gear_average)

create_composite <-
  function(data,selection, name, na.rm = T) {
    mtcars |>
      dplyr::rowwise() |>
      dplyr::mutate({{name}} := mean(c({{selection}}, na.rm = T))) |>
      dplyr::ungroup()
  }


