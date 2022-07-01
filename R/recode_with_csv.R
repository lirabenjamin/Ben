#' Recode factor or character variables with a CSV file
#' Will take as input a path to a csv file with oldname,newname syntax. Will find each of the oldnames in the column, and replace them for newname.
#'
#' @param data Dataset to manipulate
#' @param csvfile path to a csv file with oldname, newname syntax
#' @param old_column variable name to edit
#' @param new_column variable name to save the result in. Will overwrite old_column if left blank
#'
#' @return mutated dataset
#' @export
#'
#' @examples
#' mtcars |>
#' mutate(gear_f = factor(gear, labels = c("three", "four","five"))) |>
#' rename_with_csv(csvfile,gear_f)

recode_with_csv <- function(data, csvfile, old_column, new_column={{old_column}}){

  names = readr::read_csv(csvfile,col_names = F)
  result <- data |> dplyr::mutate({{new_column}} := {{old_column}})
  for (i in 1:nrow(names)){
    result <-  result |> dplyr::mutate({{new_column}} := stringr::str_replace_all({{new_column}}, names$X1[i], names$X2[i]))
  }
  return(result)
}
