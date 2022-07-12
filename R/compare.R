#' Compare
#'
#' Returns means, standard deviations, and sample sizes for two groups; inferential statistics (t-test and p-value), and effect size with confidence intervals (Cohen's D).
#' Inputs other than the dataset are strings so that the function can be used with expand_grid variables and map.
#'
#' @param df Dataset with variables in question
#' @param group string Comparison groups
#' @param y string Outcome to compare
#'
#' @return tibble object (1 row) with relevant information
#' @export
#'
#' @examples mtcars |> compare("am", "wt")
compare = function(df,group,y){
  df <- df |> select(y = {{y}}, group = {{group}})

  t <-  rstatix::t_test(df,formula(glue::glue("y ~ group")))
  d <-  as_tibble((psych::cohen.d(formula(glue::glue("y ~ group")),data=df))$cohen.d)

  msd  <-  df |>
    group_by(group) |>
    summarise(mean = mean(y,na.rm =TRUE),sd = sd(y,na.rm=TRUE)) |>
    pivot_wider(names_from = group, values_from = c(mean,sd))

  res <-  cbind(t,msd) |> cbind(d) |> rename(dlo  = lower, d = effect, dhi = upper)
  return(res)
}
