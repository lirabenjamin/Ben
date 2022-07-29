#' Title Hot and ready corrleation tables version 2
#'
#' @param data The dataframe to work with
#' @param selection Tidy selection of main variables
#' @param controls Tidy selection of control variables
#' @param leadzero Leading zero on or off for correlation table
#'
#' @return Nicely formatted correlation table
#' @export
#'
#' @examples
#' mtcars |> harcor(1:9, 10:11)
#' mmtcars |> harcor(1:9)

harcor <- function(data,selection = everything(), controls= NULL, leadzero=F)
{
require(tidyverse)
# Get main correlation table body
datacor <- data |> select({{selection}})
maincor <- datacor |> Ben::corstars()

# Get partial cors
partialcor <- data |>
  select({{selection}},{{controls}}) |>
  pivot_longer({{selection}},names_to = "variable") |>
  group_by(variable) |>
  nest() |>
  mutate(residuals = map(data,~lm(value~., data=. ) |> residuals() |> enframe())) |>
  select(-data) |>
  unnest(residuals) |>
  pivot_wider(names_from = variable, values_from = value) |>
  select(-name) |>
  Ben::corstars() |>
  t() |>
  as.data.frame()

if(!is.null(controls)){maincor[upper.tri(maincor)] = partialcor[upper.tri(partialcor)]}

rownames(maincor) = paste0(1:ncol(datacor),". ",rownames(maincor))

if(!leadzero){maincor <- maincor |> mutate_all(~str_replace_all(., "0\\.", "\\."))}

# describe
M <- datacor |> summarise_all(mean,na.rm=T) |> mutate_all(papaja::print_num)
SD <- datacor |> summarise_all(sd,na.rm=T)|> mutate_all(papaja::print_num)
N <- datacor |> summarise_all(~sum(!is.na(.)))|> mutate_all(function(x)x*1) |> mutate_all(papaja::apa_num,digits=0)

rownames(M) = c("M")
rownames(SD) = c("SD")
rownames(N) = c("n")

# merge
out <- bind_rows(maincor,M,SD,N) |>
  rownames_to_column(var = " ")

colnames(out) = c(" ",1:ncol(datacor))

return(out)
}
