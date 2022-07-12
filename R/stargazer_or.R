#' Stargazer with Odds Ratios
#'
#' @param model A model object fitted with glm()
#' @param odd.ratio logical, should the table contain log-odds coefficients or odds ratios. Defaults to odds ratios.
#' @param ... Additional parameters for stargazer()
#'
#' @return stargazer table
#' @export
#'
#' @examples
#' glm(am ~ wt, data=mtcars,family = 'binomial') |> stargazer_or()
stargazer_or <- function(model, odd.ratio = T, type = 'text', no.space = T, star.cutoffs = c(.05, .01, .001),...) {
  if(!("list" %in% class(model))) model <- list(model)
  if (odd.ratio) {
    coefOR2 <- lapply(model, function(x) exp(coef(x)))
    seOR2 <- lapply(model, function(x) exp(coef(x)) * summary(x)$coef[, 2])
    p2 <- lapply(model, function(x) summary(x)$coefficients[, 4])
    stargazer::stargazer(model, coef = coefOR2, se = seOR2, p = p2, type = type, no.space = no.space, star.cutoffs = star.cutoffs,...)
  } else {
    stargazer::stargazer(model,type = type, no.space = no.space, star.cutoffs = star.cutoffs, ...)
  }}
