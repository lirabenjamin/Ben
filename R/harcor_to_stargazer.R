# # harcor_to_stargazer
#
# startup <- function(x, out=NULL, ...){
#   undo <- gsub("\\\\textasteriskcentered", "*", stargazer::stargazer(x, ...))
#   restar <- gsub("* * *", "${}^{***}$", undo, fixed = TRUE)
#   restar <- gsub("* *", "${}^{**}$", restar, fixed = TRUE)
#   restar <- gsub("* ", "${}^{*}$", restar, fixed = TRUE)
#   if(!is.null(out)) cat(restar, file = out, sep="\n")
#   restar
# }
#
# startup(harcor,
#         summary= F,
#         type = "text",
#         colnames = TRUE,
#         notes = "Sig. Levels: ${}^{***} p < .01$, ${}^{**} p < .05$, ${}^{*} p < .1$")
