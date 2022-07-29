#' Title Stargazer from harcor V2
#'
#'Makes a stargazer table from a harcor output
#' @param x harcor output
#' @param out file to save to
#' @param type choose between 'html','latex', or 'text'. Now only html works appropriately.
#' @param rownames T or F. include rownames. defaults to F
#' @param summary T or F. F prints the whole dataset as is. Defaults to F
#' @param ... Optional arguments for stargazer
#'
#' @return stargazer table
#' @export
#'
#' @examples
harcor_to_stargazer <- function(x, out=NULL, type = 'html',rownames = F,summary = F,...){
  undo <- gsub("\\\\textasteriskcentered", "*", stargazer::stargazer(x,rownames = rownames ,summary = summary,...))

  undo <- stargazer::stargazer(x,rownames = rownames ,summary = summary,type = type)
  undo
  gsub("\\\\textasteriskcentered", "*",undo)
  undo

  if(type == "latex"){
    restar <- gsub("* * *", "$^{***}$", undo, fixed = TRUE)
    restar <- gsub("* *", "$^{**}$", restar, fixed = TRUE)
    restar <- gsub("* ", "$^{*}$", restar, fixed = TRUE)
  }

  if(type == 'text'){
    restar <- gsub("* * *", "***  ", undo, fixed = TRUE)
    restar <- gsub("* *", "** ", restar, fixed = TRUE)
    restar <- gsub("* ", "*  ", restar, fixed = TRUE)
  }

  if(type == 'html'){
    restar <- gsub("* * *", "***", undo, fixed = TRUE)
    restar <- gsub("* *", "**", restar, fixed = TRUE)
    restar <- gsub("* ", "*", restar, fixed = TRUE)
    lineloc = length(restar)
    lineplace = length(restar)-3
    line = restar[lineloc] |> str_remove("</table>")
    restar = restar[c(1:(lineplace-1), lineloc, lineplace:lineloc)]
    restar[lineplace] = line
  }

  if(!is.null(out)) cat(restar, file = out, sep="\n")
  return(restar)
}

