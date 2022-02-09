#' Display html string rendered in the viewer pane
#' @param str The html content you want to preview as a string
#' @return Will show the rendered html on the viewer pane

display_html = function(str){
  dir <- tempfile()
  dir.create(dir)
  htmlFile <- file.path(dir, "index.html")
  writeLines(str, con = htmlFile)
  # (code to write some content to the file)
  rstudioapi::viewer(htmlFile)
}
