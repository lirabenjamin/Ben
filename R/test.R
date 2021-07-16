captionR = function (prefix = "Figure", auto_space = TRUE, levels = 1, type = NULL,
                     infix = ".",out = "html")
{
  check_class(prefix, "character")
  check_class(auto_space, "logical")
  check_class(levels, "numeric")
  check_class(infix, "character")
  if (is.null(type)) {
    type <- c(rep("n", times = levels))
  }
  else if (length(type) < levels) {
    type[(length(type) + 1):levels] <- "n"
  }
  else if (length(type) > levels) {
    type <- type[1:levels]
  }
  if (!all(type %in% c("n", "c", "C"))) {
    stop("Invalid 'type' value used.  Expecting 'n', 'c', or 'C'.")
  }
  if (auto_space) {
    prefix <- paste(prefix, " ")
  }
  force(levels)
  force(prefix)
  force(infix)
  OBJECTS <- list(name = NULL, caption = NULL, number = list(list()))
  OBJECTS$number[[1]][which(type == "n")] <- 1
  OBJECTS$number[[1]][which(type == "c")] <- "a"
  OBJECTS$number[[1]][which(type == "C")] <- "A"
  function(name, caption = "", display = "full", level = FALSE,
           cite = FALSE, num = FALSE, out = "html") {
    if (level > levels) {
      stop("Level too large.")
    }
    objects <- OBJECTS
    if (any(objects$name == name)) {
      obj_ind <- match(name, objects$name)
      if (objects$caption[obj_ind] == "") {
        objects$caption[obj_ind] <- caption
      }
      else {
        caption <- objects$caption[obj_ind]
      }
    }
    else {
      obj_ind <- length(objects$name) + 1
      if (length(objects$number) == length(objects$name)) {
        if (level) {
          objects$number[[obj_ind]] <- increment(objects$number[[obj_ind -
                                                                   1]], level)
        }
        else {
          objects$number[[obj_ind]] <- increment(objects$number[[obj_ind -
                                                                   1]], levels)
        }
      }
      objects$name[obj_ind] <- name
      objects$caption[obj_ind] <- caption
    }
    assign("OBJECTS", objects, envir = parent.env(environment()))
    obj_num <- paste(objects$number[[obj_ind]], collapse = infix)
    if (cite) {
      .Deprecated(new = "display", old = "cite")
      return(paste0(prefix, obj_num))
    }
    if (num) {
      .Deprecated(new = "display", old = "num")
      return(obj_num)
    }
    if (display == FALSE) {
      return(invisible())
    }
    else if (out == "pdf" && display == "full" || display == "f") {
      return(paste0("**",prefix, obj_num,"**", "\\newline", "\\textit{",caption,"}"))
    }
    else if (out == "html" && display == "full" || display == "f") {
      return(paste0("<caption>","**",prefix, obj_num,"**", "<br>", "*",caption,"*</caption>"))
    }
    else if (display == "cite" || display == "c") {
      return(paste0("**",prefix, obj_num,"**"))
    }
    else if (display == "num" || display == "n") {
      return(obj_num)
    }
    else {
      warning("Invalid display mode used.  Caption was still saved.")
      return(invisible())
    }
  }
}

library(rmarkdown)
library(checkmate)
tcite = captionR(out = "html")
tcite(name = "test3",caption =  "This is my test caption",out = "html")
tcite(name = "test3",caption =  "This is my test caption",out = "pdf")
