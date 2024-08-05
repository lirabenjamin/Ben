exclusions <- function(dataset, exclusion_list, exclusion_names = NULL) {
  initial_count <- nrow(dataset)
  remaining_counts <- initial_count
  sankey_data <- list()
  
  cat("Initial row count:", initial_count, "\n")
  
  current_count <- initial_count
  
  if (is.null(exclusion_names) || length(exclusion_names) < length(exclusion_list)) {
    exclusion_names <- exclusion_list
  }
  
  for (i in seq_along(exclusion_list)) {
    exclusion <- exclusion_list[[i]]
    exclusion_name <- exclusion_names[[i]]
    before_count <- nrow(dataset)
    dataset <- subset(dataset, eval(parse(text = exclusion)))
    after_count <- nrow(dataset)
    
    survived <- after_count
    not_survived <- before_count - after_count
    
    if (i == 1) {
      sankey_data[[length(sankey_data) + 1]] <- paste("Initial [", survived, "]", exclusion_name, sep = "")
      sankey_data[[length(sankey_data) + 1]] <- paste("Initial [", not_survived, "]", "Not_", exclusion_name, sep = "")
    } else {
      sankey_data[[length(sankey_data) + 1]] <- paste(exclusion_names[i-1], "[", survived, "]", exclusion_name, sep = "")
      sankey_data[[length(sankey_data) + 1]] <- paste(exclusion_names[i-1], "[", not_survived, "]", "Not_", exclusion_name, sep = "")
    }
    
    cat("After exclusion", i, "(", exclusion, "):", after_count, "rows\n")
  }
  
  # For final remaining rows
  sankey_data[[length(sankey_data) + 1]] <- paste(exclusion_names[length(exclusion_list)], "[", after_count, "]", "Remaining_Rows", sep = "")
  
  cat("\nSankeyMATIC input:\n")
  for (entry in sankey_data) {
    cat(entry, "\n")
  }
  
  return(dataset)
}

# Example usage:
# library(datasets)
# filtered_data <- exclusions(mtcars, c("mpg <= 20", "wt <= 3"), c("Low MPG", "Light Weight"))
