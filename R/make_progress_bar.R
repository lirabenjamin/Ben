make_progress_bar <- function(data){progress::progress_bar$new(format = "  progress: [:bar] :percent eta: :eta",total = nrow(data));print("include pb$tick() inside the function")}
