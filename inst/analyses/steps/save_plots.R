save_plots <- function(plots, config) {
  index <- seq_len(length(plots))
  file_paths <- lapply(
    X = index,
    FUN = \(i) {
      file_name <- paste0(
        "p", i, ".png"
      )
      ggplot2::ggsave(
        filename = file_name,
        path = config$output_dir,
        plot = plots[[i]],
        device = "png",
        width = 10,
        height = 7
      )

      file.path(
        config$output_dir,
        file_name
      )
    }
  )
  unlist(file_paths)
}
