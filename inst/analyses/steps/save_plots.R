save_plots <- function(plots, config) {
  # flatten list of plots to one level
  conversion <- unlist(plots["conversion"], recursive = FALSE)
  plots[["conversion"]] <- NULL
  plot_vector <- c(plots, conversion)

  index <- seq_len(length(plot_vector))
  file_paths <- lapply(
    X = index,
    FUN = \(i) {
      file_name <- paste0(
        "p", i, ".png"
      )
      ggplot2::ggsave(
        filename = file_name,
        path = config$output_dir,
        plot = plot_vector[[i]],
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
