save_plots <- function(plots, config) {
  # flatten list of plots to one level
  conversion <- unlist(plots["conversion"], recursive = FALSE)
  plots[["conversion"]] <- NULL
  plot_vector <- c(plots, conversion)

  all_files <- lapply(
    X = c("png", "pdf"),
    FUN = function(plot_type) {
      index <- seq_len(length(plot_vector))
      lapply(
        X = index,
        FUN = \(i) {
          file_name <- paste0(
            "p", i, ".", plot_type
          )
          ggplot2::ggsave(
            filename = file_name,
            path = file.path(
              config$output_dir,
              plot_type
            ),
            plot = plot_vector[[i]],
            device = plot_type,
            width = 10,
            height = 7
          )

          file.path(
            config$output_dir,
            plot_type,
            file_name
          )
        }
      )
    }
  )
  unlist(all_files)
}
