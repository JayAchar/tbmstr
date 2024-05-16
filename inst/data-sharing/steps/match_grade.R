match_grade <- function(qt_values) {
  grading <- list(
    I = c(450, 480),
    II = c(481, 500),
    III = c(501, 10000)
  )

  vapply(
    qt_values,
    \(value) {
      if (is.na(value)) return(NA_character_)
      grade <- vapply(
        grading,
        \(g) {
          value >= g[[1]] & value <= g[[2]]
        },
        logical(1)
      )
      if (all(grade == FALSE)) {
        return(NA_character_)
      }
      names(grading)[grade]
    }, character(1)
  )
}
