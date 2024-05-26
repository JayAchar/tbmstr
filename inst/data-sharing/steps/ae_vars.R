ae_vars <- list(
  prfneugrd = list(
    grade = "prfneugrd",
    value = character(0)
  ),
  visgrd = list(
    grade = "visgrd",
    value = c("vislf", "visrt")
  ),
  hbgrd = list(
    grade = "hbgrd",
    value = "hbval",
    levels = c(105, 94, 79, 0),
    cleaner = function(df) {
      df$hbval[which(df$hbval <= 30)] <- df$hbval[which(df$hbval <= 30)] * 10
      df
    }
  ),
  platgrd = list(
    grade = "platgrd",
    value = "platval",
    levels = c(99.9, 74.9, 49.9, 20)
  ),
  wbcgrd = list(
    grade = "wbcgrd",
    value = "wbcval",
    levels = c(4.00, 2.99, 1.99, 0.99)
  ),
  qtgrd = list(
    grade = "qtgrd",
    value = "qt",
    levels = c(450, 480, 500, 10000)
  ),
  neutgrd = list(
    grade = "neutgrd",
    value = "neuval",
    #  it's hard to create a cleaner function for this variable
    # since the raw values aren't consistently formatted incorrectly. Even
    # within each country there seems to be a difference
    levels = c(1.5, 0.99, 0.74, 0.50)
  ),
  altgrd = list(
    grade = "altgrd",
    value = "altval",
    levels = 40 * c(1, 3, 5, 20)
  ),
  astgrd = list(
    grade = "astgrd",
    value = "astval",
    levels = 40 * c(1, 3, 5, 20)
  ),
  blrgrd = list(
    grade = "blrgrd",
    value = "blrval",
    levels = 20 * c(1, 1.5, 3, 10)
  ),
  creatgrd = list(
    grade = "creatgrd",
    value = "creatval",
    levels = 120 * c(1.1, 1.6, 3.1, 6)
  ),
  kgrd = list(
    grade = "kgrd",
    value = "kval",
    levels = c(3.4, 2.9, 2.4, 2.0)
  ),
  mggrd = list(
    grade = "mggrd",
    value = "mgval",
    levels = c(0.7, 0.59, 0.44, 0.30)
  ),
  nagrd = list(
    grade = "nagrd",
    value = "naval",
    levels = c(135, 129, 122, 116)
  ),
  # couldn't find grading scale for fasting glucose
  glfastgrd = list(
    grade = "glfastgrd",
    value = "glfast"
  ),
  cagrd = list(
    grade = "cagrd",
    value = "caval",
    levels = c(2.1, 1.94, 1.74, 1.52)
  ),
  glnonfastgrd = list(
    grade = "glnonfastgrd",
    value = "glnonfast",
    levels = c(6.44, 8.90, 13.88, 27.75)
  )
)
