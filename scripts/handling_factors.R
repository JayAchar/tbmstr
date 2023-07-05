library(forcats)

smear <- factor(
  c(
    "1+", "1+", "3+", "3+", "2+", "1+", "2+", "1+", "Neg", "1+",
    "Neg", "Neg", "1+", "Neg", "3+", "1+", "2+", "1+", "1+", "2+"
  ),
  levels = c("Neg", "1+", "2+", "3+")
)

# Collapse levels manually
fct_count(smear)

fct_collapse(
  smear,
  neg = "Neg",
  low = c("1+", "2+"),
  high = "3+"
) |> fct_count()


# Reorder levels
levels(smear)

fct_relevel(
  smear,
  c("3+", "2+", "1+", "Neg")
) |>
  levels()
