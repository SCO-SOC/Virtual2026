cleanup <- function(x, keep_braces = FALSE, plus = ", ") {
  xx <- x |>
    str_squish() |>
    str_remove_all("\\.") |>
    str_replace_all(" \\+ ", plus)

  if (!keep_braces) {
    xx <- str_replace_all(xx, " \\((.+)\\)", ", \\1")
  } else {
    xx <- str_replace(xx, "Climate Change Canada$", "Climate Change Canada)")
  }

  xx |>
    str_replace_all("(\\/ ?)|( at )|( in the )|( in )|( , )", ", ") |>
    str_replace_all("Master'?s", "MSc") |>
    str_replace_all("Undergraduate", "BSc") |>
    str_replace("the University", "University") |>
    str_replace("forest", "Forest") |>
    str_replace(
      "Étudiant au baccalauréat en biologie à l'UQAR",
      "BSc Student, Université du Québec à Rimouski"
    ) |>
    str_replace_all(c(
      "UNB" = "University of New Brunswick",
      "Dalhousie" = "Dalhousie University",
      "McGill" = "McGill University",
      "ECCC" = "Environment and Climate Change Canada",
      "UBC" = "University of British Columbia",
      "UQAR" = "Université du Québec à Rimouski"
    )) |>
    str_remove_all(
      "(, Peterborough, Ontario, Canada)|(, London, Canada)|(, Canada)|(, Montréal, Québec H9X 3V9)|(, Moncton, New Brunswick E1A 3E9)|(, Wolfville, Nova Scotia B4P 2R6)|(, Church Point, Nova Scotia B0W 1M0)|(, Saskatoon, SK)|(, Kingston, Ontario)|(, Ithaca, New York, USA)|(, Windsor, Ontario)|(, Tampa, Florida, USA)|(, Lethbridge, Alberta)"
    ) |>
    str_replace("Student ", "Student, ") |>
    str_replace("\\) Daniel", "\\), Daniel") |>
    str_replace_all("(,,)|(, and)", ",") |>
    str_replace("Emelie + Dykstra", "Emelie Dykstra") |>
    str_remove("\\.$")
}
