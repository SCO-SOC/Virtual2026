# Extract the student presentations from the Excel sheet into the
# individual page files.

library(tidyverse)
library(glue)
source("functions.R")


sci_names <- c(
  "Sturnus vulgaris",
  "Limosa haemastica",
  "Pipilo maculatus",
  "Mergus serrator",
  "Branta canadensis",
  "Progne subis",
  "Progne subis subis",
  "Catharus bicknelli",
  "Zonotrichia atricapilla",
  "Rissa tridactyla",
  "Poecile atricapillus",
  "Dolichonyx oryzivorus",
  "Ammospiza leconteii",
  "Cardellina canadensis",
  "Anas rubripes",
  "A. platyrhynchos",
  "Molothrus ater",
  "Spizelloides arborea",
  "Luscinia svecica",
  "Calidris bairdii",
  "Calcarius lapponicus",
  "Sterna forsteri",
  "Archilochus colubris",
  "Sphyrapicus thyroideus",
  "Setophaga striata",
  "Setophaga petechia",
  "S. pensylvanica",
  "Setophaga virens",
  "Setophaga castanea",
  "Leiothlypis peregrina",
  "Zonotrichia albicollis",
  "Setophaga aestiva",
  "Setophaga coronata",
  "Numenius spp.",
  "Limosa lapponica",
  "N. phaeopus",
  "N. hudsonicus",
  "Limnodromus griseus",
  "Calidris alpina",
  "Centrocercus urophasianus",
  "Sporophila"
) |>
  unique()
sci_names <- set_names(paste0("<i>", sci_names, "</i>"), sci_names)

p0 <- purrr::map(1:2, \(i) {
  if (i == 1) {
    lang <- "en"
  } else {
    lang <- "fr"
  }

  cols <- c("time" = 2, "name" = 4, "title" = 5, "topic" = 7)
  p1 <- readxl::read_excel(
    "SCO-SOC Student Presentation Schedule_2026_FE_Final.xlsx",
    sheet = i
  ) |>
    select(all_of(cols)) |>
    mutate(lang = .env$lang)

  cols <- c("time" = 2, "name" = 6, "title" = 7)
  p2 <- readxl::read_excel(
    "SCO-SOC Student Presentation Schedule_2026_FE_Final.xlsx",
    sheet = i
  ) |>
    select(all_of(cols)) |>
    mutate(lang = .env$lang, topic = NA)
  bind_rows(p1, p2)
})


p <- list_rbind(p0) |>
  mutate(name = if_else(is.na(name) & str_detect(title, "3MT"), "3MT", name)) |>
  filter(!str_detect(name, "SCO")) |>
  mutate(session = str_extract(name, "Session.+|(3MT)")) |>
  fill(session) |>
  filter(!str_detect(name, "Session|3MT|Student Name|Break|Nom de|Pause")) |>
  mutate(
    session = if_else(session == "3MT", paste0("3MT - ", topic), session),
    name = cleanup(name),
    pm = str_extract(time, "\\d{1,2}") |> as.numeric(),
    pm = if_else(pm > 9 & pm < 12, "am", "pm"),
    time = ymd_hm(
      paste0(
        "2026-10-06 ",
        str_extract(time, "\\d{1,2}:\\d{1,2}"),
        pm
      ),
      tz = "America/Toronto"
    ),
    time = with_tz(time, "UTC"),
    title = str_replace_all(title, sci_names)
  ) |>
  select(-topic)

abs <- readxl::read_excel(
  "SCO Student Presentations_Abstracts and Affiliations.xlsx"
) |>
  select(
    "name" = 2,
    "affiliation" = 3,
    "lang" = 4,
    "title" = 5,
    "authors" = 6,
    "abstract" = 7
  ) |>
  mutate(
    abstract = str_replace_all(abstract, sci_names),
    lang = if_else(str_detect(lang, "English"), "en", "fr"),
    affiliation = cleanup(affiliation),
    name = cleanup(name, plus = " "),
    authors = cleanup(authors, keep_braces = TRUE, plus = " "),
    authors = replace_na(authors, ""),
    have_name = map2_lgl(name, authors, \(n, a) str_detect(a, n)),
    authors = if_else(
      have_name,
      authors,
      paste0(name, " (", affiliation, "), ", authors)
    )
  ) |>
  select(-affiliation, -title, -have_name)

write_csv(
  select(abs, name, abstract) |> slice(1:20),
  "SCO_SOC_abstracts_1.csv"
)
write_csv(
  select(abs, name, abstract) |> slice(21:48),
  "SCO_SOC_abstracts_2.csv"
)

write_csv(select(abs, authors), "SCO_SOC_affiliations.csv")

abs_fr <- readxl::read_excel("SCO_SOC_abstracts.xlsx") |>
  select(-"authors", -"abstract") |>
  rename("authors" = "auteurs", "abstract" = "résumé") |>
  mutate(lang = "fr")

abs <- bind_rows(abs, abs_fr) |>
  mutate(
    affiliations_lst = map(authors, \(a) {
      str_extract_all(a, "\\([^\\)]+\\)") |>
        map(\(x) str_remove_all(x, "\\(|\\)"))
    }),
    authors_lst = map2(authors, affiliations_lst, \(a, p) {
      p <- paste0(paste0("(", unlist(p), ")"), collapse = "|")
      a <- str_remove_all(a, p) |>
        str_remove_all(" \\(|\\)") |>
        str_split_1(", ?")
      paste0("   - ", a)
    })
  )


deets <- abs |>
  full_join(p, by = c("name", "lang")) |>
  arrange(name) |>
  select(
    time,
    session,
    title,
    name,
    authors_lst,
    affiliations_lst,
    abstract,
    lang
  )

purrr::pmap(
  deets,
  \(time, session, title, name, authors_lst, affiliations_lst, abstract, lang) {
    family_name <- str_extract(name, "[^ ]+$") |> tolower()

    affiliations_lst <- unlist(affiliations_lst)

    affiliations_lst <- paste0(
      "  - ",
      seq_along(affiliations_lst),
      ". ",
      affiliations_lst
    ) |>
      paste0(collapse = "\n")

    authors_lst <- paste0(
      authors_lst,
      "<sup>",
      seq_along(authors_lst),
      "</sup>"
    ) |>
      paste0(collapse = "\n")

    y <- glue(
      "---",
      "filename: presentations/presentation_{family_name}",
      "title: \"{title}\"",
      "author: ",
      authors_lst,
      "author-affiliation:",
      affiliations_lst,
      "session:  \"{session}\"",
      "date: 2026-10-06T{format(time, '%H:%M:%S')}Z",
      "---",
      "\n\n",
      "### {if(lang == 'fr') 'Résumé' else 'Abstract'}",
      "\n\n",
      "{abstract}\n\n",
      "{if(lang == 'fr') '> **Corrections de traduction en cours**\n\n' else ''}",
      .sep = "\n"
    )
    write_file(y, glue("{lang}/presentations/presentation_{family_name}.qmd"))
  }
)
