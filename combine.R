combine_items <- function(lang, type, by = "title") {
  w <- list.files(
    file.path(lang, paste0(type, "s")),
    type,
    full.names = TRUE
  )

  w_deets <- w |>
    purrr::map(\(x) {
      txt <- readLines(x)
      yml <- stringr::str_which(txt, "---")
      body <- txt[(yml[2] + 1):length(txt)]
      txt <- txt[(yml[1] + 1):(yml[2] - 1)]
      txt <- yaml::read_yaml(text = txt)
      list("path" = x) |>
        append(txt) |>
        append(list(body = paste0(body, collapse = "\n")))
    }) |>
    setNames(w)

  w_sort <- purrr::map(w_deets, \(x) {
    data.frame(path = x$path, title = x$title, date = x$date)
  }) |>
    purrr::list_rbind() |>
    dplyr::arrange(.data[[by]]) |>
    dplyr::pull(path)

  w_print <- w_deets[w_sort] |>
    purrr::map_chr(\(txt) {
      txt <- purrr::map(txt, \(x) paste0(x, collapse = ", "))

      if (type == "workshop") {
        meta <- glue::glue_data(
          txt,
          "## {title}",
          "**{if(lang == 'en') 'Lead' else 'Plomb'}**: {author}  ",
          "**{if(lang == 'en') 'Length' else 'Longueur'}**: {length}  ",
          "{body}",
          .sep = "\n"
        )
      } else if (type == "showcase") {
        meta <- glue::glue_data(
          txt,
          "## {title}",
          "**{if(lang == 'en') 'Presenter(s)' else 'Présentateur·rice·s'}**: {paste0(author, collapse = ', ')}  ",
          "**{if(lang == 'en') 'Affiliation(s)' else 'Affiliation·s'}**: {paste0(`author-affiliation`, collapse = ', ')}  ",
          "**{if(lang == 'en') 'Organization Type' else 'Type d\\'organisation'}**: {paste0(`type`, collapse = ', ')}  ",
          "{unique(body)}",
          .sep = "\n"
        )
      }
      meta
    })

  sep <- glue::glue(
    "\n\n---\n\n[*{if(lang == 'en') 'Return to Program' else 'Retour au programme'}*](program.qmd#{type}s)\n\n",
    .trim = FALSE
  )

  w_print <- glue::glue_collapse(w_print, sep = sep)

  glue::glue(
    "---",
    "title: \"{tools::toTitleCase(type)}s\"",
    "filename: program_{type}s",
    "toc: true",
    "---",
    "\n",
    "{w_print}",
    .sep = "\n"
  ) |>
    writeLines(paste0(lang, "/program_", type, "s.qmd"))
}

combine_items("en", "workshop")
combine_items("en", "showcase")

combine_items("fr", "workshop")
combine_items("fr", "showcase")
