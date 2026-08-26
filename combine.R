time_fmt <- function(t) {
  format(lubridate::ymd_hms(t), '%Y-%m-%d %H:%M %Z')
}
time_fmt("2025-01-01T10:30:00")

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
          "**{if(lang == 'en') 'Time' else 'Heure'}**: {{{{< localtime {time_fmt(date)} format='%b %-d, %Y {if(lang == 'en') '%-I:%M%P' else '%-H:%M'} %Z' >}}}}  ",
          "{body}",
          .sep = "\n"
        )
      } else if (type == "showcase") {
        meta <- glue::glue_data(
          txt,
          "## {title}",
          "**{if(lang == 'en') 'Presenter(s)' else 'Présentateur·rice·s'}**: {paste0(author, collapse = ', ')}  ",
          "**{if(lang == 'en') 'Affiliation(s)' else 'Affiliation·s'}**: {paste0(`author-affiliation`, collapse = ', ')}  ",
          #"**{if(lang == 'en') 'Time' else 'Heure'}**: {{{{< \"{date}\" | date \"MMM D, YYYY hh:mm z\" >}}}}  ",
          "**{if(lang == 'en') 'Organization Type' else 'Type d\\'organisation'}**: {paste0(`type`, collapse = ', ')}  ",
          "**{if(lang == 'en') 'Time' else 'Heure'}**: {{{{< localtime {time_fmt(date)} format='%b %-d, %Y {if(lang == 'en') '%-I:%M%P' else '%-H:%M'} %Z' >}}}}  ",
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

# schedule <- function(lang) {
#   sched <- dplyr::tribble(
#     ~date              , ~title                         , ~class          , ~time   ,
#     # October 5th
#     "2026-10-05T15:00" , ""                             , "socialevents"  ,      30 ,
#     ""                 , "Workshops"                    , "workshops"     , 8 * 15  ,
#     ""                 , "Break / Social Event"         , "socialevents"  , 2 * 15  ,
#     ""                 , "Workshops"                    , "workshops"     , 10 * 15 ,
#     ""                 , "Annual General Meeting"       , "specialevents" , 4 * 15  ,
#     ""                 , "Social Event (?)"             , "socialevents"  , 4 * 15  ,

#     # October 6th
#     "2026-10-06T16:00" , "Announcements / Social Event" , "socialevents"  ,      30 ,
#     ""                 , "Student Presentations"        , "mainevents"    , 10 * 15 ,
#     ""                 , "Break / Social Event"         , "socialevents"  , 4 * 15  ,
#     ""                 , "Student Presentations"        , "mainevents"    , 4 * 15  ,
#     ""                 , "Three-minute Thesis (3MT)"    , "specialevents" , 4 * 15  ,
#     ""                 , "Networking Social"            , "mainevents"    , 4 * 15  ,

#     # October 7th
#     "2026-10-07T16:00" , "Announcements / Social Event" , "socialevents"  ,      30 ,
#     "2026-10-07T17:30" , "Break / Social Event"         , "socialevents"  , 2 * 15  ,
#     "2026-10-07T19:30" , "Break / Social Event"         , "socialevents"  , 2 * 15  ,
#     "2026-10-07T21:15" , "Open Question Period / Break" , "specialevents" ,      15 ,
#     "2026-10-07T21:30" , "Awards Ceremony"              , "specialevents" , 2 * 15
#   )
#   browser()
#   while (any(sched$date == "")) {
#     sched <- dplyr::mutate(
#       sched,
#       date = dplyr::if_else(
#         date == "" & dplyr::lag(date) != "",
#         format(
#           lubridate::ymd_hm(dplyr::lag(date)) +
#             lubridate::minutes(dplyr::lag(time)),
#           "%Y-%m-%dT%H:%M"
#         ),
#         date
#       )
#     )
#   }

#   w <- list.files(
#     file.path(lang, "showcases"),
#     "showcase",
#     full.names = TRUE
#   )
#   browser()
#   w_deets <- w |>
#     purrr::map(\(x) {
#       txt <- readLines(x)
#       yml <- stringr::str_which(txt, "---")
#       txt <- txt[(yml[1] + 1):(yml[2] - 1)]
#       txt <- yaml::read_yaml(text = txt)
#       list("path" = x) |>
#         append(txt)
#     }) |>
#     setNames(w)

#   w_sort <- purrr::map(w_deets, \(x) {
#     data.frame(title = x$title, date = x$date, time = 15)
#   }) |>
#     purrr::list_rbind() |>
#     dplyr::mutate(
#       date = stringr::str_remove_all(date, "\\:00Z$"),
#       class = "mainevents"
#     ) |>
#     dplyr::bind_rows(sched) |>
#     dplyr::arrange(.data$date) |>
#     dplyr::mutate(
#       date = lubridate::ymd_hm(date),
#       date = lubridate::with_tz(date, "America/Toronto"),
#       date = format(date, "%Y-%m-%d %H:%M %Z"),
#       time = time / 15
#     )

#   w_print <- dplyr::tibble(date = seq("2026-10-05 11:00", "2026-10-05 |>
#     dplyr::mutate(
#       disp = stringr::str_extract(date, "\\d{2}:\\d{2} EDT"),
#       deets = glue::glue("  - []{{rowspan={time} .{class}}} {title}")
#     )
#   w_days <- w_print |>
#     dplyr::mutate(deets = glue::glue("- - {{{{< localtime 2026-10-05 {disp} format='%H:%M %Z' >}}}}")) |>
#     dplyr::select(deets, date) |>
#     dplyr::distinct()
#   w_print <- dplyr::bind_rows(unique(dplyr::mutate(w_print, day = stringr::str_extract(date, "\\d{4}-\\d{2}-\\d{2}"),
#     dplyr::select(day, deets, disp)

#   w_deets[w_sort] |>
#     purrr::map_chr(\(txt) {
#       txt <- purrr::map(txt, \(x) paste0(x, collapse = ", "))
#       if (type == "workshop") {
#         meta <- glue::glue_data(
#           txt,
#           "## {title}",
#           "**{if(lang == 'en') 'Lead' else 'Plomb'}**: {author}  ",
#           "**{if(lang == 'en') 'Length' else 'Longueur'}**: {length}  ",
#           "**{if(lang == 'en') 'Time' else 'Heure'}**: {{{{< localtime {time_fmt(date)} format='%b %-d, %Y {if(lang == 'en') '%-I:%M%P' else '%-H:%M'} %Z' >}}}}  ",
#           "{body}",
#           .sep = "\n"
#         )
#       } else if (type == "showcase") {
#         meta <- glue::glue_data(
#           txt,
#           "## {title}",
#           "**{if(lang == 'en') 'Presenter(s)' else 'Présentateur·rice·s'}**: {paste0(author, collapse = ', ')}  ",
#           "**{if(lang == 'en') 'Affiliation(s)' else 'Affiliation·s'}**: {paste0(`author-affiliation`, collapse = ', ')}  ",
#           #"**{if(lang == 'en') 'Time' else 'Heure'}**: {{{{< \"{date}\" | date \"MMM D, YYYY hh:mm z\" >}}}}  ",
#           "**{if(lang == 'en') 'Organization Type' else 'Type d\\'organisation'}**: {paste0(`type`, collapse = ', ')}  ",
#           "{unique(body)}",
#           .sep = "\n"
#         )
#       }
#       meta
#     })

#   sep <- glue::glue(
#     "\n\n---\n\n[*{if(lang == 'en') 'Return to Program' else 'Retour au programme'}*](program.qmd#{type}s)\n\n",
#     .trim = FALSE
#   )

#   w_print <- glue::glue_collapse(w_print, sep = sep)

#   glue::glue(
#     "---",
#     "title: \"{tools::toTitleCase(type)}s\"",
#     "filename: program_{type}s",
#     "toc: true",
#     "---",
#     "\n",
#     "{w_print}",
#     .sep = "\n"
#   ) |>
#     writeLines(paste0(lang, "/program_", type, "s.qmd"))
# }

# schedule("en")
