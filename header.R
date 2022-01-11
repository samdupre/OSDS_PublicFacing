# DEFINE FOOTER #

header <- function(i18n) {
  tagList(
    shiny.i18n::usei18n(i18n),
    div(
      tags$div(
        Dropdown.shinyInput("selected_language", options = lang_options, value = "English")
      )
    )
  )}

