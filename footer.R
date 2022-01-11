# DEFINE FOOTER #

footer <- function(i18n) {
  tagList(
    shiny.i18n::usei18n(i18n),
    Stack(
      horizontal = TRUE,
      horizontalAlign = 'space-between',
      tokens = list(childrenGap = 20),
      Text(variant = "medium", i18n$t("Created in OSDS"), block=TRUE),
      Text(variant = "medium", nowrap = FALSE, i18n$t("If you'd like to learn more, reach out to us at NSO@DOMAIN.GOV")),
      Text(variant = "medium", nowrap = FALSE, i18n$t("All rights reserved"))
    )
  )}