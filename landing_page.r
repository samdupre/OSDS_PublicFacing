#SETUP FUNCTIONS TO GENERATE PAGE SECTIONS, LANDING PAGE#

# UI components
# STACK 1: leaflet population density map
landingcard1 <- function() {
  Stack(
    tokens = list(childrenGap = 10), 
    horizontal = TRUE,
    makeCard( # see components.R for function definition
      strong(i18n$t("Welcome to Guatemala, come explore our country with us")),
      div(
        strong(i18n$t("Population Distribution, Guatemala 2018")),
        br(),
        leafletOutput("landing_page_map")
      ))
)}
  
# STACK 2: 
landingcard2 <- function() {
  Stack(
    tokens = list(childrenGap = 10), 
    horizontal = TRUE,
    makeCard(
      "",
      div(
        Text(i18n$t("Use the menu on the left to explore census data by different themes"))
      ))
)}

# UI
landingPage <- function(i18n) {
  tagList(
    shiny.i18n::usei18n(i18n),
    makePage( # see components.R for function definition
      i18n$t("The Guatemala 2018 Census Results Portal"),
      "",
      div(
        landingcard1(),
        landingcard2()
        )
      )
)}