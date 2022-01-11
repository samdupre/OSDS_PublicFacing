#SETUP FUNCTIONS TO GENERATE PAGE SECTIONS, PAGE 11: Water, Sanitation, & Hygiene#

# Add subtopics to Control panel
nav_picker_eleventhPage <- function(i18n) {
  div(
  class = "nav_picker",
  DefaultButton.shinyInput(
    "water_button",
    text = i18n$t("Water"),
    className = "nav_item"
  ),
  DefaultButton.shinyInput(
    "sanitation_button",
    text = i18n$t("Sanitation"),
    className = "nav_item"
  ),
  DefaultButton.shinyInput(
    "hygiene_button",
    text = i18n$t("Hygiene"),
    className = "nav_item"
  ),
)}

# UI components
# Stack two cards next to each other
# FIRST STACK: Leaflet map
eleventh_page_first_stack <- function(i18n) {
  Stack(
  tokens = list(childrenGap = 10),
  div(
    Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(" ", # see components.R for function definition
               div(
                 strong(i18n$t("Percent of Households with Amenity by Municipality, Guatemala 2018")),
                 textOutput("eleventhPage_map_subtitle"),
                 br(),
                 leafletOutput("eleventh_page_map")
               ),
               size = 12)
    )
  )
)}


eleventhPage_first_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(" ",
             eleventh_page_first_stack(i18n), 
             size = 11)
  )}

 # SECOND STACK: All plot content; plots appear conditionally when metrics match the condition
 # defined below
eleventh_page_second_stack <- Stack(
  tokens = list(childrenGap = 10),
  div(
    conditionalPanel(
      condition = "output.eleventhPage_metric2 != 'sanitation' && output.eleventhPage_metric2 != 'hygiene'", # when subtopic == water
      Stack(
        tokens = list(childrenGap = 10), horizontal = TRUE,
        makeCard(" ",
                 div(
                   plotOutput("eleventhPage_AllWater") # Housing Unit Count by Water Supply System
                   ),
                 size = 6
        ),
        makeCard(" ",
                 div(
                   plotOutput("eleventhPage_DrinkingWater") # Housing Unit Count by Drinking Water Supply System
                 ),
                 size = 5
        ))),
    conditionalPanel(
      condition = "output.eleventhPage_metric2 == 'sanitation'", # when subtopic == sanitation
      Stack(
        tokens = list(childrenGap = 10), horizontal = FALSE,
        makeCard(" ",
                 div(
                   plotOutput("eleventhPage_Toilet") # Housing Unit Count by Type of Toilet
                 ),
                 size = 11
        ),
        makeCard(" ",
                 div(
                   plotOutput("eleventhPage_Sewage") # Housing Unit Count by Sewage Disposal System
                 ),
                 size = 11
        ),
        makeCard(" ",
                 div(
                   plotOutput("eleventhPage_SolidWaste") # Housing Unit Count by Solid Waste Disposal System
                 ),
                 size = 11
        ))
      ),
    conditionalPanel(
      condition = "output.eleventhPage_metric2 == 'hygiene'", # when subtopic == hygiene
      Stack(
        tokens = list(childrenGap = 10), horizontal = FALSE,
        makeCard(" ",
                 div(
                   plotOutput("eleventhPage_Bathing") # Housing Unit Count by Hygiene System
                 ),
                 size = 11
        ),
        makeCard(" ",
                 div(
                   plotOutput("eleventhPage_Bathing1") # Housing Unit Count by Hygiene System, No Fixed Bath or Shower Within Housing Unit
                 ),
                 size = 11
        ),
        makeCard(" ",
                 div(
                   plotOutput("eleventhPage_Bathing2") # Housing Unit Count by Hygiene System, Fixed Bath or Shower is Outside Housing Unit
                 ),
                 size = 11
        ))
  )
))

eleventhPage_second_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(" ",
             eleventh_page_second_stack, size = 11)
  )}

# THIRD STACK: Export water, sanitation and hygiene data pane
eleventh_page_third_stack <- function(i18n) {
  Stack(
  tokens = list(childrenGap = 10),
  Stack(
    tokens = list(childrenGap = 10), horizontal = FALSE,
    makeCard(" ",
             div(
               Text(i18n$t("Export water, sanitation and hygiene data (in .csv format)")),
               useShinyjs(),
               Stack(tokens = list(childrenGap = 10), horizontal = TRUE,
                     DefaultButton.shinyInput("page11_dd", text = i18n$t("Download Water, Sanitation and Hygiene Data"), iconProps = list(iconName = "Download Water, Sanitation & Hygiene Data")),
                     div(style = "visibility: hidden;", downloadButton("page11_ddb", label = "")))),
             size = 11
    )
  )
)}

eleventhPage_third_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(i18n$t("Data Download"),
             eleventh_page_third_stack(i18n),
             size = 11)
  )}

#UI
eleventhPage <- function(i18n) {
  tagList(
    shiny.i18n::usei18n(i18n),
    makePage( # see components.R for function definition
      i18n$t('Water, Sanitation, and Hygiene'),
      i18n$t("Explore each subtopic"),
      div(
        eleventhPage_first_stack_content(i18n),
        eleventhPage_second_stack_content(i18n),
        eleventhPage_third_stack_content(i18n)
      ))
  )
  }