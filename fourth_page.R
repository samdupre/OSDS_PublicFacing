#SETUP FUNCTIONS TO GENERATE PAGE SECTIONS, PAGE 4: INTERNATIONAL MIGRATION#

# Add subtopics to Control panel
nav_picker_fourthPage <- function(i18n) {
  div(
  class = "nav_picker",
  DefaultButton.shinyInput(
    "fborn_button",
    text = i18n$t("Foreign Born"),
    className = "nav_item"
  ),
  DefaultButton.shinyInput(
    "fcit_button",
    text = i18n$t("Foreign Citizenship"),
    className = "nav_item"
  )
)}

# UI components
# Stack two cards next to each other
# FIRST STACK: Leaflet map (responds to subtopic chosen by user) + statistics pane
fourth_page_first_stack <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    div(
      Stack(
        tokens = list(childrenGap = 10), horizontal = TRUE,
        makeCard(" ", # see components.R for function definition
                 div(
                   strong(i18n$t("International Migrant Population by Municipality (Percent of Population),\nGuatemala 2018")),
                   textOutput("fourthPage_map_subtitle"),
                   br(),
                   leafletOutput("fourth_page_map")
                 ),
                 size = 9
        ),
        makeCard(" ",
                 div(
                   strong(i18n$t("Viewing:")),
                   textOutput("fourthPage_statistics_text0"),
                   br(),
                   strong(i18n$t("Most common immigrant source country")),
                   textOutput("fourthPage_statistics_text1"),
                   strong(i18n$t("Foreign Born Population")),
                   textOutput("fourthPage_statistics_text2"),
                   strong(i18n$t("Population with Foreign Citizenship")),
                   textOutput("fourthPage_statistics_text3")
                 ),
                 size = 3
        )
      )
    )
  )}
  


fourthPage_first_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(" ",
             fourth_page_first_stack(i18n), 
             size = 11)
  )}

# SECOND STACK: All plot content; plots appear reactively as users choose subtopics  
fourth_page_second_stack <- Stack(
  tokens = list(childrenGap = 10),
    Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(" ",
               div(
                 plotOutput("fourthPage_first_ADM1Plot") # Percent of Population
               ),
               size = 6
      ),
      makeCard(" ",
               div(
                  plotOutput("fourthPage_second_ADM1Plot") # Average Length of Stay
                   ),
               size = 5
              )
      ),
    Stack(
      tokens = list(childrenGap = 10), horizontal = FALSE,
      makeCard(" ",
               div(
                 plotOutput("fourthPage_arrivalPlot")), # Year of Arrival
               size = 6
    )
  )
)


fourthPage_second_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(" ",
             fourth_page_second_stack, size = 11)
  )}

# THIRD STACK: Export international migration data pane
fourth_page_third_stack <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    Stack(
      tokens = list(childrenGap = 10), horizontal = FALSE,
      makeCard(" ",
               div(
                 Text(i18n$t("Export international migration data (in .csv format)")),
                 useShinyjs(),
                 Stack(tokens = list(childrenGap = 10), horizontal = TRUE,
                       DefaultButton.shinyInput("page4_dd", text = i18n$t("Download International Migration Data"), iconProps = list(iconName = "Download International Migration Data")),
                       div(style = "visibility: hidden;", downloadButton("page4_ddb", label = "")))),
               size = 11
      )
    )
  )}

fourthPage_third_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(i18n$t("Data Download"),
             fourth_page_third_stack(i18n),
             size = 11)
  )}

# UI
fourthPage <- function(i18n) {
  tagList(
    shiny.i18n::usei18n(i18n),
    makePage( # see components.R for function definition
      i18n$t('International Migration'),
      i18n$t("Explore each subtopic"),
      div(
        fourthPage_first_stack_content(i18n),
        fourthPage_second_stack_content(i18n),
        fourthPage_third_stack_content(i18n)
      ))
    )
}