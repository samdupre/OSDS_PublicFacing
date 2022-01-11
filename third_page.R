#SETUP FUNCTIONS TO GENERATE PAGE SECTIONS, PAGE 3: DISABILITY STATUS#

# Add subtopics to Control panel
nav_picker_thirdPage <- function(i18n) {
  div(
  class = "nav_picker",
  DefaultButton.shinyInput(
    "walking_button",
    text = i18n$t("Walking"),
    className = "nav_item"
  ),
  DefaultButton.shinyInput(
    "seeing_button",
    text = i18n$t("Seeing"),
    className = "nav_item"
  ),
  DefaultButton.shinyInput(
    "hearing_button",
    text = i18n$t("Hearing"),
    className = "nav_item"
  ),
  DefaultButton.shinyInput(
    "cognition_button",
    text = i18n$t("Cognition"),
    className = "nav_item"
  ),
  DefaultButton.shinyInput(
    "selfcare_button",
    text = i18n$t("Selfcare"),
    className = "nav_item"
  ),
  DefaultButton.shinyInput(
    "communication_button",
    text = i18n$t("Communication"),
    className = "nav_item"
  )
)}

# UI components
# Stack two cards next to each other
# FIRST STACK: Leaflet map (updates with user subtopic input) + statistics pane
third_page_first_stack <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    div(
      Stack(
        tokens = list(childrenGap = 10), horizontal = TRUE,
        makeCard(" ", # see components.R for function definition
                 div(
                   strong(i18n$t("Prevalence Rate of Population with at Least Some Difficulty by Municipality, Guatemala 2018")),
                   textOutput("thirdPage_map_subtitle"),
                   br(),
                   leafletOutput("third_page_map")
                 ),
                 size = 9
        ),
        makeCard(" ",
                 div(
                   strong(i18n$t("Viewing:")),
                   textOutput("thirdPage_statistics_text0"),
                   br(),
                   strong(i18n$t("At least some difficulty in any domain*:")),
                   textOutput("thirdPage_statistics_text1"),
                   strong(i18n$t("Walking")),
                   textOutput("thirdPage_statistics_text2"),
                   strong(i18n$t("Seeing")),
                   textOutput("thirdPage_statistics_text3"),
                   strong(i18n$t("Hearing")),
                   textOutput("thirdPage_statistics_text4"),
                   strong(i18n$t("Cognition")),
                   textOutput("thirdPage_statistics_text5"),
                   strong(i18n$t("Selfcare")),
                   textOutput("thirdPage_statistics_text6"),
                   strong(i18n$t("Communication")),
                   textOutput("thirdPage_statistics_text7"),
                   br(),
                   p(i18n$t("*included are walking, seeing, hearing, cognition, selfcare, and/or communication"))
                 ),
                 size = 3
        )
      )
    )
)}

thirdPage_first_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(" ",
             third_page_first_stack(i18n),
             size = 11)
  )}

# SECOND STACK: all plot content
third_page_second_stack <- Stack(
  tokens = list(childrenGap = 10),
  Stack(
    tokens = list(childrenGap = 10), horizontal = FALSE,
    makeCard(" ",
             div(
               plotOutput("thirdPage_Dis_ADM1Plot") # Prevalence Rate of Population with at Least Some Difficulty in a Disability Subtopic by Department 
             ),
             size = 11
    )
  )
)

thirdPage_second_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(" ",
             third_page_second_stack,
             size = 11)
  )}

# THIRD STACK: Export disability data pane
third_page_third_stack <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    Stack(
      tokens = list(childrenGap = 10), horizontal = FALSE,
      makeCard(" ",
               div(
                 Text(i18n$t("Export disability status data (in .csv format)")),
                 useShinyjs(),
                 Stack(tokens = list(childrenGap = 10), horizontal = TRUE,
                       DefaultButton.shinyInput("page3_dd", text = i18n$t("Download Disability Status Data"), iconProps = list(iconName = "Download Disability Status Data")),
                       div(style = "visibility: hidden;", downloadButton("page3_ddb", label = "")))
               ),
               size = 11
      )
    )
)}

thirdPage_third_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(i18n$t("Data Download"),
             third_page_third_stack(i18n),
             size = 11)
  )}

# UI
thirdPage <- function(i18n) {
  tagList(
    shiny.i18n::usei18n(i18n),
    makePage( # see components.R for function definition
      i18n$t("Disability Status"),
      i18n$t("Explore each subtopic"),
      div(
        thirdPage_first_stack_content(i18n),
        thirdPage_second_stack_content(i18n),
        thirdPage_third_stack_content(i18n)
      ))
  )
}