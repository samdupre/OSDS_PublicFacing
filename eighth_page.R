#SETUP FUNCTIONS TO GENERATE PAGE SECTIONS, PAGE 8: EDUCATION#

# Add subtopics to Control panel
nav_picker_eighthPage <- function(i18n) {
  div(
  class = "nav_picker",
  DefaultButton.shinyInput(
    "aLit_button",
    text = i18n$t("Adult Literacy"),
    className = "nav_item"
  ),
  DefaultButton.shinyInput(
    "yLit_button",
    text = i18n$t("Youth Literacy"),
    className = "nav_item"
  )
)}

# UI components
# Stack two cards next to each other
# FIRST STACK: Leaflet map + statistics pane
eighth_page_first_stack <- function(i18n) {
  Stack(
  tokens = list(childrenGap = 10),
  div(
    Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(" ", # see components.R for function definition
               div(
                 strong(i18n$t("Literacy Rate by Municipality, Guatemala 2018")),
                 textOutput("eighthPage_map_subtitle"),
                 br(),
                 leafletOutput("eighth_page_map")
               ),
               size = 9
      ),
      makeCard(" ",
               div(
                 strong(i18n$t("Viewing:")),
                 textOutput("eighthPage_statistics_text0"),
                 br(),
                 strong(i18n$t("Adult Literacy Rate")),
                 textOutput("eighthPage_statistics_text1"),
                 strong(i18n$t("Youth Literacy Rate")),
                 textOutput("eighthPage_statistics_text2"),
                 strong(i18n$t("Primary School Attendance")),
                 textOutput("eighthPage_statistics_text3"),
                 strong(i18n$t("Secondary School Attendance")),
                 textOutput("eighthPage_statistics_text4")
               ),
               size = 3
      )
    )
  )
)}


eighthPage_first_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(" ",
             eighth_page_first_stack(i18n), 
             size = 11)
  )}

# SECOND STACK: Adult Literacy Rate by Department, School Attendance 
# by Sex, & Educational Attainment PLOTS
eighth_page_second_stack <- Stack(
  tokens = list(childrenGap = 10),
  div(
    Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(" ",
               div(
                 plotOutput("eighthPage_LiteracyRate_ADM1Plot")), # Adult Literacy Rate by Department
               size = 6
      ),
      
      makeCard(" ",
               div(
                 plotOutput("eighthPage_attendance")), # School Attendance by Sex
               size = 5
      )
    ),
    Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(" ",
               div(
                 plotOutput("eighthPage_attainment")), # Educational Attainment
               size = 11
      )
    )
  )
)

eighthPage_second_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(" ",
             eighth_page_second_stack, size = 11)
  )}

# THIRD STACK: Export education data pane
eighth_page_third_stack <- function(i18n) {
  Stack(
  tokens = list(childrenGap = 10),
  Stack(
    tokens = list(childrenGap = 10), horizontal = FALSE,
    makeCard(" ",
             div(
               Text(i18n$t("Export education data (in .csv format)")),
               useShinyjs(),
               Stack(tokens = list(childrenGap = 10), horizontal = TRUE,
                     DefaultButton.shinyInput("page8_dd", text = i18n$t("Download Education Data"), iconProps = list(iconName = "Download Education Data")),
                     div(style = "visibility: hidden;", downloadButton("page8_ddb", label = "")))),
             size = 11
    )
  )
)}

eighthPage_third_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(i18n$t("Data Download"),
             eighth_page_third_stack(i18n),
             size = 11)
  )}

# UI
eighthPage <- function(i18n) {
  tagList(
    shiny.i18n::usei18n(i18n),
    makePage( # see components.R for function definition
      i18n$t("Education"),
      i18n$t("Explore each subtopic"),
      div(
        eighthPage_first_stack_content(i18n),
        eighthPage_second_stack_content(i18n),
        eighthPage_third_stack_content(i18n)
      ))
    )
}
  

