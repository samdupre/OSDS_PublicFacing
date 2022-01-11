#SETUP FUNCTIONS TO GENERATE PAGE SECTIONS, PAGE 5: DOMESTIC MIGRATION#

# Add subtopics to Control panel
nav_picker_fifthPage <- function(i18n) {
  div(
  class = "nav_picker",
  DefaultButton.shinyInput(
    "net_button",
    text = i18n$t("Net Migration"),
    className = "nav_item"
  ),
  DefaultButton.shinyInput(
    "in_button",
    text = i18n$t("Inbound"),
    className = "nav_item"
  ),
  DefaultButton.shinyInput(
    "out_button",
    text = i18n$t("Outbound"),
    className = "nav_item"
  )
)}

# UI components
# Stack two cards next to each other
# FIRST STACK: Leaflet map + statistics pane
fifth_page_first_stack <- function(i18n) {
  Stack(
  tokens = list(childrenGap = 10),
  div(
    Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(" ", # see components.R for function definition
               div(
                 Pivot(
                   PivotItem(headerText = i18n$t("National"),
                             strong(textOutput("fifthPage_map_national_title")),
                             br(),
                             leafletOutput("fifth_page_map_national")
                   ),
                   PivotItem(headerText = i18n$t("Internal Flows"),
                             strong(textOutput("fifthPage_map_flows_title")),
                             textOutput("fifthPage_map_flows_subtitle"),
                             br(),
                             textOutput("fifthPage_map_flows_text"),
                             br(),
                             div(
                               leafletOutput("fifth_page_map_flows")
                             ))
                   )
              ),
               size = 9
      ),
      makeCard(" ",
               div(
                 strong(i18n$t("Viewing:")),
                 textOutput("fifthPage_statistics_text0"),
                 br(),
                 strong(i18n$t("Most common destination from here")),
                 textOutput("fifthPage_statistics_text1"),
                 strong(i18n$t("Population that is a new resident")),
                 textOutput("fifthPage_statistics_text2"),
                 strong(i18n$t("Movers from a different department")),
                 textOutput("fifthPage_statistics_text3"),
                 strong(i18n$t("Movers to a different department")),
                 textOutput("fifthPage_statistics_text4"),
                 strong(i18n$t("Movers from abroad")),
                 textOutput("fifthPage_statistics_text5")
               ),
               size = 3
      )
    )
  )
)}


fifthPage_first_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(" ",
             fifth_page_first_stack(i18n), 
             size = 11)
  )}

# SECOND STACK: All plot content; plots appear reactively as users choose subtopics
fifth_page_second_stack <- Stack(
  tokens = list(childrenGap = 10),
  div(Stack(
    tokens = list(childrenGap = 10), horizontal = TRUE,
    makeCard(" ",
             div(
               plotOutput("fifthPage_national_ADM1Plot")
             ),
             size = 11
   )
  )
 )
)

fifthPage_second_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(" ",
             fifth_page_second_stack, size = 11)
  )}

# THIRD STACK: Export domestic migration data pane
fifth_page_third_stack <- function(i18n) {
  Stack(
  tokens = list(childrenGap = 10),
  div(Stack(
    tokens = list(childrenGap = 10), horizontal = TRUE,
    makeCard(" ",
             div(
               Text(i18n$t("Export domestic migration data (in .csv format)")),
               useShinyjs(),
               Stack(tokens = list(childrenGap = 10), horizontal = TRUE,
                     DefaultButton.shinyInput("page5_dd", text = i18n$t("Download Domestic Migration Data"), iconProps = list(iconName = "Download Domestic Migration Data")),
                     div(style = "visibility: hidden;", downloadButton("page5_ddb", label = "")))),
             size = 11
    )
  )
 )
)}

fifthPage_third_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(i18n$t("Data Download"),
             fifth_page_third_stack(i18n), 
             size = 11)
  )}

# UI
fifthPage <- function(i18n) {
  tagList(
    shiny.i18n::usei18n(i18n),
    makePage( # see components.R for function definition
      i18n$t('Domestic Migration'),
      i18n$t("Explore each subtopic"),
      div(
        fifthPage_first_stack_content(i18n),
        fifthPage_second_stack_content(i18n),
        fifthPage_third_stack_content(i18n)
      ))
  )
}