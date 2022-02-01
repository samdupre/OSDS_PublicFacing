#SETUP FUNCTIONS TO GENERATE PAGE SECTIONS, PAGE 7: FERTILITY#

#UI components
# Stack two cards next to each other
# FIRST STACK: Leaflet map + statistics pane
seventh_page_first_stack <- function(i18n) {
  Stack(
  tokens = list(childrenGap = 10),
  div(
    Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(" ", # see components.R for function definition
               div(
                 strong(i18n$t("Crude Birth Rate by Municipality, Guatemala 2018")),
                 br(),
                 strong(i18n$t("Annual live births per 1,000 people")),
                 br(),
                 leafletOutput("seventh_page_map")
               ),
               size = 9
      ),
      makeCard(" ",
               div(
                 strong(i18n$t("Viewing:")),
                 textOutput("seventhPage_statistics_text0"),
                 br(),
                 strong(i18n$t("Births")),
                 textOutput("seventhPage_statistics_text1"),
                 strong(i18n$t("Crude Birth Rate")),
                 textOutput("seventhPage_statistics_text2"),
                 strong(i18n$t("Total Fertility Rate")),
                 textOutput("seventhPage_statistics_text3"),
                 strong(i18n$t("Life Expectancy at Birth")),
                 textOutput("seventhPage_statistics_text4"),
                 strong(i18n$t("Sex Ratio at Birth")),
                 textOutput("seventhPage_statistics_text5")
               ),
               size = 3
      )
    )
  )
)}


seventhPage_first_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(" ",
             seventh_page_first_stack(i18n), 
             size = 11)
  )}

# SECOND STACK: All plot content
seventh_page_second_stack <- Stack(
  tokens = list(childrenGap = 10),
  div(

    Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(" ",
               div(
                 plotOutput("seventhPage_CBRate_ADM1Plot")), # Crude birth rate
               size = 11
      )
    )
  )
)


seventhPage_second_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(" ",
             seventh_page_second_stack, size = 11)
  )}

# THIRD STACK: Export fertility data pane
seventh_page_third_stack <- function(i18n) {
  Stack(
  tokens = list(childrenGap = 10),
  div(Stack(
    tokens = list(childrenGap = 10), horizontal = TRUE,
    makeCard(" ",
             div(
               Text(i18n$t("Export fertility data (in .csv format)")),
               useShinyjs(),
               Stack(tokens = list(childrenGap = 10), horizontal = TRUE,
                     DefaultButton.shinyInput("page7_dd", text = i18n$t("Download Fertility Data"), iconProps = list(iconName = "Download Fertility Data")),
                     div(style = "visibility: hidden;", downloadButton("page7_ddb", label = "")))),
             size = 11
    )
  ))
)}

seventhPage_third_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(i18n$t("Data Download"),
             seventh_page_third_stack(i18n), 
             size = 11)
  )}

# UI
seventhPage <- function(i18n) {
  tagList(
    shiny.i18n::usei18n(i18n),
    makePage( # see components.R for function definition
      i18n$t('Fertility'),
      i18n$t("Explore each subtopic"),
      div(
        seventhPage_first_stack_content(i18n),
        seventhPage_second_stack_content(i18n),
        seventhPage_third_stack_content(i18n)
      ))
  )
}