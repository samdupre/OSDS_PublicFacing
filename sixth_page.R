#SETUP FUNCTIONS TO GENERATE PAGE SECTIONS, PAGE 6: MORTALITY#

# UI components
# Stack two cards next to each other
# FIRST STACK: Leaflet map + statistics pane
sixth_page_first_stack <- function(i18n) {
  Stack(
  tokens = list(childrenGap = 10),
  div(
    Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(" ", # see components.R for function definition
               div(
                 strong(i18n$t("Crude Death Rate by Municipality, Guatemala 2018")),
                 br(),
                 strong(i18n$t("Annual deaths per 1,000 people")),
                 br(),
                 leafletOutput("sixth_page_map")
                 ),
               size = 9
      ),
      makeCard(" ",
               div(
                 strong(i18n$t("Viewing:")),
                 textOutput("sixthPage_statistics_text0"),
                 br(),
                 strong(i18n$t("Deaths")),
                 textOutput("sixthPage_statistics_text1"),
                 strong(i18n$t("Crude Death Rate")),
                 textOutput("sixthPage_statistics_text2"),
                 strong(i18n$t("Infant Mortality Rate")),
                 textOutput("sixthPage_statistics_text3"),
                 strong(i18n$t("Child Mortality Rate")),
                 textOutput("sixthPage_statistics_text4")
               ),
               size = 3
      )
    )
  )
)}

# SECOND STACK: All plot content
sixthPage_first_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(" ",
             sixth_page_first_stack(i18n), 
             size = 11)
  )}

sixth_page_second_stack <- Stack(
  tokens = list(childrenGap = 10),
  div(
    Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(" ",
               div(
                 plotOutput("sixthPage_Deaths_ADM1Plot")), # Crude Death Rate by Department
               size = 6
      ),
      
      makeCard(" ",
               div(
                 plotOutput("sixthPage_death_cause")), # Deaths by Cause
               size = 5
      )
    ),
    Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(" ",
               div(
                 plotOutput("sixthPage_death_pyramid")), # Deaths by Age Category
               size = 6
               )
      )
  )
)


sixthPage_second_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(" ",
             sixth_page_second_stack, size = 11)
  )}

# THIRD STACK: Export mortality data pane
sixth_page_third_stack <- function(i18n) {
  Stack(
  tokens = list(childrenGap = 10),
  Stack(
    tokens = list(childrenGap = 10), horizontal = FALSE,
    makeCard(" ",
             div(
               Text(i18n$t("Export mortality data (in .csv format)")),
               useShinyjs(),
               Stack(tokens = list(childrenGap = 10), horizontal = TRUE,
                     DefaultButton.shinyInput("page6_dd", 
                                              text = i18n$t("Download Mortality Data"), 
                                              iconProps = list(iconName = "Download Mortality Data")),
                     div(style = "visibility: hidden;", 
                         downloadButton("page6_ddb", 
                                        label = "")))),
             size = 11
    )
  )
)}

sixthPage_third_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(i18n$t("Data Download"),
             sixth_page_third_stack(i18n),
             size = 11)
  )}

# UI
sixthPage <- function(i18n) {
  tagList(
    shiny.i18n::usei18n(i18n),
    makePage( # see components.R for function definition
      i18n$t('Mortality'),
      i18n$t("Explore each subtopic"),
      div(
        sixthPage_first_stack_content(i18n),
        sixthPage_second_stack_content(i18n),
        sixthPage_third_stack_content(i18n)
      ))
  )
}