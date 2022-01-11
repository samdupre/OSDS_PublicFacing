#SETUP FUNCTIONS TO GENERATE PAGE SECTIONS, PAGE 1: DEMOGRAPHIC AND SOCIAL#

# UI components
# Stack two cards next to each other
# FIRST STACK: Leaflet map + statistics pane
first_page_overview_stack <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(" ", # see components.R for function definition
               div(
                 strong(i18n$t("Population by Municipality, Guatemala 2018")),
                 br(),
                 leafletOutput("first_page_map")
               ),
               size = 9
      ),
      makeCard(" ",
               div(
                 strong(i18n$t("Viewing:")),
                 textOutput("firstPage_statistics_text0"),
                 br(),
                 strong(i18n$t("Total Population")),
                 textOutput("firstPage_statistics_text1"),
                 strong(i18n$t("Population Density")),
                 textOutput("firstPage_statistics_text2"),
                 strong(i18n$t("Sex Ratio")),
                 textOutput("firstPage_statistics_text3"),
                 strong(i18n$t("Percent Aged 0 to 14 Years")),
                 textOutput("firstPage_statistics_text4"),
                 strong(i18n$t("Percent Aged 15 to 64 Years")),
                 textOutput("firstPage_statistics_text5"),
                 strong(i18n$t("Percent Aged 65+ Years")),
                 textOutput("firstPage_statistics_text6"),
                 strong(i18n$t("Dependency Ratio, Total")),
                 textOutput("firstPage_statistics_text7"),
                 strong(i18n$t("Dependency Ratio, Child")),
                 textOutput("firstPage_statistics_text8"),
                 strong(i18n$t("Dependency Ratio, Aged")),
                 textOutput("firstPage_statistics_text9"),
                 strong(i18n$t("Median Age")),
                 textOutput("firstPage_statistics_text10"),
                 strong(i18n$t("Life Expectancy")),
                 textOutput("firstPage_statistics_text11")
               ),
               size = 3
      )
    )
  )}

firstPage_first_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(" ",
             first_page_overview_stack(i18n), size = 11)
  )}

# SECOND STACK: All plot contents
first_page_second_stack <- Stack(
  tokens = list(childrenGap = 10),
  div(
    Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(" ",
               div(
                 plotOutput("firstPage_Pop_ADM1Plot")), # Population by Department
               size = 6
      ),
      makeCard(" ",
               div(
                 plotOutput("firstPage_Sex")), # Population by Sex
               size = 5
      ),
    ),
    Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(" ",
               div(
                 plotOutput("firstPage_UrbanRural")), # Population by Urban/Rural Status
               size = 6
      ),
      makeCard(" ",
               div(
                 plotOutput("firstPage_Age")), # Population by Age
               size = 5
      )
    ) 
  )
)

firstPage_second_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(" ",
             first_page_second_stack, size = 11)
  )}

# THIRD STACK: Export demographic & social data pane
first_page_third_stack <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    Stack(
      tokens = list(childrenGap = 10), horizontal = FALSE,
      makeCard(" ",
               div(
                 Text(i18n$t("Export demographic and social data (in .csv format)")),
                 useShinyjs(),
                 Stack(tokens = list(childrenGap = 10), horizontal = TRUE,
                       DefaultButton.shinyInput("page1_dd", 
                                                text = i18n$t("Download Demographic and Social Data"), 
                                                iconProps = list(iconName = "Download Demographic and Social Data")),
                       div(
                         style = "visibility: hidden;",
                         downloadButton("page1_ddb", 
                                          label = "")))),
               size = 11
      )
    )
)}

firstPage_third_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(i18n$t("Data Download"),
             first_page_third_stack(i18n),
             size = 11)
  )}

# UI
firstPage <- function(i18n) {
  tagList(
    shiny.i18n::usei18n(i18n),
    makePage( # see components.R for function definition
      i18n$t('Demography and Social Characteristics'),
      i18n$t('Explore each subtopic'),
      div(
        firstPage_first_stack_content(i18n),
        firstPage_second_stack_content(i18n),
        firstPage_third_stack_content(i18n)
      ))
  )
}