#SETUP FUNCTIONS TO GENERATE PAGE SECTIONS, PAGE 2: HOUSEHOLD AND FAMILY#

# UI components
# Stack two cards next to each other
# FIRST STACK: Leaflet map + statistics pane
second_page_first_stack <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(" ", # see components.R for function definition
               div(
                 strong(i18n$t("Average Household Size by Municipality, Guatemala 2018")),
                 br(),
                 leafletOutput("second_page_map")
               ),
               size = 9
      ),
      makeCard(" ",
               div(
                 strong(i18n$t("Viewing:")),
                 textOutput("secondPage_statistics_text0"),
                 br(),
                 strong(i18n$t("Households")),
                 textOutput("secondPage_statistics_text1"),
                 strong(i18n$t("Average Household Size")),
                 textOutput("secondPage_statistics_text2")
               ),
               size = 3
      )
    )
)}

secondPage_first_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(" ",
             second_page_first_stack(i18n),
             size = 11)
  )}

# SECOND STACK: All plot content
second_page_second_stack <- Stack(
  tokens = list(childrenGap = 10),
  Stack(
    tokens = list(childrenGap = 10), horizontal = TRUE,
    makeCard(" ",
           div(
             plotOutput("secondPage_HHSize_ADM1Plot") # Average Household Size by Department
           ),
           size = 6
    ),
    makeCard(" ",
             div(
               plotOutput("secondPage_HouseholdTypePlot") # Household Count by Type
             ),
             size = 5
    )
  ),
  Stack(
    tokens = list(childrenGap = 10), horizontal = TRUE,
    makeCard(" ",
             div(
               plotOutput("secondPage_FamilyNucleusPlot") # Household Count by Family Nucleus 
             ),
             size = 6
    ),
    makeCard(" ",
             div(
               plotOutput("secondPage_MaritalStatusPlot") # Population by Marital Status
             ),
             size = 5
    )
  ),
  Stack(
    tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(" ",
             div(
               plotOutput("secondPage_RefPersonRelationshipPlot") # Population by Relationship to the Reference Person of the Household
             ),
             size = 6
    )
  )
)

secondPage_second_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(" ",
             second_page_second_stack,
             size = 11)
  )}

# THIRD STACK: Export household & family data pane
second_page_third_stack <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    Stack(
      tokens = list(childrenGap = 10), horizontal = FALSE,
      makeCard(" ",
               div(
                 Text(i18n$t("Export household and family data (in .csv format)")),
                 useShinyjs(),
                 Stack(tokens = list(childrenGap = 10), horizontal = TRUE,
                       DefaultButton.shinyInput("page2_dd", text = i18n$t("Download Household and Family Data"), iconProps = list(iconName = "Download Household & Family Data")),
                       div(style = "visibility: hidden;", downloadButton("page2_ddb", label = "")))
               ),
               size = 11
      )
    )
)}

secondPage_third_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(i18n$t("Data Download"),
             second_page_third_stack(i18n),
             size = 11)
  )}

#UI

secondPage <- function(i18n) {
  tagList(
    shiny.i18n::usei18n(i18n),
    makePage( # see components.R for function definition
      i18n$t('Household and Family Characteristics'),
      i18n$t("Explore each subtopic"),
      div(
        secondPage_first_stack_content(i18n),
        secondPage_second_stack_content(i18n),
        secondPage_third_stack_content(i18n)
      ))
    )
}

