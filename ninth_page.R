#SETUP FUNCTIONS TO GENERATE PAGE SECTIONS, PAGE 9: LABOUR AND ECONOMY#

# UI components
# Stack two cards next to each other
# FIRST STACK: Leaflet map + statistics pane
ninth_page_first_stack <- function(i18n) {
  Stack(
  tokens = list(childrenGap = 10),
  div(
    Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(" ", # see components.R for function definition
               div(
                 strong(i18n$t("Unemployment Rate by Municipality, Guatemala 2018")),
                 br(),
                 leafletOutput("ninth_page_map")
               ),
               size = 9

      ),
      makeCard(" ",
               div(
                 strong(i18n$t("Viewing:")),
                 textOutput("ninthPage_statistics_text0"),
                 br(),
                 strong(i18n$t("Unemployment Rate")),
                 textOutput("ninthPage_statistics_text1")
               ),
               size = 3
      )
    )
  )
)}

ninthPage_first_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(" ",
             ninth_page_first_stack(i18n), 
             size = 11)
  )}

# SECOND STACK: All plot content
ninth_page_second_stack <- Stack(
  tokens = list(childrenGap = 10),
  div(
    Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(" ",
               div(
                 plotOutput("ninthPage_national_ADM1Plot")), # Unemployment Rate by Department
               size = 6
      ),

      makeCard(" ",
               div(
                 plotOutput("ninthPage_LabourStatus")), # Population by Labour Force Status
               size = 5
      )
    ),
    Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(" ",
               div(
                 plotOutput("ninthPage_EmploymentStatus1")), # Population by Employment Status
               size = 6
      ),
      makeCard(" ",
               div(
                 plotOutput("ninthPage_EmploymentStatus2")), # Self-Employed Population by Employment Status
               size = 5
      )
    ),
    Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(" ",
               div(
                 plotOutput("ninthPage_Occupation")), # Population by Occupation
               size = 11
      )
    ),
    Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(" ",
               div(
                 plotOutput("ninthPage_Industry")), # Population by Industry
               size = 11
      )
    ),
    Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(" ",
               div(
                 plotOutput("ninthPage_ParticipationOwnUseProductionofGoods")), # Participation in Own-Use Production of Goods by Department
               size = 11
      )
    )
  )
)

ninthPage_second_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(" ",
             ninth_page_second_stack, size = 11)
  )}

# THIRD STACK: Export labour & economy data pane
ninth_page_third_stack <- function(i18n) {
  Stack(
  tokens = list(childrenGap = 10),
  Stack(
    tokens = list(childrenGap = 10), horizontal = FALSE,
    makeCard(" ",
             div(
               Text(i18n$t("Export labour and economy data (in .csv format)")),
               useShinyjs(),
               Stack(tokens = list(childrenGap = 10), horizontal = TRUE,
                     DefaultButton.shinyInput("page9_dd", text = i18n$t("Download Labour and Economy Data"), iconProps = list(iconName = "Download Labour and Economy Data")),
                     div(style = "visibility: hidden;", downloadButton("page9_ddb", label = "")))
    ),
    size = 11
  )
 )
)}

ninthPage_third_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(i18n$t("Data Download"),
             ninth_page_third_stack(i18n),
             size = 11)
  )}

# UI
ninthPage <- function(i18n) {
  tagList(
    shiny.i18n::usei18n(i18n),
    makePage( # see components.R for function definition
      i18n$t('Labour and Economy'),
      i18n$t("Explore each subtopic"),
      div(
        ninthPage_first_stack_content(i18n),
        ninthPage_second_stack_content(i18n),
        ninthPage_third_stack_content(i18n)
      ))
    )
}