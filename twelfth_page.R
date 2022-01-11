#SETUP FUNCTIONS TO GENERATE PAGE SECTIONS, PAGE 12: POWER & TECHNOLOGY#

# Add subtopics to Control panel
nav_picker_twelfthPage <- function(i18n) {
  div(
  class = "nav_picker",
  DefaultButton.shinyInput(
    "electricity_button",
    text = i18n$t("Fuel and Electricity"),
    className = "nav_item"
  ),
  DefaultButton.shinyInput(
    "ICT_button",
    text = i18n$t("ICT"),
    className = "nav_item"
  ),
)}

# UI components
# Stack two cards next to each other
# FIRST STACK: Leaflet map (updates with user subtopic input) + statistics pane
twelfth_page_first_stack <- function(i18n) {
  Stack(
  tokens = list(childrenGap = 10),
  div(
    Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(" ", # see components.R for function definition
               div(
                 strong(textOutput("twelfthPage_map_title")),
                 br(),
                 leafletOutput("twelfth_page_map")
               ),
               size = 9
      ),
      makeCard(" ",
               div(
                 strong(i18n$t("Viewing:")),
                 textOutput("twelfthPage_statistics_text0"),
                 br(),
                 strong(i18n$t("Most Common Fuel for Cooking")),
                 textOutput("twelfthPage_statistics_text1"),
                 strong(i18n$t("Most Common Fuel for Lighting")),
                 textOutput("twelfthPage_statistics_text2"),
                 strong(i18n$t("Electricity Access (Percent of Households)")),
                 textOutput("twelfthPage_statistics_text3"),
                 strong(i18n$t("Internet Access (Percent of Households)")),
                 textOutput("twelfthPage_statistics_text4")
               ),
               size = 3
      )
    )
  )
)}

twelfthPage_first_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(" ",
             twelfth_page_first_stack(i18n), 
             size = 11)
  )}

# SECOND STACK: plot content, changes conditionally based on user-selected subtopic
twelfth_page_second_stack <- Stack(
  tokens = list(childrenGap = 10),
  div(
    conditionalPanel(
      condition = "output.twelfthPage_metric2 != 'ICT'", # when subtopic == Fuel & Electricity
      Stack(
        tokens = list(childrenGap = 10), horizontal = TRUE,
        makeCard(" ",
                 div(
                   plotOutput("twelfthPage_ElectricADM1Plot") # Percent of Households With Electricity
                 ),
                 size = 6
        ),
        makeCard(" ",
                 div(
                   plotOutput("twelfthPage_CookingADMPlot") # Most Common Cooking Fuel by Department
                 ),
                 size = 5
        )
      ),
      Stack(
        tokens = list(childrenGap = 10), horizontal = TRUE,
        makeCard(" ",
                 div(
                   plotOutput("twelfthPage_lightingPlot") # Household Count by Main Lighting Fuel
                 ),
                 size = 6
        ),
        makeCard(" ",
                 div(
                   plotOutput("twelfthPage_cookingPlot") # Household Count by Main Cooking Fuel
                 ),
                 size = 5
        )
      )
    ),
    conditionalPanel(
      condition = "output.twelfthPage_metric2 == 'ICT'", # when subtopic == "ICT"
      Stack(
        tokens = list(childrenGap = 10), horizontal = FALSE,
        makeCard(" ",
                 div(
                   plotOutput("twelfthPage_InternetADM1Plot") # Percent of Households with Internet Access by Department
                 ),
                 size = 11
        ),
        makeCard(" ",
                 div(
                   plotOutput("twelfthPage_ICTPlot") # Household Count by ICT Asset Ownership
                 ),
                 size = 11
        )
      )
    )
  )
)
        

twelfthPage_second_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(" ",
             twelfth_page_second_stack, size = 11)
  )}

# THIRD STACK: Export power& technology data pane
twelfth_page_third_stack <- function(i18n) {
  Stack(
  tokens = list(childrenGap = 10),
  Stack(
    tokens = list(childrenGap = 10), horizontal = FALSE,
    makeCard(" ",
             div(
               Text(i18n$t("Export power and technology data (in .csv format)")),
               useShinyjs(),
               Stack(tokens = list(childrenGap = 10), horizontal = TRUE,
                     DefaultButton.shinyInput("page12_dd", text = i18n$t("Download Power and Technology Data"), iconProps = list(iconName = "Download Power & Technology Data")),
                     div(style = "visibility: hidden;", downloadButton("page12_ddb", label = "")))),
             size = 11
    )
  )
)}

twelfthPage_third_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(i18n$t("Data Download"),
             twelfth_page_third_stack(i18n),
             size = 11)
  )}

# UI
twelfthPage <- function(i18n) {
  tagList( 
    shiny.i18n::usei18n(i18n),
    makePage( # see components.R for function definition
      i18n$t('Power and Technology'),
      i18n$t("Explore each subtopic"),
      div(
        twelfthPage_first_stack_content(i18n),
        twelfthPage_second_stack_content(i18n),
        twelfthPage_third_stack_content(i18n)
      ))
    )
}