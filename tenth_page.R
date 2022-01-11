#SETUP FUNCTIONS TO GENERATE PAGE SECTIONS, PAGE 10: HOUSING#

# UI components
# Stack two cards next to each other
# FIRST STACK: Leaflet map + statistics pane
tenth_page_first_stack <- function(i18n) {
  Stack(
  tokens = list(childrenGap = 10),
  div(
    Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(" ", # see components.R for function definition
               div(
                 strong(i18n$t("Housing Unit Count by Municipality, Guatemala 2018")),
                 br(),
                 leafletOutput("tenth_page_map")
               ),
               size = 9
      ),
      makeCard(" ",
               div(
                 strong(i18n$t("Viewing:")),
                 textOutput("tenthPage_statistics_text0"),
                 br(),
                 strong(i18n$t("Average Rooms per Housing Unit")),
                 textOutput("tenthPage_statistics_text1"),
                 strong(i18n$t("Average People per Housing Unit Room")),
                 textOutput("tenthPage_statistics_text2")
               ),
               size = 3
      )
    )
  )
)}


tenthPage_first_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(" ",
             tenth_page_first_stack(i18n), 
             size = 11)
  )}

# SECOND STACK: Living Quarters content
tenth_page_second_stack <- Stack(
  tokens = list(childrenGap = 10),
  div(
    Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(" ",
               div(
                 plotOutput("tenthPage_national_ADM1Plot")), # Housing Unit Count by Department
               size = 6
      ),

      makeCard(" ",
               div(
                 plotOutput("tenthPage_LQType")), # Living Quarters Count by Type
               size = 5
               
      )),
      Stack(
        tokens = list(childrenGap = 10), horizontal = TRUE,
        makeCard(" ",
                 div(
                   plotOutput("tenthPage_HUType")), # Housing Unit Count by Type
                 size = 6
        )
    )
  )
)

tenthPage_second_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(i18n$t("Living Quarters"),
             tenth_page_second_stack, size = 11)
  )}

# THIRD STACK: Occupied Structures, Ownership, and Tenure content
tenth_page_third_stack <- Stack(
  tokens = list(childrenGap = 10),
  div(
    Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(" ",
               div(
                 plotOutput("tenthPage_Occupancy")), # Conventional Dwelling Count by Occupancy Status
               size = 6
      ),
      
      makeCard(" ",
               div(
                 plotOutput("tenthPage_Ownership")), # Housing Unit Count by Ownership Status
               size = 5
      )
    ),
    Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(" ",
               div(
                 plotOutput("tenthPage_Tenure")), # Housing Unit Count by Tenure Status
               size = 6
      )
    )
  )
)

tenthPage_third_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(i18n$t("Occupied Structures, Ownership, and Tenure"),
             tenth_page_third_stack, size = 11)
  )}

# FOURTH STACK: Build Characteristics content
tenth_page_fourth_stack <- Stack(
  tokens = list(childrenGap = 10),
  div(
    Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(" ",
               div(
                 plotOutput("tenthPage_BuildingType1")), # Building Count by Type
               size = 6
      ),

      makeCard(" ",
               div(
                 plotOutput("tenthPage_BuildingType2")), # Residential Building Count by Type
               size = 5
      )
    ),
    Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(" ",
               div(
                 plotOutput("tenthPage_OuterWalls")), # Building Count by Outer Wall Material
               size = 6
      ),

      makeCard(" ",
               div(
                 plotOutput("tenthPage_NumRooms")), # Housing Unit Count by Number of Rooms
               size = 5
      )
    ),
    Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(" ",
               div(
                 plotOutput("tenthPage_OccupancyDensityPeople")), # Average People per Housing Unit Room by Department
               size = 6
      ),
      
      makeCard(" ",
               div(
                 plotOutput("tenthPage_OccupancyDensityHH")), # Percent of Living Quarters With More Than One Household by Department
               size = 5
      )
    )
      )
    )

tenthPage_fourth_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(i18n$t("Building Characteristics"),
             tenth_page_fourth_stack, size = 11)
  )}

# FIFTH STACK: Export housing data pane
tenth_page_fifth_stack <- function(i18n) {
  Stack(
  tokens = list(childrenGap = 10),
  Stack(
    tokens = list(childrenGap = 10), horizontal = FALSE,
    makeCard(" ",
             div(
               Text(i18n$t("Export housing data (in .csv format)")),
               useShinyjs(),
               Stack(tokens = list(childrenGap = 10), horizontal = TRUE,
                     DefaultButton.shinyInput("page10_dd", text = i18n$t("Download Housing Data"), iconProps = list(iconName = "Download Housing Data")),
                     div(style = "visibility: hidden;", downloadButton("page10_ddb", label = "")))),
             size = 11
    )
  )
)}

tenthPage_fifth_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(i18n$t("Data Download"),
             tenth_page_fifth_stack(i18n),
             size = 11)
  )}

# UI
tenthPage <- function(i18n) {
  tagList(
    shiny.i18n::usei18n(i18n),
    makePage( # see components.R for function definition
      i18n$t("Housing"),
      i18n$t("Explore each subtopic"),
      div(
        tenthPage_first_stack_content(i18n),
        tenthPage_second_stack_content(i18n),
        tenthPage_third_stack_content(i18n),
        tenthPage_fourth_stack_content(i18n),
        tenthPage_fifth_stack_content(i18n)
      ))
    )
}