#SETUP FUNCTIONS TO GENERATE PAGE SECTIONS, SIDEBAR#

# UI components
# FIRST STACK: build "Select Location" dropdown and "subtopic" buttons under Controls pane
sidebar_first_stack <- function(i18n) {
  tagList(
    shiny.i18n::usei18n(i18n),
    Stack(
      makeCard(i18n$t("Controls"), # see components.R for function definition
               Stack(
                 Label(i18n$t("Select a location")),
                 Dropdown.shinyInput("dropdown", 
                                     value = "All", 
                                     options = ADM1s),
                 br(),
                 conditionalPanel(
                   condition = "output.current_page == 3",
                   br(),
                   Label(i18n$t("Select a subtopic")),
                   nav_picker_thirdPage(i18n)
                 ),
                 conditionalPanel(
                   condition = "output.current_page == 4",
                   br(),
                   Label(i18n$t("Select a subtopic")),
                   nav_picker_fourthPage(i18n)
                 ),
                 conditionalPanel(
                   condition = "output.current_page == 5",
                   br(),
                   Label(i18n$t("Select a metric")),
                   nav_picker_fifthPage(i18n)
                 ),
                 conditionalPanel(
                   condition = "output.current_page == 8",
                   br(),
                   Label(i18n$t("Select a metric")),
                   nav_picker_eighthPage(i18n)
                 ),
                 conditionalPanel(
                   condition = "output.current_page == 11",
                   br(),
                   Label(i18n$t("Select a subtopic")),
                   nav_picker_eleventhPage(i18n)
                 ),
                 conditionalPanel(
                   condition = "output.current_page == 12",
                   br(),
                   Label(i18n$t("Select a subtopic")),
                   nav_picker_twelfthPage(i18n)
                 )
               ),
               size = 11)
    )
  )}

sidebar_first_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 20),
    sidebar_first_stack(i18n), 
             size = 12)
  }

# SECOND STACK: Build navigation to various pages
sidebar_second_stack <- function(i18n) {
  tagList(
    shiny.i18n::usei18n(i18n),
    Stack(
    Nav(
      groups = list(
        list(links = list(
          list(name = i18n$t('Home'), url = '#!/', key = 'sidebar_first', icon = 'Home'),
          list(name = i18n$t('Demographic and Social'), url = '#!/1', key = 'sidebar_second', icon = 'Group'),
          list(name = i18n$t('Household and Family'), url = '#!/2', key = 'sidebar_third', icon = 'Street'),
          list(name = i18n$t('Disability Status'), url = '#!/3', key = 'sidebar_fourth', icon = 'Glasses'),
          list(name = i18n$t('International Migration'), url = '#!/4', key = 'sidebar_fifth', icon = 'AnalyticsQuery'),
          list(name = i18n$t('Domestic Migration'), url = '#!/5', key = 'sidebar_sixth', icon = 'World'),
          list(name = i18n$t('Mortality'), url = '#!/6', key = 'sidebar_seventh', icon = 'Health'),
          list(name = i18n$t('Fertility'), url = '#!/7', key = 'sidebar_eighth', icon = 'Family'),
          list(name = i18n$t('Education'), url = '#!/8', key = 'sidebar_ninth', icon = 'Education'),
          list(name = i18n$t('Labour and Economy'), url = '#!/9', key = 'sidebar_tenth', icon = 'BacklogBoard'),
          list(name = i18n$t('Housing'), url = '#!/10', key = 'sidebar_eleventh', icon = 'Home'),
          list(name = i18n$t('Water, Sanitation and Hygiene'), url = '#!/11', key = 'sidebar_twelfth', icon = 'Precipitation'),
          list(name = i18n$t('Power and Technology'), url = '#!/12', key = 'sidebar_thirteenth', icon = 'LightningBolt')
        ))
      ),
      initialSelectedKey = 'sidebar_main_first',
      styles = list(
        root = list(
          height = '100%',
          boxSizing = 'border-box',
          overflowY = 'auto'
        )
      ))
  ))}

sidebar_second_stack_content <- function(i18n) {
  Stack(
    tokens = list(childrenGap = 20),
    sidebar_second_stack(i18n), 
             size = 12)
  }

# UI
sidebar <- function(i18n) {
  tagList(
    shiny.i18n::usei18n(i18n),
    makePage( # see components.R for function definition
      title = "",
      subtitle = "",
      div(
        sidebar_first_stack_content(i18n),
        sidebar_second_stack_content(i18n)
      ))
  )
}