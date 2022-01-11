################################################################################
#### OSDS ####
################################################################################
# Bring in all packages, components, pages, and data############################
source("packages-data-run.R", encoding = "utf-8") # makes all data, packages, and objects available to app, including built functions
source("components.R")       # Specifications for various components
source("footer.R")           # OSDS information
source("header.R")           # Language dropdown
source("side_bar.R")         # Navigation on left side of browser window
source("landing_page.R")     # Home
source("first_page.R")       # Demographic & Social
source("second_page.R")      # Household & Family
source("third_page.R")       # Disability Status
source("fourth_page.R")      # International Migration
source("fifth_page.R")       # Domestic Migration
source("sixth_page.R")       # Mortality
source("seventh_page.R")     # Fertility
source("eighth_page.R")      # Education
source("ninth_page.R")       # Labour & Economy
source("tenth_page.R")       # Housing
source("eleventh_page.R")    # Water, Sanitation, & Hygiene
source("twelfth_page.R")     # Power & Technology

#SETUP THE ROUTER FUNCTIONALITY######################################
# allows each page to be navigable based on browser definition; e.g., /1 = Page 1, "Demographic and Social"
# ensures each page is translated using i18n
router <- make_router(
  route("/", landingPage(i18n)),
  route("1", firstPage(i18n)),
  route("2", secondPage(i18n)),
  route("3", thirdPage(i18n)),
  route("4", fourthPage(i18n)),
  route("5", fifthPage(i18n)),
  route("6", sixthPage(i18n)),
  route("7", seventhPage(i18n)),
  route("8", eighthPage(i18n)),
  route("9", ninthPage(i18n)),
  route("10", tenthPage(i18n)),
  route("11", eleventhPage(i18n)),
  route("12", twelfthPage(i18n))
)

# Add shiny.router dependencies manually: they are not picked up because they're added in a non-standard way.
shiny::addResourcePath("shiny.router", system.file("www", package = "shiny.router"))
shiny_router_js_src <- file.path("shiny.router", "shiny.router.js")
shiny_router_script_tag <- shiny::tags$script(type = "text/javascript", src = shiny_router_js_src)

#SETUP LAYOUT for the UI########################################################
#For all css specifications, see www/style.css

layout <- function(mainUI){
  div(class = "grid-container",
      div(class = "header",
          header(i18n)),
      div(class = "sidenav",
          sidebar(i18n)),
      div(class = "main", mainUI),
      div(class = "footer",
          footer(i18n))
  )
}

#UI#############################################################################

ui <- fluentPage(
  useShinyjs(),
  shiny.i18n::usei18n(i18n), # allows direct translation across the app
  layout(router$ui),
  tags$head(
    includeCSS("./www/style.css"), # see the www folder for css specifications
    shiny_router_script_tag # allows each page to be navigable based on browser definition; e.g., /1 = Page 1
  ),
  tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Lato');
                    ")) # imports Lato, a google font, to use across the a[[]]
)

#SERVER#########################################################################
# Allows user input and navigation to change the UI#############################

server <- function(input, output, session) {
  
  router$server(input, output, session)

  output$current_page <- renderText({
    page <- get_page(session)
  })

  outputOptions(output, "current_page", suspendWhenHidden = FALSE)

  # Define initial reactive values for various page metrics  
  r <- reactiveValues(
    reactive_sf_ADM_2 = sf_ADM_2,
    thirdPage_focused_dataset = imported_data %>%
      filter(Type == "DisabilityRate" & Metric == "Walking" & Level == "ADM1"),
    fourthPage_focused_dataset = imported_data %>%
      filter(Metric == "ForeignBorn" & Level == "ADM1"),
    fifthPage_focused_dataset = imported_data %>%
      filter(Metric == "DomMigrants" & Level == "ADM1" & Type == "Net"),
    eighthPage_focused_dataset = imported_data %>%
      filter(Metric == "AdultLiteracyRate" & Level == "ADM1" & Type == "Total"),
    ninthPage_focused_dataset = imported_data %>%
      filter(Level == "ADM1"),
    tenthPage_focused_dataset = imported_data %>%
      filter(Level == "ADM1"),
    eleventhPage_focused_dataset = imported_data %>%
      filter(Level == "ADM1"),
    eleventhPage_metric = "water",
    twelfthPage_focused_dataset = imported_data %>%
      filter(Level == "ADM1"),
    twelfthPage_metric = "electricity",
    currentLang = "English"
  )

 # Below, we define all labels for the leaflet maps on pages 1 - 12; i18n has trouble 
 # translating within leaflet, so we define reactive values based on the if else statements 
 # defined below. e.g., if r$currentLang == "Spanish", do X.
  firstParams <- reactiveValues(
    labels_en = c("0 - 19,999", "20,000 - 39,999", "40,000 - 79,999", "80,000 - 99,999", "100,000 or more"),
    labels_sp = c("0 - 19,999", "20,000 - 39,999", "40,000 - 79,999", "80,000 - 99,999", "100,000 o más")
  )
  
  secondParams <- reactiveValues(
    labels_en = c("0.0 - 1.9", "2.0 - 3.9", "4.0 - 5.9", "6.0 - 7.9", "8.0 or more"),
    labels_sp = c("0.0 - 1.9", "2.0 - 3.9", "4.0 - 5.9", "6.0 - 7.9", "8.0 o más")
  )
  
  thirdParams <- reactiveValues(
    labels_en = c("0.0 - 8.0", "8.0 - 9.9", "10.0 - 11.9", "12.0 - 13.9", "14.0 or more"),
    labels_sp = c("0.0 - 8.0", "8.0 - 9.9", "10.0 - 11.9", "12.0 - 13.9", "14.0 o más")
  )
  
  fourthParams <- reactiveValues(
    labels_en = c("0.0 - 1.9", "2.0 - 3.9", "4.0 - 5.9", "6.0 - 7.9", "8.0 - 9.9", "10.0 or more"),
    labels_sp = c("0.0 - 1.9", "2.0 - 3.9", "4.0 - 5.9", "6.0 - 7.9", "8.0 - 9.9", "10.0 o más")
  )

  fifthParams <- reactiveValues(
    metric = "Net",
    bins = c(-1000000, -10000, -5000, 0, 5000, 10000, 1000000),
    labels_net_en = c("Fewer than -10,000", "-10,000 - -5,001", "-5,000 - -1", "0 - 4,999", "5,000 - 9,999", "10,000 or more"),
    labels_out_en = c("0 - 1,999", "2,000 - 3,999", "4,000 - 5,999", "6,000 - 7,999", "8,000 - 9,999", "10,000 or more"),
    labels_in_en = c("0 - 1,999", "2,000 - 3,999", "4,000 - 5,999", "6,000 - 7,999", "8,000 - 9,999", "10,000 or more"),
    labels_net_sp = c("Menos de -10,000 ", "-10,000 - -5,001", "-5,000 - -1", "0 - 4,999", "5,000 - 9,999", "10,000 o más"),
    labels_out_sp = c("0 - 1,999", "2,000 - 3,999", "4,000 - 5,999", "6,000 - 7,999", "8,000 - 9,999", "10,000 o más"),
    labels_in_sp = c("0 - 1,999", "2,000 - 3,999", "4,000 - 5,999", "6,000 - 7,999", "8,000 - 9,999", "10,000 o más"),
    legend_title = "Net People",
    palette = "PRGn"
  )
  
  fifthParams_labels_disp <- reactive(
    if (r$currentLang == "English" & fifthParams$metric == "Net") {
      fifthParams$labels_net_en
    } else if (r$currentLang == "Spanish" & fifthParams$metric == "Net") {
      fifthParams$labels_net_sp
    } else if (r$currentLang == "English" & fifthParams$metric == "Out") {
      fifthParams$labels_out_en
    } else if (r$currentLang == "Spanish" & fifthParams$metric == "Out") {
      fifthParams$labels_out_sp
    } else if (r$currentLang == "English" & fifthParams$metric == "In") {
      fifthParams$labels_in_en
    } else {
      fifthParams$labels_in_sp
    }
  )
  
  flowParams <- reactiveValues(
    metric = "Net",
    bins = c(-1000000, -10000, -5000, 0, 5000, 10000, 1000000),
    labels_net_en = c("Fewer than -10,000", "-10,000 - -5,001", "-5,000 - -1", "0 - 4,999", "5,000 - 9,999", "10,000 or more"),
    labels_out_en = c("0 - 1,999", "2,000 - 3,999", "4,000 - 5,999", "6,000 - 7,999", "8,000 - 9,999", "10,000 or more"),
    labels_in_en = c("0 - 1,999", "2,000 - 3,999", "4,000 - 5,999", "6,000 - 7,999", "8,000 - 9,999", "10,000 or more"),
    labels_net_sp = c("Menos de -10,000 ", "-10,000 - -5,001", "-5,000 - -1", "0 - 4,999", "5,000 - 9,999", "10,000 o más"),
    labels_out_sp = c("0 - 1,999", "2,000 - 3,999", "4,000 - 5,999", "6,000 - 7,999", "8,000 - 9,999", "10,000 o más"),
    labels_in_sp = c("0 - 1,999", "2,000 - 3,999", "4,000 - 5,999", "6,000 - 7,999", "8,000 - 9,999", "10,000 o más"),
    legend_title = "Net People",
    palette = "PRGn"
  )
  
  flowParams_labels_disp <- reactive(
    if (r$currentLang == "English" & flowParams$metric == "Net") {
      flowParams$labels_net_en
    } else if (r$currentLang == "Spanish" & flowParams$metric == "Net") {
      flowParams$labels_net_sp
    } else if (r$currentLang == "English" & flowParams$metric == "Out") {
      flowParams$labels_out_en
    } else if (r$currentLang == "Spanish" & flowParams$metric == "Out") {
      flowParams$labels_out_sp
    } else if (r$currentLang == "English" & flowParams$metric == "In") {
      flowParams$labels_in_en
    } else {
      flowParams$labels_in_sp
    }
  )
  
  sixthParams <- reactiveValues(
    labels_en = c("0.0 - 3.9", "2.0 - 5.9", "4.0 - 7.9", "8.0 or more"),
    labels_sp = c("0.0 - 3.9", "2.0 - 5.9", "4.0 - 7.9", "8.0 o más")
  )

  seventhParams <- reactiveValues(
    labels_en = c("0.0 - 14.9", "15.0 - 19.9", "20.0 - 24.9", "25.0 - 29.9", "30.0 or more"),
    labels_sp = c("0.0 - 14.9", "15.0 - 19.9", "20.0 - 24.9", "25.0 - 29.9", "30.0 o más")
  )
  
  eighthParams <- reactiveValues(
    labels_en = c("0.0 - 75.0", "75.0 - 79.9", "80.0 - 84.9", "85.0 - 89.9", "90.0 or more"),
    labels_sp = c("0.0 - 75.0", "75.0 - 79.9", "80.0 - 84.9", "85.0 - 89.9", "90.0 o más")
  )
  
  ninthParams <- reactiveValues(
    labels_en = c("0.0 - 1.9", "2.0 - 3.9", "4.0 - 5.9", "6.0 - 7.9", "8.0 or more"),
    labels_sp = c("0.0 - 1.9", "2.0 - 3.9", "4.0 - 5.9", "6.0 - 7.9", "8.0 o más")
  )
  
  tenthParams <- reactiveValues(
    labels_en = c("0 - 3,999", "4,000 - 7,999", "8,000 - 11,999", "12,000 - 15,999", "16,000 or more"),
    labels_sp = c("0 - 3,999", "4,000 - 7,999", "8,000 - 11,999", "12,000 - 15,999", "16,000 o más")
  )
  
  eleventhParams <- reactiveValues(
    labels_en = c("0.0 - 74.9", "75.0 - 79.9", "80.0 - 84.9", "85.0 - 89.9", "90.0 or more"),
    labels_sp = c("0.0 - 74.9", "75.0 - 79.9", "80.0 - 84.9", "85.0 - 89.9", "90.0 o más")
  )
  
  twelfthParams <- reactiveValues(
    #metric = "Electric",
    bins = c(0, 85, 90, 95, Inf),
    labels_electric_en = c("0.0 - 84.9", "85.0 - 89.9", "90.0 - 94.9","95.0 or more"),
    labels_internet_en = c("0.0 - 59.9", "60.0 - 69.9", "70.0 - 79.9","80.0 or more"),
    labels_electric_sp = c("0.0 - 84.9", "85.0 - 89.9", "90.0 - 94.9","95.0 o más"),
    labels_internet_sp = c("0.0 - 59.9", "60.0 - 69.9", "70.0 - 79.9","80.0 o más"),
    legend_title = "Households (Percent)",
    map_labels = "percent of households",
    palette = "YlOrRd"
  )

  twelfthParams_labels_disp <- reactive(
    if (r$currentLang == "English" & r$twelfthPage_metric == "electricity") {
      twelfthParams$labels_electric_en
    } else if (r$currentLang == "Spanish" & r$twelfthPage_metric == "electricity") {
      twelfthParams$labels_electric_sp
    } else if (r$currentLang == "English" & r$twelfthPage_metric == "ICT") {
      twelfthParams$labels_internet_en
    } else {
      twelfthParams$labels_internet_sp
    } 
  )
  
  tr <- reactiveVal()
  
  output$eleventhPage_metric2 <- renderText({
    metric <- r$eleventhPage_metric
  })
  
  output$twelfthPage_metric2 <- renderText({
    metric <- r$twelfthPage_metric
  })
  
  outputOptions(output, "eleventhPage_metric2", suspendWhenHidden = FALSE)
  
  outputOptions(output, "twelfthPage_metric2", suspendWhenHidden = FALSE)
 
  #FILTER DATA TABLES###########################################################  
  # Reactive data for indicator widgets (right card, adjacent to leaflet maps);
  # and for all ggplots. secondaryText column in dropdownFilteredData for i18n
  # translations.
  
  filtered_data_from_dropdown_ind_widgets <- reactive({
    req(input$dropdown)
    if (input$dropdown == "All") filtered_data <- imported_data_wide_all %>%
        dplyr::filter(Level == "ADM0")
    else filtered_data <- imported_data_wide_all %>%
        dplyr::filter(Level == "ADM1"& ADM1 == input$dropdown)
  })
  
  dropdownFilteredData <- reactive({
    req(input$dropdown)
    if (input$dropdown == "All") filtered_data <- imported_data %>%
        dplyr::mutate(secondaryText = tr()$t(secondaryText)) %>%
        dplyr::filter(Level == "ADM0")
    else filtered_data <- imported_data %>%
        dplyr::mutate(secondaryText = tr()$t(secondaryText)) %>%
        dplyr::filter(ADM1 == input$dropdown)
  })
  
  #RENDER TEXT FOR TOP INDICATOR WIDGETS########################################   
  # First, define all widget statistics by page and user-selected geography, 
  # then define text output in UI.
  # Additionally, define all reactive map elements (e.g., title, subtitle) by page
  # based on click for "Metric" or "Topic".
  
  name_indicator <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    
    if (internal$Name == "National figures") {
      internal <- i18n$t("National figures")
    } else {
      internal <- internal$Name
    }
  })
  
  # PAGE 1: Demographic & Social################################################
  output$firstPage_statistics_text0 <- renderText({
    name_indicator()
  })
  # Total population
  firstPage_stat1 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- comma(internal$Pop)
  })
  
  output$firstPage_statistics_text1 <- renderText({
    paste(firstPage_stat1(), i18n$t("people"))
  })
  # Population density
  firstPage_stat2 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- internal$PopDensity
  })
  
  output$firstPage_statistics_text2 <- renderText({
    paste(firstPage_stat2(), i18n$t("people per square kilometer"))
  })
  # Population sex ratio
  firstPage_stat3 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- decimal(internal$SexRatio,2)
  })
  
  output$firstPage_statistics_text3 <- renderText({
    paste(firstPage_stat3(), i18n$t("men per woman"))
  })
  # Percent population ages 0 - 14
  firstPage_stat4 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- decimal(internal$Perc0to14,1)
  })
  
  output$firstPage_statistics_text4 <- renderText({
    firstPage_stat4()
  })
  # Percent population ages 15 - 64
  firstPage_stat5 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- decimal(internal$Perc15to64,1)
  })
  
  output$firstPage_statistics_text5 <- renderText({
    firstPage_stat5()
  })
  # Percent population 65+
  firstPage_stat6 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- decimal(internal$Perc65Plus,1)
  })
  
  output$firstPage_statistics_text6 <- renderText({
    firstPage_stat6()
  })
  # Dependency ratio, total
  firstPage_stat7 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- decimal(internal$TotDepRat,1)
  })
  
  output$firstPage_statistics_text7 <- renderText({
    firstPage_stat7()
  })
  # Dependency ratio, child
  firstPage_stat8 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- decimal(internal$CDepRate,1)
  })
  
  output$firstPage_statistics_text8 <- renderText({
    firstPage_stat8()
  })
  # Dependency ratio, adult
  firstPage_stat9 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- decimal(internal$ADepRate,1)
  })
  
  output$firstPage_statistics_text9 <- renderText({
    firstPage_stat9()
  })
  # Median age of population
  firstPage_stat10 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- internal$MedianAge
  })
  
  output$firstPage_statistics_text10 <- renderText({
    paste(firstPage_stat10(), i18n$t("years"))
  })
  # Life expectancy
  firstPage_stat11 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- decimal(internal$LifeExpect,1)
  })

  output$firstPage_statistics_text11 <- renderText({
    paste(firstPage_stat11(), i18n$t("years"))
  })
  
  # PAGE 2: Household & Family##################################################
  output$secondPage_statistics_text0 <- renderText({
    name_indicator()
  })
  # Number of households
  secondPage_stat1 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- comma(internal$Households)
  })
  
  output$secondPage_statistics_text1 <- renderText({
    secondPage_stat1()
  })
  # Average household size
  secondPage_stat2 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- decimal(internal$HHSize,1)
  })

  output$secondPage_statistics_text2 <- renderText({
    secondPage_stat2()
  })
  
  # PAGE 3: Disability Status###################################################
  output$thirdPage_statistics_text0 <- renderText({
    name_indicator()
  })
  # People with at least some difficult in any disability domain
  disability_indicator <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- comma(internal$Dis)
  })
  
  output$thirdPage_statistics_text1 <- renderText({
    paste(disability_indicator(), i18n$t("people"))
  })
  # Difficult walking
  walking_dis_indicator <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- comma(mean(internal$Walk))
  })
  
  walkingrate_dis_indicator <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- decimal(mean(internal$WalkRate),1)
  })
  
  output$thirdPage_statistics_text2 <- renderText({
    paste0(walking_dis_indicator()," "," (",walkingrate_dis_indicator(),"%)")
  })
  # Difficulty seeing
  seeing_dis_indicator <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- comma(mean(internal$See))
  })
  
  seeingrate_dis_indicator <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- decimal(mean(internal$SeeRate),1)
  })
  
  output$thirdPage_statistics_text3 <- renderText({
    paste0(seeing_dis_indicator()," ",i18n$t("people")," (",seeingrate_dis_indicator(),"%)")
  })
  # Difficult hearing
  hearing_dis_indicator <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- comma(mean(internal$Hear))
  })
  
  hearingrate_dis_indicator <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- decimal(mean(internal$HearRate),1)
  })
  
  output$thirdPage_statistics_text4 <- renderText({
    paste0(hearing_dis_indicator()," ",i18n$t("people")," (",hearingrate_dis_indicator(),"%)")
  })
  # Difficulty in cognition
  cognition_dis_indicator <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- comma(mean(internal$Cog))
  })
  
  cognitionrate_dis_indicator <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- decimal(mean(internal$CogRate),1)
  })
  
  output$thirdPage_statistics_text5 <- renderText({
    paste0(cognition_dis_indicator()," ",i18n$t("people")," (",cognitionrate_dis_indicator(),"%)")
  })
  # Difficulty with selfcare
  selfcare_dis_indicator <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- comma(mean(internal$Selfca))
  })
  
  selfcarerate_dis_indicator <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- decimal(mean(internal$SelfcaRate),1)
  })
  
  output$thirdPage_statistics_text6 <- renderText({
    paste0(selfcare_dis_indicator()," ",i18n$t("people")," (",selfcarerate_dis_indicator(),"%)")
  })
  # Difficulty with communication
  communication_dis_indicator <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- comma(mean(internal$Comm))
  })
  
  commrate_dis_indicator <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- decimal(mean(internal$CommRate),1)
  })
  
  output$thirdPage_statistics_text7 <- renderText({
    paste0(communication_dis_indicator()," ",i18n$t("people")," (",commrate_dis_indicator(),"%)")
  }) 
  # PAGE 3 leaflet map subtitles, reactive based on subtopic button click for 'Metric'
  thirdPage_map_subtitle_topic <- reactive({
    internal <- r$thirdPage_focused_dataset['Metric'] %>%
      slice(2) %>%
      unlist(., use.names = FALSE)
    
    if (internal == "Walking") {
      mapSubtitle <- i18n$t("Walking")
    } else if (internal == "Seeing") {
      mapSubtitle <- i18n$t("Seeing")
    } else if (internal == "Hearing") {
      mapSubtitle <- i18n$t("Hearing")
    } else if (internal == "Cognition") {
      mapSubtitle <- i18n$t("Cognition")
    } else if (internal == "Selfcare") {
      mapSubtitle <- i18n$t("Selfcare")
    } else if (internal == "Communication") {
      mapSubtitle <- i18n$t("Communication")
    } else {
      mapSubtitle <- internal
    }
    mapSubtitle
  })
  
  output$thirdPage_map_subtitle <- renderText({
    paste(i18n$t("Subtopic:"), thirdPage_map_subtitle_topic())
  })
  
  # PAGE 4: International Migration#############################################
  output$fourthPage_statistics_text0 <- renderText({
    name_indicator()
  })
  # Most common immigrant source country
  fourthPage_stat1 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- internal$MCommOrig
  })
  
  output$fourthPage_statistics_text1 <- renderText({
    fourthPage_stat1()
  })
  # Foreign born population
  fourthPage_stat2 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- comma(internal$FBorn)
  })
  
  output$fourthPage_statistics_text2 <- renderText({
    paste0(fourthPage_stat2(),i18n$t("people"))
  })
  # Population with foreign citizenship
  fourthPage_stat3 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- comma(internal$FCitiz)
  })
  
  output$fourthPage_statistics_text3 <- renderText({
    paste0(fourthPage_stat3(),i18n$t("people"))
  })
  # PAGE 4 leaflet map subtitles, reactive based on subtopic button click for 'Metric'
  fourthPage_map_subtitle_topic <- reactive({
    internal <- r$fourthPage_focused_dataset['Metric'] %>%
      slice(2) %>%
      unlist(., use.names = FALSE)
    
    if (internal == "ForeignBorn") {
      mapSubtitle <- i18n$t("Foreign Born Population")
    } else if (internal == "ForeignCitizenship") {
      mapSubtitle <- i18n$t("Population with Foreign Citizenship")
    } else {
      mapSubtitle <- internal
    }
    mapSubtitle
  })
  
  output$fourthPage_map_subtitle <- renderText({
    paste(i18n$t("Subtopic:"), fourthPage_map_subtitle_topic())
  })
  
  # PAGE 5: Domestic Migration##################################################
  output$fifthPage_statistics_text0 <- renderText({
    name_indicator()
  })
  # Most common destination 
  fifthPage_stat1 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- internal$MCommDest
  })
  
  output$fifthPage_statistics_text1 <- renderText({
    fifthPage_stat1()
  })
  # Percent of population that is a new resident
  fifthPage_stat2 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- decimal(internal$NewResid,1)
  })
  
  output$fifthPage_statistics_text2 <- renderText({
    paste0(fifthPage_stat2(),"%")
  })
  # Number of mobers from a different department
  fifthPage_stat3 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- comma(internal$DomImmig)
  })
  
  output$fifthPage_statistics_text3 <- renderText({
    fifthPage_stat3()
  })
  # Number of movers to a different department
  fifthPage_stat4 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- comma(internal$DomEmig)
  })
  
  output$fifthPage_statistics_text4 <- renderText({
    fifthPage_stat4()
  })
  # Number of movers from abroad
  fifthPage_stat5 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- comma(internal$IntImmi)
  })
 
  output$fifthPage_statistics_text5 <- renderText({
    fifthPage_stat5()
  })
  # PAGE 5 leaflet map titles, reactive based on subtopic button click for 'Type'  
  fifthPage_map_national_title_topic <- reactive({
    internal <- r$fifthPage_focused_dataset['Type'] %>%
      slice(2) %>%
      unlist(., use.names = FALSE)
    
    if (internal == "Net") {
      mapTitle <- i18n$t("Net Domestic Migrants by Municipality, Guatemala 2018")
    } else if (internal == "Inbound") {
      mapTitle <- i18n$t("Inbound Domestic Migrants by Municipality, Guatemala 2018")
    } else if (internal == "Outbound") {
      mapTitle <- i18n$t("Outbound Domestic Migrants by Municipality, Guatemala 2018")
    } else {
      mapTitle <- internal
    }
    mapTitle
  })
  
  output$fifthPage_map_national_title <- renderText({
    fifthPage_map_national_title_topic()
  })
  
  # PAGE 5 leaflet Internal Flows map title
  output$fifthPage_map_flows_title <- renderText({
    i18n$t("Domestic Population Flow Mapper, Guatemala 2018")
  })
  
  # PAGE 5 leaflet map Internal Flows subtitles, reactive based on subtopic button click for 'Metric'  
  fifthPage_map_flows_subtitle_topic <- reactive({
    
    if (flowParams$metric == "Net") {
      mapSubtitle <- i18n$t("Net domestic migrants from the selected location")
    } else if (flowParams$metric == "Inbound") {
      mapSubtitle <- i18n$t("Inbound domestic migrants to the selected location")
    } else if (flowParams$metric == "Outbound") {
      mapSubtitle <- i18n$t("Outbound domestic migrants from the selected location")
    } else {
      mapSubtitle <- internal
    }
    mapSubtitle
  })
  
  output$fifthPage_map_flows_subtitle <- renderText({
    paste(i18n$t("Subtopic:"), fifthPage_map_flows_subtitle_topic())
  })
  
  # PAGE 5 leaflet Internal Flows map text; details how to use the map
  output$fifthPage_map_flows_text <- renderText({
    i18n$t("Select a location from the Controls dropdown to see the spatial distribution of migrants to and from the selected location from other domestic locations")
  })
  
  # PAGE 6: Mortality###########################################################
  output$sixthPage_statistics_text0 <- renderText({
    name_indicator()
  })
  # Number of deaths
  sixthPage_stat1 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- comma(internal$Deaths)
  })
  
  output$sixthPage_statistics_text1 <- renderText({
    sixthPage_stat1()
  })
  # Crude death rate
  sixthPage_stat2 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- decimal(internal$CDeathRate,1)
  })
  
  output$sixthPage_statistics_text2 <- renderText({
    sixthPage_stat2()
  })
  # Infant mortality rate
  sixthPage_stat3 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- decimal(internal$IMortRatio,1)
  })
  
  output$sixthPage_statistics_text3 <- renderText({
    sixthPage_stat3()
  })
  # Child mortality rate
  sixthPage_stat4 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- decimal(internal$CMortRatio,1)
  })

  output$sixthPage_statistics_text4 <- renderText({
    sixthPage_stat4()
  })
  
  # PAGE 7: Fertility###########################################################
  output$seventhPage_statistics_text0 <- renderText({
    name_indicator()
  })
  # Number of births
  seventhPage_stat1 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- comma(internal$Births)
  })
  
  output$seventhPage_statistics_text1 <- renderText({
    seventhPage_stat1()
  })
  # Crude birth rate
  seventhPage_stat2 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- decimal(internal$CBRate,1)
  })
  
  output$seventhPage_statistics_text2 <- renderText({
    seventhPage_stat2()
  })
  # Total fertility rate
  seventhPage_stat3 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- decimal(internal$TFertRate,1)
  })
  
  output$seventhPage_statistics_text3 <- renderText({
    seventhPage_stat3()
  })
  # Life expectancy at birth
  seventhPage_stat4 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- internal$LEB
  })
  
  output$seventhPage_statistics_text4 <- renderText({
    paste0(seventhPage_stat4()," years")
  })
  # sex ratio at birth
  seventhPage_stat5 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- decimal(internal$SexRatioB,2)
  })
  
  output$seventhPage_statistics_text5 <- renderText({
    seventhPage_stat5()
  })
  
  # PAGE 8: Education###########################################################
  output$eighthPage_statistics_text0 <- renderText({
    name_indicator()
  })
  # Adult literacy rate
  eighthPage_stat1 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- decimal(internal$ALitRate,1)
  })
  
  output$eighthPage_statistics_text1 <- renderText({
    paste0(eighthPage_stat1(),"%")
  })
  # Youth literacy rate
  eighthPage_stat2 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- decimal(internal$YLitRate,1)
  })
  
  output$eighthPage_statistics_text2 <- renderText({
    paste0(eighthPage_stat2(),"%")
  })
  # Primary school attendance
  eighthPage_stat3 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- comma(internal$PrimAttend)
  })
  
  output$eighthPage_statistics_text3 <- renderText({
    paste0(eighthPage_stat3()," ",i18n$t("people"))
  })
  # Secondary school attendance
  eighthPage_stat4 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- comma(internal$SecAttend)
  })
 
  output$eighthPage_statistics_text4 <- renderText({
    paste0(
      eighthPage_stat4(),
      " ",
      i18n$t("people"))
  })
  # PAGE 8 leaflet map subtitles, reactive based on subtopic button click for 'Metric'  
  eighthPage_map_subtitle_topic <- reactive({
    internal <- r$eighthPage_focused_dataset['Metric'] %>%
      slice(2) %>%
      unlist(., use.names = FALSE)
    
    if (internal == "AdultLiteracyRate") {
      mapSubtitle <- i18n$t("Adult Literacy")
    } else if (internal == "YouthLiteracyRate") {
      mapSubtitle <- i18n$t("Youth Literacy")
    } else {
      mapSubtitle <- internal
    }
    mapSubtitle
  })
  
  output$eighthPage_map_subtitle <- renderText({
    paste(i18n$t("Subtopic:"),eighthPage_map_subtitle_topic())
  })
  
  # PAGE 9: Labour & Economy####################################################
  output$ninthPage_statistics_text0 <- renderText({
    name_indicator()
  })
  # Unemployment rate
  ninthPage_stat1 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- decimal(internal$UnemployRa,1)
  })
  
  output$ninthPage_statistics_text1 <- renderText({
    ninthPage_stat1()
  })
  
  # PAGE 10: Housing############################################################
  output$tenthPage_statistics_text0 <- renderText({
    name_indicator()
  })
  # Average rooms per housing unit
  tenthPage_stat1 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- decimal(internal$RoomsNum,1)
  })
  
  output$tenthPage_statistics_text1 <- renderText({
    tenthPage_stat1()
  })
  # Average people per housing unit room
  tenthPage_stat2 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- decimal(internal$RoomsDens,1)
  })
  
  output$tenthPage_statistics_text2 <- renderText({
    tenthPage_stat2()
  })
  
  # PAGE 11: Water, Sanitation, & Hygiene#######################################
  output$eleventhPage_statistics_text0 <- renderText({
    name_indicator()
  })
  # PAGE 11 leaflet map subtitles, reactive based on subtopic button click for 'Metric'  
  eleventhPage_map_subtitle_topic <- reactive({

    if (r$eleventhPage_metric == "water") {
      mapSubtitle <- i18n$t("Piped water inside the unit")
    } else if (r$eleventhPage_metric == "sanitation") {
      mapSubtitle <- i18n$t("Toilet within housing unit")
    } else if (r$eleventhPage_metric == "hygiene") {
      mapSubtitle <- i18n$t("With fixed bath or shower within housing unit")
    } else {
      mapSubtitle <- output$eleventhPage_metric2
    }
    mapSubtitle
  })
  
  output$eleventhPage_map_subtitle <- renderText({
    paste(i18n$t("Amenity:"), eleventhPage_map_subtitle_topic())
  })
  
  # PAGE 12: Power & Technology#################################################
  output$twelfthPage_statistics_text0 <- renderText({
    name_indicator()
  })
  # Most common fuel for cooking
  twelfthPage_stat1 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- i18n$t(internal$MCommCoFu)
  })
  
  output$twelfthPage_statistics_text1 <- renderText({
    twelfthPage_stat1()
  })
  # Most common fuel for lighting
  twelfthPage_stat2 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- i18n$t(internal$MCommLiFu)
  })
  
  output$twelfthPage_statistics_text2 <- renderText({
    twelfthPage_stat2()
  })
  # Electricity access (% of households)
  twelfthPage_stat3 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- decimal(internal$Electric,1)
  })
  
  output$twelfthPage_statistics_text3 <- renderText({
    paste0(twelfthPage_stat3(),"%")
  })
  # Internet access (% of households)
  twelfthPage_stat4 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- decimal(internal$Internet,1)
  })
  
  output$twelfthPage_statistics_text4 <- renderText({
    paste0(twelfthPage_stat4(),"%")
  })
  # PAGE 12 leaflet map subtitles, reactive based on subtopic button click for 'Metric'  
  twelfthPage_map_title_topic <- reactive({
    
    if (r$twelfthPage_metric == "electricity") {
      mapTitle <- i18n$t("Percent of Households with Electricity by Municipality, Guatemala 2018")
    } else if (r$twelfthPage_metric == "ICT") {
      mapTitle <- i18n$t("Percent of Households with Internet Access by Municipality, Guatemala 2018")
    } else {
      mapTitle <- output$twelfthPage_metric2
    }
    mapTitle
  })
  
  output$twelfthPage_map_title <- renderText({
    twelfthPage_map_title_topic()
  })

  #FOCUS BUTTONS################################################################
  # The follow buttons appear on the left-hand navigation under "Select a subtopic".
  # When the user clicks one of these buttons, the follow code observes the click,
  # and updates the data shown in the UI, accordingly. e.g., when a user clicks on 
  # the selfcare button on page 3, maps & plots show selfcare data as defined, below.
  
  # PAGE 3: Disability Status###################################################
  observeEvent(input$walking_button, {
    r$thirdPage_focused_dataset <- imported_data %>%
      filter(Type == "DisabilityRate" & Metric == "Walking" & Level == "ADM1")
  })
      
  observeEvent(input$seeing_button, {
    r$thirdPage_focused_dataset <- imported_data %>%
      filter(Type == "DisabilityRate" & Metric == "Seeing" & Level == "ADM1")
  })
  
  observeEvent(input$hearing_button, {
    r$thirdPage_focused_dataset <- imported_data %>%
      filter(Type == "DisabilityRate" & Metric == "Hearing" & Level == "ADM1")
  })
  
  observeEvent(input$cognition_button, {
    r$thirdPage_focused_dataset <- imported_data %>%
      filter(Type == "DisabilityRate" & Metric == "Cognition" & Level == "ADM1")
  })
  
  observeEvent(input$selfcare_button, {
    r$thirdPage_focused_dataset <- imported_data %>%
      filter(Type == "DisabilityRate" & Metric == "Selfcare" & Level == "ADM1")
  })
  
  observeEvent(input$communication_button, {
    r$thirdPage_focused_dataset <- imported_data %>%
      filter(Type == "DisabilityRate" & Metric == "Communication" & Level == "ADM1")
  })
  
  # PAGE 4: International Migration#############################################
  observeEvent(input$fborn_button, {
    r$fourthPage_focused_dataset <- imported_data %>%
      filter(Metric == "ForeignBorn" & Level == "ADM1")
  })
  
  observeEvent(input$fcit_button, {
    r$fourthPage_focused_dataset <- imported_data %>%
      filter(Metric == "ForeignCitizenship" & Level == "ADM1")
  })
  
  # PAGE 5: Domestic Migration##################################################
  observeEvent(input$net_button, {
    r$fifthPage_focused_dataset <- imported_data %>%
      filter(Metric == "DomMigrants" & Level == "ADM1" & Type == "Net")
  })
  
  observeEvent(input$in_button, {
    r$fifthPage_focused_dataset <- imported_data %>%
      filter(Metric == "DomMigrants" & Level == "ADM1" & Type == "Inbound")
  })
  
  observeEvent(input$out_button, {
    r$fifthPage_focused_dataset <- imported_data %>%
      filter(Metric == "DomMigrants" & Level == "ADM1" & Type == "Outbound")
  })
  
  # PAGE 8: Education###########################################################
  observeEvent(input$aLit_button, {
    r$eighthPage_focused_dataset <- imported_data %>%
      filter(Metric == "AdultLiteracyRate" & Level == "ADM1")
  })
  
  observeEvent(input$yLit_button, {
    r$eighthPage_focused_dataset <- imported_data %>%
      filter(Metric == "YouthLiteracyRate" & Level == "ADM1")
  })
  
  # PAGE 11: Water, Sanitation, & Hygiene#######################################
  observeEvent(input$water_button, {
    r$eleventhPage_metric <- "water"
  })
  
  observeEvent(input$sanitation_button, {
    r$eleventhPage_metric <- "sanitation"
  })
  
  observeEvent(input$hygiene_button, {
    r$eleventhPage_metric <- "hygiene"
  })
  
  # PAGE 12: Power & Technology#################################################
  observeEvent(input$electricity_button, {
    r$twelfthPage_metric <- "electricity"
  })
  
  observeEvent(input$ICT_button, {
    r$twelfthPage_metric <- "ICT"
  })

  #LEAFLET MAPS BY PAGE#########################################################
  # The code below produces leaflet maps for each page (1-12) and the upload and 
  # homepages of OSDS.
  
  #Landing page MAP#############################################################
  # Population dot density map
  output$landing_page_map <- renderLeaflet({

    landing_page_map_internal <- leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter", layerId = "basetile",options = providerTileOptions(minZoom = 6)) %>%
      addCircles(data = pop_dots_data,
                 lng = ~lon,
                 lat = ~lat,
                 weight = 1,
                 radius = 80,
                 fillColor = "white",
                 stroke = FALSE,
                 fillOpacity = 1) %>%
      addLegend(
        colors = "#FFFFFF",
        labels = i18n$t("one dot = 500 people"),
        opacity = 1,
        position = "bottomleft") %>%
      setView(lng = -90,
              lat = 16,
              zoom = 7) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
  })
  
  observeEvent(input$dropdown, {
    
    leafletProxy("landing_page_map", session) %>%
      removeShape("select") %>% 
      addPolylines(data = sf_ADM1_p[sf_ADM1_p$ADM1 == input$dropdown,c("geometry")],
                   weight = 6,
                   color = "white",
                   layerId = "select") 
  })
  
  #PAGE 1: Demographic & Social MAPS############################################
  # Population by municipality
  first_page_map_bins <- c(0, 20000, 40000, 80000, 100000, Inf)
  
  first_page_map_pal <- colorBin("YlGn", domain = sf_ADM_2$Pop, bins = first_page_map_bins)
  
  first_map_labels <- reactive(
    if (r$currentLang == "English") {
      first_map_labels <- firstParams$labels_en
    } else {
      first_map_labels <- firstParams$labels_sp
    }
  )
  # Render the map polygons, basemap, labels, and legend
  output$first_page_map <- renderLeaflet({
    req(input$dropdown)
    
    first_page_map_labels <- sprintf(
      "<strong>%s </strong><br/>%s<br/>%g %s",
      sf_ADM_2$ADM1, sf_ADM_2$ADM2, sf_ADM_2$Pop, i18n$t("people")) %>%
      lapply(htmltools::HTML)
    
    first_page_map_internal <- leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter", layerId = "basetile",options = providerTileOptions(minZoom = 6)) %>%
      addPolygons(data = sf_ADM_2,
                  fillColor = ~first_page_map_pal(Pop),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = first_page_map_labels) %>%
      addLegend_decreasing(pal=first_page_map_pal,
                           values = sf_ADM_2$Pop,
                           opacity = 0.7,
                           title = i18n$t("Total Population"),
                           decreasing = TRUE,
                           position = "bottomright",
                           labFormat = function(type, cuts, p) {
                             paste0(first_map_labels())
                           }) %>%
      setView(lng = -90,
              lat = 16,
              zoom = 7) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
  })
  # Update map with selected ADM1 polylines overlayed when a location is selected
  # from the controls panel
  observeEvent(input$dropdown, {
    
    leafletProxy("first_page_map", session) %>%
      removeShape("select") %>% 
      addPolylines(data = sf_ADM1_p[sf_ADM1_p$ADM1 == input$dropdown,c("geometry")],
                   weight = 6,
                   color = "white",
                   layerId = "select")
  })
  
  # PAGE 2: Household & Family Maps#############################################
  # Average household size by municipality
  second_page_map_bins <- c(0, 2, 4, 6, 8, Inf)
  
  second_map_labels <- reactive(
    if (r$currentLang == "English") {
      second_map_labels <- secondParams$labels_en
    } else {
      second_map_labels <- secondParams$labels_sp
    }
  )
  
  second_page_map_pal <- colorBin("Purples", domain = sf_ADM_2$HHSize, bins = second_page_map_bins)
 
  output$second_page_map <- renderLeaflet({
    
    second_page_map_labels <- sprintf(
      "<strong>%s </strong><br/>%s<br/>%g %s",
      sf_ADM_2$ADM1, sf_ADM_2$ADM2, sf_ADM_2$HHSize, i18n$t("people")) %>% 
      lapply(htmltools::HTML)
    # Render the map polygons, basemap, labels, and legend    
    second_page_map_internal <- leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter", layerId = "basetile",options = providerTileOptions(minZoom = 6)) %>%
      addPolygons(data = sf_ADM_2,
                  fillColor = ~second_page_map_pal(HHSize),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = second_page_map_labels) %>%
      addLegend_decreasing(pal=second_page_map_pal, 
                values = sf_ADM_2$HHSize, 
                opacity = 0.7, 
                title = i18n$t("Average Household Size"),
                decreasing = TRUE,
                position = "bottomright",
                labFormat = function(type, cuts, p) {
                  paste0(second_map_labels())
                }) %>%
      setView(lng = -90,
              lat = 16,
              zoom = 7) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl() 
    })
  # Update map with selected ADM1 polylines overlayed when a location is selected
  # from the controls panel  
  observeEvent(input$dropdown, {
    
    leafletProxy("second_page_map", session) %>%
      removeShape("select") %>% 
      addPolylines(data = sf_ADM1_p[sf_ADM1_p$ADM1 == input$dropdown,c("geometry")],
                   weight = 6,
                   color = "white",
                   layerId = "select") 
  })
    
  # PAGE 3: Disability Status MAPS##############################################
  # Prevalence rate of population with at least some difficulty in a given subtopic by municipality
  third_map_labels <- reactive(
    if (r$currentLang == "English") {
      third_map_labels <- thirdParams$labels_en
    } else {
      third_map_labels <- thirdParams$labels_sp
    }
  )
  
  third_page_map_bins <- c(6, 8, 10, 12, 14, Inf)
  # Render the map polygons, basemap, labels, and legend   
  output$third_page_map <- renderLeaflet({

      third_page_map_pal <- colorBin("YlOrRd", domain = r$reactive_sf_ADM_2$ThirdShow, bins = third_page_map_bins)
      
      third_page_map_labels <- sprintf(
        "<strong>%s </strong><br/>%s<br/>%g %s",
        r$reactive_sf_ADM_2$ADM1, r$reactive_sf_ADM_2$ADM2, r$reactive_sf_ADM_2$ThirdShow, i18n$t("percent prevalence, people with at least some difficulty")) %>% 
        lapply(htmltools::HTML)

      third_page_map_internal <- leaflet() %>%
        addProviderTiles("CartoDB.DarkMatter", layerId = "basetile",options = providerTileOptions(minZoom = 6)) %>%
        addPolygons(data = r$reactive_sf_ADM_2,
                    fillColor = ~third_page_map_pal(ThirdShow),
                    weight = 1,
                    fillOpacity = 0.8,
                    color = "white",
                    highlightOptions = highlightOptions(color = "white",
                                                        weight = 3,
                                                        bringToFront = TRUE),
                    label = third_page_map_labels) %>%
        addLegend_decreasing(pal=third_page_map_pal, 
                  values = r$reactive_sf_ADM_2$ThirdShow, 
                  opacity = 0.7, 
                  title = i18n$t("Prevalence Rate"),
                  decreasing = TRUE,
                  position = "bottomright",
                  labFormat = function(type, cuts, p) {
                    paste0(third_map_labels())
                    }) %>%
        setView(lng = -90,
                lat = 16,
                zoom = 7) %>%
        setMapWidgetStyle(list(background = "white")) %>%
        addFullscreenControl()

    })
  # Update map with selected ADM1 polylines overlayed when a location is selected
  # from the controls panel    
  observeEvent(input$dropdown, {
    
    leafletProxy("third_page_map", session) %>%
      removeShape("select") %>% 
      addPolylines(data = sf_ADM1_p[sf_ADM1_p$ADM1 == input$dropdown,c("geometry")],
                   weight = 6,
                   color = "white",
                   layerId = "select") 
  })
  
  # Update map with selected disability subtopic: WALKING
  observeEvent(input$walking_button, {
    
    r$reactive_sf_ADM_2$ThirdShow <- r$reactive_sf_ADM_2$WalkRate
    
    third_page_map_pal <- colorBin("YlOrRd", domain = r$reactive_sf_ADM_2$ThirdShow, bins = third_page_map_bins)
    
    third_page_map_labels <- sprintf(
      "<strong>%s </strong><br/>%s<br/>%g %s",
      r$reactive_sf_ADM_2$ADM1, r$reactive_sf_ADM_2$ADM2, r$reactive_sf_ADM_2$ThirdShow, i18n$t("percent prevalence, people with at least some difficulty")) %>% 
      lapply(htmltools::HTML)
    
    leafletProxy("third_page_map", data = r$reactive_sf_ADM_2) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(fillColor = ~third_page_map_pal(ThirdShow),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = third_page_map_labels) %>%
      addPolylines(data = sf_ADM1_p[sf_ADM1_p$ADM1 == input$dropdown,c("geometry")],
                   weight = 6,
                   color = "white",
                   layerId = "select") %>%
      addLegend_decreasing(pal=third_page_map_pal,
                values = ~r$reactive_sf_ADM_2$ThirdShow,
                opacity = 0.7,
                title = i18n$t("Prevalence Rate"),
                decreasing = TRUE,
                position = "bottomright",
                labFormat = function(type, cuts, p) {
                  paste0(third_map_labels())
                }) %>%
      setView(lng = -90,
              lat = 16,
              zoom = 7) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
    
  })
  
  # Update map with selected disability subtopic: SEEING  
  observeEvent(input$seeing_button, {

    r$reactive_sf_ADM_2$ThirdShow <- r$reactive_sf_ADM_2$SeeRate
    
    third_page_map_pal <- colorBin("YlOrRd", domain = r$reactive_sf_ADM_2$ThirdShow, bins = third_page_map_bins)
    
    third_page_map_labels <- sprintf(
      "<strong>%s </strong><br/>%s<br/>%g %s",
      r$reactive_sf_ADM_2$ADM1, r$reactive_sf_ADM_2$ADM2, r$reactive_sf_ADM_2$ThirdShow, i18n$t("percent prevalence, people with at least some difficulty")) %>% 
      lapply(htmltools::HTML)
    
    leafletProxy("third_page_map", data = r$reactive_sf_ADM_2) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(fillColor = ~third_page_map_pal(ThirdShow),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = third_page_map_labels) %>%
      addPolylines(data = sf_ADM1_p[sf_ADM1_p$ADM1 == input$dropdown,c("geometry")],
                   weight = 6,
                   color = "white",
                   layerId = "select") %>%
      addLegend_decreasing(pal=third_page_map_pal,
                values = ~r$reactive_sf_ADM_2$ThirdShow,
                opacity = 0.7,
                title = i18n$t("Prevalence Rate"),
                decreasing = TRUE,
                position = "bottomright",
                labFormat = function(type, cuts, p) {
                  paste0(third_map_labels())
                }) %>%
      setView(lng = -90,
              lat = 16,
              zoom = 7) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()

  })
  
  # Update map with selected disability subtopic: HEARING  
  observeEvent(input$hearing_button, {

    r$reactive_sf_ADM_2$ThirdShow <- r$reactive_sf_ADM_2$HearRate
    
    third_page_map_pal <- colorBin("YlOrRd", domain = r$reactive_sf_ADM_2$ThirdShow, bins = third_page_map_bins)
    
    third_page_map_labels <- sprintf(
      "<strong>%s </strong><br/>%s<br/>%g %s",
      r$reactive_sf_ADM_2$ADM1, r$reactive_sf_ADM_2$ADM2, r$reactive_sf_ADM_2$ThirdShow, i18n$t("percent prevalence, people with at least some difficulty")) %>% 
      lapply(htmltools::HTML)
    
    leafletProxy("third_page_map", data = r$reactive_sf_ADM_2) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(fillColor = ~third_page_map_pal(ThirdShow),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = third_page_map_labels) %>%
      addPolylines(data = sf_ADM1_p[sf_ADM1_p$ADM1 == input$dropdown,c("geometry")],
                   weight = 6,
                   color = "white",
                   layerId = "select") %>%
      addLegend_decreasing(pal=third_page_map_pal,
                values = ~r$reactive_sf_ADM_2$ThirdShow,
                opacity = 0.7,
                title = i18n$t("Prevalence Rate"),
                decreasing = TRUE,
                position = "bottomright",
                labFormat = function(type, cuts, p) {
                  paste0(third_map_labels())
                }) %>%
      setView(lng = -90,
              lat = 16,
              zoom = 7) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
    
  })

  # Update map with selected disability subtopic: COGNITION  
  observeEvent(input$cognition_button, {
    
    r$reactive_sf_ADM_2$ThirdShow <- r$reactive_sf_ADM_2$CogRate
    
    third_page_map_pal <- colorBin("YlOrRd", domain = r$reactive_sf_ADM_2$ThirdShow, bins = third_page_map_bins)
    
    third_page_map_labels <- sprintf(
      "<strong>%s </strong><br/>%s<br/>%g %s",
      r$reactive_sf_ADM_2$ADM1, r$reactive_sf_ADM_2$ADM2, r$reactive_sf_ADM_2$ThirdShow, i18n$t("percent prevalence, people with at least some difficulty")) %>% 
      lapply(htmltools::HTML)
    
    leafletProxy("third_page_map", data = r$reactive_sf_ADM_2) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(fillColor = ~third_page_map_pal(ThirdShow),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = third_page_map_labels) %>%
      addPolylines(data = sf_ADM1_p[sf_ADM1_p$ADM1 == input$dropdown,c("geometry")],
                   weight = 6,
                   color = "white",
                   layerId = "select") %>%
      addLegend_decreasing(pal=third_page_map_pal,
                values = ~r$reactive_sf_ADM_2$ThirdShow,
                opacity = 0.7,
                title = i18n$t("Prevalence Rate"),
                decreasing = TRUE,
                position = "bottomright",
                labFormat = function(type, cuts, p) {
                  paste0(third_map_labels())
                }) %>%
      setView(lng = -90,
              lat = 16,
              zoom = 7) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
    
  })

  # Update map with selected disability subtopic: SELFCARE  
  observeEvent(input$selfcare_button, {

    r$reactive_sf_ADM_2$ThirdShow <- r$reactive_sf_ADM_2$SelfcaRate
    
    third_page_map_pal <- colorBin("YlOrRd", domain = r$reactive_sf_ADM_2$ThirdShow, bins = third_page_map_bins)
    
    third_page_map_labels <- sprintf(
      "<strong>%s </strong><br/>%s<br/>%g %s",
      r$reactive_sf_ADM_2$ADM1, r$reactive_sf_ADM_2$ADM2, r$reactive_sf_ADM_2$ThirdShow, i18n$t("percent prevalence, people with at least some difficulty")) %>% 
      lapply(htmltools::HTML)
    
    leafletProxy("third_page_map", data = r$reactive_sf_ADM_2) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(fillColor = ~third_page_map_pal(ThirdShow),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = third_page_map_labels) %>%
      addPolylines(data = sf_ADM1_p[sf_ADM1_p$ADM1 == input$dropdown,c("geometry")],
                   weight = 6,
                   color = "white",
                   layerId = "select") %>%
      addLegend_decreasing(pal=third_page_map_pal,
                values = ~r$reactive_sf_ADM_2$ThirdShow,
                opacity = 0.7,
                title = i18n$t("Prevalence Rate"),
                decreasing = TRUE,
                position = "bottomright",
                labFormat = function(type, cuts, p) {
                  paste0(third_map_labels())
                }) %>%
      setView(lng = -90,
              lat = 16,
              zoom = 7) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
    
  })

  # Update map with selected disability subtopic: COMMUNICATION  
  observeEvent(input$communication_button, {
    
    r$reactive_sf_ADM_2$ThirdShow <- r$reactive_sf_ADM_2$CommRate
    
    third_page_map_pal <- colorBin("YlOrRd", domain = r$reactive_sf_ADM_2$ThirdShow, bins = third_page_map_bins)
    
    third_page_map_labels <- sprintf(
      "<strong>%s </strong><br/>%s<br/>%g %s",
      r$reactive_sf_ADM_2$ADM1, r$reactive_sf_ADM_2$ADM2, r$reactive_sf_ADM_2$ThirdShow, i18n$t("percent prevalence, people with at least some difficulty")) %>% 
      lapply(htmltools::HTML)
    
    leafletProxy("third_page_map", data = r$reactive_sf_ADM_2) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(fillColor = ~third_page_map_pal(ThirdShow),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = third_page_map_labels) %>%
      addPolylines(data = sf_ADM1_p[sf_ADM1_p$ADM1 == input$dropdown,c("geometry")],
                   weight = 6,
                   color = "white",
                   layerId = "select") %>%
      addLegend_decreasing(pal=third_page_map_pal,
                values = ~r$reactive_sf_ADM_2$ThirdShow,
                opacity = 0.7,
                title = i18n$t("Prevalence Rate"),
                decreasing = TRUE,
                position = "bottomright",
                labFormat = function(type, cuts, p) {
                  paste0(third_map_labels())
                }) %>%
      setView(lng = -90,
              lat = 16,
              zoom = 7) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
    
  })

  # PAGE 4: International Migration MAPS########################################
  # International migrant population by municipality and subtopic
  fourth_map_labels <- reactive(
    if (r$currentLang == "English") {
      fourth_map_labels <- fourthParams$labels_en
    } else {
      fourth_map_labels <- fourthParams$labels_sp
    }
  )
  
  fourth_page_map_bins <- c(0, 2, 4, 6, 8, 10, Inf)
  # Render the map polygons, basemap, labels, and legend    
  output$fourth_page_map <- renderLeaflet({
    
    fourth_page_map_pal <- colorBin("Greens", domain = r$reactive_sf_ADM_2$FourthShow, bins = fourth_page_map_bins)
    
    fourth_page_map_labels <- sprintf(
      "<strong>%s</strong><br/>%s<br/>%g %s",
      r$reactive_sf_ADM_2$ADM1, r$reactive_sf_ADM_2$ADM2, r$reactive_sf_ADM_2$FourthShow, i18n$t("percent of the population")) %>%
      lapply(htmltools::HTML)
    
    fourth_page_map_internal <- leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter", layerId = "basetile",options = providerTileOptions(minZoom = 6)) %>%
      addPolygons(data = r$reactive_sf_ADM_2,
                  fillColor = ~fourth_page_map_pal(FourthShow),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = fourth_page_map_labels) %>%
      addPolylines(data = sf_ADM1_p[sf_ADM1_p$ADM1 == input$dropdown,c("geometry")],
                   weight = 6,
                   color = "white",
                   layerId = "select") %>%
      addLegend_decreasing(pal=fourth_page_map_pal, 
                values = r$reactive_sf_ADM_2$FourthShow, 
                opacity = 0.7, 
                title = i18n$t("Percent of Population"), 
                decreasing = TRUE,
                position = "bottomright",
                labFormat = function(type, cuts, p) {
                  paste0(fourth_map_labels())
                }) %>%
      setView(lng = -90,
              lat = 16,
              zoom = 7) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
  })
  # Update map with selected international migration subtopic: FOREIGN BORN    
  observeEvent(input$fborn_button, {

    r$reactive_sf_ADM_2$FourthShow <- r$reactive_sf_ADM_2$FBRate
    
    fourth_page_map_pal <- colorBin("Greens", domain = r$reactive_sf_ADM_2$FourthShow, bins = fourth_page_map_bins)
    
    fourth_page_map_labels <- sprintf(
      "<strong>%s</strong><br/>%s<br/>%g %s",
      r$reactive_sf_ADM_2$ADM1, r$reactive_sf_ADM_2$ADM2, r$reactive_sf_ADM_2$FourthShow, i18n$t("percent of the population")) %>% 
      lapply(htmltools::HTML)
    
    leafletProxy("fourth_page_map", data = r$reactive_sf_ADM_2) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(fillColor = ~fourth_page_map_pal(FourthShow),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = fourth_page_map_labels) %>%
      addPolylines(data = sf_ADM1_p[sf_ADM1_p$ADM1 == input$dropdown,c("geometry")],
                   weight = 6,
                   color = "white",
                   layerId = "select") %>%
      addLegend_decreasing(pal=fourth_page_map_pal, 
                values = ~r$reactive_sf_ADM_2$FourthShow, 
                opacity = 0.7, 
                title = i18n$t("Percent of Population"), 
                decreasing = TRUE,
                position = "bottomright",
                labFormat = function(type, cuts, p) {
                  paste0(fourth_map_labels())
                }) %>%
      setView(lng = -90,
              lat = 16,
              zoom = 7) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
  })
  # Update map with selected international migration subtopic: FOREIGN CITIZENSHIP    
  observeEvent(input$fcit_button, {
    
    r$reactive_sf_ADM_2$FourthShow <- r$reactive_sf_ADM_2$FCitizRate
    
    fourth_page_map_pal <- colorBin("Greens", domain = r$reactive_sf_ADM_2$FourthShow, bins = fourth_page_map_bins)
    
    fourth_page_map_labels <- sprintf(
      "<strong>%s</strong><br/>%s<br/>%g %s",
      r$reactive_sf_ADM_2$ADM1, r$reactive_sf_ADM_2$ADM2, r$reactive_sf_ADM_2$FourthShow, i18n$t("percent of the population")) %>% 
      lapply(htmltools::HTML)
    
    leafletProxy("fourth_page_map", data = r$reactive_sf_ADM_2) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(fillColor = ~fourth_page_map_pal(FourthShow),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  label = fourth_page_map_labels,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE)) %>%
      addPolylines(data = sf_ADM1_p[sf_ADM1_p$ADM1 == input$dropdown,c("geometry")],
                   weight = 6,
                   color = "white",
                   layerId = "select") %>%
      addLegend_decreasing(pal=fourth_page_map_pal, 
                values = ~r$reactive_sf_ADM_2$FourthShow, 
                opacity = 0.7, 
                title = i18n$t("Percent of Population"), 
                decreasing = TRUE,
                position = "bottomright",
                labFormat = function(type, cuts, p) {
                  paste0(fourth_map_labels())
                }) %>%
      setView(lng = -90,
              lat = 16,
              zoom = 7) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
  })
  
  # PAGE 5: Domestic Migration MAPS#############################################
  # Domestic migrants by municipality and subtopic
  # Render the map polygons, basemap, labels, and legend  
  output$fifth_page_map_national <- renderLeaflet({
    
    fifth_page_map_national_pal <- colorBin(palette = fifthParams$palette, domain = r$reactive_sf_ADM_2$FifthShow, bins = fifthParams$bins)

    fifth_page_map_labels <- sprintf(
      "<strong>%s</strong><br/>%s<br/>%g %s",
      r$reactive_sf_ADM_2$ADM1, r$reactive_sf_ADM_2$ADM2, r$reactive_sf_ADM_2$FifthShow, i18n$t("people")) %>%
      lapply(htmltools::HTML)
    
    fifth_page_map_internal <- leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter", layerId = "basetile",options = providerTileOptions(minZoom = 6)) %>%
      addPolygons(data = r$reactive_sf_ADM_2,
                  fillColor = ~fifth_page_map_national_pal(FifthShow),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = fifth_page_map_labels) %>%
      addPolylines(data = sf_ADM1_p[sf_ADM1_p$ADM1 == input$dropdown,c("geometry")],
                   weight = 6,
                   color = "white",
                   layerId = "select") %>%
      addLegend_decreasing(pal=fifth_page_map_national_pal, 
                           values = r$reactive_sf_ADM_2$FifthShow, 
                           opacity = 0.7, 
                           title = i18n$t(fifthParams$legend_title), 
                           decreasing = TRUE,
                           position = "bottomright",
                           labFormat = function(type, cuts, p) {
                             paste0(fifthParams_labels_disp())
                           }) %>%
      setView(lng = -90,
              lat = 16,
              zoom = 7) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
    
  })
  # Update map with selected domestic migration subtopic: NET MIGRATION     
  observeEvent(input$net_button, {
    
    r$reactive_sf_ADM_2$FifthShow <- r$reactive_sf_ADM_2$DomNet
    
    fifthParams$bins = c(-100000, -10000, -5000, 0, 5000, 10000, 100000)
    
    fifthParams$metric = "Net"
    
    fifthParams$legend_title = "Net People"
    
    fifthParams$palette = "PRGn"
    
    fifth_page_map_national_pal <- colorBin(palette = fifthParams$palette, domain = r$reactive_sf_ADM_2$FifthShow, bins = fifthParams$bins)
    
    fifth_page_map_labels <- sprintf(
      "<strong>%s</strong><br/>%s<br/>%g %s",
      r$reactive_sf_ADM_2$ADM1, r$reactive_sf_ADM_2$ADM2, r$reactive_sf_ADM_2$FifthShow, i18n$t("people")) %>%
      lapply(htmltools::HTML)
    
    leafletProxy("fifth_page_map_national", data = r$reactive_sf_ADM_2) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(fillColor = ~fifth_page_map_national_pal(FifthShow),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  label = fifth_page_map_labels,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE)) %>%
      addPolylines(data = sf_ADM1_p[sf_ADM1_p$ADM1 == input$dropdown,c("geometry")],
                   weight = 6,
                   color = "white",
                   layerId = "select") %>%
      addLegend_decreasing(pal=fifth_page_map_national_pal, 
                           values = ~r$reactive_sf_ADM_2$FifthShow, 
                           opacity = 0.7, 
                           title = i18n$t("Net People"), 
                           decreasing = TRUE,
                           position = "bottomright",
                           labFormat = function(type, cuts, p) {
                             paste0(fifthParams_labels_disp())
                           }) %>%
      setView(lng = -90,
              lat = 16,
              zoom = 7) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
    
  })
  # Update map with selected domestic migration subtopic: INBOUND    
  observeEvent(input$in_button, {
    
    r$reactive_sf_ADM_2$FifthShow <- r$reactive_sf_ADM_2$DomIn
    
    fifthParams$bins = c(0, 5000, 10000, 15000, 20000, 25000, Inf)
    
    fifthParams$metric = "In"
    
    fifthParams$legend_title = "Inbound People"
    
    fifthParams$palette = "Purples"
    
    fifth_page_map_national_pal <- colorBin(palette = fifthParams$palette, domain = r$reactive_sf_ADM_2$FifthShow, bins = fifthParams$bins)
    
    fifth_page_map_labels <- sprintf(
      "<strong>%s</strong><br/>%s<br/>%g %s",
      r$reactive_sf_ADM_2$ADM1, r$reactive_sf_ADM_2$ADM2, r$reactive_sf_ADM_2$FifthShow, i18n$t("people")) %>%
      lapply(htmltools::HTML)
    
    leafletProxy("fifth_page_map_national", data = r$reactive_sf_ADM_2) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(fillColor = ~fifth_page_map_national_pal(FifthShow),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  label = fifth_page_map_labels,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE)) %>%
      addPolylines(data = sf_ADM1_p[sf_ADM1_p$ADM1 == input$dropdown,c("geometry")],
                   weight = 6,
                   color = "white",
                   layerId = "select") %>%
      addLegend_decreasing(pal=fifth_page_map_national_pal, 
                           values = ~r$reactive_sf_ADM_2$FifthShow, 
                           opacity = 0.7, 
                           title = i18n$t(fifthParams$legend_title), 
                           decreasing = TRUE,
                           position = "bottomright",
                           labFormat = function(type, cuts, p) {
                             paste0(fifthParams_labels_disp())
                           }) %>%
      setView(lng = -90,
              lat = 16,
              zoom = 7) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
    
  })
  # Update map with selected domestic migration subtopic: OUTBOUND     
  observeEvent(input$out_button, {
    
    r$reactive_sf_ADM_2$FifthShow <- r$reactive_sf_ADM_2$DomOut
    
    fifthParams$bins = c(0, 5000, 10000, 15000, 20000, 25000, Inf)
    
    fifthParams$metric = "Out"
    
    fifthParams$legend_title = "Outbound People"
    
    fifthParams$palette = "Purples"
    
    fifth_page_map_national_pal <- colorBin(palette = fifthParams$palette, domain = r$reactive_sf_ADM_2$FifthShow, bins = fifthParams$bins)
    
    fifth_page_map_labels <- sprintf(
      "<strong>%s</strong><br/>%s<br/>%g %s",
      r$reactive_sf_ADM_2$ADM1, r$reactive_sf_ADM_2$ADM2, r$reactive_sf_ADM_2$FifthShow, i18n$t("people")) %>%
      lapply(htmltools::HTML)
    
    leafletProxy("fifth_page_map_national", data = r$reactive_sf_ADM_2) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(fillColor = ~fifth_page_map_national_pal(FifthShow),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  label = fifth_page_map_labels,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE)) %>%
      addPolylines(data = sf_ADM1_p[sf_ADM1_p$ADM1 == input$dropdown,c("geometry")],
                   weight = 6,
                   color = "white",
                   layerId = "select") %>%
      addLegend_decreasing(pal=fifth_page_map_national_pal, 
                           values = ~r$reactive_sf_ADM_2$FifthShow, 
                           opacity = 0.7, 
                           title = i18n$t(fifthParams$legend_title), 
                           decreasing = TRUE,
                           position = "bottomright",
                           labFormat = function(type, cuts, p) {
                             paste0(fifthParams_labels_disp())
                           }) %>%
      setView(lng = -90,
              lat = 16,
              zoom = 7) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
    
  })
  # Domestic migration flows by municipality (ADM1)
  # Render the map polygons, basemap, labels, and legend   
  output$fifth_page_map_flows <- renderLeaflet({

    #Primary is origin, secondary is destination
    if(input$dropdown == "All"){
      
      sf_mig <- sf_ADM_1
      sf_mig$Migrants <- NA
    } else {
    mig_filtered_flows <-  imported_migration_data %>% 
      dplyr::filter(Primary == input$dropdown) %>% 
      dplyr::select(Secondary, Net, Inbound, Outbound) %>% 
      pivot_longer(cols = 2:4, names_to = "Metric", values_to = "Migrants") %>% 
      dplyr::filter(Metric == flowParams$metric)
    
    sf_mig <- merge(sf_ADM_1, mig_filtered_flows, by.x = "ADM1", by.y = "Secondary")
    
    }

    fifth_page_flow_map_pal <- colorBin(flowParams$palette, domain = sf_mig$Migrants, bins = flowParams$bins)
    
    fifth_page_map_labels <- sprintf(
      "<strong>%s</strong><br/>%g %s",
      sf_mig$ADM1, sf_mig$Migrants, i18n$t("people")) %>%
      lapply(htmltools::HTML)
    
    fifth_page_map_internal <- leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter", layerId = "basetile",options = providerTileOptions(minZoom = 6)) %>%
      addPolygons(data = sf_mig,
                  fillColor = ~fifth_page_flow_map_pal(Migrants),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = fifth_page_map_labels) %>%
      addPolylines(data = sf_ADM1_p[sf_ADM1_p$ADM1 == input$dropdown,c("geometry")],
                   weight = 6,
                   color = "white",
                   layerId = "select") %>%
      addLegend_decreasing(pal=fifth_page_flow_map_pal,
                           values = sf_mig$Migrants,
                           opacity = 0.7,
                           title = i18n$t(flowParams$legend_title),
                           decreasing = TRUE,
                           position = "bottomright",
                           na.label = i18n$t("Selected"),
                           labFormat = function(type, cuts, p) {
                             paste0(flowParams_labels_disp())
                           }) %>%
      setView(lng = -90,
              lat = 16,
              zoom = 7) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
    
  })
  # Update map with selected domestic migration subtopic: NET MIGRATION    
  observeEvent(input$net_button, {
    
    flowParams$metric = "Net"
    
    flowParams$bins = c(-1000000, -10000, -5000, 0, 5000, 10000, 1000000)
    
    flowParams$legend_title = "Net People"
    
    flowParams$palette = "PRGn"
    
    if(input$dropdown == "All"){
      
      sf_mig <- sf_ADM_1
      sf_mig$Migrants <- NA
      
    } else {
      
      mig_filtered_flows <-  imported_migration_data %>% 
        dplyr::filter(Primary == input$dropdown) %>% 
        dplyr::select(Secondary, Net, Inbound, Outbound) %>% 
        pivot_longer(cols = 2:4, names_to = "Metric", values_to = "Migrants") %>% 
        dplyr::filter(Metric == flowParams$metric)
      
      sf_mig <- merge(sf_ADM_1, mig_filtered_flows, by.x = "ADM1", by.y = "Secondary")
      
    }
    
    fifth_page_flow_map_pal <- colorBin(flowParams$palette, domain = sf_mig$Migrants, bins = flowParams$bins)

    fifth_page_map_labels <- sprintf(
      "<strong>%s</strong><br/>%g %s",
      sf_mig$ADM1, sf_mig$Migrants, i18n$t("people")) %>%
      lapply(htmltools::HTML)
    
    leafletProxy("fifth_page_map_flows", data = sf_mig) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(fillColor = ~fifth_page_flow_map_pal(Migrants),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  label = fifth_page_map_labels,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE)) %>%
      addPolylines(data = sf_ADM1_p[sf_ADM1_p$ADM1 == input$dropdown,c("geometry")],
                   weight = 6,
                   color = "white",
                   layerId = "select") %>%
      addLegend_decreasing(pal=fifth_page_flow_map_pal, 
                           values = ~sf_mig$Migrants, 
                           opacity = 0.7, 
                           title = i18n$t(flowParams$legend_title), 
                           decreasing = TRUE,
                           position = "bottomright",
                           na.label = i18n$t("Selected"),
                           labFormat = function(type, cuts, p) {
                             paste0(flowParams_labels_disp())
                           }) %>%
      setView(lng = -90,
              lat = 16,
              zoom = 7) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
    
  })
  # Update map with selected domestic migration subtopic: INBOUND     
  observeEvent(input$in_button, {
    
    flowParams$metric = "Inbound"
    
    flowParams$bins = c(0, 2000, 4000, 6000, 8000, 10000, 1000000)
    
    flowParams$legend_title = "Inbound People"
    
    flowParams$palette = "Purples"
    
    if(input$dropdown == "All"){
      
      sf_mig <- sf_ADM_1
      sf_mig$Migrants <- NA
      
    } else {
      
      mig_filtered_flows <-  imported_migration_data %>% 
        dplyr::filter(Primary == input$dropdown) %>% 
        dplyr::select(Secondary, Net, Inbound, Outbound) %>% 
        pivot_longer(cols = 2:4, names_to = "Metric", values_to = "Migrants") %>% 
        dplyr::filter(Metric == flowParams$metric)
      
      sf_mig <- merge(sf_ADM_1, mig_filtered_flows, by.x = "ADM1", by.y = "Secondary")
      
    }
    
    fifth_page_flow_map_pal <- colorBin(flowParams$palette, domain = sf_mig$Migrants, bins = flowParams$bins)
    
    fifth_page_map_labels <- sprintf(
      "<strong>%s</strong><br/>%g %s",
      sf_mig$ADM1, sf_mig$Migrants, i18n$t("people")) %>%
      lapply(htmltools::HTML)
    
    leafletProxy("fifth_page_map_flows", data = sf_mig) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(fillColor = ~fifth_page_flow_map_pal(Migrants),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  label = fifth_page_map_labels,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE)) %>%
      addPolylines(data = sf_ADM1_p[sf_ADM1_p$ADM1 == input$dropdown,c("geometry")],
                   weight = 6,
                   color = "white",
                   layerId = "select") %>%
      addLegend_decreasing(pal=fifth_page_flow_map_pal, 
                           values = ~sf_mig$Migrants, 
                           opacity = 0.7, 
                           title = i18n$t(flowParams$legend_title), 
                           decreasing = TRUE,
                           position = "bottomright",
                           na.label = i18n$t("Selected"),
                           labFormat = function(type, cuts, p) {
                             paste0(flowParams_labels_disp())
                           }) %>%
      setView(lng = -90,
              lat = 16,
              zoom = 7) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
    
  })
  # Update map with selected domestic migration subtopic: OUTBOUND     
  observeEvent(input$out_button, {
    
    flowParams$metric = "Outbound"
    
    flowParams$bins = c(0, 2000, 4000, 6000, 8000, 10000, 1000000)
    
    flowParams$legend_title = "Outbound People"
    
    flowParams$palette = "Purples"
    
    if(input$dropdown == "All"){
      
      sf_mig <- sf_ADM_1
      sf_mig$Migrants <- NA
      
    } else {
      
      mig_filtered_flows <-  imported_migration_data %>% 
        dplyr::filter(Primary == input$dropdown) %>% 
        dplyr::select(Secondary, Net, Inbound, Outbound) %>% 
        pivot_longer(cols = 2:4, names_to = "Metric", values_to = "Migrants") %>% 
        dplyr::filter(Metric == flowParams$metric)
      
      sf_mig <- merge(sf_ADM_1, mig_filtered_flows, by.x = "ADM1", by.y = "Secondary")
      
    }
    
    fifth_page_flow_map_pal <- colorBin(flowParams$palette, domain = sf_mig$Migrants, bins = flowParams$bins)
    
    fifth_page_map_labels <- sprintf(
      "<strong>%s</strong><br/>%g %s",
      sf_mig$ADM1, sf_mig$Migrants, i18n$t("people")) %>%
      lapply(htmltools::HTML)
    
    leafletProxy("fifth_page_map_flows", data = sf_mig) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(fillColor = ~fifth_page_flow_map_pal(Migrants),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  label = fifth_page_map_labels,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE)) %>%
      addPolylines(data = sf_ADM1_p[sf_ADM1_p$ADM1 == input$dropdown,c("geometry")],
                   weight = 6,
                   color = "white",
                   layerId = "select") %>%
      addLegend_decreasing(pal=fifth_page_flow_map_pal, 
                           values = ~sf_mig$Migrants, 
                           opacity = 0.7, 
                           title = i18n$t(flowParams$legend_title), 
                           decreasing = TRUE,
                           position = "bottomright",
                           na.label = i18n$t("Selected"),
                           labFormat = function(type, cuts, p) {
                             paste0(flowParams_labels_disp())
                           }) %>%
      setView(lng = -90,
              lat = 16,
              zoom = 7) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
    
  })
  
  # PAGE 6: Mortality MAPS######################################################
  # Crude death rate by municipality
  sixth_page_map_bins <- c(0, 4, 6, 8, Inf)
  
  sixth_map_labels <- reactive(
    if (r$currentLang == "English") {
      sixth_map_labels <- sixthParams$labels_en
    } else {
      sixth_map_labels <- sixthParams$labels_sp
    }
  )

  sixth_page_map_pal <- colorBin("YlOrRd", domain = sf_ADM_2$CDeathRate, bins = sixth_page_map_bins)
  # Render the map polygons, basemap, labels, and legend   
  output$sixth_page_map <- renderLeaflet({
    
    sixth_page_map_labels <- sprintf(
      "<strong>%s </strong><br/>%s<br/>%g %s",
      sf_ADM_2$ADM1, sf_ADM_2$ADM2, sf_ADM_2$CDeathRate, i18n$t("deaths")) %>% 
      lapply(htmltools::HTML)

    sixth_page_map_internal <- leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter", layerId = "basetile",options = providerTileOptions(minZoom = 6)) %>%
      addPolygons(data = sf_ADM_2,
                  fillColor = ~sixth_page_map_pal(CDeathRate),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = sixth_page_map_labels) %>%
      setView(lng = -90,
              lat = 16,
              zoom = 7) %>%
      addLegend_decreasing(pal=sixth_page_map_pal, 
                           values = sf_ADM_2$CDeathRate, 
                           opacity = 0.7, 
                           title = i18n$t("Annual Deaths"), 
                           decreasing = TRUE,
                           position = "bottomright",
                           labFormat = function(type, cuts, p) {
                             paste0(sixth_map_labels())
                           }) %>%
     setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
    
  })
  # Update map with selected ADM1 polylines overlayed when a location is selected
  # from the controls panel     
  observeEvent(input$dropdown, {
    
    leafletProxy("sixth_page_map", session) %>%
      removeShape("select") %>% 
      addPolylines(data = sf_ADM1_p[sf_ADM1_p$ADM1 == input$dropdown,c("geometry")],
                   weight = 6,
                   color = "white",
                   layerId = "select") 
  })
  
  # PAGE 7: Fertility MAPS######################################################
  # Crude birth rate by municipality
  seventh_map_labels <- reactive(
    if (r$currentLang == "English") {
      seventh_map_labels <- seventhParams$labels_en
    } else {
      seventh_map_labels <- seventhParams$labels_sp
    }
  )
  
  seventh_page_map_bins <- c(0, 15, 20, 25, 30, Inf)
  
  seventh_page_map_pal <- colorBin("Greens", domain = sf_ADM_2$CBRate, bins = seventh_page_map_bins)
  # Render the map polygons, basemap, labels, and legend    
  output$seventh_page_map <- renderLeaflet({
    
    seventh_page_map_labels <- sprintf(
      "<strong>%s </strong><br/>%s<br/>%g %s",
      sf_ADM_2$ADM1, sf_ADM_2$ADM2, sf_ADM_2$CBRate, i18n$t("live births")) %>% 
      lapply(htmltools::HTML)
    
    seventh_page_map_internal <- leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter", layerId = "basetile",options = providerTileOptions(minZoom = 6)) %>%
      addPolygons(data = sf_ADM_2,
                  fillColor = ~seventh_page_map_pal(CBRate),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = seventh_page_map_labels) %>%
      setView(lng = -90,
              lat = 16,
              zoom = 7) %>%
      addLegend_decreasing(pal=seventh_page_map_pal, 
                           values = sf_ADM_2$CBRate, 
                           opacity = 0.7, 
                           title = i18n$t("Annual Live Births"), 
                           decreasing = TRUE,
                           position = "bottomright",
                           labFormat = function(type, cuts, p) {
                             paste0(seventh_map_labels())
                           }) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
    
  })
  # Update map with selected ADM1 polylines overlayed when a location is selected
  # from the controls panel     
  observeEvent(input$dropdown, {
    
    leafletProxy("seventh_page_map", session) %>%
      removeShape("select") %>% 
      addPolylines(data = sf_ADM1_p[sf_ADM1_p$ADM1 == input$dropdown,c("geometry")],
                   weight = 6,
                   color = "white",
                   layerId = "select") 
  })
  
  # PAGE 8: Education MAPS######################################################
  # Literacy rate by municipality
  eighth_map_labels <- reactive(
    if (r$currentLang == "English") {
      eighth_map_labels <- eighthParams$labels_en
    } else {
      eighth_map_labels <- eighthParams$labels_sp
    }
  )
  
  eighth_page_map_bins <- c(0, 75, 80, 85, 90, Inf)
  # Render the map polygons, basemap, labels, and legend      
  output$eighth_page_map <- renderLeaflet({

    eighth_page_map_pal <- colorBin("Purples", domain = r$reactive_sf_ADM_2$EighthShow, bins = eighth_page_map_bins)

    eighth_page_map_labels <- sprintf(
      "<strong>%s </strong><br/>%s<br/>%g %s",
      r$reactive_sf_ADM_2$ADM1, r$reactive_sf_ADM_2$ADM2, r$reactive_sf_ADM_2$EighthShow, i18n$t("percent literacy")) %>% 
      lapply(htmltools::HTML)
    
    eighth_page_map_internal <- leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter", 
                       layerId = "basetile", 
                       options = providerTileOptions(
                         minZoom = 6)) %>%
      addPolygons(data = r$reactive_sf_ADM_2,
                  fillColor = ~eighth_page_map_pal(EighthShow),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = eighth_page_map_labels) %>%
      addPolylines(data = sf_ADM1_p[sf_ADM1_p$ADM1 == input$dropdown,c("geometry")],
                   weight = 6,
                   color = "white",
                   layerId = "select") %>%
      setView(lng = -90,
              lat = 16,
              zoom = 7) %>%
      addLegend_decreasing(pal=eighth_page_map_pal, 
                           values = r$reactive_sf_ADM_2$EighthShow, 
                           opacity = 0.7, 
                           title = i18n$t("Literacy Rate"),
                           decreasing = TRUE,
                           position = "bottomright",
                           labFormat = function(type, cuts, p) {
                             paste0(eighth_map_labels())
                           }) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
    
  })
  # Update map with selected education subtopic: ADULT LITERACY    
  observeEvent(input$aLit_button, {
    
    r$reactive_sf_ADM_2$EighthShow <- r$reactive_sf_ADM_2$ALitRate
    
    eighth_page_map_pal <- colorBin("Purples", domain = r$reactive_sf_ADM_2$EighthShow, bins = eighth_page_map_bins)
    
    eighth_page_map_labels <- sprintf(
      "<strong>%s </strong><br/>%s<br/>%g %s",
      r$reactive_sf_ADM_2$ADM1, r$reactive_sf_ADM_2$ADM2, r$reactive_sf_ADM_2$EighthShow, i18n$t("percent literacy")) %>% 
      lapply(htmltools::HTML)
    
    leafletProxy("eighth_page_map", data = r$reactive_sf_ADM_2) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(fillColor = ~eighth_page_map_pal(EighthShow),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = eighth_page_map_labels) %>%
      addPolylines(data = sf_ADM1_p[sf_ADM1_p$ADM1 == input$dropdown,c("geometry")],
                   weight = 6,
                   color = "white",
                   layerId = "select") %>%
      setView(lng = -90,
              lat = 16,
              zoom = 7) %>%
      addLegend_decreasing(pal=eighth_page_map_pal,
                           values = ~r$reactive_sf_ADM_2$EighthShow,
                           opacity = 0.7,
                           title = i18n$t("Literacy Rate"),
                           decreasing = TRUE,
                           position = "bottomright",
                           labFormat = function(type, cuts, p) {
                             paste0(eighth_map_labels())
                           }) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
    
  })
  # Update map with selected education subtopic: YOUTH LITERACY     
  observeEvent(input$yLit_button, {
    
    r$reactive_sf_ADM_2$EighthShow <- r$reactive_sf_ADM_2$YLitRate
    
    eighth_page_map_pal <- colorBin("Purples", domain = r$reactive_sf_ADM_2$EighthShow, bins = eighth_page_map_bins)
    
    eighth_page_map_labels <- sprintf(
      "<strong>%s </strong><br/>%s<br/>%g %s",
      r$reactive_sf_ADM_2$ADM1, r$reactive_sf_ADM_2$ADM2, r$reactive_sf_ADM_2$EighthShow, i18n$t("percent literacy")) %>% 
      lapply(htmltools::HTML)
    
    leafletProxy("eighth_page_map", data = r$reactive_sf_ADM_2) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(fillColor = ~eighth_page_map_pal(EighthShow),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = eighth_page_map_labels) %>%
      addPolylines(data = sf_ADM1_p[sf_ADM1_p$ADM1 == input$dropdown,c("geometry")],
                   weight = 6,
                   color = "white",
                   layerId = "select") %>%
      setView(lng = -90,
              lat = 16,
              zoom = 7) %>%
      addLegend_decreasing(pal=eighth_page_map_pal,
                           values = ~r$reactive_sf_ADM_2$EighthShow,
                           opacity = 0.7,
                           title = i18n$t("Literacy Rate"),
                           decreasing = TRUE,
                           position = "bottomright",
                           labFormat = function(type, cuts, p) {
                             paste0(eighth_map_labels())
                           }) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
    
  })
  
  # PAGE 9: Labour & Economy MAPS###############################################
  # Unemployment rate by municipality
  ninth_map_labels <- reactive(
    if (r$currentLang == "English") {
      ninth_map_labels <- ninthParams$labels_en
    } else {
      ninth_map_labels <- ninthParams$labels_sp
    }
  )
  
  ninth_page_map_bins <- c(0, 2, 4, 6, 8, Inf)
  
  ninth_page_map_pal <- colorBin("YlOrRd", domain = sf_ADM_2$UnemployRa, bins = ninth_page_map_bins)
  # Render the map polygons, basemap, labels, and legend  
  output$ninth_page_map <- renderLeaflet({
    req(sf_ADM_2)
    
    ninth_page_map_labels <- sprintf(
      "<strong>%s </strong><br/>%s<br/>%g %s",
      sf_ADM_2$ADM1, sf_ADM_2$ADM2, sf_ADM_2$UnemployRa, i18n$t("percent unemployment")) %>% 
    lapply(htmltools::HTML)
  
    ninth_page_map_internal <- leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter", 
                       layerId = "basetile",
                       options = providerTileOptions(minZoom = 6)) %>%
      addPolygons(data = sf_ADM_2,
                  fillColor = ~ninth_page_map_pal(UnemployRa),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = ninth_page_map_labels) %>%
      setView(lng = -90,
              lat = 16,
              zoom = 7) %>%
      addLegend_decreasing(pal=ninth_page_map_pal, 
                           values = sf_ADM_2$UnemployRa, 
                           opacity = 0.7, 
                           title = i18n$t("Unemployment Rate"),
                           decreasing = TRUE,
                           position = "bottomright",
                           labFormat = function(type, cuts, p) {
                             paste0(ninth_map_labels())
                           }) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
  }) 
  # Update map with selected ADM1 polylines overlayed when a location is selected
  # from the controls panel    
  observeEvent(input$dropdown, {
    
    leafletProxy("ninth_page_map", session) %>%
      removeShape("select") %>% 
      addPolylines(data = sf_ADM1_p[sf_ADM1_p$ADM1 == input$dropdown,c("geometry")],
                   weight = 6,
                   color = "white",
                   layerId = "select") 
  })
  
  # PAGE 10: Housing MAPS#######################################################
  # Housing unit count by municipality
  tenth_map_labels <- reactive(
    if (r$currentLang == "English") {
      tenth_map_labels <- tenthParams$labels_en
    } else {
      tenth_map_labels <- tenthParams$labels_sp
    }
  )
  
  tenth_page_map_bins <- c(0, 4000, 8000, 12000, 16000, Inf)
  
  tenth_page_map_pal <- colorBin("Greens", domain = sf_ADM_2$HUnits, bins = tenth_page_map_bins)
  # Render the map polygons, basemap, labels, and legend   
  output$tenth_page_map <- renderLeaflet({
    req(sf_ADM_2)
    
    tenth_page_map_labels <- sprintf(
      "<strong>%s </strong><br/>%s<br/>%g %s",
      sf_ADM_2$ADM1, sf_ADM_2$ADM2, sf_ADM_2$HUnits, i18n$t("housing units")) %>% 
      lapply(htmltools::HTML)

    tenth_page_map_internal <- leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter", 
                       layerId = "basetile",
                       options = providerTileOptions(
                         minZoom = 6)) %>%
      addPolygons(data = sf_ADM_2,
                  fillColor = ~tenth_page_map_pal(HUnits),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = tenth_page_map_labels) %>%
      setView(lng = -90,
              lat = 16,
              zoom = 7) %>%
      addLegend_decreasing(pal=tenth_page_map_pal, 
                           values = sf_ADM_2$HUnits, 
                           opacity = 0.7, 
                           title = i18n$t("Housing Units"),
                           decreasing = TRUE,
                           position = "bottomright",
                           labFormat = function(type, cuts, p) {
                             paste0(tenth_map_labels())
                           }) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
  })
  # Update map with selected ADM1 polylines overlayed when a location is selected
  # from the controls panel  
  observeEvent(input$dropdown, {
    
    leafletProxy("tenth_page_map", session) %>%
      removeShape("select") %>% 
      addPolylines(data = sf_ADM1_p[sf_ADM1_p$ADM1 == input$dropdown,c("geometry")],
                   weight = 6,
                   color = "white",
                   layerId = "select") 
  })
  
  # PAGE 11: Water, Sanitation, & Hygiene MAPS##################################
  # Percent of households with amenity by municipality
  eleventh_map_labels <- reactive(
    if (r$currentLang == "English") {
      eleventh_map_labels <- eleventhParams$labels_en
    } else {
      eleventh_map_labels <- eleventhParams$labels_sp
    }
  )
  
  eleventh_page_map_bins <- c(0, 75, 80, 85, 90, Inf)
  # Render the map polygons, basemap, labels, and legend    
  output$eleventh_page_map <- renderLeaflet({
  
    eleventh_page_map_pal <- colorBin("Purples", domain = r$reactive_sf_ADM_2$EleventhSh, bins = eleventh_page_map_bins)
      
    eleventh_page_map_labels <- sprintf(
      "<strong>%s </strong><br/>%s<br/>%g %s",
      r$reactive_sf_ADM_2$ADM1, r$reactive_sf_ADM_2$ADM2, r$reactive_sf_ADM_2$EleventhSh, i18n$t("percent of households with amenity")) %>% 
      lapply(htmltools::HTML)
    
    eleventh_page_map_internal <- leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter", layerId = "basetile",options = providerTileOptions(minZoom = 6)) %>%
      addPolygons(data = r$reactive_sf_ADM_2,
                  fillColor = ~eleventh_page_map_pal(EleventhSh),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = eleventh_page_map_labels) %>%
      addPolylines(data = sf_ADM1_p[sf_ADM1_p$ADM1 == input$dropdown,c("geometry")],
                   weight = 6,
                   color = "white",
                   layerId = "select") %>%
      setView(lng = -90,
              lat = 16,
              zoom = 7) %>%
      addLegend_decreasing(pal=eleventh_page_map_pal, 
                values = r$reactive_sf_ADM_2$EleventhSh, 
                opacity = 0.7, 
                title = i18n$t("Housing Units (Percent)"), 
                decreasing = TRUE,
                position = "bottomright",
                labFormat = function(type, cuts, p) {
                  paste0(eleventh_map_labels())
                }) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
  })
  # Update map with selected water, sanitation, & hygiene subtopic: WATER   
  observeEvent(input$water_button, {
    
    r$reactive_sf_ADM_2$EleventhSh <- r$reactive_sf_ADM_2$PipedWater
    
    eleventh_page_map_pal <- colorBin("Purples", domain = r$reactive_sf_ADM_2$EleventhSh, bins = eleventh_page_map_bins)
    
    eleventh_page_map_labels <- sprintf(
      "<strong>%s </strong><br/>%s<br/>%g %s",
      r$reactive_sf_ADM_2$ADM1, r$reactive_sf_ADM_2$ADM2, r$reactive_sf_ADM_2$EleventhSh, i18n$t("percent of households with amenity")) %>% 
      lapply(htmltools::HTML)
    
    leafletProxy("eleventh_page_map", data = r$reactive_sf_ADM_2) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(fillColor = ~eleventh_page_map_pal(EleventhSh),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = eleventh_page_map_labels) %>%
      addPolylines(data = sf_ADM1_p[sf_ADM1_p$ADM1 == input$dropdown,c("geometry")],
                   weight = 6,
                   color = "white",
                   layerId = "select") %>%
      addLegend_decreasing(pal=eleventh_page_map_pal, 
                values = r$reactive_sf_ADM_2$EleventhSh, 
                opacity = 0.7, 
                title = i18n$t("Housing Units (Percent)"), 
                decreasing = TRUE,
                position = "bottomright",
                labFormat = function(type, cuts, p) {
                  paste0(eleventh_map_labels())
                }) %>%
      setView(lng = -90,
              lat = 16,
              zoom = 7) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
  })
  # Update map with selected water, sanitation, & hygiene subtopic: SANITATION   
  observeEvent(input$sanitation_button, {
    
    r$reactive_sf_ADM_2$EleventhSh <- r$reactive_sf_ADM_2$ToiletinHU
    
    eleventh_page_map_pal <- colorBin("Purples", domain = r$reactive_sf_ADM_2$EleventhSh, bins = eleventh_page_map_bins)
    
    eleventh_page_map_labels <- sprintf(
      "<strong>%s </strong><br/>%s<br/>%g %s",
      r$reactive_sf_ADM_2$ADM1, r$reactive_sf_ADM_2$ADM2, r$reactive_sf_ADM_2$EleventhSh, i18n$t("percent of households with amenity")) %>% 
      lapply(htmltools::HTML)
    
    leafletProxy("eleventh_page_map", data = r$reactive_sf_ADM_2) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(fillColor = ~eleventh_page_map_pal(EleventhSh),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = eleventh_page_map_labels) %>%
      addPolylines(data = sf_ADM1_p[sf_ADM1_p$ADM1 == input$dropdown,c("geometry")],
                   weight = 6,
                   color = "white",
                   layerId = "select") %>%
      addLegend_decreasing(pal=eleventh_page_map_pal, 
                values = r$reactive_sf_ADM_2$EleventhSh, 
                opacity = 0.7, 
                title = i18n$t("Housing Units (Percent)"), 
                decreasing = TRUE,
                position = "bottomright",
                labFormat = function(type, cuts, p) {
                  paste0(eleventh_map_labels())
                }) %>%
      setView(lng = -90,
              lat = 16,
              zoom = 7) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
  })
  # Update map with selected water, sanitation, & hygiene subtopic: HYGIENE   
  observeEvent(input$hygiene_button, {
    
    r$reactive_sf_ADM_2$EleventhSh <- r$reactive_sf_ADM_2$BathShinHU
    
    eleventh_page_map_pal <- colorBin("Purples", domain = r$reactive_sf_ADM_2$EleventhSh, bins = eleventh_page_map_bins)
    
    eleventh_page_map_labels <- sprintf(
      "<strong>%s </strong><br/>%s<br/>%g %s",
      r$reactive_sf_ADM_2$ADM1, r$reactive_sf_ADM_2$ADM2, r$reactive_sf_ADM_2$EleventhSh, i18n$t("percent of households with amenity")) %>% 
      lapply(htmltools::HTML)
    
    leafletProxy("eleventh_page_map", data = r$reactive_sf_ADM_2) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(fillColor = ~eleventh_page_map_pal(EleventhSh),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = eleventh_page_map_labels) %>%
      addPolylines(data = sf_ADM1_p[sf_ADM1_p$ADM1 == input$dropdown,c("geometry")],
                   weight = 6,
                   color = "white",
                   layerId = "select") %>%
      addLegend_decreasing(pal=eleventh_page_map_pal, 
                values = r$reactive_sf_ADM_2$EleventhSh, 
                opacity = 0.7, 
                title = i18n$t("Housing Units (Percent)"), 
                decreasing = TRUE,
                position = "bottomright",
                labFormat = function(type, cuts, p) {
                  paste0(eleventh_map_labels())
                }) %>%
      setView(lng = -90,
              lat = 16,
              zoom = 7) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
  })
  
  # PAGE 12: Power & Technology MAPS############################################
  #
  output$twelfth_page_map <- renderLeaflet({
    
    twelfth_page_map_pal <- colorBin(palette = twelfthParams$palette, domain = r$reactive_sf_ADM_2$TwelfthSho, bins = twelfthParams$bins)
    
    twelfth_page_map_labels <- sprintf(
      "<strong>%s </strong><br/>%s<br/>%g %s",
      r$reactive_sf_ADM_2$ADM1, r$reactive_sf_ADM_2$ADM2, r$reactive_sf_ADM_2$TwelfthSho, i18n$t(twelfthParams$map_labels)) %>% 
      lapply(htmltools::HTML)
    
    twelfth_page_map_internal <- leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter", layerId = "basetile",options = providerTileOptions(minZoom = 6)) %>%
      addPolygons(data = r$reactive_sf_ADM_2,
                  fillColor = ~twelfth_page_map_pal(TwelfthSho),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = twelfth_page_map_labels) %>%
      addPolylines(data = sf_ADM1_p[sf_ADM1_p$ADM1 == input$dropdown,c("geometry")],
                 weight = 6,
                 color = "white",
                 layerId = "select") %>%
      setView(lng = -90,
              lat = 16,
              zoom = 7) %>%
      addLegend_decreasing(pal=twelfth_page_map_pal, 
                           values = r$reactive_sf_ADM_2$TwelfthSho, 
                           opacity = 0.7, 
                           title = i18n$t(twelfthParams$legend_title), 
                           decreasing = TRUE,
                           position = "bottomright",
                           labFormat = function(type, cuts, p) {
                             paste0(twelfthParams_labels_disp())
                           }) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
  })
  # Update map with selected power & technology subtopic: ELECTRICITY    
  observeEvent(input$electricity_button, {

    r$reactive_sf_ADM_2$TwelfthSho <- r$reactive_sf_ADM_2$Electric

    twelfthParams$bins = c(0, 85, 90, 95, Inf)
    
    twelfthParams$legend_title = "Households (Percent)"
    
    twelfthParams$palette = "YlOrRd"
    
    twelfthParams$map_labels = "percent of households"

    twelfth_page_map_pal <- colorBin(palette = twelfthParams$palette, domain = r$reactive_sf_ADM_2$TwelfthSho, bins = twelfthParams$bins)

    twelfth_page_map_labels <- sprintf(
      "<strong>%s </strong><br/>%s<br/>%g %s",
      r$reactive_sf_ADM_2$ADM1, r$reactive_sf_ADM_2$ADM2, r$reactive_sf_ADM_2$TwelfthSho, i18n$t(twelfthParams$map_labels)) %>% 
      lapply(htmltools::HTML)

    leafletProxy("twelfth_page_map", data = r$reactive_sf_ADM_2) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(fillColor = ~twelfth_page_map_pal(TwelfthSho),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = twelfth_page_map_labels) %>%
      addPolylines(data = sf_ADM1_p[sf_ADM1_p$ADM1 == input$dropdown,c("geometry")],
                   weight = 6,
                   color = "white",
                   layerId = "select") %>%
      setView(lng = -90,
              lat = 16,
              zoom = 7) %>%
      addLegend_decreasing(pal=twelfth_page_map_pal, 
                values = r$reactive_sf_ADM_2$TwelfthSho, 
                opacity = 0.7, 
                title = i18n$t(twelfthParams$legend_title), 
                decreasing = TRUE,
                position = "bottomright",
                labFormat = function(type, cuts, p) {
                  paste0(twelfthParams_labels_disp())
                }) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
  })
  # Update map with selected power & technology subtopic: INTERNET ACCESS 
  observeEvent(input$ICT_button, {
    
    r$reactive_sf_ADM_2$TwelfthSho <- r$reactive_sf_ADM_2$Internet
    
    twelfthParams$bins = c(0, 60, 70, 80, Inf)

    twelfthParams$legend_title = "Households (Percent)"
    
    twelfthParams$palette = "YlOrRd"
    
    twelfthParams$map_labels = "percent of households"

    twelfth_page_map_pal <- colorBin("YlOrRd", domain = r$reactive_sf_ADM_2$TwelfthSho, bins = twelfthParams$bins)
    
    twelfth_page_map_labels <- sprintf(
      "<strong>%s </strong><br/>%s<br/>%g %s",
      r$reactive_sf_ADM_2$ADM1, r$reactive_sf_ADM_2$ADM2, r$reactive_sf_ADM_2$TwelfthSho, i18n$t(twelfthParams$map_labels)) %>% 
      lapply(htmltools::HTML)
    
    leafletProxy("twelfth_page_map", data = r$reactive_sf_ADM_2) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(fillColor = ~twelfth_page_map_pal(TwelfthSho),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = twelfth_page_map_labels) %>%
      addPolylines(data = sf_ADM1_p[sf_ADM1_p$ADM1 == input$dropdown,c("geometry")],
                   weight = 6,
                   color = "white",
                   layerId = "select") %>%
      setView(lng = -90,
              lat = 16,
              zoom = 7) %>%
      addLegend_decreasing(pal=twelfth_page_map_pal, 
                values = r$reactive_sf_ADM_2$TwelfthSho, 
                opacity = 0.7, 
                title = i18n$t(twelfthParams$legend_title), 
                decreasing = TRUE,
                position = "bottomright",
                labFormat = function(type, cuts, p) {
                  paste0(twelfthParams_labels_disp())
                }) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
  })
  
  #PLOTS BY PAGE################################################################
  # The code below produces data visualization for each page (1-12).
  
  # PAGE 1 PLOTS: Demographic & Social##########################################
  # Population by department
  output$firstPage_Pop_ADM1Plot <- renderPlot({
    
    req(imported_data, input$dropdown)

    a <- imported_data %>%
      filter(Metric == "Population" & Level == "ADM1" & Type == "Total") %>%
      mutate(ADM1 = fct_reorder(ADM1, Value)) %>%
      ggplot(aes(x = reorder(ADM1, Value), y = Value/1000000)) +
      geom_bar(fill = "#ffffcc", stat = "identity", width = 0.8) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      theme(legend.position = "none") +
      ylab(i18n$t("People (in millions)")) +
      xlab("") +
      ggtitle(i18n$t("Population by Department, Guatemala 2018")) +
      coord_flip() +
      theme_plot() 
    
    if(input$dropdown != "All") {
      a <- a  + gghighlight(ADM1 == input$dropdown)
      a
    } else {
      a
    }
  })
  
  # Population by Sex
    output$firstPage_Sex <- renderPlot({
      req(r$currentLang, input$dropdown, dropdownFilteredData())
      
      if(input$dropdown == "All") {
        selectedLocation <- "Guatemala" 
      } else {
        selectedLocation <- input$dropdown
      }
      
      firstPage_Sex_Title <- sprintf(
        "%s, %s 2018",
        i18n$t("Population by Sex"), selectedLocation) %>%
        lapply(htmltools::HTML)
      
    sexPlot <- dropdownFilteredData() %>%
      filter(Metric == "Population" & Type %in% c("Male", "Female")) %>%
      ggplot(aes(x = secondaryText, y = Value/100000, fill = secondaryText)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      scale_fill_manual(values = c("#addd8e","#31a354")) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      ggtitle(firstPage_Sex_Title) +
      ylab(i18n$t("People (in hundred thousands)")) +
      xlab("") +
      theme_plot() +
      coord_flip()
    
    sexPlot
  })

  # Population by age  
  output$firstPage_Age <- renderPlot({
    req(input$dropdown, dropdownFilteredData())
    
    xLabels <- c("0 - 4", "5 - 9", "10 - 14", "15 - 19", "20 - 24", "25 - 29", "30 - 34", "35 - 39", "40 - 44", "45 - 49", "50 - 54", "55 - 59", "60 - 64", "65+")
    
    if(input$dropdown == "All") {
      selectedLocation <- "Guatemala" 
    } else {
      selectedLocation <- input$dropdown
    }
    
    firstPage_Age_Title <- sprintf(
      "%s, %s 2018",
      i18n$t("Population by Age"), selectedLocation) %>%
      lapply(htmltools::HTML)
    
    
    agePlot <- dropdownFilteredData() %>%
      filter(Metric == "Population" & Type %in% c("0 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65+")) %>%
      mutate(Type = fct_relevel(Type, "0 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65+")) %>%
      ggplot(aes(x = Type, y = Value/10000)) +
      geom_bar(stat = "identity", fill = "#78c679") +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      ggtitle(firstPage_Age_Title) +
      ylab(i18n$t("People (in ten thousands)")) +
      xlab("") +
      scale_x_discrete(labels = xLabels) +
      coord_flip() +
      theme_plot()
    
    agePlot
  })
  
  # Population by Urban/Rural status
  output$firstPage_UrbanRural <- renderPlot({
    req(r$currentLang, input$dropdown, dropdownFilteredData())
    
    if (r$currentLang == "English") {
      xLabels <- c("Rural", "Urban")
    } else if (r$currentLang == "Spanish") {
      xLabels <- c("Rural", "Urbana")
    } else {
      xLabels <- c("XY", "XX")
    }
    
    if(input$dropdown == "All") {
      selectedLocation <- "Guatemala" 
    } else {
      selectedLocation <- input$dropdown
    }
    
    firstPage_UR_Title <- sprintf(
      "%s, %s 2018",
      i18n$t("Population by Urban / Rural Residence"), selectedLocation) %>%
      lapply(htmltools::HTML)
    
    urbanRuralPlot <- dropdownFilteredData() %>%
      filter(Metric == "Population" & Type %in% c("Urban", "Rural")) %>%
      mutate(UrbanRural = fct_relevel(UrbanRural, "Urban", "Rural")) %>%
      ggplot(aes(x = Type, y = Value/100000, fill = Type)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      scale_fill_manual(values = c("#31a354","#006837")) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      ggtitle(firstPage_UR_Title) +
      ylab(i18n$t("People (in hundred thousands)")) +
      xlab("") +
      scale_x_discrete(labels = xLabels) +
      coord_flip() +
      theme_plot()
    
    urbanRuralPlot
  })
  # PAGE 2 PLOTS: Household & Family############################################
  # Average hosuehold size by department
  output$secondPage_HHSize_ADM1Plot <- renderPlot({
    req(imported_data,input$dropdown)
    
    a <- secondPage_HHSize_ADM1Plot <- imported_data %>%
      filter(Level == "ADM1" & Metric == "Household Size" & Type == "Total") %>%
      mutate(ADM1 = fct_reorder(ADM1, Value)) %>%
      ggplot(aes(x = reorder(ADM1, Value), y = Value)) +
      geom_bar(fill = "#f2f0f7", stat = "identity", width = 0.8) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      theme(legend.position = "none") +
      ylab(i18n$t("Average Household Size")) +
      xlab("") +
      ggtitle(i18n$t("Average Household Size by Department, Guatemala 2018")) +
      theme_plot() +
      coord_flip()
    
    if(input$dropdown != "All") {
      a <- a + gghighlight(ADM1 == input$dropdown)
      a
    } else {
      a
    }
  })
  
  # Household count by type
  output$secondPage_HouseholdTypePlot <- renderPlot({
    req(input$dropdown, dropdownFilteredData())
    
    if(input$dropdown == "All") {
      selectedLocation <- "Guatemala" 
    } else {
      selectedLocation <- input$dropdown
    }
    
    secondPage_householdType_Title <- sprintf(
      "%s, %s 2018",
      i18n$t("Household Count by Type"), selectedLocation) %>%
      lapply(htmltools::HTML)
    
    householdTypePlot <- dropdownFilteredData() %>%
      dplyr::filter(Metric == "HHTypes") %>%
      ggplot(aes(x = reorder(secondaryText, Value), y = Value/1000, fill = secondaryText)) +
      geom_bar(fill = "#dadaeb", stat = "identity", show.legend = FALSE) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      ggtitle(secondPage_householdType_Title) +
      ylab(i18n$t("Households (in thousands)")) +
      xlab("") +
      theme(legend.position = "none") +
      theme_plot() +
      coord_flip()
    
    householdTypePlot
  })
  
  # Household Count by Family Nucleus
  output$secondPage_FamilyNucleusPlot <- renderPlot({
    req(input$dropdown, dropdownFilteredData())
    
    if(input$dropdown == "All") {
      selectedLocation <- "Guatemala" 
    } else {
      selectedLocation <- input$dropdown
    }
    
    secondPage_familyNucleus_Title <- sprintf(
      "%s, %s 2018",
      i18n$t("Household Count by Family Nucleus"), selectedLocation) %>%
      lapply(htmltools::HTML)
    
    familyNucleusPlot <- dropdownFilteredData() %>%
      dplyr::filter(Metric == "FamilyNucleus") %>%
      ggplot(aes(x = reorder(secondaryText, Value), y = Value/1000, fill = secondaryText)) +
      geom_bar(fill = "#9e9ac8", stat = "identity", show.legend = FALSE) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      ggtitle(secondPage_familyNucleus_Title) +
      ylab(i18n$t("Households (in thousands)")) +
      xlab("") +
      theme(legend.position = "none") +
      theme_plot() +
      coord_flip()
    
    familyNucleusPlot
  })
  
  # Population by Marital Status
  output$secondPage_MaritalStatusPlot <- renderPlot({
    req(input$dropdown, dropdownFilteredData())
    
    if(input$dropdown == "All") {
      selectedLocation <- "Guatemala" 
    } else {
      selectedLocation <- input$dropdown
    }
    
    secondPage_maritalStatus_Title <- sprintf(
      "%s, %s 2018",
      i18n$t("Population by Marital Status"), selectedLocation) %>%
      lapply(htmltools::HTML)
    
    maritalStatusPlot <- dropdownFilteredData() %>%
      dplyr::filter(Metric == "MaritalStatus") %>%
      ggplot(aes(x = reorder(secondaryText, Value), y = Value/100000, fill = secondaryText)) +
      geom_bar(fill = "#756bb1", stat = "identity", show.legend = FALSE) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      # scale_x_discrete(labels = c("Married Separated" = "Separated")) +
      ggtitle(secondPage_maritalStatus_Title) +
      ylab(i18n$t("People (in hundred thousands)")) +
      xlab("") +
      theme(legend.position = "none") +
      theme_plot() +
      coord_flip()
    
    maritalStatusPlot
  })
  
  # Population by Relationship to the Reference Person of the Household
  output$secondPage_RefPersonRelationshipPlot <- renderPlot({
    req(input$dropdown, dropdownFilteredData())
    
    if(input$dropdown == "All") {
      selectedLocation <- "Guatemala" 
    } else {
      selectedLocation <- input$dropdown
    }
    
    secondPage_refPerson_Title <- sprintf(
      "%s, %s 2018",
      i18n$t("Population by Relationship to the Reference Person of the Household"), selectedLocation) %>%
      lapply(htmltools::HTML)
    
    refpersonRelationshipPlot <- dropdownFilteredData() %>%
      dplyr::filter(Metric == "RelationshipReferencePerson") %>%
      ggplot(aes(x = reorder(secondaryText, Value), y = Value/100000, fill = secondaryText)) +
      geom_bar(fill = "#54278f", stat = "identity", show.legend = FALSE) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      ggtitle(secondPage_refPerson_Title) +
      ylab(i18n$t("People (in hundred thousands)")) +
      xlab("") +
      theme(legend.position = "none") +
      theme_plot() +
      coord_flip()

    refpersonRelationshipPlot
  })
  # PAGE 3 PLOTS: Disability Status#############################################
  # Prevalence rate of each disability subtopic, by department
  output$thirdPage_Dis_ADM1Plot <- renderPlot({
    req(r$thirdPage_focused_dataset, input$dropdown)
    
    internal <- r$thirdPage_focused_dataset['Metric'] %>%
      slice(2) %>%
      unlist(., use.names = FALSE)
    
    if (internal == "Walking") {
      plotTitle <- "Prevalence Rate of Population with at Least Some Difficulty in Walking\nby Department"
    } else if (internal == "Seeing") {
      plotTitle <- "Prevalence Rate of Population with at Least Some Difficulty in Seeing\nby Department"
    } else if (internal == "Hearing") {
      plotTitle <- "Prevalence Rate of Population with at Least Some Difficulty in Hearing\nby Department"
    } else if (internal == "Cognition") {
      plotTitle <- "Prevalence Rate of Population with at Least Some Cognition Difficulty\nby Department"
    } else if (internal == "Selfcare") {
      plotTitle <- "Prevalence Rate of Population with at Least Some Difficulty With Selfcare\nby Department"
    } else if (internal == "Communication") {
      plotTitle <- "Prevalence Rate of Population with at Least Some Difficulty With Communication\nby Department"
    } else {
      plotTitle <- internal
    }
    
    a <- r$thirdPage_focused_dataset %>%
      filter(Level == "ADM1") %>%
      filter(Type == "DisabilityRate") %>%
      mutate(ADM1 = fct_reorder(ADM1, Value)) %>%
      ggplot(aes(x = reorder(ADM1, Value), y = Value)) +
      geom_bar(fill = "#fecc5c", stat = "identity", width = 0.8, alpha = 0.8) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      xlab("") +
      theme(legend.position = "none") +
      ylab(i18n$t("Prevalence Rate (Percent)")) +
      theme_plot() +
      ggtitle(i18n$t(plotTitle)) +
      coord_flip()
    
    if(input$dropdown != "All") {
      a <- a  + gghighlight(ADM1 == input$dropdown)
      a
    } else {
      a
    }
  })
  
  # PAGE 4 PLOTS: International Migration#######################################
  # Percent of Population by international migration subtopic 
  output$fourthPage_first_ADM1Plot <- renderPlot({
    req(r$fourthPage_focused_dataset, input$dropdown)

    internal <- r$fourthPage_focused_dataset %>% 
      filter(Type != "LengthofStay")
      
    internal2 <-  internal['Metric'] %>%
      slice(2) %>%
      unlist(., use.names = FALSE)

    if (internal2 == "ForeignBorn") {
      plotTitle <- "Percent of Population Foreign Born, Guatemala 2018"
    } else if (internal2 == "ForeignCitizenship") {
      plotTitle <- "Percent of Population with Foreign Citizenship, Guatemala 2018"
    } else {
      plotTitle <- internal2
    }

    a <- r$fourthPage_focused_dataset %>%
      filter(Level == "ADM1") %>%
      filter(Type == "ForeignRate") %>%
      mutate(ADM1 = fct_reorder(ADM1, Value)) %>%
      ggplot(aes(x = reorder(ADM1, Value), y = Value)) +
      geom_bar(fill = "#addd8e", stat = "identity", width = 0.8) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      xlab("") +
      theme(legend.position = "none") +
      ylab(i18n$t("Percent of Population")) +
      ggtitle(i18n$t(plotTitle)) +
      theme_plot() +
      coord_flip()
    
    if(input$dropdown != "All") {
      a <- a  + gghighlight(ADM1 == input$dropdown)
      a
    } else {
      a
    } 
  })
  
  # Average length of stay by international migration subtopic     
  output$fourthPage_second_ADM1Plot <- renderPlot({
    req(r$fourthPage_focused_dataset, input$dropdown)
    
    internal <- r$fourthPage_focused_dataset %>% 
      filter(Type != "ForeignRate")
    
    internal2 <-  internal['Metric'] %>%
      slice(2) %>%
      unlist(., use.names = FALSE)
    
    if (internal2 == "ForeignBorn") {
      plotTitle <- "Average Length of Stay, Population Foreign\nBorn by Department, Guatemala 2018"
    } else if (internal2 == "ForeignCitizenship") {
      plotTitle <- "Average Length of Stay, Population with\nForeign Citizenship by Department, Guatemala 2018"
    } else {
      plotTitle <- internal2
    }
    
    a <- r$fourthPage_focused_dataset %>%
      filter(Level == "ADM1") %>%
      filter(Type == "LengthofStay") %>%
      mutate(ADM1 = fct_reorder(ADM1, Value)) %>%
      ggplot(aes(x = reorder(ADM1, Value), y = Value)) +
      geom_bar(fill = "#31a354", stat = "identity", width = 0.8) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      xlab("") +
      theme(legend.position = "none") +
      ylab(i18n$t("Years")) +
      ggtitle(i18n$t(plotTitle)) +
      theme_plot() +
      coord_flip()
    
    if(input$dropdown != "All") {
      a <- a  + gghighlight(ADM1 == input$dropdown)
      a
    } else {
      a
    }
  })

  # Year of arrival in country   
  output$fourthPage_arrivalPlot <- renderPlot({
    req(dropdownFilteredData, input$dropdown)
    
    if(input$dropdown == "All") {
      selectedLocation <- "Guatemala"
    } else {
      selectedLocation <- input$dropdown
    }

    fourthPage_Arrival_Title <- sprintf(
      "%s,\n%s 2018",
      i18n$t("Year of Arrival in Guatemela, Population Foreign Born"), selectedLocation) %>%
      lapply(htmltools::HTML)
    
    arrivalPlot <- dropdownFilteredData() %>%
      dplyr::filter(Metric == "ArrivalYear") %>%
      ggplot(aes(x = Type, y = Value/1000)) +
      geom_bar(fill = "#006837", stat = "identity") +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      scale_x_discrete(guide = guide_axis(angle = 45)) +
      ylab(i18n$t("People (in thousands)")) +
      xlab(i18n$t("Year")) +
      ggtitle(fourthPage_Arrival_Title) +
      theme_plot()
    
    arrivalPlot
  })

  # PAGE 5 PLOTS: Domestic Migration#######################################
  # Total migrants by domestic migration subtopic   
  output$fifthPage_national_ADM1Plot <- renderPlot({
    req(r$fifthPage_focused_dataset, input$dropdown)

    internal <-  r$fifthPage_focused_dataset['Type'] %>%
      slice(2) %>%
      unlist(., use.names = FALSE)
    
    if (internal == "Net") {
      plotTitle <- i18n$t("Net Domestic Migrants by Department, Guatemala 2018")
    } else if (internal == "Inbound") {
      plotTitle <- i18n$t("Inbound Domestic Migrants by Department, Guatemala 2018")
    } else if (internal == "Outbound") {
      plotTitle <- i18n$t("Outbound Domestic Migrants by Department, Guatemala 2018")
    } else {
      plotTitle <- internal
    }

    a <- r$fifthPage_focused_dataset %>%
      ggplot(aes(x = reorder(ADM1, Value), y = Value/1000)) +
      geom_bar(fill = "#807CBA", stat = "identity", width = 0.8) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      xlab(i18n$t("")) +
      theme(legend.position = "none") +
      ylab(i18n$t("People (in thousands)")) +
      xlab("") +
      theme_plot() +
      ggtitle(plotTitle) +
      coord_flip()
  
    if(input$dropdown != "All") {
      a <- a  + gghighlight(ADM1 == input$dropdown)
      a
    } else {
      a
    }
  })

  # PAGE 6 PLOTS: Mortality#######################################
  # Crude death Rate by Department
  output$sixthPage_Deaths_ADM1Plot <- renderPlot({
    req(imported_data, input$dropdown)

    a <- imported_data %>%
      filter(Level == "ADM1" & Metric == "CrudeDeathRate" & Type == "Total") %>%
      mutate(ADM1 = fct_reorder(ADM1, Value)) %>%
      ggplot(aes(x = reorder(ADM1, Value), y = Value)) +
      geom_bar(fill = "#fd8d3c", stat = "identity", alpha = 0.8, width = 0.8) +
      xlab("") +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      theme(legend.position = "none") +
      ylab(i18n$t("Annual Deaths per 1,000 People")) +
      ggtitle(i18n$t("Crude Death Rate by Department, Guatemala 2018")) +
      theme_plot() +
      coord_flip()
    
    if(input$dropdown != "All") {
      a <- a + gghighlight(ADM1 == input$dropdown)
      a
    } else {
      a
    }
  })
  
  # Deaths by Cause
  output$sixthPage_death_cause <- renderPlot({
    req(input$dropdown, dropdownFilteredData())
    
    if(input$dropdown == "All") {
      selectedLocation <- "Guatemala" 
    } else {
      selectedLocation <- input$dropdown
    }
    
    sixthPage_deathCause_Title <- sprintf(
      "%s, %s 2018",
      i18n$t("Deaths by Cause"), selectedLocation) %>%
      lapply(htmltools::HTML)
    
    deathCausePlot <- dropdownFilteredData() %>%
      dplyr::filter(Metric == "Deaths" & Type %in% c("External", "Pregnancy-Related", "Other", "Unknown")) %>%
      ggplot(aes(x = reorder(secondaryText, desc(Value)), y = Value/1000, fill = secondaryText)) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      geom_bar(stat = "identity", fill = "#f03b20", alpha = 0.8, show.legend = FALSE) +
      ggtitle(sixthPage_deathCause_Title) +
      scale_x_discrete(guide = guide_axis(angle = 45)) +
      ylab(i18n$t("Deaths (in thousands)")) +
      xlab(i18n$t("Cause")) +
      theme_plot()
    
    deathCausePlot
  })
  
  # Deaths by age category
  output$sixthPage_death_pyramid <- renderPlot({
    req(input$dropdown, dropdownFilteredData())
    
    xLabels <- c("0 - 4", "5 - 9", "10 - 14", "15 - 19", "20 - 24", "25 - 29", "30 - 34", "35 - 39", "40 - 44", "45 - 49", "50 - 54", "55 - 59", "60 - 64", "65+")
    
    if(input$dropdown == "All") {
      selectedLocation <- "Guatemala" 
    } else {
      selectedLocation <- input$dropdown
    }
    
    sixthPage_deathPyramid_Title <- sprintf(
      "%s, %s 2018",
      i18n$t("Deaths by Age Category"), selectedLocation) %>%
      lapply(htmltools::HTML)

    deathPyramidPlot <- dropdownFilteredData() %>%
      dplyr::filter(Metric == "Deaths" & Type == "Age_Sex") %>%
      mutate(Population = ifelse(Sex == "Male", Value/100, -Value/100)) %>%
      mutate(Age = fct_relevel(Age, "0 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49","50 to 54", "55 to 59", "60 to 64", "65+")) %>%
      ggplot(aes(
        x = Age,
        y = Population,
        fill = Sex)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("#feb24c", "#ffffb2"),
                        labels = c("Male" = i18n$t("Male"),
                                   "Female" = i18n$t("Female"))) +
      coord_flip() +
      facet_share(
        ~ Sex,
        dir = "h",
        scales = "free",
        reverse_num = TRUE) +
      scale_y_continuous(labels = label_number(accuracy = 1)) +
      scale_x_discrete(labels = xLabels) +
      xlab("") +
      ylab(i18n$t("Deaths (in hundreds)")) +
      ggtitle(sixthPage_deathPyramid_Title) +
      theme_plot(strip.background = element_blank(),
                 strip.text.x = element_blank())
    
    deathPyramidPlot
  })

  # PAGE 7 PLOTS: Fertility#######################################
  # Crude birth Rate by Department      
  output$seventhPage_CBRate_ADM1Plot <- renderPlot({
    req(imported_data, input$dropdown)
    
    a <- imported_data %>%
      filter(Metric == "CrudeBirthRate" & Type == "Total" & Level == "ADM1") %>%
      mutate(ADM1 = fct_reorder(ADM1, Value)) %>%
      ggplot(aes(x = reorder(ADM1, Value), y = Value)) +
      geom_bar(fill = "#41AB5D", stat = "identity", width = 0.8) +
      theme(legend.position = "none") +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      ylab(i18n$t("Annual Live Births per 1,000 People")) +
      xlab("") +
      ggtitle(i18n$t("Crude Birth Rate by Department, Guatemala 2018")) +
      coord_flip() +
      theme_plot()
    
    if(input$dropdown != "All") {
      a <- a + gghighlight(ADM1 == input$dropdown)
      a
    } else {
      a
    }
  })

  # PAGE 8 PLOTS: Education#######################################
  # Literacy rate by education subtopic  
  output$eighthPage_LiteracyRate_ADM1Plot <- renderPlot({
    req(r$eighthPage_focused_dataset, input$dropdown)
    
    internal <- r$eighthPage_focused_dataset['Metric'] %>%
      slice(2) %>%
      unlist(., use.names = FALSE)
    
    if (internal == "AdultLiteracyRate") {
      plotTitle <- i18n$t("Adult Literacy Rate by Department, Guatemala 2018")
    } else if (internal == "YouthLiteracyRate") {
      plotTitle <- i18n$t("Youth Literacy Rate by Department, Guatemala 2018")
    } else {
      plotTitle <- internal
    }
    
    a <- r$eighthPage_focused_dataset %>%
      filter(Level == "ADM1" & Type == "Total") %>%
      mutate(ADM1 = fct_reorder(ADM1, Value)) %>%
      ggplot(aes(x = reorder(ADM1, Value), y = Value)) +
      geom_bar(fill = "#756bb1", stat = "identity", width = 0.8, alpha = 0.8) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      xlab("") +
      theme(legend.position = "none") +
      ylab(i18n$t("Literacy Rate (Percent)")) +
      theme_plot() +
      ggtitle(plotTitle) +
      coord_flip() 
    
    if(input$dropdown != "All") {
      a <- a + gghighlight(ADM1 == input$dropdown)
      a
    } else {
      a
    }
  })
  
  # school attendance by sex
  output$eighthPage_attendance <- renderPlot({
    req(input$dropdown, dropdownFilteredData())
    
    if(input$dropdown == "All") {
      selectedLocation <- "Guatemala" 
    } else {
      selectedLocation <- input$dropdown
    }
    
    eighthPage_attendance_Title <- sprintf(
      "%s, %s 2018",
      i18n$t("School Attendance by Sex"), selectedLocation) %>%
      lapply(htmltools::HTML)
    
    attendancePlot <- dropdownFilteredData() %>%
      dplyr::filter(Metric == "SchoolAttendance") %>%
      ggplot(aes(fill = Sex, y = Value/1000, x = Type)) +
      geom_bar(position="dodge", stat = "identity") +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      scale_x_discrete(labels = c("ISCED levels 5-8: Tertiary education" = i18n$t("Tertiary"),
                                  "ISCED levels 2-3: Secondary education" = i18n$t("Secondary"),
                                  "ISCED level 1: Primary education" = i18n$t("Primary"))) +
      xlab("") +
      ylab(i18n$t("People (in thousands)")) +
      ggtitle(eighthPage_attendance_Title) +
      theme_plot() +
      coord_flip() +
      scale_fill_manual(values = c("#bcbddc","#9e9ac8","#54278f"),
                        labels = c("Total" = i18n$t("Total"),
                                   "Male" = i18n$t("Male"),
                                   "Female" = i18n$t("Female")),
                        guide = guide_legend(reverse = TRUE))
    
    attendancePlot
  })
  
  # Educational attainment (highest level completed)
  output$eighthPage_attainment <- renderPlot({
    req(input$dropdown, dropdownFilteredData())
    
    if(input$dropdown == "All") {
      selectedLocation <- "Guatemala" 
    } else {
      selectedLocation <- input$dropdown
    }
    
    eighthPage_attainment_Title <- sprintf(
      "%s, %s 2018",
      i18n$t("Educational Attainment (Highest Education Level Completed)"), selectedLocation) %>%
      lapply(htmltools::HTML)
    
    attainmentPlot <- dropdownFilteredData() %>%
      dplyr::filter(Metric == "EducationalAttainment" & !is.na(Type)) %>%
      ggplot(aes(
        y = Value/100000,
        x = Type)) +
      geom_bar(stat = "identity", fill = "#dadaeb") +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      scale_x_discrete(labels = c("ISCED level 0: Less than primary education" = i18n$t("ISCED 0:\nLess than\nprimary"),
                                  "ISCED level 1: Primary education" = i18n$t("ISCED 1:\nPrimary"),
                                  "ISCED level 2: Lower secondary education" = i18n$t("ISCED 2:\nLower secondary"),
                                  "ISCED level 3: Upper secondary education" = i18n$t("ISCED 3:\nUpper secondary"),
                                  "ISCED level 4: Post-secondary non-tertiary education" = i18n$t("ISCED 4:\nPost-secondary\nnon-tertiary"),
                                  "ISCED level 5: Short-cycle tertiary education" = i18n$t("ISCED 5:\nShort-cycle\ntertiary"),
                                  "ISCED level 6: Bachelor’s or equivalent level" = i18n$t("ISCED 6:\nBachelor's or\nequivalent"),
                                  "ISCED level 7: Master’s or equivalent level" = i18n$t("ISCED 7:\nMaster's or\nequivalent"),
                                  "ISCED level 8: Doctoral or equivalent level" = i18n$t("ISCED 8:\nDoctoral or\nequivalent"))) +
      theme(legend.position = "none") +
      xlab("") +
      ylab(i18n$t("People (in hundred thousands)")) +
      ggtitle(eighthPage_attainment_Title) +
      theme_plot() +
      coord_flip()
    
    attainmentPlot
  })

  # PAGE 9 PLOTS: Labour & Economy#######################################
  # Unemployment Rate by Department
  output$ninthPage_national_ADM1Plot <- renderPlot({
    req(input$dropdown, imported_data)
    
    a <- imported_data %>%
      filter(Metric == "LabourForceStatus" & Type == "UnemploymentRate" & Level == "ADM1") %>%
      mutate(ADM1 = fct_reorder(ADM1, Value)) %>%
      ggplot(aes(x = reorder(ADM1, Value), y = Value)) +
      geom_bar(fill = "#f03b20", stat = "identity", width = 0.8) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      xlab("") +
      theme(legend.position = "none") +
      ylab(i18n$t("Unemployment Rate")) +
      theme_plot() +
      ggtitle(i18n$t("Unemployment Rate by Department, Guatemala 2018")) +
      coord_flip()
    
      if(input$dropdown != "All") {
        a <- a + gghighlight(ADM1 == input$dropdown)
        a
      } else {
        a
      }
  })
  
  # Population by Labour Force Status
  output$ninthPage_LabourStatus <- renderPlot({
    req(input$dropdown, dropdownFilteredData())
    
    if(input$dropdown == "All") {
      selectedLocation <- "Guatemala" 
    } else {
      selectedLocation <- input$dropdown
    }
    
    ninthPage_labourStatus_Title <- sprintf(
      "%s, %s 2018",
      i18n$t("Population by Labour Force Status"), selectedLocation) %>%
      lapply(htmltools::HTML)
    
    labourForceStatusPlot <- dropdownFilteredData() %>%
      dplyr::filter(Metric == "LabourForceStatus" & Type %in% c("Employed", "Unemployed", "Outside the labour force")) %>%
      ggplot(aes(x = reorder(secondaryText, Value), y = Value/100000, fill = secondaryText)) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      geom_bar(stat = "identity", fill = "#fd8d3c", show.legend = FALSE) +
      ggtitle(ninthPage_labourStatus_Title) +
      scale_x_discrete(labels = c("Employed" = i18n$t("Employed"),
                                  "Outside the labour force" = i18n$t("Outside the\nlabour force"),
                                  "Unemployed" = i18n$t("Unemployed"))) +
      ylab(i18n$t("People (in hundred thousands)")) +
      xlab("") +
      theme_plot() +
      coord_flip()
    
    labourForceStatusPlot
  })
  
  # Population by Employment Status
  output$ninthPage_EmploymentStatus1 <- renderPlot({
    req(input$dropdown, dropdownFilteredData())
    
    if(input$dropdown == "All") {
      selectedLocation <- "Guatemala" 
    } else {
      selectedLocation <- input$dropdown
    }
    
    ninthPage_employmentStatus1_Title <- sprintf(
      "%s, %s 2018",
      i18n$t("Population by Employment Status"), selectedLocation) %>%
      lapply(htmltools::HTML)
    
    employmentStatusPlot <- dropdownFilteredData() %>%
      dplyr::filter(Metric == "EmploymentStatus") %>%
      ggplot(aes(x = reorder(Type, Value), y = Value/10000, fill = Type)) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      geom_bar(stat = "identity", fill = "#fed976", show.legend = FALSE) +
      ggtitle(ninthPage_employmentStatus1_Title) +
      scale_x_discrete(labels = c("Persons not classifiable by status" = i18n$t("Persons not classifiable\nby status"),
                                  "Employees" = i18n$t("Employees"),
                                  "Self-employed" = i18n$t("Self-employed"))) +
      ylab(i18n$t("People (in ten thousands)")) +
      xlab("") +
      theme_plot() +
      coord_flip()
    
    employmentStatusPlot 
  })
  
  # Self-Employed Population by Employment Status
  output$ninthPage_EmploymentStatus2 <- renderPlot({
    req(input$dropdown, dropdownFilteredData())
    
    if(input$dropdown == "All") {
      selectedLocation <- "Guatemala" 
    } else {
      selectedLocation <- input$dropdown
    }
    
    ninthPage_employmentStatus2_Title <- sprintf(
      "%s, %s 2018",
      i18n$t("Self-Employed Population by Employment\nStatus"), selectedLocation) %>%
      lapply(htmltools::HTML)
    
    selfEmploymentStatusPlot <- dropdownFilteredData() %>%
      dplyr::filter(Metric == "SelfEmploymentStatus") %>%
      ggplot(aes(x = reorder(Type, Value), y = Value/10000, fill = Type)) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      geom_bar(stat = "identity", fill = "#bd0026", alpha = 0.8, show.legend = FALSE) +
      ggtitle(ninthPage_employmentStatus2_Title) +
      scale_x_discrete(labels = c("Own-account workers" = i18n$t("Own-account\nworkers"),
                                  "Employers" = i18n$t("Employers"),
                                  "Contributing family workers" = i18n$t("Contributing family\nworkers"),
                                  "Member of producers' cooperatives" = i18n$t("Member of producers'\ncooperatives"))) +
      ylab(i18n$t("People (in ten thousands)")) +
      xlab("") +
      theme_plot() +
      coord_flip()
    
    selfEmploymentStatusPlot 
  })
  
  # Population by Occupation
  output$ninthPage_Occupation <- renderPlot({
    req(input$dropdown, dropdownFilteredData())
    
    if(input$dropdown == "All") {
      selectedLocation <- "Guatemala" 
    } else {
      selectedLocation <- input$dropdown
    }
    
    ninthPage_occupation_Title <- sprintf(
      "%s, %s 2018",
      i18n$t("Population by Occupation"), selectedLocation) %>%
      lapply(htmltools::HTML)
    
    occupationPlot <- dropdownFilteredData() %>%
      dplyr::filter(Metric == "Occupation") %>%
      ggplot(aes(x = reorder(Type, Value), y = Value/10000)) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      geom_bar(stat = "identity", fill = "#ffffb2", width = 0.8) +
      ggtitle(ninthPage_occupation_Title) +
      scale_x_discrete(labels = c("0. Armed forces occupations" = i18n$t("Armed Forces"),
                                  "1. Managers" = i18n$t("Managers"),
                                  "2. Professionals" = i18n$t("Professionals"),
                                  "3. Technicians and associate professionals" = i18n$t("Technicians and associate\nprofessionals"),
                                  "4. Clerical support workers" = i18n$t("Clerical support\nworkers"),
                                  "5. Service and sales workers" = i18n$t("Service and\nsales workers"),
                                  "6. Skilled agricultural, forestry and fishery workers" = i18n$t("Skilled agricultural,\nforestry and fishery\nworkers"),
                                  "7. Craft and related trades workers" = i18n$t("Craft and related\ntrade workers"),
                                  "8. Plant and machine operators, and assemblers" = i18n$t("Plant and machine\noperators, and\nassemblers"),
                                  "9. Elementary occupations" = i18n$t("Elementary\noccupations"))) +
      ylab(i18n$t("People (in ten thousands)")) +
      xlab("") +
      theme_plot() +
      coord_flip()
     
    
    occupationPlot
  })
  
  # Population by Industry
  output$ninthPage_Industry <- renderPlot({
    req(input$dropdown, dropdownFilteredData())
    
    if(input$dropdown == "All") {
      selectedLocation <- "Guatemala" 
    } else {
      selectedLocation <- input$dropdown
    }
    
    ninthPage_industry_Title <- sprintf(
      "%s, %s 2018",
      i18n$t("Population by Industry"), selectedLocation) %>%
      lapply(htmltools::HTML)
    
    industryPlot <- dropdownFilteredData() %>%
      dplyr::filter(Metric == "Industry") %>%
      ggplot(aes(x = reorder(Type, Value), y = Value/10000)) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      geom_bar(stat = "identity", fill = "#fed976", width = 0.8) +
      ggtitle(ninthPage_industry_Title) +
      scale_x_discrete(labels = c("A. Agriculture, forestry and fishing" = i18n$t("Agriculture, forestry and fishing"),                                                                                        
                                  "B. Mining and quarrying" = i18n$t("Mining and quarrying"),                                                                                                     
                                  "C. Manufacturing" = i18n$t("Manufacturing"),                                                                                                            
                                  "D. Electricity, gas, steam and air conditioning supply" = i18n$t("Electricity, gas, steam and air conditioning supply"),                                                                  
                                  "E. Water supply; sewerage, waste management and remediation activities" = i18n$t("Water supply; sewerage, waste management and remediation activities"),                                                  
                                  "F. Construction"   = i18n$t("Construction"),                                                                                                          
                                  "G. Wholesale and retail trade; repair of motor vehicles and motorcycles"  = i18n$t("Wholesale and retail trade; repair of motor vehicles and motorcycles"),                                                 
                                  "H. Transportation and storage"   = i18n$t("Transportation and storage"),                                                                                            
                                  "I. Accommodation and food service activities"  = i18n$t("Accommodation and food service activities"),                                                                              
                                  "J. Information and communication" = i18n$t("Information and communication"),                                                                                        
                                  "K. Financial and insurance activities"   = i18n$t("Financial and insurance activities"),                                                                                    
                                  "L. Real estate activities"   = i18n$t("Real estate activities"),                                                                                                
                                  "M. Professional, scientific and technical activities"  = i18n$t("Professional, scientific and technical activities"),                                                                      
                                  "N. Administrative and support service activities" = i18n$t("Administrative and support service activities"),                                                                           
                                  "O. Public administration and defence; compulsory social security" = i18n$t("Public administration and defence; compulsory social security"),                                                           
                                  "P. Education"  = i18n$t("Education"),                                                                                                              
                                  "Q. Human health and social work activities" = i18n$t("Human health and social work activities"),                                                                                 
                                  "R. Arts, entertainment and recreation" = i18n$t("Arts, entertainment and recreation"),                                                                                      
                                  "S. Other service activities"   = i18n$t("Other service activities"),                                                                                              
                                  "T. Activities of households as employers; undifferentiated goods and services-producing activities of households for own use" = i18n$t("Activities of households as employers; undifferentiated goods and\nservices-producing activities of households for own use"),
                                  "U. Activities of extraterritorial organizations and bodies" = i18n$t("Activities of extraterritorial organizations and bodies"))) +
      ylab(i18n$t("People (in ten thousands)")) +
      xlab("") +
      theme_plot() +
      coord_flip() 
    
    industryPlot
  })
  
  # Participation in Own-Use Production of Goods by Departmen
  output$ninthPage_ParticipationOwnUseProductionofGoods <- renderPlot({
    req(input$dropdown, imported_data)

    a <- imported_data %>%
      filter(Metric == "ParticipationOwnUseProduction" & Level == "ADM1") %>%
      mutate(ADM1 = fct_reorder(ADM1, Value)) %>%
      ggplot(aes(x = reorder(ADM1, Value), y = Value/10000)) +
      geom_bar(fill = "#fd8d3c", stat = "identity", width = 0.8) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      xlab("") +
      theme(legend.position = "none") +
      ylab(i18n$t("People (in ten thousands)")) +
      theme_plot() +
      ggtitle(i18n$t("Participation in Own-Use Production of Goods by Department, Guatemala 2018")) +
      coord_flip()
    
    if(input$dropdown != "All") {
      a <- a + gghighlight(ADM1 == input$dropdown)
      a
    } else {
      a
    }
  })

  # PAGE 10 PLOTS: Housing######################################################
  # Housing Unit Count by Department
  output$tenthPage_national_ADM1Plot <- renderPlot({
    req(input$dropdown, imported_data)
    
    a <- imported_data %>%
      filter(Metric == "LivingQuarters" & Type == "Housing units" & Level == "ADM1") %>%
      mutate(ADM1 = fct_reorder(ADM1, Value)) %>%
      ggplot(aes(x = reorder(ADM1, Value), y = Value/100000)) +
      geom_bar(fill = "#41AB5D", stat = "identity", width = 0.8) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      xlab("") +
      theme(legend.position = "none") +
      ylab(i18n$t("Housing Units (in hundred thousands)")) +
      ggtitle(i18n$t("Housing Unit Count by Department, Guatemala 2018")) +
      coord_flip() +
      theme_plot()
    
    if(input$dropdown != "All") {
      a <- a + gghighlight(ADM1 == input$dropdown)
      a
    } else {
      a
    }
  })
  
  # Living Quarters Count by Type
  output$tenthPage_LQType <- renderPlot({
    req(dropdownFilteredData(), input$dropdown)
    
    if(input$dropdown == "All") {
      selectedLocation <- "Guatemala" 
    } else {
      selectedLocation <- input$dropdown
    }
    
    tenthPage_LQType_Title <- sprintf(
      "%s, %s 2018",
      i18n$t("Living Quarters Count by Type"), selectedLocation) %>%
      lapply(htmltools::HTML)
    
    livingQuartersTypePlot <- dropdownFilteredData() %>%
      dplyr::filter(Metric == "LivingQuarters" & Type %in% c("Housing units", "Collective living quarters")) %>%
      ggplot(aes(x = reorder(secondaryText, Value), y = Value/10000, fill = secondaryText)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      ggtitle(tenthPage_LQType_Title) +
      ylab(i18n$t("Living Quarters (in ten thousands)")) +
      xlab("") +
      theme_plot() +
      scale_fill_manual(values = c("#addd8e","#31a354")) +
      theme(legend.position = "none") +
      coord_flip()
    
    livingQuartersTypePlot 
  })
  
  # Housing Unit Count by Type
  output$tenthPage_HUType <- renderPlot({
    req(dropdownFilteredData(), input$dropdown)
    
    if(input$dropdown == "All") {
      selectedLocation <- "Guatemala" 
    } else {
      selectedLocation <- input$dropdown
    }
    
    tenthPage_HUType_Title <- sprintf(
      "%s, %s 2018",
      i18n$t("Housing Unit Count by Type"), selectedLocation) %>%
      lapply(htmltools::HTML)
    
    housingUnitTypePlot <- dropdownFilteredData() %>%
      dplyr::filter(Metric == "HousingUnits" & Type %in% c("Conventional dwellings", "Other housing units")) %>%
      ggplot(aes(x = reorder(secondaryText, Value), y = Value/1000, fill = secondaryText)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      ggtitle(tenthPage_HUType_Title) +
      ylab(i18n$t("Housing Units (in thousands)")) +
      xlab("") +
      theme_plot() +
      scale_fill_manual(values = c("#ffffcc", "#addd8e")) +
      theme(legend.position = "none") +
      coord_flip()
    
    housingUnitTypePlot
  })
  
  # Conventional Dwelling Count by Occupancy Status
  output$tenthPage_Occupancy <- renderPlot({
    req(dropdownFilteredData(), input$dropdown)
    
    if(input$dropdown == "All") {
      selectedLocation <- "Guatemala" 
    } else {
      selectedLocation <- input$dropdown
    }
    
    tenthPage_Occupancy_Title <- sprintf(
      "%s, %s 2018",
      i18n$t("Conventional Dwelling Count by Occupancy Status"), selectedLocation) %>%
      lapply(htmltools::HTML)
    
    occupancyPlot <- dropdownFilteredData() %>%
      dplyr::filter(Metric == "Occupancy" & Type %in% c("Occupied", "Vacant / not occupied")) %>%
      ggplot(aes(x = reorder(secondaryText, Value), y = Value/1000, fill = secondaryText)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      ggtitle(tenthPage_Occupancy_Title) +
      ylab(i18n$t("Conventional Dwellings (in thousands)")) +
      xlab("") +
      theme_plot() +
      scale_fill_manual(values = c("#78c679", "#006837")) +
      theme(legend.position = "none") +
      coord_flip()
    
    occupancyPlot
  })
  
  # Housing Unit Count by Ownership Status
  output$tenthPage_Ownership <- renderPlot({
    req(dropdownFilteredData(), input$dropdown)
    
    if(input$dropdown == "All") {
      selectedLocation <- "Guatemala" 
    } else {
      selectedLocation <- input$dropdown
    }
    
    tenthPage_Ownership_Title <- sprintf(
      "%s, %s 2018",
      i18n$t("Housing Unit Count by Ownership Status"), selectedLocation) %>%
      lapply(htmltools::HTML)
    
    ownershipPlot <- dropdownFilteredData() %>%
      dplyr::filter(Metric == "Ownership" & Type %in% c("Owner-occupied", "Non-owner-occupied")) %>%
      ggplot(aes(x = reorder(secondaryText, Value), y = Value/1000000, fill = secondaryText)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      ggtitle(tenthPage_Ownership_Title) +
      ylab(i18n$t("Housing Units (in millions)")) +
      xlab("") +
      theme_plot() +
      scale_fill_manual(values = c("#31a354","#78c679")) +
      theme(legend.position = "none") +
      coord_flip()
    
    ownershipPlot
  })
  
  # Housing Unit Count by Tenure Status
  output$tenthPage_Tenure <- renderPlot({
    req(dropdownFilteredData(), input$dropdown)
    
    if(input$dropdown == "All") {
      selectedLocation <- "Guatemala" 
    } else {
      selectedLocation <- input$dropdown
    }
    
    tenthPage_Tenure_Title <- sprintf(
      "%s, %s 2018",
      i18n$t("Housing Unit Count by Tenure Status"), selectedLocation) %>%
      lapply(htmltools::HTML)
    
    tenurePlot <- dropdownFilteredData() %>%
      dplyr::filter(Metric == "Tenure" & Type %in% c("Household owns housing unit", "Household rents all or a part of housing unit", "Household occupies housing unit partly free of rent", "Household occupies housing unit wholly free of rent", "Household occupies housing unit under some other arrangement")) %>%
      ggplot(aes(x = reorder(Type, Value), y = Value/1000)) +
      geom_bar(stat = "identity", fill = "#addd8e") +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      ggtitle(tenthPage_Tenure_Title) +
      ylab(i18n$t("Housing Units (in thousands)")) +
      xlab("") +
      theme_plot() +
      scale_x_discrete(labels = c("Household owns housing unit" = i18n$t("Household owns\nhousing unit"), 
                                  "Household rents all or a part of housing unit" = i18n$t("Household rents\nall or a part of\nhousing unit"),
                                  "Household occupies housing unit partly free of rent" = i18n$t("Household occupies\nhousing unit partly\nfree of rent"), 
                                  "Household occupies housing unit wholly free of rent" = i18n$t("Household occupies\nhousing unit wholly\nfree of rent"), 
                                  "Household occupies housing unit under some other arrangement" = i18n$t("Household occupies\nhousing unit under\nsome other arrangement"))) +
      theme(legend.position = "none") +
      coord_flip()
    
    tenurePlot
  })
  
  # Building Count by Type
  output$tenthPage_BuildingType1 <- renderPlot({
    req(dropdownFilteredData(), input$dropdown)
    
    if(input$dropdown == "All") {
      selectedLocation <- "Guatemala" 
    } else {
      selectedLocation <- input$dropdown
    }
    
    tenthPage_BuildingType1_Title <- sprintf(
      "%s, %s 2018",
      i18n$t("Building Count by Type"), selectedLocation) %>%
      lapply(htmltools::HTML)
    
    buildingType1Plot <- dropdownFilteredData() %>%
      dplyr::filter(Metric == "BuildingType1" & Type %in% c("Residential buildings", "Non-residential buildings")) %>%
      ggplot(aes(x = reorder(secondaryText, Value), y = Value/1000, fill = secondaryText)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      ggtitle(tenthPage_BuildingType1_Title) +
      ylab(i18n$t("Buildings (in thousands)")) +
      xlab("") +
      theme_plot() +
      scale_fill_manual(values = c("#ffffcc","#78c679")) +
      theme(legend.position = "none") +
      coord_flip()
    
    buildingType1Plot
  })
  
  # Residential Building Count by Type
  output$tenthPage_BuildingType2 <- renderPlot({
    req(dropdownFilteredData(), input$dropdown)
    
    if(input$dropdown == "All") {
      selectedLocation <- "Guatemala" 
    } else {
      selectedLocation <- input$dropdown
    }
    
    tenthPage_BuildingType2_Title <- sprintf(
      "%s, %s 2018",
      i18n$t("Residential Building Count by Type"), selectedLocation) %>%
      lapply(htmltools::HTML)
    
    buildingType2Plot <- dropdownFilteredData() %>%
      dplyr::filter(Metric == "BuildingType2" & Type %in% c("Buildings containing a single housing unit", "Buildings containing more than one housing unit", "Buildings for persons living in institutions", "Other residential buildings")) %>%
      ggplot(aes(x = reorder(Type, desc(Value)), y = Value/1000)) +
      geom_bar(stat = "identity", fill = "#addd8e") +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      ggtitle(tenthPage_BuildingType2_Title) +
      ylab(i18n$t("Buildings (in thousands)")) +
      xlab("") +
      theme_plot() +
      scale_x_discrete(labels = c("Buildings containing a single housing unit" = i18n$t("Buildings containing a\nsingle housing unit"), 
                                  "Buildings containing more than one housing unit" = i18n$t("Buildings containing more\nthan one housing unit"), 
                                  "Buildings for persons living in institutions" = i18n$t("Buildings for persons\nliving in institutions"), 
                                  "Other residential buildings" = i18n$t("Other residential\nbuildings"))) +
      coord_flip()
    
    buildingType2Plot
  })
  
  # Building Count by Outer Wall Material
  output$tenthPage_OuterWalls <- renderPlot({
    req(dropdownFilteredData(), input$dropdown)
    
    if(input$dropdown == "All") {
      selectedLocation <- "Guatemala" 
    } else {
      selectedLocation <- input$dropdown
    }
    
    tenthPage_OuterWalls_Title <- sprintf(
      "%s, %s 2018",
      i18n$t("Building Count by Outer Wall Material"), selectedLocation) %>%
      lapply(htmltools::HTML)
    
    outerWallsPlot <- dropdownFilteredData() %>%
      dplyr::filter(Metric == "OuterWalls" & Type %in% c("Burnt clay (bricks, blocks, panels), stone, concrete", "Unburnt clay, mud, earth", "Wood", "Bamboo", "Corrugated sheets", "Prefabricated units", "Other materials")) %>%
      ggplot(aes(x = reorder(Type, desc(Value)), y = Value/1000)) +
      geom_bar(stat = "identity", fill = "#31a354") +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      ggtitle(tenthPage_OuterWalls_Title) +
      ylab(i18n$t("Buildings (in thousands)")) +
      xlab("") +
      theme_plot() +
      scale_x_discrete(labels = c("Burnt clay (bricks, blocks, panels), stone, concrete" = i18n$t("Burnt clay (bricks,\nblocks, panels),\nstone, concrete"), 
                                 "Unburnt clay, mud, earth" = i18n$t("Unburnt clay,\nmud, earth"),
                                 "Wood" = i18n$t("Wood"), 
                                 "Bamboo" = i18n$t("Bamboo"), 
                                 "Corrugated sheets" = i18n$t("Corrugated sheets"), 
                                 "Prefabricated units" = i18n$t("Prefabricated units"), 
                                 "Other materials" = i18n$t("Other materials"))) +
      coord_flip()
    
    outerWallsPlot
  })
  
  # Housing Unit Count by Number of Rooms
  output$tenthPage_NumRooms <- renderPlot({
    req(dropdownFilteredData(), input$dropdown)
    
    if(input$dropdown == "All") {
      selectedLocation <- "Guatemala" 
    } else {
      selectedLocation <- input$dropdown
    }
    
    tenthPage_NumRooms_Title <- sprintf(
      "%s, %s 2018",
      i18n$t("Housing Unit Count by Number of Rooms"), selectedLocation) %>%
      lapply(htmltools::HTML)
    
    numRoomsPlot <- dropdownFilteredData() %>%
      dplyr::filter(Metric == "NumberofRooms" & Type %in% c("1 to 3", "4 to 6", "7 or more")) %>%
      ggplot(aes(x = secondaryText, y = Value/1000, fill = secondaryText)) +
      geom_bar(stat = "identity", show.legend = FALSE, fill = "#006837") +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      ggtitle(tenthPage_NumRooms_Title) +
      ylab(i18n$t("Housing Units (in thousands)")) +
      xlab("") +
      theme_plot() +
      coord_flip()
    
    numRoomsPlot
  })
  
  # Average People per Housing Unit Room by Department
  output$tenthPage_OccupancyDensityPeople <- renderPlot({
    req(input$dropdown, imported_data)
    
    a <- imported_data %>%
      filter(Metric == "OccupancyDensity" & Type == "People" & Level == "ADM1") %>%
      mutate(ADM1 = fct_reorder(ADM1, Value)) %>%
      ggplot(aes(x = reorder(ADM1, Value), y = Value)) +
      geom_bar(fill = "#ffffcc", stat = "identity", width = 0.8) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      xlab("") +
      theme(legend.position = "none") +
      ylab(i18n$t("Average People per Room")) +
      theme_plot() +
      ggtitle(i18n$t("Average People per Housing Unit Room by\nDepartment, Guatemala 2018")) +
      coord_flip()
    
    if(input$dropdown != "All") {
      a <- a  + gghighlight(ADM1 == input$dropdown)
      a
    } else {
      a
    }
  })
  
  # Percent of Living Quarters With More Than One Household by Department
  output$tenthPage_OccupancyDensityHH <- renderPlot({
    req(input$dropdown, imported_data)

    a <- imported_data %>%
      filter(Metric == "OccupancyDensity" & Type == "PercMT1Households" & Level == "ADM1") %>%
      mutate(ADM1 = fct_reorder(ADM1, Value)) %>%
      ggplot(aes(x = reorder(ADM1, Value), y = Value)) +
      geom_bar(fill = "#78c679", stat = "identity", width  = 0.8) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      xlab("") +
      theme(legend.position = "none") +
      ylab(i18n$t("Percent of Living Quarters")) +
      theme_plot() +
      ggtitle(i18n$t("Percent of Living Quarters With More Than One\nHousehold by Department, Guatemala 2018")) +
      coord_flip()
    
    if(input$dropdown != "All") {
      a <- a  + gghighlight(ADM1 == input$dropdown)
      a
    } else {
      a
    }
  })
  
  # PAGE 11 PLOTS: Water, Sanitation, & Hygiene#################################
  # Housing Unit Count by Water Supply System 
  output$eleventhPage_AllWater <- renderPlot({
    req(dropdownFilteredData(), input$dropdown)
    
    if(input$dropdown == "All") {
      selectedLocation <- "Guatemala" 
    } else {
      selectedLocation <- input$dropdown
    }
    
    eleventhPage_AllWater_Title <- sprintf(
      "%s, %s 2018",
      i18n$t("Housing Unit Count by Water Supply\nSystem"), selectedLocation) %>%
      lapply(htmltools::HTML)
    
    AllWaterPlot <- dropdownFilteredData() %>%
      dplyr::filter(Metric == "WaterSystem") %>%
      mutate(Type = fct_relevel(Type, "Other", "Piped water outside the unit, within 200 metres", "Piped water inside the unit")) %>%
      arrange(Type) %>%
      ggplot(aes(y = Value/1000000, x = Type)) +
      geom_bar(stat = "identity", fill = "#dadaeb") +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      xlab("") +
      ylab(i18n$t("Housing Units (in millions)")) +
      ggtitle(eleventhPage_AllWater_Title) +
      theme_plot() +
      coord_flip() +
      scale_x_discrete(
        labels = c("Piped water outside the unit, within 200 metres" = i18n$t("Piped water outside the unit,\nwithin 200 metres"),
                   "Piped water inside the unit" = i18n$t("Piped water inside the unit"),
                   "Other" = i18n$t("Other"))) +
      theme(legend.position = "none")
    
    AllWaterPlot
  })
  
  # Housing Unit Count by Drinking Water Supply System
  output$eleventhPage_DrinkingWater <- renderPlot({
    req(dropdownFilteredData(), input$dropdown)
    
    if(input$dropdown == "All") {
      selectedLocation <- "Guatemala" 
    } else {
      selectedLocation <- input$dropdown
    }
    
    eleventhPage_DrinkingWater_Title <- sprintf(
      "%s, %s 2018",
      i18n$t("Housing Unit Count by Drinking Water Supply\nSystem"), selectedLocation) %>%
      lapply(htmltools::HTML)
    
    DrinkingWaterPlot <- dropdownFilteredData() %>%
      dplyr::filter(Metric == "DrinkingWater") %>%
      mutate(Type = fct_relevel(Type, "Other", "Piped water outside the unit, within 200 metres", "Piped water inside the unit")) %>%
      arrange(Type) %>%
      ggplot(aes(y = Value/1000000, x = Type)) +
      geom_bar(stat = "identity", fill = "#9e9ac8") +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      xlab("") +
      ylab(i18n$t("Housing Units (in millions)")) +
      ggtitle(eleventhPage_DrinkingWater_Title) +
      theme_plot() +
      coord_flip() +
      scale_x_discrete(
        labels = c("Piped water outside the unit, within 200 metres" = i18n$t("Piped water outside the unit,\nwithin 200 metres"),
                   "Piped water inside the unit" = i18n$t("Piped water inside the unit"),
                   "Other" = i18n$t("Other")))
    
    DrinkingWaterPlot
  })
  
  # Housing Unit Count by Type of Toilet
  output$eleventhPage_Toilet <- renderPlot({
    req(dropdownFilteredData(), input$dropdown)
    
    if(input$dropdown == "All") {
      selectedLocation <- "Guatemala" 
    } else {
      selectedLocation <- input$dropdown
    }
    
    eleventhPage_Toilet_Title <- sprintf(
      "%s, %s 2018",
      i18n$t("Housing Unit Count by Type of Toilet"), selectedLocation) %>%
      lapply(htmltools::HTML)
    
    ToiletPlot <- dropdownFilteredData() %>%
      dplyr::filter(Metric == "ToiletType") %>%
      mutate(Type = fct_relevel(Type, "No toilet available", "Toilet outside housing unit", "Toilet within housing unit")) %>%
      arrange(Type) %>%
      ggplot(aes(y = Value/1000000, x = Type)) +
      geom_bar(stat = "identity", fill = "#dadaeb") +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      xlab("") +
      ylab(i18n$t("Housing Units (in millions)")) +
      ggtitle(eleventhPage_Toilet_Title) +
      theme_plot() +
      coord_flip() +
      scale_x_discrete(
        labels = c("No toilet available" = i18n$t("No toilet available"),
                   "Toilet outside housing unit" = i18n$t("Toilet outside\nhousing unit"),
                   "Toilet within housing unit" = i18n$t("Toilet within\nhousing unit")))
    
    ToiletPlot
  })
  
  # Housing Unit Count by Sewage Disposal System
  output$eleventhPage_Sewage <- renderPlot({
    req(dropdownFilteredData(), input$dropdown)
    
    if(input$dropdown == "All") {
      selectedLocation <- "Guatemala" 
    } else {
      selectedLocation <- input$dropdown
    }
    
    eleventhPage_Sewage_Title <- sprintf(
      "%s, %s 2018",
      i18n$t("Housing Unit Count by Sewage Disposal\nSystem"), selectedLocation) %>%
      lapply(htmltools::HTML)
    
    SewagePlot <- dropdownFilteredData() %>%
      dplyr::filter(Metric == "SewageDisposal") %>%
      mutate(Type = fct_relevel(Type, "Other", "No disposal system", "Empties into a piped system connected to an individual sewage disposal system", "Empties into a piped sewer system")) %>%
      arrange(Type) %>%
      ggplot(aes(y = Value/1000000, x = Type)) +
      geom_bar(stat = "identity", fill = "#9e9ac8") +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      xlab("") +
      ylab(i18n$t("Housing Units (in millions)")) +
      ggtitle(eleventhPage_Sewage_Title) +
      theme_plot() +
      coord_flip() +
      scale_x_discrete(
        labels = c("Other" = i18n$t("Other"),
                   "No disposal system" = i18n$t("No disposal system"),
                   "Empties into a piped system connected to an individual sewage disposal system" = i18n$t("Empties into a piped system\nconnected to an individual\nsewage disposal system"),
                   "Empties into a piped sewer system" = i18n$t("Empties into a piped sewer\nsystem")))
    
    SewagePlot
  })
  
  # Housing Unit Count by Solid Waste Disposal System
  output$eleventhPage_SolidWaste <- renderPlot({
    req(dropdownFilteredData(), input$dropdown)
    
    if(input$dropdown == "All") {
      selectedLocation <- "Guatemala" 
    } else {
      selectedLocation <- input$dropdown
    }
    
    eleventhPage_SolidWaste_Title <- sprintf(
      "%s, %s 2018",
      i18n$t("Housing Unit Count by Solid Waste Disposal\nSystem"), selectedLocation) %>%
      lapply(htmltools::HTML)
    
    SolidWastePlot <- dropdownFilteredData() %>%
      dplyr::filter(Metric == "SolidWasteDisposal") %>%
      ggplot(aes(y = Value/1000000, x = reorder(Type, Value))) +
      geom_bar(stat = "identity", fill = "#54278f") +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      xlab("") +
      ylab(i18n$t("Housing Units (in millions)")) +
      ggtitle(eleventhPage_SolidWaste_Title) +
      theme_plot() +
      coord_flip() +
      scale_x_discrete(
        labels = c("Occupants burn solid waste" = i18n$t("Occupants burn solid waste"),
                   "Occupants bury solid waste" = i18n$t("Occupants bury solid waste"),
                   "Occupants compost solid waste" = i18n$t("Occupants compost solid waste"),
                   "Occupants dispose of solid waste in a local dump not supervised by authorities" = i18n$t("Occupants dispose of solid waste\nin a local dump not supervised\nby authorities"),
                   "Occupants dispose of solid waste in a local dump supervised by authorities" = i18n$t("Occupants dispose of solid waste in\na local dump supervised by authorities"),
                   "Occupants dispose solid waste into river, sea, creek, pond" = i18n$t("Occupants dispose of solid waste into\nriver, sea, creek, pond"),
                   "Other arrangement" = i18n$t("Other arrangement"),
                   "Solid waste collected by self-appointed collectors" = i18n$t("Solid waste collected by\nself-appointed collectors"),
                   "Solid waste collected on a regular basis by authorized collectors" = i18n$t("Solid waste collected on a regular\nbasis by authorized collectors"),
                   "Solid waste collected on an irregular basis by authorized collectors" = i18n$t("Solid waste collected on an irregular\nbasis by authorized collectors")))
    
    SolidWastePlot
  })
  
  # Housing Unit Count by Hygiene System
  output$eleventhPage_Bathing <- renderPlot({
    req(dropdownFilteredData(), input$dropdown)
    
    if(input$dropdown == "All") {
      selectedLocation <- "Guatemala" 
    } else {
      selectedLocation <- input$dropdown
    }
    
    eleventhPage_Bathing_Title <- sprintf(
      "%s, %s 2018",
      i18n$t("Housing Unit Count by Hygiene System"), selectedLocation) %>%
      lapply(htmltools::HTML)
    
    BathingPlot <- dropdownFilteredData() %>%
      dplyr::filter(Metric == "BathingFacilities") %>%
      ggplot(aes(y = Value/1000000, x = reorder(Type, Value))) +
      geom_bar(stat = "identity", fill = "#dadaeb") +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      xlab("") +
      ylab(i18n$t("Housing Units (in millions)")) +
      ggtitle(eleventhPage_Bathing_Title) +
      theme_plot() +
      coord_flip() +
      scale_x_discrete(labels = c("With fixed bath or shower within housing unit" = i18n$t("With fixed bath or shower\nwithin housing unit"),
                                  "Without fixed bath or shower within housing unit" = i18n$t("Without fixed bath or shower\nwithin housing unit")))
    
    BathingPlot
  })
  
  # Housing Unit Count by Hygiene System, No Fixed Bath or Shower Within Housing Unit
  output$eleventhPage_Bathing1 <- renderPlot({
    req(dropdownFilteredData(), input$dropdown)
    
    if(input$dropdown == "All") {
      selectedLocation <- "Guatemala" 
    } else {
      selectedLocation <- input$dropdown
    }
    
    eleventhPage_Bathing1_Title <- sprintf(
      "%s, %s 2018",
      i18n$t("Housing Unit Count by Hygiene System, No Fixed Bath or Shower Within Housing Unit"), selectedLocation) %>%
      lapply(htmltools::HTML)
    
    BathingPlot1 <- dropdownFilteredData() %>%
      dplyr::filter(Metric == "BathingFacilitiesExpanded1") %>%
      mutate(Type = fct_reorder(Type, Value)) %>%
      ggplot(aes(y = Value/1000000, x = reorder(Type, Value), fill = reorder(Type, Value))) +
      geom_bar(stat = "identity", fill = "#9e9ac8") +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      xlab("") +
      ylab(i18n$t("Housing Units (in millions)")) +
      ggtitle(eleventhPage_Bathing1_Title) +
      theme_plot() +
      coord_flip() +
      scale_x_discrete(labels = c("Fixed bath or shower available outside housing unit" = i18n$t("Fixed bath or shower available\noutside housing unit"),
                                  "No fixed bath or shower available" = i18n$t("No fixed bath or shower available"))) +
      theme(legend.position = "none")
    
    BathingPlot1
  })
  
  # Housing Unit Count by Hygiene System, Fixed Bath or Shower is Outside Housing Unit
  output$eleventhPage_Bathing2 <- renderPlot({
    req(dropdownFilteredData(), input$dropdown)
    
    if(input$dropdown == "All") {
      selectedLocation <- "Guatemala" 
    } else {
      selectedLocation <- input$dropdown
    }
    
    eleventhPage_Bathing2_Title <- sprintf(
      "%s, %s 2018",
      i18n$t("Housing Unit Count by Hygiene System, Fixed Bath or Shower is Outside Housing Unit"), selectedLocation) %>%
      lapply(htmltools::HTML)
    
    BathingPlot2 <- dropdownFilteredData() %>%
      dplyr::filter(Metric == "BathingFacilitiesExpanded2") %>%
      ggplot(aes(y = Value/1000000, x = reorder(secondaryText, Value), fill = secondaryText)) +
      geom_bar(stat = "identity", fill = "#54278f", show.legend = FALSE) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      xlab("") +
      ylab(i18n$t("Housing Units (in millions)")) +
      ggtitle(eleventhPage_Bathing2_Title) +
      theme_plot() +
      coord_flip()
    
    BathingPlot2
  })
  
  # PAGE 12 PLOTS: Power & Technology###########################################
  # Most Common Cooking Fuel by Department
  output$twelfthPage_CookingADMPlot <- renderPlot({
    req(input$dropdown, imported_data_wide_all)
    
    if (input$dropdown == "All") {
      internal_df <- imported_data_wide_all %>%
        dplyr::filter(Level == "ADM1") %>%
        count(MCommCoFu) %>%
        arrange(desc(n))
    } else { 
      internal_df <- imported_data_wide_all %>%
        dplyr::filter(Level == "ADM2"& ADM1 == input$dropdown) %>%
        count(MCommCoFu) %>%
        arrange(desc(n))
    }

    if (input$dropdown == "All") {
      plotTitle <- i18n$t("Most Common Cooking Fuel by Department,\nGuatemala 2018")
    } else {
      plotTitle <- sprintf(
        "%s,\n%s 2018",
        i18n$t("Most Common Cooking Fuel by Municipality"), input$dropdown) %>%
        lapply(htmltools::HTML)
    }

    if (input$dropdown == "All") {
      plotXAxis <- i18n$t("1 square = 1 department")
    } else {
      plotXAxis <- i18n$t("1 square = 1 municipality")
    }
    
    ggplot(internal_df, aes(fill = MCommCoFu, values = n)) +
      geom_waffle(color = "white", size = 1.25, n_rows = 7, flip = TRUE) +
      scale_y_discrete() + 
      ggthemes::scale_fill_tableau(
        name=NULL,
        labels = c("Charcoal" = i18n$t("Charcoal"), 
                   "Electricity" = i18n$t("Electricity"), 
                   "Firewood" = i18n$t("Firewood"), 
                   "Gas" = i18n$t("Gas"), 
                   "Oil"= i18n$t("Oil"))) +
      coord_equal() +
      labs(
        title = plotTitle,
        xlab = plotXAxis
      ) +
      theme(
        
        plot.title.position = "plot",
        text = element_text(family = "Lato"),
        
        # background colors
        plot.background = element_rect(fill = "transparent",
                                       color = NA),
        panel.background = element_rect(fill = "transparent",
                                        color = NA),
        legend.background = element_rect(fill = "transparent",
                                         color = NA),
       # titles
        legend.title = element_blank(),
        legend.text = element_text(size = 10, 
                                   color = "black",
                                   face = "plain"),
        plot.title = element_text(size = 11, 
                                  color = "black",
                                  face = "bold"),
        axis.text.x = element_blank()
      )
    
  })
  
  # Percent of Households with Electricity by Department
  output$twelfthPage_ElectricADM1Plot <- renderPlot({
    req(input$dropdown, imported_data)
    
    plotTitle <- i18n$t("Percent of Households with Electricity by Department")
    
    a <- imported_data %>%
      filter(Level == "ADM1" & Metric == "PercElectric") %>%
      ggplot(aes(x = reorder(ADM1, Value), y = Value)) +
      geom_bar(fill = "#bd0026", stat = "identity", width = 0.8, alpha = 0.8) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      xlab("") +
      theme(legend.position = "none") +
      ylab(i18n$t("Households (Percent)")) +
      ggtitle(plotTitle) +
      theme_plot() +
      coord_flip()
    
      if(input$dropdown != "All") {
        a <- a  + gghighlight(ADM1 == input$dropdown)
        a
      } else {
        a
      }
  })
  
  # Percent of Households with Internet Access by Department
  output$twelfthPage_InternetADM1Plot <- renderPlot({
    req(input$dropdown, imported_data)
    
    plotTitle <- i18n$t("Percent of Households with Internet Access by Department")
      
    a <- imported_data %>%
      filter(Level == "ADM1" & Metric == "PercInternet") %>%
      ggplot(aes(x = reorder(ADM1, Value), y = Value)) +
      geom_bar(fill = "#bd0026", stat = "identity", width = 0.8, alpha = 0.8) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      xlab("") +
      theme(legend.position = "none") +
      ylab(i18n$t("Households (Percent)")) +
      ggtitle(plotTitle) +
      theme_plot() +
      coord_flip()
    
    if(input$dropdown != "All") {
      a <- a  + gghighlight(ADM1 == input$dropdown)
      a
    } else {
      a
    }
  })
  
  # Household Count by Main Cooking Fuel
  output$twelfthPage_cookingPlot <- renderPlot({
    req(dropdownFilteredData(), input$dropdown)
    
    if(input$dropdown == "All") {
      selectedLocation <- "Guatemala" 
    } else {
      selectedLocation <- input$dropdown
    }
    
    twelfthPage_cookingPlot_Title <- sprintf(
      "%s, %s 2018",
      i18n$t("Household Count by Main Cooking Fuel"), selectedLocation) %>%
      lapply(htmltools::HTML)
    
    cookingPlot <- dropdownFilteredData() %>%
      dplyr::filter(Metric == "CookingFuel") %>%
      ggplot(aes(x = reorder(secondaryText, Value), y = Value/10000, fill = secondaryText)) +
      geom_bar(stat = "identity", fill = "#fed976", show.legend = FALSE) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      xlab("") +
      ylab(i18n$t("Households (in ten thousands)")) +
      ggtitle(twelfthPage_cookingPlot_Title) +
      theme_plot() +
      coord_flip()
    
    cookingPlot
  })
  
  # Household Count by Main Lighting Fuel
  output$twelfthPage_lightingPlot <- renderPlot({
    req(dropdownFilteredData(), input$dropdown)
    
    if(input$dropdown == "All") {
      selectedLocation <- "Guatemala" 
    } else {
      selectedLocation <- input$dropdown
    }
    
    twelfthPage_lightingPlot_Title <- sprintf(
      "%s, %s 2018",
      i18n$t("Household Count by Main Lighting Fuel"), selectedLocation) %>%
      lapply(htmltools::HTML)
    
    lightingPlot <- dropdownFilteredData() %>%
      dplyr::filter(Metric == "LightingFuel") %>%
      ggplot(aes(x = reorder(secondaryText, Value), y = Value/10000, fill = secondaryText)) +
      geom_bar(stat = "identity", fill = "#fd8d3c", show.legend = FALSE) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      xlab("") +
      ylab(i18n$t("Households (in ten thousands)")) +
      ggtitle(twelfthPage_lightingPlot_Title) +
      theme_plot() +
      coord_flip()
    
    lightingPlot
  })
  
  # Household Count by ICT Asset Ownership
  output$twelfthPage_ICTPlot <- renderPlot({
    req(dropdownFilteredData(), input$dropdown)
    
    if(input$dropdown == "All") {
      selectedLocation <- "Guatemala" 
    } else {
      selectedLocation <- input$dropdown
    }
    
    twelfthPage_ICTPlot_Title <- sprintf(
      "%s, %s 2018",
      i18n$t("Household Count by ICT Asset Ownership"), selectedLocation) %>%
      lapply(htmltools::HTML)
    
    ICTPlot <- dropdownFilteredData() %>%
      dplyr::filter(Metric == "ICT") %>%
      ggplot(aes(x = reorder(secondaryText, Value), y = Value/1000, fill = secondaryText)) +
      geom_bar(stat = "identity", fill = "#fd8d3c", show.legend = FALSE) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      xlab("") +
      ylab(i18n$t("Households (in thousands)")) +
      ggtitle(twelfthPage_ICTPlot_Title) +
      theme_plot() +
      coord_flip()
    
    ICTPlot
  })

    
  #TRANSLATION##################################################################
  # The code below enables OSDS to translate from language to language with user 
  # selection from the dropdown menu in the header
  
  observeEvent(input$selected_language, {
    lang <- input$selected_language
    update_lang(session, lang)
    i18n$set_translation_language(lang)
    tr(i18n)
    r$currentLang <- lang
  })
  
  observe({
    req(tr())
    newOptions <- purrr::map(lang_options, ~ list(key = .x$key, text = tr()$t(.x$text)))
    updateDropdown.shinyInput(session = session, inputId = "selected_language", options = newOptions)
  })
  
  #DATA CHECKS##################################################################
  
  # observeEvent(input$intialize_ADM0, {
  #   if(file.exists("./www/GTM_adm0.shp")){
  #     output$ADM0Present <- renderText({
  #       "present"
  #     })
  #   } else {
  #     output$ADM0Present <- renderText({
  #       "not present"
  #     })
  #   }
  # })

  #DATA DOWNLOAD BY PAGE########################################################
  # The code below provides users with a data download on each page. These data
  # are used to build all visualizations. Thus, users can inspect the processed
  # census data NSOs provide to OSDS, effectively disseminating important pieces
  # in small, workable chunks.
  
  # PAGE 1 DATA DOWNLOAD: Demographic & Social##################################
  observeEvent(input$page1_dd,
               click("page1_ddb"))
  
  page1_data <- imported_data %>% 
    filter(Metric == "Population") %>% 
    dplyr::select(!c(Metric,secondaryText))
  
  output$page1_ddb <- downloadHandler(
    filename = function() {
      'demographic-social-data.csv' 
    },
    
    content = function(file) {
      write.csv(page1_data, file, row.names=F)
    }
  )
  
  # PAGE 2 DATA DOWNLOAD: Household & Family####################################
  observeEvent(input$page2_dd,
               click("page2_ddb"))
  
  page2_data <- imported_data %>% 
    filter(Metric %in% c("Housing Units","Household Size","HHTypes", "FamilyNucleus", "MaritalStatus", "RelationshipReferencePerson")) %>% 
   dplyr::select(!c(Sex,Age,secondaryText))
  
  output$page2_ddb <- downloadHandler(
    filename = function() {
      'household-family-data.csv' 
    },
    
    content = function(file) {
      write.csv(page2_data, file, row.names=F)
    }
  )
  
  # PAGE 3 DATA DOWNLOAD: Disability Status#####################################
  observeEvent(input$page3_dd,
               click("page3_ddb"))
  
  page3_data <- imported_data %>% 
    filter(Metric %in% c("Walking","Seeing","Hearing","Cognition","Selfcare","Communication")) %>% 
    dplyr::select(!c(Sex,UrbanRural,Age,secondaryText))
  
  output$page3_ddb <- downloadHandler(
    filename = function() {
      'disability-status-data.csv' 
    },
    
    content = function(file) {
      write.csv(page3_data, file, row.names=F)
    }
  )
  
  # PAGE 4 DATA DOWNLOAD: International Migration###############################
  observeEvent(input$page4_dd,
               click("page4_ddb"))
  
  page4_data <- imported_data %>% 
    filter(Metric %in% c("ForeignBorn","ForeignCitizenship","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021")) %>% 
    dplyr::select(!c(Sex,UrbanRural,Age,secondaryText))    
  
  output$page4_ddb <- downloadHandler(
    filename = function() {
      'international-migration-data.csv' 
    },
    
    content = function(file) {
      write.csv(page4_data, file, row.names=F)
    }
  )

  # PAGE 5 DATA DOWNLOAD: Domestic Migration####################################
  observeEvent(input$page5_dd,
               click("page5_ddb"))
  
  page5_data <- imported_data %>% 
    filter(Metric %in% c("DomMigrants")) %>% 
    dplyr::select(!c(Sex,UrbanRural,Age,Metric,secondaryText))
  
  output$page5_ddb <- downloadHandler(
    filename = function() {
      'domestic-migration-data.csv' 
    },
    
    content = function(file) {
      write.csv(page5_data, file, row.names=F)
    }
  )
  # PAGE 6 DATA DOWNLOAD: Mortality#############################################
  observeEvent(input$page6_dd,
               click("page6_ddb"))

  page6_data <- imported_data %>% 
    filter(Metric %in% c("CrudeDeathRate","Deaths")) %>% 
    dplyr::select(!c(UrbanRural,secondaryText))

  output$page6_ddb <- downloadHandler(
    filename = function() {
      'mortality-data.csv'
    },

    content = function(file) {
      write.csv(page6_data, file, row.names=F)
    }
  )
  
  # PAGE 7 DATA DOWNLOAD: Fertility#############################################  
  observeEvent(input$page7_dd,
               click("page7_ddb"))
  
  page7_data <- imported_data %>% 
    filter(Metric %in% c("CrudeBirthRate","Births")) %>% 
    dplyr::select(!c(Sex,UrbanRural,Age,Type,secondaryText))
  
  output$page7_ddb <- downloadHandler(
    filename = function() {
      'fertility-data.csv' 
    },
    
    content = function(file) {
      write.csv(page7_data, file, row.names=F)
    }
  )
  
  # PAGE 8 DATA DOWNLOAD: Education#############################################
  observeEvent(input$page8_dd,
               click("page8_ddb"))
  
  page8_data <- imported_data %>% 
    filter(Metric %in% c("AdultLiteracyRate", "YouthLiteracyRate", "SchoolAttendance", "EducationalAttainment")) %>% 
    dplyr::select(!c(UrbanRural,Age,secondaryText))
  
  output$page8_ddb <- downloadHandler(
    filename = function() {
      'education-data.csv' 
    },
    
    content = function(file) {
      write.csv(page8_data, file, row.names=F)
    }
  )
  # PAGE 9 DATA DOWNLOAD: Labour & Economy######################################
  observeEvent(input$page9_dd,
               click("page9_ddb"))
  
  page9_data <- imported_data %>% 
    filter(Metric %in% c("LabourForceStatus", "EmploymentStatus", "SelfEmploymentStatus", "Occupation", "Industry", "ParticipationOwnUseProduction")) %>% 
    dplyr::select(!c(Sex,UrbanRural,Age,secondaryText))
  
  output$page9_ddb <- downloadHandler(
    filename = function() {
      'labour-economy-data.csv' 
    },
    
    content = function(file) {
      write.csv(page9_data, file, row.names=F)
    }
  )
  # PAGE 10 DATA DOWNLOAD: Housing##############################################
  observeEvent(input$page10_dd,
               click("page10_ddb"))
  
  page10_data <- imported_data %>% 
    filter(Metric %in% c("LivingQuarters", "HousingUnits", "Occupancy", "Ownership", "Tenure", 
                                                        "BuildingType1", "BuildingType2", "OuterWalls", "NumberofRooms", "OccupancyDensity")) %>% 
    dplyr::select(!c(Sex,UrbanRural,Age,secondaryText))   
  
  output$page10_ddb <- downloadHandler(
    filename = function() {
      'housing-data.csv' 
    },
    
    content = function(file) {
      write.csv(page10_data, file, row.names=F)
    }
  )
  
  # PAGE 11 DATA DOWNLOAD: Water, Sanitation, & Hygiene#########################
  observeEvent(input$page11_dd,
               click("page11_ddb"))
  
  page11_data <- imported_data %>% 
    filter(Metric %in% c("WaterSystem", "DrinkingWater", "ToiletType", "SewageDisposal", "SolidWasteDisposal", 
                                                        "BathingFacilities", "BathingFacilitiesExpanded1", "BathingFacilitiesExpanded2")) %>% 
    dplyr::select(!c(Sex,UrbanRural,Age,secondaryText))
  
  output$page11_ddb <- downloadHandler(
    filename = function() {
      'water-sanitation-hygiene-data.csv' 
    },
    
    content = function(file) {
      write.csv(page11_data, file, row.names=F)
    }
  )
  
  # PAGE 12 DATA DOWNLOAD: Power & Technology###################################
  observeEvent(input$page12_dd,
               click("page12_ddb"))
  
  page12_data <- imported_data %>% 
    filter(Metric %in% c("CookingFuel","LightingFuel","ICT")) %>% 
    dplyr::select(!c(Sex,UrbanRural,Age,secondaryText))
  
  output$page12_ddb <- downloadHandler(
    filename = function() {
      'power-technology-data.csv' 
    },
    
    content = function(file) {
      write.csv(page12_data, file, row.names=F)
    }
  )
}


################################################################################

shinyApp(ui, server)
