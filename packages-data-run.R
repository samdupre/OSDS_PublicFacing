####### LOAD ALL DATA, PACKAGES, FUNCTIONS, and OPTIONS NECESSARY FOR OSDS TO FUNCTION #######
#PACKAGES#####################################################################################################################
library(shiny.fluent) #MS Fluent UI wrapper
library(shiny) #base
library(tidyverse) #reshaping, etc.
library(glue) #stringing together commands with glue()
library(leaflet) #create maps
library(leaflet.extras) #setMapWidgetStyle()
library(sf) #shapefile processing
library(shiny.i18n) #internationalization
library(shiny.router) #page control
#library(raster) #GADM shapefile pull, among others
library(shinyjs) #for JS functionality including the file uploads. Not used for now, but will be once this is transitioned.
library(htmltools) #for label popups
library(gghighlight) #for selection highlighting in ggplot
library(waffle) #for waffleplots. Requires v. 1.0+. If only 0.7.0 installs, use: install.packages("waffle", repos = "https://cinc.rud.is")
library(ggplot2)
library(showtext)
library(scales)
library(ggpol)


#LOAD THE DATA##################################################################################################################
imported_data <- readRDS("data/app-data/census.RDS")
imported_data_wide_all <- readRDS("data/app-data/census-wide.RDS")
imported_migration_data <- readRDS("data/app-data/migration-flows.RDS")
ADM0_data <- readRDS("data/app-data/ADM0-data.RDS")
ADM1_data <- readRDS("data/app-data/ADM1-data.RDS")
ADM2_data <- readRDS("data/app-data/ADM2-data.RDS")

#Spatial data
# ADM0 census data
sf_ADM_0 <- readRDS("data/app-data/ADM0.RDS")
# ADM1 census data
sf_ADM1_p <- readRDS("data/app-data/ADM1-polyline.RDS")
sf_ADM_1 <- readRDS("data/app-data/ADM1.RDS")
# ADM2 census data
sf_ADM_2 <- readRDS("data/app-data/ADM2.RDS")
# population density data
sf_pop_dots <- readRDS("./data/app-data/sf_pop_dots.RDS")
pop_dots_data <- readRDS("./data/app-data/pop_dots_data.RDS")

#OPTIONS#########################################################################################################################
#DROPDOWN NAMES, ADM1s###################################################
ADM1s <- list(
  list(key = "All", text = "Total"),
  list(key = "Alta Verapaz", text = "Alta Verapaz"),
  list(key = "Baja Verapaz", text = "Baja Verapaz"),
  list(key = "Chimaltenango", text = "Chimaltenango"),
  list(key = "Chiquimula", text = "Chiquimula"),
  list(key = "El Progreso", text = "El Progreso"),
  list(key = "Escuintla", text = "Escuintla"),
  list(key = "Guatemala", text = "Guatemala"),
  list(key = "Huehuetenango", text = "Huehuetenango"),
  list(key = "Izabal", text = "Izabal"),
  list(key = "Jalapa", text = "Jalapa"),
  list(key = "Jutiapa", text = "Jutiapa"),
  list(key = "Petén", text = "Petén"),
  list(key = "Quetzaltenango", text = "Quetzaltenango"),
  list(key = "Quiché", text = "Quiché"),
  list(key = "Retalhuleu", text = "Retalhuleu"),
  list(key = "Sacatepéquez", text = "Sacatepéquez"),
  list(key = "San Marcos", text = "San Marcos"),
  list(key = "Santa Rosa", text = "Santa Rosa"),
  list(key = "Sololá", text = "Sololá"),
  list(key = "Suchitepéquez", text = "Suchitepéquez"),
  list(key = "Totonicapán", text = "Totonicapán"),
  list(key = "Zacapa", text = "Zacapa")
)

#INTERNATIONALIZATION#####################################################
i18n <- Translator$new(translation_json_path='translations/translation.json')

i18n$set_translation_language('English')

lang_options <- list(
  list(key = "English", text = "English"),
  list(key = "Spanish", text = "Spanish")
)

#NSO LOGO PLACEHOLDER#####################################################
logo <- png::readPNG("./logo-placeholder.png")

#GOOGLE FONT##############################################################
font_add_google(name = "Lato", family = "Lato", regular.wt = 400, bold.wt = 700)
showtext_auto()

#SUPRESS SCIENTIFIC NOTATION##############################################
options(scipen = 100000000)

#SOURCE BUILT FUNCTIONS USED IN OSDS######################################
source("functions.R")
