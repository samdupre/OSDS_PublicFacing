####### FUNCTIONS USED IN OSDS #######
##PRE-PROCESSING###############################################################
preprocessing <- function(...){
  
  # LOAD PACKAGES
  library(readxl)
  
  # LOAD THE DATA
  imported_data <- read_excel("data/census.xlsx", sheet = "Population") %>%
    mutate(secondaryText = Type) %>% 
    dplyr::select(!c(ADM2, OSDS_Join_Code,AgeOld,WealthQuintile))

  saveRDS(imported_data, "data/app-data/census.RDS")

  imported_data_wide_all <- read_excel("data/census-wide.xlsx", sheet = "Population") %>% 
    mutate(ADM1 = ifelse(ADM1 == "Quezaltenango", "Quetzaltenango", ADM1))
  saveRDS(imported_data_wide_all, "data/app-data/census-wide.RDS")

  imported_migration_data <- read_excel("data/census-migration.xlsx", sheet = "Place1YearAgo") %>% 
    dplyr::select(!c(AlphaJoinC, Method, Type))
  saveRDS(imported_migration_data, "data/app-data/migration-flows.RDS")

  ADM0_data <- imported_data_wide_all %>% filter(Level == "ADM0")
  saveRDS(ADM0_data, "data/app-data/ADM0-data.RDS")
  ADM1_data <- imported_data_wide_all %>% filter(Level == "ADM1")
  saveRDS(ADM1_data, "data/app-data/ADM1-data.RDS")
  ADM2_data <- imported_data_wide_all %>% filter(Level == "ADM2")
  saveRDS(ADM2_data, "data/app-data/ADM2-data.RDS")

  # pull in spatial data as sf() objects and merge with country data, above
  # ADM0
  sf0 <- st_as_sf(st_read("./www/ADM0.shp"), "Spatial")
  st_crs(sf0) <- 4326
  sf_ADM_0 <- merge(sf0, ADM0_data)
  saveRDS(sf_ADM_0, "data/app-data/ADM0.RDS")

  # ADM1 + fix typos
  sf1 <- st_as_sf(st_read("./www/ADM1.shp"), "Spatial")
  st_crs(sf1) <- 4326
  sf_ADM_1 <- sf1 %>% dplyr::select(NAME_1) %>% mutate(NAME_1 = ifelse(NAME_1 == "Quezaltenango", "Quetzaltenango", NAME_1))
  colnames(sf_ADM_1)[1] <- "ADM1"
  saveRDS(sf_ADM_1, "data/app-data/ADM1-polyline.RDS")
  sf_ADM_1 <- merge(sf_ADM_1, ADM1_data, all = TRUE)
  saveRDS(sf_ADM_1, "data/app-data/ADM1.RDS")

  # ADM2 + fix typos
  sf2 <- st_as_sf(st_read("./www/ADM2.shp"), "Spatial")
  st_crs(sf2) <- 4326
  sf_ADM_2 <- sf2 %>% dplyr::select(NAME_1,NAME_2) %>% mutate(NAME_1 = ifelse(NAME_1 == "Quezaltenango", "Quetzaltenango", NAME_1))
  colnames(sf_ADM_2)[1] <- "ADM1"
  colnames(sf_ADM_2)[2] <- "ADM2"
  sf_ADM_2 <- merge(sf_ADM_2, ADM2_data, all = TRUE)
  saveRDS(sf_ADM_2, "data/app-data/ADM2.RDS")

  #DOT DENSITY - POPULATION###################################################
    random_round <- function(x) {
      v=as.integer(x)
      r=x-v
      test=runif(length(r), 0.0, 1.0)
      add=rep(as.integer(0),length(r))
      add[r>test] <- as.integer(1)
      value=v+add
      ifelse(is.na(value) | value<0,0,value)
      return(value)
    }
    
    # Rename object for manipulation
    sf2.dots <- sf_ADM_2
    
    # Population/1000 in each ADM2
    num_dots_entire <- sf2.dots %>% 
      dplyr::select(Pop) %>% 
      mutate(Pop = (Pop / 1000),
             Pop = random_round(Pop))
    
    # One dot = 500 people
    sf_dots <- map_df(names(num_dots_entire), 
                      ~ st_sample(sf2.dots, size = num_dots_entire$Pop, type = "random") %>% # generate the points in each polygon
                        st_cast("POINT") %>%                                          # cast the geom set as 'POINT' data
                        st_coordinates() %>%                                          # pull out coordinates into a matrix
                        as_tibble() %>%                                               # convert to tibble
                        setNames(c("lon","lat")) %>%                                  # set column names
                        mutate(Dissaggregation = .x)                                            # add categorical party variable
    ) %>% 
      slice(sample(1:n())) # once map_df binds rows randomise order to avoid bias in plotting order
    
    # Save the data as both aspatial and spatial objects for use in the app
    saveRDS(sf_dots, "./data/app-data/pop_dots_data.RDS")
    sf_pop_dots <- st_as_sf(sf_dots, coords = c("lon", "lat"), crs = 4326)
    saveRDS(sf_pop_dots, "./data/app-data/sf_pop_dots.RDS")
}


#SPECIFY DECIMAL PLACES###################################################
decimal <- function(x,k) trimws(format(round(x,k), nsmall =k))

#PLOT THEME###############################################################
theme_plot <- function(...) {
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
    plot.subtitle = element_text(size = 11, 
                                 color = "black"),
    axis.title.x = element_text(size = 10, 
                                color = "black"),
    axis.title.y = element_text(size = 10, 
                                color = "black"),
    axis.text.x = element_text(size = 8, 
                               color = "black",
                               face = "plain"),
    axis.text.y = element_text(size = 8, 
                               color = "black",
                               face = "plain"),
    ...
  )
}

#LEAFLET LEGEND ORDER##########################################################
addLegend_decreasing <- function(map, position = c("topright", "bottomright", "bottomleft", 
                                                   "topleft"), pal, values, na.label = "NA", bins = 7, colors, 
                                 opacity = 0.5, labels = NULL, labFormat = labelFormat(), 
                                 title = NULL, className = "info legend", layerId = NULL, 
                                 group = NULL, data = getMapData(map), decreasing = FALSE) {
  position <- match.arg(position)
  type <- "unknown"
  na.color <- NULL
  extra <- NULL
  if (!missing(pal)) {
    if (!missing(colors)) 
      stop("You must provide either 'pal' or 'colors' (not both)")
    if (missing(title) && inherits(values, "formula")) 
      title <- deparse(values[[2]])
    values <- evalFormula(values, data)
    type <- attr(pal, "colorType", exact = TRUE)
    args <- attr(pal, "colorArgs", exact = TRUE)
    na.color <- args$na.color
    if (!is.null(na.color) && col2rgb(na.color, alpha = TRUE)[[4]] == 
        0) {
      na.color <- NULL
    }
    if (type != "numeric" && !missing(bins)) 
      warning("'bins' is ignored because the palette type is not numeric")
    if (type == "numeric") {
      cuts <- if (length(bins) == 1) 
        pretty(values, bins)
      else bins	
      
      if (length(bins) > 2) 
        if (!all(abs(diff(bins, differences = 2)) <= 
                 sqrt(.Machine$double.eps))) 
          stop("The vector of breaks 'bins' must be equally spaced")
      n <- length(cuts)
      r <- range(values, na.rm = TRUE)
      cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
      n <- length(cuts)
      p <- (cuts - r[1])/(r[2] - r[1])
      extra <- list(p_1 = p[1], p_n = p[n])
      p <- c("", paste0(100 * p, "%"), "")
      if (decreasing == TRUE){
        colors <- pal(rev(c(r[1], cuts, r[2])))
        labels <- rev(labFormat(type = "numeric", cuts))
      }else{
        colors <- pal(c(r[1], cuts, r[2]))
        labels <- rev(labFormat(type = "numeric", cuts))
      }
      colors <- paste(colors, p, sep = " ", collapse = ", ")
      
    }
    else if (type == "bin") {
      cuts <- args$bins
      n <- length(cuts)
      mids <- (cuts[-1] + cuts[-n])/2
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "bin", cuts))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "bin", cuts)
      }
      
    }
    else if (type == "quantile") {
      p <- args$probs
      n <- length(p)
      cuts <- quantile(values, probs = p, na.rm = TRUE)
      mids <- quantile(values, probs = (p[-1] + p[-n])/2, 
                       na.rm = TRUE)
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "quantile", cuts, p))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "quantile", cuts, p)
      }
    }
    else if (type == "factor") {
      v <- sort(unique(na.omit(values)))
      colors <- pal(v)
      labels <- labFormat(type = "factor", v)
      if (decreasing == TRUE){
        colors <- pal(rev(v))
        labels <- rev(labFormat(type = "factor", v))
      }else{
        colors <- pal(v)
        labels <- labFormat(type = "factor", v)
      }
    }
    else stop("Palette function not supported")
    if (!any(is.na(values))) 
      na.color <- NULL
  }
  else {
    if (length(colors) != length(labels)) 
      stop("'colors' and 'labels' must be of the same length")
  }
  legend <- list(colors = I(unname(colors)), labels = I(unname(labels)), 
                 na_color = na.color, na_label = na.label, opacity = opacity, 
                 position = position, type = type, title = title, extra = extra, 
                 layerId = layerId, className = className, group = group)
  invokeMethod(map, data, "addLegend", legend)
}


gadm_shapefile_pull <- function(your_country, level_needed) {
  st_as_sf(raster::getData("GADM", country = your_country, level = level_needed))
}
