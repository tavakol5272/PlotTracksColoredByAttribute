library(shiny)
library(move2)
library(sf)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(pals)
library(colourpicker)
library(shinycssloaders)
library(htmlwidgets)
library(webshot2)
library(zip)
library(shinybusy)
library(grDevices)
library(htmltools)

my_data <- readRDS("./data/raw/input4_move2loc_LatLon.rds")
#my_data <- readRDS("./data/raw/input2_move2loc_Mollweide.rds")



#transfer to WGS84: standard GPS coordinate system if it is not

if (!sf::st_is_longlat(my_data)) {
  my_data <- sf::st_transform(my_data, 4326)
}

########### helpers ###########

## helper 1: attribute type
continuous_attr <- function(vals, threshold = 12) {
  is_num <- is.numeric(vals) || inherits(vals, "units")
  if (!is_num) return(FALSE)  # categorical 
  n_unique <- length(unique(stats::na.omit(as.numeric(vals))))
  n_unique > threshold        # continuous
}

## helper2: making segments with two attributes

make_segments_2attr <- function(tracks, cat_name, cont_name) {
  if (nrow(tracks) < 2) {
    return(sf::st_sf(track = character(0),
                     cat   = character(0),
                     cont  = numeric(0),
                     geometry = sf::st_sfc(crs = sf::st_crs(tracks))))
  }
  segs <- mt_segments(tracks)
  id   <- as.character(mt_track_id(tracks))
  dd   <- sf::st_drop_geometry(tracks)
  
  catv <- as.character(dd[[cat_name]])
  conv <- if (inherits(dd[[cont_name]], "units")) units::drop_units(dd[[cont_name]]) else as.numeric(dd[[cont_name]])
  
  same_track_next <- c(id[-length(id)] == id[-1], FALSE)
  if (!any(same_track_next)) {
    return(sf::st_sf(track = character(0),
                     cat   = character(0),
                     cont  = numeric(0),
                     geometry = sf::st_sfc(crs = sf::st_crs(tracks))))
  }
  
  seg_cat  <- catv[same_track_next]
  v        <- conv
  seg_cont <- (v[same_track_next] + v[which(same_track_next) + 1]) / 2
  seg_track <- id[which(same_track_next)]
  sf::st_sf(track = seg_track, cat = seg_cat, cont = seg_cont, geometry = segs[same_track_next])
}

## helper 3: selecting base map
base_map_fun <- function(map, basemap) {
  if (identical(basemap, "TopoMap")) {
    addProviderTiles(map, "Esri.WorldTopoMap")
  } else if (identical(basemap, "Aerial")) {
    addProviderTiles(map, "Esri.WorldImagery")
  } else {
    addTiles(map)
  }
}

# helper 5:  generate HCL colors
color_generator <- function(pal, n, step = NULL) {
  if (n <= 0) return(character(0))
  m <- length(pal)
  if (m == 0 || n > m) {
    golden <- 137.50776405003785
    hues   <- ((0:(n - 1)) * golden) %% 360
    return(hcl(h = hues, c = 65, l = 60))
  }
  if (is.null(step)) step <- max(3L, as.integer(round(m / 4)))
  step <- max(1L, as.integer(step))
  idx  <- ((0:(n - 1)) * step) %% m + 1L
  pal[idx]
}

####### UI 
ui <- fluidPage(
  titlePanel("Tracks colored by two attributes"),
  
  sidebarLayout(
    sidebarPanel(width = 4,
                 h4("Animals"),
                 checkboxGroupInput("animals", NULL, choices = NULL),
                 fluidRow(
                   column(6, actionButton("select_all_animals", "Select All Animals", class = "btn-sm")),
                   column(6, actionButton("unselect_animals", "Unselect All Animals", class = "btn-sm"))
                 ),
                 
                 h4("Display"),
                 radioButtons("panel_mode", NULL,
                              choices = c("Single panel","Multipanel"),
                              selected = "Single panel", inline = TRUE),
                 
                 h4("Base map"),
                 radioButtons("basemap", NULL,
                              choices  = c("OpenStreetMap", "TopoMap", "Aerial"),
                              selected = "OpenStreetMap", inline = TRUE),
                 hr(),
                 
                 h4("Attributes"),
                 fluidRow(
                   column(6, selectInput("cat_attr","Categorical Attribute", choices = NULL)),
                   column(6, selectInput("cat_pal", "Palette",
                                         choices  = c("Glasbey","Set2","Set3","Dark2","Paired","Accent"),
                                         selected = "Glasbey"))
                 ),
                 
                 fluidRow(
                   column(6, selectInput("cont_attr", "Continuous Attribute", choices = NULL)),
                   column(6, selectInput("cont_pal", "Shade",
                                         choices  = c("Light to Dark","Dark to Light"),
                                         selected = "Light to Dark"))
                 ),
                 h6("Note: Numeric attributes with fewer than 12 unique values are considered as categorical."),
                 
                 hr(),
                 h4("Style"),
                 fluidRow(
                   column(6, numericInput("linesize_att", "Line width", 3, min = 1, max = 10, step = 1)),
                   column(6, sliderInput("linealpha_att", "Transparency", min = 0, max = 1, value = 0.9, step = 0.05))
                 ),
                 
                 hr(),
                 actionButton("apply_btn", "Apply Changes", class = "btn-primary btn-block"),
                 hr(),
                 
                 h4("Download:"),
                 fluidRow(
                   column(6, downloadButton("save_html","Download as HTML", class = "btn-sm")),
                   column(6, downloadButton("save_png", "Save Map as PNG", class = "btn-sm"))
                 )
    ),
    mainPanel(uiOutput("maps_ui"))
  )
)

### server 
server <- function(input, output, session) {
  
  # Locked so that only change on clicking on button
  locked_settings <- reactiveVal(NULL)
  locked_mv       <- reactiveVal(NULL)
  
  # keep tracks with at least 2 
  mv_all <- reactive({
    my_data %>%
      arrange(mt_track_id(), mt_time()) %>%
      { .[!duplicated(data.frame(id = mt_track_id(.), t = mt_time(.))), ] } %>%
      group_by(track = mt_track_id()) %>%
      filter(n() >= 2) %>%
      ungroup()
  })
  
  observeEvent(input$select_all_animals, {
    ids <- as.character(unique(mt_track_id(mv_all())))
    updateCheckboxGroupInput(session, "animals", selected = ids)
  })
  observeEvent(input$unselect_animals, {
    updateCheckboxGroupInput(session, "animals", selected = character(0))
  })
  observe({
    ids <- as.character(unique(mt_track_id(mv_all())))
    updateCheckboxGroupInput(session, "animals", choices = ids, selected = ids)
  })
  
  #  attribute selectors 
  observe({
    dd <- sf::st_drop_geometry(mv_all()) |> as.data.frame()
    keep <- colSums(!is.na(dd)) > 0
    keep <- keep & !sapply(dd, inherits, what = "POSIXt")
    keep <- keep & (sapply(dd, class) != "Date")
    if (!any(keep)) keep[["track"]] <- TRUE
    
    is_cont_col <- sapply(names(dd), function(nm) {
      vals <- dd[[nm]]
      continuous_attr(vals, threshold = 12)
    })
    
    cat_cols   <- names(dd)[keep & !is_cont_col]  
    cont_cols  <- names(dd)[keep &  is_cont_col]  
    
    updateSelectInput(session, "cat_attr",  choices = cat_cols,  selected = if (length(cat_cols))  cat_cols[1]  else NULL)
    updateSelectInput(session, "cont_attr", choices = cont_cols, selected = if (length(cont_cols)) cont_cols[1] else NULL)
  })
  
  # live selection
  mv_sel <- reactive({
    mv <- mv_all()
    sel <- input$animals
    if (is.null(sel) || length(sel) == 0) return(mv[0, ])
    mv[as.character(mt_track_id(mv)) %in% sel, ] %>%
      arrange(mt_track_id(), mt_time())
  })
  
  # first time shows map 
  observe({
    mv <- mv_sel()
    if (!is.null(input$cat_attr) && !is.null(input$cont_attr) &&
        nrow(mv) > 0 &&
        is.null(locked_mv()) && is.null(locked_settings())) {
      locked_mv(mv)
      locked_settings(list(
        animals   = input$animals,
        panel_mode= input$panel_mode,
        basemap   = input$basemap,
        cat_attr  = input$cat_attr,
        cont_attr = input$cont_attr,
        cat_pal   = input$cat_pal,
        cont_pal  = input$cont_pal,  
        linesize  = input$linesize_att,
        linealpha = input$linealpha_att
      ))
    }
  })
  
  # update locked state when Apply button is clicked
  observeEvent(input$apply_btn, {
    locked_mv(mv_sel())
    locked_settings(list(
      animals   = input$animals,
      panel_mode= input$panel_mode,
      basemap   = input$basemap,
      cat_attr  = input$cat_attr,
      cont_attr = input$cont_attr,
      cat_pal   = input$cat_pal,
      cont_pal  = input$cont_pal,
      linesize  = input$linesize_att,
      linealpha = input$linealpha_att
    ))
  }, ignoreInit = TRUE)
  
  # segments for locked selection (two attributes)
  segs_all <- reactive({
    s  <- locked_settings()
    mv <- locked_mv()
    req(s, mv, s$cat_attr, s$cont_attr)
    segs <- make_segments_2attr(mv, s$cat_attr, s$cont_attr)
    validate(need(nrow(segs) > 0, "No segments for selected animals."))
    segs
  })
  
  # palette + transient colors for locked selection
  pal_info <- reactive({
    s <- locked_settings()
    segs <- segs_all()
    req(s, segs)
    
    # categorical base colors
    levs  <- sort(unique(stats::na.omit(as.character(segs$cat))))
    n     <- length(levs)
    pname <- if (is.null(s$cat_pal)) "Dark2" else s$cat_pal
    
    if (tolower(pname) == "glasbey") {
      base <- pals::glasbey(max(32, n))
    } else {
      maxn <- RColorBrewer::brewer.pal.info[pname, "maxcolors"]
      base <- RColorBrewer::brewer.pal(maxn, pname)
    }
    cols_base <- if (n <= length(base)) base[seq_len(n)] else color_generator(base, n)
    names(cols_base) <- levs
    
    base_vec <- cols_base[as.character(segs$cat)]
    
    # normalize continuous to [0,1]
    v <- segs$cont
    rng <- range(v, na.rm = TRUE)
    w <- if (!is.finite(rng[1]) || rng[1] == rng[2]) rep(0.5, length(v)) else (v - rng[1]) / (rng[2] - rng[1])
    
    # shade: mix white and black in two steps
    rgb_base <- t(grDevices::col2rgb(base_vec)) / 255
    if (identical(s$cont_pal, "Light to Dark")) {
      rgb1 <- (w * rgb_base) + ((1 - w) * 1)    
      amt2 <- w * 0.6
      rgb2 <- (1 - amt2) * rgb1 + amt2 * 0      
    } else {  # dark to light
      rgb1 <- (w * rgb_base) + ((1 - w) * 0)    
      amt2 <- w * 0.6
      rgb2 <- (1 - amt2) * rgb1 + amt2 * 1      
    }
    rgb2[rgb2 < 0] <- 0
    rgb2[rgb2 > 1] <- 1
    seg_cols <- grDevices::rgb(rgb2[,1], rgb2[,2], rgb2[,3])  #back to hex for Leaflet
    
    list(seg_cols = seg_cols, legend_vals = cols_base, cont_range = rng)
  })
  
  # build a leaflet map 
  leaflet_map <- function(track_id = NULL) {
    s <- locked_settings()
    segs_all_df <- segs_all()
    pinfo <- pal_info()
    req(s, segs_all_df, pinfo)
    
    # subset for multipanel
    if (!is.null(track_id)) {
      idx  <- segs_all_df$track == track_id
      segs <- segs_all_df[idx, , drop = FALSE]
      pcols <- pinfo$seg_cols[idx]
      validate(need(nrow(segs) > 0, "No data for this animal."))
    } else {
      segs  <- segs_all_df
      pcols <- pinfo$seg_cols
    }
    
    bb <- as.vector(sf::st_bbox(segs))
    m <- leaflet(options = leafletOptions(minZoom = 2, preferCanvas = TRUE)) %>%
      fitBounds(bb[1], bb[2], bb[3], bb[4])
    m <- base_map_fun(m, s$basemap)
    
    m <- m %>%
      addScaleBar(position = "topleft") %>%
      addPolylines(data = segs, weight = s$linesize, opacity = s$linealpha,
                   color  = pcols, smoothFactor = 1)
    
    
    
    # legend for continuous attribute
    cont_legend <- sprintf( "<div style='background:transparent;padding:4px 6px 6px 6px;font-size:12px;opacity:.85;font-weight:700;'>
     %s: %s  ",
      htmltools::htmlEscape(s$cont_attr),
      if (identical(s$cont_pal, "Light to Dark")) "Light to Dark" else "Dark to Light"
    )
    
    m <- leaflet::addControl( m, html = cont_legend,  position = "topright"   )
    
    
    # legend for categorical attr 
    m <- leaflet::addLegend(
      m,
      position = "topright",
      colors   = unname(pinfo$legend_vals),
      labels   = names(pinfo$legend_vals),
      opacity  = 1,
      title    = s$cat_attr
    )
    
    m
  }
  
  #### layout 
  output$maps_ui <- renderUI({
    s <- locked_settings()
    if (is.null(s)) return(div("Loading…"))
    ids <- s$animals
    if (is.null(ids) || length(ids) == 0)
      return(div(style="color:red; font-weight:700; padding:10px;",
                 "Please select one or more animals."))
    if (identical(s$panel_mode, "Single panel")) {
      return(withSpinner(leafletOutput("map_single", height = "85vh"), type = 4, color = "blue", size = 0.9))
    }
    width <- 6
    cols <- lapply(seq_along(ids), function(i) {
      content <- tagList(
        tags$h5(paste("Animal:", ids[i]),
                style = "text-align: center; margin-top: 5px; margin-bottom: 5px;"),
        withSpinner(leafletOutput(paste0("map_", ids[i]), height = "45vh"), type = 4, color = "blue", size = 0.9)
      )
      column(width, content)
    })
    rows <- lapply(split(cols, ceiling(seq_along(cols) / 2)), function(chunk) do.call(fluidRow, chunk))
    tagList(rows)
  })
  
  ##single panel
  output$map_single <- renderLeaflet({
    validate(need(!is.null(locked_settings()) && !is.null(locked_mv()), "Loading…"))
    leaflet_map()
  })
  
  observe({
    s <- locked_settings()
    req(s, identical(s$panel_mode, "Multipanel"))
    ids <- s$animals; if (is.null(ids) || length(ids) == 0) return()
    
    lapply(ids, function(id_i){
      local({
        id_loc <- id_i
        output[[paste0("map_", id_loc)]] <- renderLeaflet({
          validate(need(!is.null(locked_settings()) && !is.null(locked_mv()), "Loading…"))
          leaflet_map(track_id = id_loc)
        })
      })
    })
  })
  
  ###### downloads part ######
  
  #download map as html
  output$save_html <- downloadHandler(
    filename = function() {
      s <- locked_settings(); req(s)
      if (identical(s$panel_mode, "Multipanel")) paste0("Plots_HTML_", Sys.Date(), ".zip")
      else                                       paste0("Plots_HTML_", Sys.Date(), ".html")
    },
    content = function(file) {
      shinybusy::show_modal_spinner(spin = "fading-circle", text = "Saving HTML…")
      on.exit(shinybusy::remove_modal_spinner(), add = TRUE)
      
      s <- locked_settings(); req(s)
      
      if (!identical(s$panel_mode, "Multipanel")) {
        htmlwidgets::saveWidget(leaflet_map(), file = file, selfcontained = TRUE)
        return(invisible())
      }
      
      ids <- s$animals; req(length(ids) > 0)
      td <- tempfile("tracks_html_"); dir.create(td)
      for (id in ids) {
        out <- file.path(td, paste0(id, "_", Sys.Date(), ".html"))
        htmlwidgets::saveWidget(leaflet_map(track_id = id), file = out, selfcontained = TRUE, libdir = NULL)
      }
      files <- list.files(td, pattern = "\\.html$", recursive = FALSE)
      zip::zipr(zipfile = file, files = files, root = td)
    }
  )
  
  #download map as png
  output$save_png <- downloadHandler(
    filename = function() {
      s <- locked_settings(); req(s)
      if (identical(s$panel_mode, "Multipanel")) paste0("Plots_PNG_", Sys.Date(), ".zip")
      else                                       paste0("Plots_PNG_", Sys.Date(), ".png")
    },
    content = function(file) {
      shinybusy::show_modal_spinner(spin = "fading-circle", text = "Saving PNG…")
      on.exit(shinybusy::remove_modal_spinner(), add = TRUE)
      
      s <- locked_settings(); req(s)
      
      # single
      if (!identical(s$panel_mode, "Multipanel")) {
        tf  <- tempfile(fileext = ".html")
        htmlwidgets::saveWidget(leaflet_map(), tf, selfcontained = TRUE)
        url <- if (.Platform$OS.type == "windows")
          paste0("file:///", gsub("\\\\", "/", normalizePath(tf))) else tf
        webshot2::webshot(url, file, vwidth = 1400, vheight = 900, delay = 1)
        return(invisible())
      }
      
      # multi
      ids <- s$animals; req(length(ids) > 0)
      td <- tempfile("tracks_png_"); dir.create(td)
      for (id in ids) {
        tf  <- tempfile(fileext = ".html")
        htmlwidgets::saveWidget(leaflet_map(track_id = id), tf, selfcontained = TRUE)
        url <- if (.Platform$OS.type == "windows")
          paste0("file:///", gsub("\\\\", "/", normalizePath(tf))) else tf
        out <- file.path(td, paste0(id, "_", Sys.Date(), ".png"))
        webshot2::webshot(url, out, vwidth = 1400, vheight = 900, delay = 1)
      }
      files <- list.files(td, recursive = FALSE)
      zip::zipr(zipfile = file, files = files, root = td)
    }
  )
}

shinyApp(ui, server)
