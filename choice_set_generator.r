# app.R – Choice-Set Image Generator

library(shiny)
library(dplyr)
library(tidyr)
library(webshot2)
library(base64enc)
library(htmltools)
library(magick)

# ---- tunables ----------------------------------------------------------
PNG_WIDTH   <- 800
PNG_ZOOM    <- 2
ICON_SCALE  <- 1.2  

FONT_FAMILY <- "font-family:Arial,Helvetica,sans-serif;" 

# ---- ui ----------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Choice-Set Image Generator"),
  sidebarLayout(
    sidebarPanel(
      textInput("alt_names", "Alternative names (comma-separated)", "A,B"),
      sliderInput("font_px", "Font size (px)", min = 12, max = 30,
                  value = 18, step = 1),
      hr(),
      actionButton("add_def", "Add attribute-level definition"),
      tableOutput("defs_tbl"),
      hr(),
      uiOutput("set_builder"),
      hr(),
      downloadButton("download_png", "Download choice-set PNG")
    ),
    mainPanel(
      h4("Live preview"),
      uiOutput("preview_ui")
    )
  )
)

# ---- helper: tabla HTML idéntica para preview y export -----------------
build_table_html <- function(tbl, alts, font_px) {
  paste0(
    '<div style="max-width:', PNG_WIDTH, 'px;', FONT_FAMILY,
    'font-size:', font_px, 'px;">',
    '<table border="1" cellspacing="0" cellpadding="6" width="100%">',
    '<tr style="background:#f0f0f0;"><th>Attribute</th>',
    paste0(sprintf("<th>%s</th>", alts), collapse = ""), "</tr>",
    paste(purrr::map_chr(seq_len(nrow(tbl)), function(i) {
      paste0("<tr><td>", htmlEscape(tbl$attribute[i]), "</td>",
             paste0(purrr::map_chr(alts,
                                   ~ paste0("<td>", tbl[[.x]][i], "</td>")),
                    collapse = ""), "</tr>")
    }), collapse = ""),
    "</table></div>"
  )
}

# ---- server ------------------------------------------------------------
server <- function(input, output, session) {
  
  defs <- reactiveVal(tibble(attribute = character(),
                             level     = character(),
                             data_uri  = character()))
  
  # 1) add new definitions ----------------------------------------------
  observeEvent(input$add_def, {
    showModal(modalDialog(
      title = "New definition",
      textInput("def_attr",  "Attribute"),
      textInput("def_level", "Level"),
      fileInput("def_img",   "Level icon (PNG/JPG)",
                accept = c("image/png", "image/jpeg")),
      footer = tagList(modalButton("Cancel"),
                       actionButton("confirm_def", "Add to list"))
    ))
  })
  
  observeEvent(input$confirm_def, {
    req(input$def_attr, input$def_level, input$def_img)
    removeModal()
    ext  <- tools::file_ext(input$def_img$name)
    mime <- ifelse(tolower(ext) == "png", "image/png", "image/jpeg")
    uri  <- paste0("data:", mime, ";base64,",
                   base64enc::base64encode(input$def_img$datapath))
    defs(bind_rows(defs(), tibble(attribute = input$def_attr,
                                  level     = input$def_level,
                                  data_uri  = uri)))
  })
  
  output$defs_tbl <- renderTable(defs() |> select(attribute, level))
  
  # 2) dynamic selectors -------------------------------------------------
  output$set_builder <- renderUI({
    df <- defs(); req(nrow(df) > 0)
    alts <- strsplit(input$alt_names, "\\s*,\\s*")[[1]]
    lapply(unique(df$attribute), function(att) {
      div(
        strong(att),
        lapply(alts, function(alt) {
          selectInput(paste0("sel_", att, "_", alt),
                      label    = paste("Alt", alt),
                      choices  = df |> filter(attribute == att) |> pull(level),
                      selected = NULL, width = "100%")
        }),
        tags$hr()
      )
    })
  })
  
  # 3) collect chosen levels --------------------------------------------
  cs_data <- reactive({
    df <- defs(); req(nrow(df) > 0)
    alts  <- strsplit(input$alt_names, "\\s*,\\s*")[[1]]
    attrs <- unique(df$attribute)
    
    grid <- expand.grid(attribute = attrs, alt = alts,
                        stringsAsFactors = FALSE)
    
    grid$level <- vapply(seq_len(nrow(grid)), function(i)
      input[[paste0("sel_", grid$attribute[i], "_", grid$alt[i])]],
      FUN.VALUE = character(1))
    
    req(all(nzchar(grid$level)))
    left_join(grid, df, by = c("attribute", "level"))
  })
  
  # helper: icon + text (proporcional) -----------------------------------
  cell_html <- function(uri, txt, font_px) {
    icon_h  <- round(font_px * ICON_SCALE)
    pad_v   <- round(font_px * 0.35)
    sprintf(
      '<div style="display:flex;align-items:center;padding:%dpx 0;"><img src="%s" style="max-height:%dpx;margin-right:6px;"><span>%s</span></div>',
      pad_v, uri, icon_h, htmlEscape(txt))
  }
  
  # 4) live preview ------------------------------------------------------
  output$preview_ui <- renderUI({
    cd <- cs_data(); req(nrow(cd) > 0)
    tbl <- cd |>
      mutate(cell = mapply(function(u, t) cell_html(u, t, input$font_px),
                           data_uri, level, SIMPLIFY = FALSE)) |>
      select(attribute, alt, cell) |>
      pivot_wider(names_from = alt, values_from = cell)
    
    alts <- setdiff(names(tbl), "attribute")
    HTML(build_table_html(tbl, alts, input$font_px))
  })
  
  # 5) download PNG ------------------------------------------------------
  output$download_png <- downloadHandler(
    filename = function() "choice_set.png",
    content  = function(file) {
      cd <- cs_data(); req(nrow(cd) > 0)
      tbl <- cd |>
        mutate(cell = mapply(function(u, t) cell_html(u, t, input$font_px),
                             data_uri, level, SIMPLIFY = FALSE)) |>
        select(attribute, alt, cell) |>
        pivot_wider(names_from = alt, values_from = cell)
      alts <- setdiff(names(tbl), "attribute")
      
      html_path <- tempfile(fileext = ".html")
      save_html(HTML(build_table_html(tbl, alts, input$font_px)), html_path)
      
      webshot(html_path, file, vwidth = PNG_WIDTH, zoom = PNG_ZOOM)
      
      img <- image_trim(image_read(file))
      img <- image_border(img, "white", "0x20")
      image_write(img, path = file)
    },
    contentType = "image/png"
  )
}

shinyApp(ui, server)

