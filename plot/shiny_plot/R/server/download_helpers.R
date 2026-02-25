show_download_modal <- function(plot_type, plot_name, current_save_dir = NULL) {
  save_dir_value <- if (!is.null(current_save_dir) && nzchar(trimws(current_save_dir))) {
    current_save_dir
  } else {
    ""
  }

  showModal(modalDialog(
    title = paste("📥 Download", plot_name),
    size = "m",

    fluidRow(
      column(6,
             h5("📐 Dimensions", style = "font-weight: bold; margin-top: 0;"),
             numericInput(paste0(plot_type, "_width"), "Width (inches):",
                          value = 12, min = 4, max = 24, step = 1),
             numericInput(paste0(plot_type, "_height"), "Height (inches):",
                          value = 8, min = 4, max = 20, step = 1),

             h5("📊 Presets", style = "font-weight: bold; margin-top: 15px;"),
             actionButton(paste0(plot_type, "_preset_wide"), "Wide (16:9)",
                          class = "btn-sm btn-default",
                          style = "width: 100%; margin-bottom: 5px;"),
             actionButton(paste0(plot_type, "_preset_standard"), "Standard (4:3)",
                          class = "btn-sm btn-default",
                          style = "width: 100%; margin-bottom: 5px;"),
             actionButton(paste0(plot_type, "_preset_square"), "Square (1:1)",
                          class = "btn-sm btn-default",
                          style = "width: 100%;")
      ),
      column(6,
             h5("🎨 Quality", style = "font-weight: bold; margin-top: 0;"),
             selectInput(paste0(plot_type, "_dpi"), "Resolution (DPI):",
                         choices = c("Screen (96)" = 96,
                                     "Print Draft (150)" = 150,
                                     "Print Standard (300)" = 300,
                                     "Print High (600)" = 600),
                         selected = 300),

             h5("📄 Format", style = "font-weight: bold; margin-top: 15px;"),
             radioButtons(paste0(plot_type, "_format"), NULL,
                          choices = c("PNG (Raster)" = "png",
                                      "PDF (Vector)" = "pdf",
                                      "SVG (Vector)" = "svg",
                                      "JPEG (Raster)" = "jpeg"),
                          selected = "png"),

             helpText("💡 PDF/SVG recommended for reports (scalable)",
                      style = "font-size: 11px; font-style: italic; color: #666;")
      )
    ),
    tags$hr(style = "margin: 10px 0;"),
    fluidRow(
      column(
        12,
        h5("📂 Download Folder", style = "font-weight: bold; margin-top: 0;"),
        textInput(paste0(plot_type, "_save_dir"), "Save folder:", value = save_dir_value, placeholder = "/path/to/save/plots"),
        shinyFiles::shinyDirButton(paste0(plot_type, "_browse_save_dir"), "Browse...",
                                   title = "Select Save Folder",
                                   icon = icon("folder-open"),
                                   class = "btn-default btn-sm",
                                   style = "width: 100%; margin-top: -4px;"),
             tags$div(
               style = "margin-top: 8px; padding: 8px; background: #f7fbff; border: 1px solid #d7e6f5; border-radius: 4px;",
               tags$div("Download target", style = "font-weight: 600; font-size: 11px; color: #2c3e50; margin-bottom: 3px;"),
               textOutput(paste0(plot_type, "_save_dir_label"), container = tags$div, inline = FALSE)
             )
      )
    ),

    footer = tagList(
      modalButton("Cancel"),
      actionButton(paste0(plot_type, "_save_to_folder"), "Download",
                   class = "btn-primary")
    )
  ))
}

sanitize_export_filename <- function(x) {
  x <- as.character(x)
  x <- gsub("[/\\\\:*?\"<>|]+", "_", x)
  x <- gsub("\\s+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  if (!nzchar(x)) x <- "plot"
  x
}

save_plot_with_format <- function(plot_obj, file, width = 12, height = 8, dpi = 300, format = NULL) {
  if (is.null(format) || !nzchar(format)) format <- tolower(tools::file_ext(file))
  format <- tolower(format)

  if (format == "png") {
    ggsave(file, plot = plot_obj, width = width, height = height, dpi = dpi, device = "png", bg = "white")
  } else if (format == "pdf") {
    ggsave(file, plot = plot_obj, width = width, height = height, device = "pdf")
  } else if (format == "svg") {
    ggsave(file, plot = plot_obj, width = width, height = height, device = "svg", bg = "white")
  } else if (format %in% c("jpg", "jpeg")) {
    ggsave(file, plot = plot_obj, width = width, height = height, dpi = dpi, device = "jpeg", bg = "white", quality = 95)
  } else {
    stop("Unsupported export format: ", format)
  }
}

register_folder_save_button <- function(plot_type, plot_reactive, input, session, output, filename_fun) {
  volumes <- shinyFiles::getVolumes()

  observe({
    shinyFiles::shinyDirChoose(input, paste0(plot_type, "_browse_save_dir"), roots = volumes(), session = session)
  })

  observeEvent(input[[paste0(plot_type, "_browse_save_dir")]], {
    selected <- shinyFiles::parseDirPath(volumes(), input[[paste0(plot_type, "_browse_save_dir")]])
    if (length(selected) > 0 && dir.exists(selected)) {
      updateTextInput(session, paste0(plot_type, "_save_dir"), value = selected)
      updateTextInput(session, "plot_export_dir", value = selected)
    }
  }, ignoreInit = TRUE)

  output[[paste0(plot_type, "_save_dir_label")]] <- renderText({
    cur <- input[[paste0(plot_type, "_save_dir")]]
    if (is.null(cur) || !nzchar(trimws(cur))) "(not set)" else cur
  })

  observeEvent(input[[paste0(plot_type, "_save_to_folder")]], {
    export_dir <- trimws(if (is.null(input[[paste0(plot_type, "_save_dir")]])) "" else input[[paste0(plot_type, "_save_dir")]])
    if (!nzchar(export_dir)) {
      showNotification("Set a save folder in the download dialog.", type = "error", duration = 6)
      return()
    }
    if (!dir.exists(export_dir)) {
      ok_create <- tryCatch(dir.create(export_dir, recursive = TRUE, showWarnings = FALSE), error = function(e) FALSE)
      if (!isTRUE(ok_create) && !dir.exists(export_dir)) {
        showNotification(paste("Could not create save folder:", export_dir), type = "error", duration = 8)
        return()
      }
    }

    p <- tryCatch(plot_reactive(), error = function(e) e)
    if (inherits(p, "error")) {
      showNotification(paste("Plot build failed:", p$message), type = "error", duration = 8)
      return()
    }

    width <- input[[paste0(plot_type, "_width")]]
    height <- input[[paste0(plot_type, "_height")]]
    dpi <- suppressWarnings(as.numeric(input[[paste0(plot_type, "_dpi")]]))
    if (!is.finite(dpi) || dpi <= 0) dpi <- 300
    format <- tolower(input[[paste0(plot_type, "_format")]])

    raw_name <- tryCatch(filename_fun(), error = function(e) paste0(plot_type, "_", Sys.Date(), ".", format))
    ext <- tolower(tools::file_ext(raw_name))
    stem <- tools::file_path_sans_ext(raw_name)
    if (!nzchar(stem)) stem <- raw_name
    if (!nzchar(ext) || ext != format) ext <- format
    out_file <- file.path(export_dir, paste0(sanitize_export_filename(stem), ".", ext))

    res <- tryCatch({
      save_plot_with_format(p, out_file, width = width, height = height, dpi = dpi, format = format)
      TRUE
    }, error = function(e) e)
    if (inherits(res, "error")) {
      showNotification(paste("Save failed:", res$message), type = "error", duration = 8)
      return()
    }

    updateTextInput(session, "plot_export_dir", value = export_dir)
    removeModal()
    showNotification(paste("Saved to", out_file), type = "message", duration = 5)
  }, ignoreInit = TRUE)
}
