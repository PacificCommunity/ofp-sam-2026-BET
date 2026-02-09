show_download_modal <- function(plot_type, plot_name) {
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

    footer = tagList(
      modalButton("Cancel"),
      downloadButton(paste0(plot_type, "_download_confirm"), "Download",
                     class = "btn-primary")
    )
  ))
}
