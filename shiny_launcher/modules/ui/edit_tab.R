edit_tab_ui <- function() {
      tabItem(
        tabName = "edit",
        fluidRow(
          box(
            title = "Manage Models", status = "warning", solidHeader = TRUE, width = 12,
            
            fluidRow(
              column(6,
                     selectInput("edit_model_select", "Select Model to Edit:",
                                 choices = NULL)
              ),
              column(3,
                     br(),
                     actionButton("add_new_model", "Add New Model", 
                                  class = "btn-success btn-block", icon = icon("plus"))
              ),
              column(3,
                     br(),
                     actionButton("save_config_btn", "Save Run Config", 
                                  class = "btn-info btn-block", icon = icon("file-export"))
              )
            ),
            
            shiny::hr(),
            
            div(class = "param-label", "Model Description:"),
            textAreaInput("edit_description", NULL,
                          placeholder = "Describe what this model does, changes from base model, etc.",
                          rows = 3, width = "100%"),
            
            uiOutput("model_editor_ui"),
            
            shiny::hr(),
            
            fluidRow(
              column(4,
                     actionButton("save_model", "Save Changes", 
                                  class = "btn-success btn-block", icon = icon("save"))
              ),
              column(4,
                     actionButton("reset_model", "Reset to Original", 
                                  class = "btn-warning btn-block", icon = icon("undo"))
              ),
              column(4,
                     actionButton("delete_model", "Delete Model", 
                                  class = "btn-danger btn-block", icon = icon("trash"))
              )
            )
          )
        )
      )
}
