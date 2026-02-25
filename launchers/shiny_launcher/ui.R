# ==================== UI ====================
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Condor Job Manager"),
  
  dashboardSidebar(
    div(
      style = "margin: 10px 12px 8px 12px; padding: 10px; background: #f7fbff; border: 1px solid #cfe3f2; border-left: 4px solid #3c8dbc; border-radius: 4px; color: #1f2d3d; line-height: 1.35;",
      tags$div(
        style = "margin-bottom: 6px;",
        tags$img(src = "spc-logo.svg", alt = "Pacific Community (SPC) logo", style = "max-width: 100%; height: 34px; display: block;")
      ),
      tags$div("Created by Kyuhan Kim", style = "font-weight: 700; font-size: 12px;"),
      tags$div(
        style = "font-size: 11px; margin-top: 2px;",
        "Contact info: ",
        tags$a("kyuhank@spc.int", href = "mailto:kyuhank@spc.int", style = "color: #3c8dbc;")
      ),
      tags$div(
        style = "font-size: 11px; margin-top: 2px;",
        "GitHub: ",
        tags$a("github.com/kyuhank", href = "https://github.com/kyuhank", target = "_blank", style = "color: #3c8dbc;")
      )
    ),
    sidebarMenu(
      id = "tabs",
      menuItem("Launch Jobs", tabName = "launch", icon = icon("rocket")),
      menuItem("Monitor Jobs", tabName = "monitor", icon = icon("chart-line")),
      menuItem("Retrieve Results", tabName = "retrieve", icon = icon("download")) #,
  #    menuItem("Edit Models", tabName = "edit", icon = icon("edit")),
  #    menuItem("Settings", tabName = "settings", icon = icon("cog"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    
 tags$head(
  tags$style(HTML("
    .box-body { font-size: 14px; }
    .btn-launch { width: 100%; margin-top: 10px; }
    .status-running { color: #3c8dbc; }
    .status-completed { color: #00a65a; }
    .status-failed { color: #dd4b39; }
    .progress { height: 12px; }

    /* Model selector styling */
    .model-selector-container {
      background: #f9f9f9;
      border: 2px solid #ddd;
      border-radius: 4px;
      padding: 12px;
      min-height: 100px;
      max-height: 400px;
      overflow-y: auto;
      overflow-x: hidden;
    }

    .model-selector-container::-webkit-scrollbar {
      width: 12px;
    }
    .model-selector-container::-webkit-scrollbar-track {
      background: #f1f1f1;
      border-radius: 10px;
    }
    .model-selector-container::-webkit-scrollbar-thumb {
      background: #888;
      border-radius: 10px;
    }
    .model-selector-container::-webkit-scrollbar-thumb:hover {
      background: #555;
    }

    .model-checkbox-item {
      padding: 5px 8px;
      margin: 3px 0;
      border-bottom: 1px solid #eee;
      transition: background 0.2s;
    }
    .model-checkbox-item:hover {
      background: #e8f4f8;
      border-radius: 3px;
    }
    .model-checkbox-item:last-child {
      border-bottom: none;
    }
    
    /* Fix Shiny checkbox styling */
    .model-checkbox-item .shiny-input-container {
      margin-bottom: 0;
    }
    .model-checkbox-item .checkbox {
      margin-top: 0;
      margin-bottom: 0;
    }

    .model-details-container {
      max-height: 400px;
      overflow-y: auto;
      overflow-x: hidden;
      padding-right: 10px;
    }

    .model-details-container::-webkit-scrollbar {
      width: 12px;
    }
    .model-details-container::-webkit-scrollbar-track {
      background: #f1f1f1;
      border-radius: 10px;
    }
    .model-details-container::-webkit-scrollbar-thumb {
      background: #3c8dbc;
      border-radius: 10px;
    }
    .model-details-container::-webkit-scrollbar-thumb:hover {
      background: #2c6d8c;
    }

    .model-details-card {
      background: #ffffff;
      border: 1px solid #ddd;
      border-radius: 4px;
      padding: 12px;
      margin: 8px 0;
      box-shadow: 0 1px 3px rgba(0,0,0,0.1);
    }
    .model-name-header {
      font-size: 15px;
      font-weight: bold;
      color: #3c8dbc;
      margin-bottom: 8px;
    }
    .model-param {
      font-size: 12px;
      margin: 3px 0;
      color: #555;
    }
    .model-desc {
      background: #e8f4f8;
      padding: 8px;
      margin: 8px 0;
      border-left: 3px solid #3c8dbc;
      font-style: italic;
      font-size: 12px;
    }
    .param-label { 
      font-weight: bold; 
      margin-top: 10px;
      margin-bottom: 5px;
    }
    .description-box {
      background: #e8f4f8;
      padding: 10px;
      border-left: 4px solid #3c8dbc;
      margin: 10px 0;
      font-style: italic;
    }
    .config-card {
      background: #f9f9f9;
      border: 1px solid #ddd;
      border-radius: 4px;
      padding: 12px;
      margin-bottom: 12px;
    }
    .config-card:hover {
      background: #f0f0f0;
      cursor: pointer;
    }
    .job-history {
      background: #fff9e6;
      border-left: 4px solid #f39c12;
      padding: 8px;
      margin: 5px 0;
      font-size: 11px;
    }
    .search-box {
      margin-bottom: 10px;
    }
    .path-input-group {
      margin-bottom: 15px;
    }
    
    /* Fix Browse button alignment */
    .download-settings-content {
      display: flex;
      flex-direction: column;
      gap: 10px;
    }
    
    .download-path-row {
      display: flex;
      gap: 10px;
      align-items: flex-start;
    }
    
    .download-path-input {
      flex: 1;
    }
    
    .download-path-button {
      flex-shrink: 0;
      padding-top: 0;
    }
    
    .commands-preview {
      background: #f5f5f5;
      border: 1px solid #ddd;
      border-radius: 4px;
      padding: 10px;
      margin-top: 10px;
      font-family: monospace;
      font-size: 12px;
      color: #333;
    }
    .download-location-display {
      background: #e8f4f8;
      padding: 10px;
      border-radius: 4px;
      margin: 10px 0;
      font-family: monospace;
      font-size: 12px;
    }
    
    /* Folders selector container with scrolling */
    .folders-selector-container {
      background: #f9f9f9;
      border: 2px solid #ddd;
      border-radius: 4px;
      padding: 12px;
      max-height: 400px;
      overflow-y: auto;
      overflow-x: hidden;
    }
    
    .folders-selector-container::-webkit-scrollbar {
      width: 12px;
    }
    .folders-selector-container::-webkit-scrollbar-track {
      background: #f1f1f1;
      border-radius: 10px;
    }
    .folders-selector-container::-webkit-scrollbar-thumb {
      background: #888;
      border-radius: 10px;
    }
    .folders-selector-container::-webkit-scrollbar-thumb:hover {
      background: #555;
    }
    
    .folder-checkbox-item {
      padding: 5px 8px;
      margin: 3px 0;
      border-bottom: 1px solid #eee;
      transition: background 0.2s;
      display: flex;
      align-items: center;
      justify-content: space-between;
    }
    .folder-checkbox-item:hover {
      background: #e8f4f8;
      border-radius: 3px;
    }
    .folder-checkbox-item:last-child {
      border-bottom: none;
    }
    
    .folder-left-content {
      display: flex;
      align-items: center;
      flex: 1;
      gap: 8px;
    }
    
    /* Fix folder checkbox styling */
    .folder-left-content .shiny-input-container {
      margin-bottom: 0;
      width: auto;
    }
    .folder-left-content .checkbox {
      margin-top: 0;
      margin-bottom: 0;
      padding-left: 0;
    }
    .folder-left-content .checkbox label {
      padding-left: 0;
      margin-bottom: 0;
    }
    
    .folder-name {
      font-weight: 500;
      color: #333;
    }
    
    .folder-files-count {
      color: #666;
      font-size: 11px;
      background: #e8f4f8;
      padding: 3px 10px;
      border-radius: 12px;
      white-space: nowrap;
    }
    
    /* Retrieval log scrollable */
    .retrieval-log-container {
      max-height: 300px;
      overflow-y: auto;
      background: #f5f5f5;
      border: 1px solid #ddd;
      border-radius: 4px;
      padding: 10px;
      font-family: monospace;
      font-size: 12px;
    }
    
    .retrieval-log-container::-webkit-scrollbar {
      width: 10px;
    }
    .retrieval-log-container::-webkit-scrollbar-track {
      background: #f1f1f1;
    }
    .retrieval-log-container::-webkit-scrollbar-thumb {
      background: #888;
      border-radius: 5px;
    }
    
    /* Fix Show Log checkbox in box title */
    .log-title-container {
      display: flex;
      justify-content: space-between;
      align-items: center;
      width: 100%;
    }
    
    .log-checkbox-wrapper {
      display: flex;
      align-items: center;
      gap: 8px;
    }
    
    .log-checkbox-wrapper .shiny-input-container {
      margin-bottom: 0;
    }
    
    .log-checkbox-wrapper .checkbox {
      margin: 0;
      padding: 0;
    }
    
    .log-checkbox-wrapper .checkbox label {
      margin: 0;
      padding-left: 20px;
      font-weight: normal;
    }
    
    /* Archive contents tree view */
    .archive-tree {
      background: #f9f9f9;
      border: 1px solid #ddd;
      border-radius: 4px;
      padding: 10px;
      margin: 10px 0;
      max-height: 400px;
      overflow-y: auto;
      font-family: monospace;
      font-size: 12px;
    }
    
    .tree-item {
      padding: 2px 0;
      color: #333;
    }
    
    .tree-folder {
      color: #3c8dbc;
      font-weight: bold;
    }
    
    .tree-file {
      color: #666;
    }
    
    .extract-path-input {
      background: #fff3cd;
      border: 2px solid #ffc107;
      border-radius: 4px;
      padding: 15px;
      margin: 10px 0;
    }
    
    .path-preview-box {
      background: #e8f4f8;
      padding: 10px;
      border-radius: 4px;
      margin-top: 10px;
      font-family: monospace;
      font-size: 13px;
    }
    
    /* Button loading state and spinner */
    .btn-launch.loading {
      background-color: #f39c12 !important;
      border-color: #f39c12 !important;
      cursor: wait !important;
      opacity: 0.8;
    }

    .spinner {
      border: 4px solid #f3f3f3;
      border-top: 4px solid #3c8dbc;
      border-radius: 50%;
      width: 50px;
      height: 50px;
      animation: spin-anim 1s linear infinite;
      margin: 0 auto 20px;
    }

    @keyframes spin-anim {
      from { transform: rotate(0deg); }
      to { transform: rotate(360deg); }
    }
    
    /* Model checkbox with inline description */
    .model-checkbox-row {
      display: flex;
      align-items: flex-start;
      gap: 10px;
      padding: 8px;
      margin: 3px 0;
      border-bottom: 1px solid #eee;
      transition: background 0.2s;
    }
    
    .model-checkbox-row:hover {
      background: #e8f4f8;
      border-radius: 3px;
    }
    
    .model-checkbox-left {
      flex-shrink: 0;
      width: 30px;
      padding-top: 3px;
    }
    
    .model-checkbox-content {
      flex: 1;
      min-width: 0;
    }
    
    .model-name-label {
      font-weight: 600;
      color: #333;
      font-size: 13px;
      margin-bottom: 3px;
    }
    
    .model-desc-inline {
      color: #666;
      font-size: 11px;
      font-style: italic;
      line-height: 1.4;
      overflow: hidden;
      text-overflow: ellipsis;
      display: -webkit-box;
      -webkit-line-clamp: 2;
      -webkit-box-orient: vertical;
      margin: 0;
    }
    
    .model-desc-inline.expanded {
      display: block;
      -webkit-line-clamp: unset;
    }
    
    .expand-desc-btn {
      color: #3c8dbc;
      font-size: 10px;
      cursor: pointer;
      text-decoration: underline;
      margin-top: 2px;
      display: inline-block;
    }
    
    .expand-desc-btn:hover {
      color: #2c6d8c;
    }
    
    .no-description {
      color: #999;
      font-size: 11px;
      font-style: italic;
    }
    
    /* Script editor styles */
    .script-editor-container {
      width: 100%;
      height: 500px;
      font-family: 'Courier New', monospace;
      font-size: 12px;
      border: 1px solid #ddd;
      border-radius: 4px;
    }

    .editor-toolbar {
      background: #f5f5f5;
      padding: 8px;
      border-bottom: 1px solid #ddd;
      display: flex;
      gap: 10px;
      align-items: center;
    }

    .line-numbers {
      background: #f9f9f9;
      padding: 10px 5px;
      text-align: right;
      color: #999;
      border-right: 1px solid #ddd;
      user-select: none;
      min-width: 40px;
    }
  ")),
  
  tags$script(HTML("
    Shiny.addCustomMessageHandler('updateProgress', function(message) {
      var element = document.getElementById(message.id);
      if (element) {
        element.innerHTML = message.text;
        if (message.id === 'launch_progress_details' || 
            message.id === 'delete_progress_details' ||
            message.id === 'download_progress_details') {
          element.scrollTop = element.scrollHeight;
        }
      }
    });
  "))
),

    tabItems(
      launch_tab_ui(),
      edit_tab_ui(),
      monitor_tab_ui(),
      retrieve_tab_ui(),
      settings_tab_ui()
    )
  )
)
