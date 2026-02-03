folder_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$style(HTML("
      .folder-wrapper {
        max-width: 900px;
        margin: 40px auto;
      }

      .folder-card {
        background: #ffffff;
        border: 1px solid #e5e7eb;
        border-radius: 12px;
        padding: 24px 28px;
      }

      .folder-section {
        margin-top: 24px;
      }

      .folder-buttons button {
        margin: 6px 6px 6px 0;
        padding: 10px 18px;
        border-radius: 20px;
        font-weight: 500;
      }

      .file-list {
        max-height: 320px;
        overflow-y: auto;
        margin-top: 8px;
      }

      .file-list button {
        width: 100%;
        text-align: left;
        margin-bottom: 6px;
        padding: 10px 14px;
        border-radius: 8px;
        font-size: 14px;
      }

      .dir-path {
        background: #f8f9fa;
        border: 1px solid #e5e7eb;
        padding: 8px 12px;
        border-radius: 6px;
        font-size: 13px;
        color: #374151;
      }

      .section-title {
        font-weight: 600;
        margin-bottom: 10px;
      }
      
      .file-btn.selected {
        background: #e0f2fe !important;     /* light blue */
        border-color: #0284c7 !important;   /* teal-ish border */
        color: #0f172a !important;          /* readable text */
      }

    ")),
    
    div(
      class = "folder-wrapper",
      div(
        class = "folder-card",
        
        tags$h4("Clinical Data"),
        
        shinyFiles::shinyDirButton(
          ns("dir"),
          "Choose Report Folder",
          "Please select a folder",
          class = "btn btn-primary"
        ),
        
        tags$br(), tags$br(),
        
        uiOutput(ns("dir_path_ui")),
        uiOutput(ns("folders_ui")),
        uiOutput(ns("files_ui")),
        uiOutput(ns("file_selected_ui"))
      )
    )
  )
}