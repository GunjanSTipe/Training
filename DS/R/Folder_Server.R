folder_server <- function(id, roots) {
  moduleServer(id, function(input, output, session) {
    
    volumes <- c(
      "Home" = normalizePath("~"),
      "C:"   = "C:/",
      "D:"   = "D:/"
    )
    
    selected_root <- reactiveVal(NULL)
    selected_folder <- reactiveVal(NULL)
    loaded_df <- reactiveVal(NULL)
    selected_file   <- reactiveVal(NULL) 
    
    shinyFiles::shinyDirChoose(
      input, "dir",
      roots = volumes,
      session = session
    )
    
    observeEvent(input$dir, {
      path <- shinyFiles::parseDirPath(volumes, input$dir)
      if (length(path) > 0) {
        selected_root(normalizePath(path))
        selected_folder(NULL)
        loaded_df(NULL)
      }
    })
    
    
    output$folder_buttons <- renderUI({
      req(selected_root())
      
      dirs <- list.dirs(
        selected_root(),
        recursive = FALSE,
        full.names = TRUE
      )
      
      tagList(
        lapply(dirs, function(d) {
          actionButton(
            session$ns(basename(d)),
            label = basename(d),
            class = "btn btn-primary",
            style = "margin:6px;"
          )
        })
      )
    })
    has_subfolders <- reactive({
      req(selected_root())
      length(list.dirs(selected_root(), recursive = FALSE)) > 0
    })
    observe({
      req(selected_root())
      dirs <- list.dirs(selected_root(), recursive = FALSE, full.names = TRUE)
      
      lapply(dirs, function(d) {
        observeEvent(input[[basename(d)]], {
          selected_folder(d)
          loaded_df(NULL)
        }, ignoreInit = TRUE)
      })
    })
    id_map <- reactiveVal(setNames(character(0), character(0)))    
    make_safe_id <- function(x) {
      paste0("file_", gsub("[^A-Za-z0-9_\\-]", "_", x))
    }
    

    
    output$file_buttons <- renderUI({
      req(selected_root())
      
      path <- if (!is.null(selected_folder())) {
        selected_folder()
      } else {
        selected_root()
      }
      
      
      files <- list.files(
        path,
        pattern = "\\.(csv|rda|xlsx?|CSV|RDA|XLSX?|Csv|Rda|Xlsx?)$",
        full.names = TRUE
      )
      
      
      if (length(files) == 0) return(NULL)
      
      safe_ids <- make_safe_id(basename(files))
      id_map(setNames(files, safe_ids))
             
      
      tagList(lapply(seq_along(files), function(i) {
        f   <- files[i]
        fid <- safe_ids[i]
        is_sel <- !is.null(selected_file()) && identical(selected_file(), f)
        btn_class <- paste("btn btn-outline-secondary file-btn",
                           if (is_sel) "selected" else "")
        actionButton(
          session$ns(fid),
          label = basename(f),
          class = btn_class,
          style = "display:block; margin-bottom:6px; text-align:left;"
        )
      }))
    })
    
    observe({
      imap <- id_map()
      if (!length(imap)) return()
      lapply(names(imap), function(fid) {
        observeEvent(input[[fid]], {
          f <- imap[[fid]]
          selected_file(f)  # remember selection
          
          # Load the data
          ext <- tolower(tools::file_ext(f))
          df  <- NULL
          if (ext == "rda") {
            env <- new.env()
            load(f, envir = env)
            # if multiple objects exist, take the first data.frame-like
            nm <- ls(env)
            obj <- env[[nm[1]]]
            df <- if (is.data.frame(obj)) obj else as.data.frame(obj)
          } else if (ext == "csv") {
            df <- read.csv(f, check.names = FALSE, stringsAsFactors = FALSE)
          } else if (ext %in% c("xlsx", "xls")) {
            df <- as.data.frame(readxl::read_excel(f))
          }
          loaded_df(df)
        }, ignoreInit = TRUE)
      })
    })
    
    
    output$file_selected_ui <- renderUI({
      req(selected_file())
      div(
        class = "folder-section",
        div(class = "section-title", "Selected file"),
        div(class = "dir-path", basename(selected_file()))
      )
    })
    
    output$has_subfolders <- reactive(has_subfolders())
    outputOptions(output, "has_subfolders", suspendWhenHidden = FALSE)
    
    output$dir_path <- renderText({
      req(selected_root())
      selected_root()
    })
    # ---- Path ----
    output$dir_path_ui <- renderUI({
      req(selected_root())
      div(class = "dir-path", selected_root())
    })
    
    output$folders_ui <- renderUI({
      req(selected_root())
      
      dirs <- list.dirs(selected_root(), recursive = FALSE, full.names = TRUE)
      if (length(dirs) == 0) return(NULL)
      
      div(
        class = "folder-section",
        div(class = "section-title", "Folders"),
        div(
          class = "folder-buttons",
          uiOutput(session$ns("folder_buttons"))
        )
      )
    })
    
    output$files_ui <- renderUI({
      req(selected_root())
      
      dirs <- list.dirs(selected_root(), recursive = FALSE, full.names = TRUE)
      
      # If subfolders exist → wait for selection
      if (length(dirs) > 0 && is.null(selected_folder())) {
        return(NULL)
      }
      
      div(
        class = "folder-section",
        div(class = "section-title", "Files"),
        div(
          class = "file-list",
          uiOutput(session$ns("file_buttons"))
        )
      )
    })
    
    
    return(list(
      selected_df     = loaded_df,
      selected_folder = selected_folder,
      selected_file   = selected_file,
      has_subfolders  = has_subfolders
    ))
  })
}
