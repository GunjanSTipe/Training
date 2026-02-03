
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(readxl)
library(dplyr)
library(janitor)

options(scipen = 999)

ui <- dashboardPage(
  dashboardHeader(title = "Shiny Basics & Deployment", disable = TRUE),  # header removed (hides hamburger)
  
  dashboardSidebar(
    sidebarMenu(
      id = "tab",
      menuItem("Home",    tabName = "Home",    icon = icon("house")),
      menuItem("Summary", tabName = "Summary", icon = icon("clipboard-list")),
      menuItem("Plots",   tabName = "Plots",   icon = icon("chart-area"))
    ),
    
    # Single upload control
    fileInput("file", "Choose CSV/Excel file(s)", multiple = TRUE,
              accept = c(".csv", ".xlsx", ".xls"))
  ),
  
  dashboardBody(
    tabItems(
      # ---------------- Home ----------------
      tabItem(
        tabName = "Home",
        # Center the upload intro box horizontally
        fluidRow(
          column(
            width = 8, offset = 2,
            box(
              width = 12, status = "primary", solidHeader = TRUE,
              tags$h4("Welcome!", align = "center"),
              tags$p("To begin, please upload CSV or Excel files.", align = "center"),
              # Keep a simple fileInput here if you want a second visible upload on home
              # (it uses the same ID as the sidebar fileInput, so I keep only the sidebar one)
              # fileInput("file", label = NULL, multiple = TRUE, accept = c(".csv", ".xlsx", ".xls")),
              p("Use the file chooser in the left menu to upload.", align = "center")
            )
          )
        ),
        # File handling options (merge vs separate) shown only when multi-file uploaded
        uiOutput("file_mode_ui")
      ),
      
      # ---------------- Summary ----------------
      tabItem(
        tabName = "Summary",
        fluidRow(
          box(
            title = "Data Preview", width = 12, status = "primary", solidHeader = TRUE,
            DTOutput("tableSummary")
          )
        ),
        fluidRow(
          box(
            title = "Table Numeric Summary", width = 12, status = "info", solidHeader = TRUE,
            verbatimTextOutput("num_summary_text")
          )
        )
      ),
      
      # ---------------- Plots ----------------
      tabItem(
        tabName = "Plots",
        fluidRow(
          # Plot controls panel (3 cols) NEXT to the sidebar
          column(
            width = 3,
            box(
              title = "Plot Inputs", width = 12, status = "warning", solidHeader = TRUE,
              checkboxGroupInput(
                "plot_type", "Select Plots :",
                c("Histogram" = "His", "Bar Chart" = "Bar",
                  "Box Plot" = "Box", "Scatterplot" = "Point")
              ),
              
              # Histogram controls
              conditionalPanel(
                "input.plot_type && input.plot_type.indexOf('His') !== -1",
                h5("Histogram controls"),
                selectInput("His_x", "X axis (numeric)", choices = NULL),
                sliderInput("His_Bin", "Bin Width", min = 1, max = 100, value = 10, step = 1)
              ),
              # Bar controls
              conditionalPanel(
                "input.plot_type && input.plot_type.indexOf('Bar') !== -1",
                h5("Bar chart controls"),
                selectInput("Bar_x",  "X axis (categorical)", choices = NULL),
                selectInput("fill_Bar", "Fill by", choices = c("None" = ""), selected = "")
              ),
              # Box controls
              conditionalPanel(
                "input.plot_type && input.plot_type.indexOf('Box') !== -1",
                h5("Box plot controls"),
                selectInput("Box_x", "X axis (categorical)", choices = NULL),
                selectInput("Box_y", "Y axis (numeric)", choices = NULL),
                checkboxInput("outliers_Box", "Show outliers", value = TRUE)
              ),
              # Scatter controls
              conditionalPanel(
                "input.plot_type && input.plot_type.indexOf('Point') !== -1",
                h5("Scatterplot controls"),
                selectInput("Point_x", "X axis (numeric)", choices = NULL),
                selectInput("Point_y", "Y axis (numeric)", choices = NULL),
                selectInput("fill_Point", "Color by", choices = c("None" = ""), selected = ""),
                sliderInput("size_Point",  "Point size", min = 1, max = 6, value = 2, step = 0.5),
                sliderInput("alpha_Point", "Transparency (alpha)", min = 0.1, max = 1, value = 0.5, step = 0.1)
              )
            )
          ),
          # Plot area (9 cols)
          column(
            width = 9,
            box(
              width = 12, status = "primary", solidHeader = TRUE,
              h5(textOutput("plotMSG"))
            ),
            box(
              title = "Histogram & Brush", width = 12, status = "primary", solidHeader = TRUE,
              plotOutput('plot1', brush = brushOpts(id = "user_brush", resetOnNew = TRUE, direction = "x")),
              DTOutput("table1")
            ),
            box(
              title = "Bar Chart", width = 12, status = "primary", solidHeader = TRUE,
              plotOutput('plot2')
            ),
            box(
              title = "Box Plot", width = 12, status = "primary", solidHeader = TRUE,
              plotOutput('plot3')
            ),
            box(
              title = "Scatterplot", width = 12, status = "primary", solidHeader = TRUE,
              plotOutput('plot4')
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # ---------- file list (reactive) ----------
  df_list <- reactive({
    files <- input$file
    req(files)
    # read all files into a named list using file names
    res <- lapply(seq_len(nrow(files)), function(i) {
      path <- files$datapath[i]
      name <- files$name[i]
      ext  <- tolower(tools::file_ext(name))
      
      df <- if (ext == "csv") {
        read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
      } else if (ext %in% c("xlsx","xls")) {
        as.data.frame(readxl::read_excel(path), check.names = FALSE)
      } else {
        stop("Unsupported file: ", name)
      }
      
      names(df) <- janitor::make_clean_names(names(df))
      df
    })
    names(res) <- files$name
    res
  })
  
  # ---------- merge logic (reactive) ----------
  df_merged <- reactive({
    dfs <- df_list()
    req(length(dfs) >= 2)
    
    # find common columns across all files
    common_cols <- Reduce(intersect, lapply(dfs, names))
    validate(need(length(common_cols) > 0,
                  "No common column found across uploaded files, so cannot merge."))
    
    # pick best key automatically using first df
    score_key <- function(col) {
      vals <- as.character(dfs[[1]][[col]])
      miss <- mean(is.na(vals) | vals == "")
      uniq <- length(unique(na.omit(vals))) / max(1, nrow(dfs[[1]]))
      uniq - miss
    }
    key <- common_cols[which.max(vapply(common_cols, score_key, numeric(1)))]
    
    merged <- Reduce(function(a, b) dplyr::full_join(a, b, by = key, suffix = c("", ".dup")), dfs)
    
    # coalesce any ".dup" columns
    dup_cols <- grep("\\.dup$", names(merged), value = TRUE)
    for (dc in dup_cols) {
      base <- sub("\\.dup$", "", dc)
      if (base %in% names(merged)) {
        merged[[base]] <- dplyr::coalesce(merged[[base]], merged[[dc]])
        merged[[dc]] <- NULL
      }
    }
    
    # drop completely empty columns
    is_empty_col <- function(x) all(is.na(x) | trimws(as.character(x)) == "")
    merged <- merged[, !vapply(merged, is_empty_col, logical(1)), drop = FALSE]
    
    # convert "numeric-like" character columns
    to_num <- function(x) {
      x2 <- gsub(",", "", trimws(as.character(x)))
      suppressWarnings(as.numeric(x2))
    }
    is_num_like <- function(x) {
      if (!is.character(x)) return(FALSE)
      y <- to_num(x)
      mean(!is.na(y)) >= 0.8
    }
    for (nm in names(merged)) {
      if (is_num_like(merged[[nm]])) merged[[nm]] <- to_num(merged[[nm]])
    }
    
    merged
  })
  
  # ---------- file mode UI (merge vs separate) ----------
  output$file_mode_ui <- renderUI({
    files <- input$file
    req(files)
    if (nrow(files) <= 1) return(NULL)  # hide when only one file
    fluidRow(
      column(
        width = 8, offset = 2,
        box(
          title = "File Handling", width = 12, status = "warning", solidHeader = TRUE,
          radioButtons(
            "file_mode", "How would you like to use the uploaded files?",
            choices = c("Merge all files" = "merge",
                        "Use one file at a time" = "separate"),
            selected = "merge", inline = TRUE
          ),
          # Only show dataset dropdown when "separate" is chosen
          conditionalPanel(
            "input.file_mode === 'separate'",
            selectInput("dataset_select", "Choose dataset (file):",
                        choices = files$name, selected = files$name[1])
          )
        )
      )
    )
  })
  
  # Keep the dataset list selector in sync if uploads change
  observeEvent(input$file, {
    files <- input$file
    if (is.null(files) || nrow(files) == 0) return()
    if (nrow(files) == 1) {
      updateRadioButtons(session, "file_mode", selected = "separate")
    } else {
      if (is.null(input$file_mode)) updateRadioButtons(session, "file_mode", selected = "merge")
    }
    updateSelectInput(session, "dataset_select", choices = files$name, selected = files$name[1])
  }, ignoreInit = TRUE)
  
  # ---------- active dataset (merged or selected single) ----------
  df_active <- reactive({
    files <- input$file
    req(files)
    if (nrow(files) == 1 || identical(input$file_mode, "separate")) {
      # single or 'separate' mode
      dfs <- df_list()
      nm <- if (!is.null(input$dataset_select) && input$dataset_select %in% names(dfs)) {
        input$dataset_select
      } else {
        names(dfs)[1]
      }
      dfs[[nm]]
    } else {
      # merge mode (requires 2+ files)
      df_merged()
    }
  })
  
  # ---------- Drive select inputs for plots & histogram binwidth ----------
  observeEvent(list(df_active(), input$His_x, input$Bar_x, input$Box_x, input$Box_y,
                    input$Point_x, input$Point_y, input$fill_Point, input$plot_type), {
                      df <- df_active()
                      cols <- names(df)[vapply(df, is.numeric, logical(1))]
                      if (length(cols) == 0) return()
                      
                      cat_cols_all <- names(df)[vapply(df, function(x) is.factor(x) || is.character(x), logical(1))]
                      if (length(cat_cols_all) == 0) return()
                      
                      max_levels <- 30
                      cat_cols <- Filter(function(col) {
                        n <- length(unique(na.omit(df[[col]])))
                        n >= 2 && n <= max_levels
                      }, cat_cols_all)
                      if (length(cat_cols) == 0) cat_cols <- cat_cols_all
                      
                      # Histogram X
                      xcol_His <- input$His_x
                      if (is.null(xcol_His) || !(xcol_His %in% cols)) xcol_His <- cols[1]
                      if ("Point" %in% (input$plot_type %||% character(0)) && !("His" %in% (input$plot_type %||% character(0)))) xcol_His <- input$Point_x
                      updateSelectInput(session, "His_x", choices = cols, selected = xcol_His)
                      
                      # Bar X
                      xcol_Bar <- input$Bar_x
                      if (is.null(xcol_Bar) || !(xcol_Bar %in% cat_cols)) xcol_Bar <- cat_cols[1]
                      if ("Box" %in% (input$plot_type %||% character(0)) && !("Bar" %in% (input$plot_type %||% character(0)))) xcol_Bar <- input$Box_x
                      updateSelectInput(session, "Bar_x", choices = cat_cols, selected = xcol_Bar)
                      
                      # Fill choices for Bar
                      current_bar <- if (is.null(input$Bar_x)) "" else input$Bar_x
                      fill_choices <- setdiff(cat_cols, current_bar)
                      updateSelectInput(
                        session, "fill_Bar",
                        choices = c("None" = "", setNames(fill_choices, fill_choices)),
                        selected = if (!is.null(input$fill_Bar) && input$fill_Bar %in% fill_choices) input$fill_Bar else ""
                      )
                      
                      # Box X
                      xcol_Box <- input$Box_x
                      if (is.null(xcol_Box) || !(xcol_Box %in% cat_cols)) xcol_Box <- cat_cols[1]
                      if ("Bar" %in% (input$plot_type %||% character(0)) && !("Box" %in% (input$plot_type %||% character(0)))) xcol_Box <- input$Bar_x
                      updateSelectInput(session, "Box_x", choices = cat_cols, selected = xcol_Box)
                      
                      # Box Y
                      ycol_Box <- input$Box_y
                      if (is.null(ycol_Box) || !(ycol_Box %in% cols)) ycol_Box <- cols[1]
                      if ("Point" %in% (input$plot_type %||% character(0)) && !("Box" %in% (input$plot_type %||% character(0)))) ycol_Box <- input$Point_y
                      updateSelectInput(session, "Box_y", choices = cols, selected = ycol_Box)
                      
                      # Scatter X
                      xcol_P <- input$Point_x
                      if (is.null(xcol_P) || !(xcol_P %in% cols)) xcol_P <- cols[1]
                      if ("His" %in% (input$plot_type %||% character(0)) && !("Point" %in% (input$plot_type %||% character(0)))) xcol_P <- input$His_x
                      diff_xcols <- setdiff(cols, if (is.null(input$Point_y)) character(0) else input$Point_y)
                      updateSelectInput(session, "Point_x", choices = diff_xcols, selected = xcol_P)
                      
                      # Scatter Y
                      ycol_P <- input$Point_y
                      if (is.null(ycol_P) || !(ycol_P %in% cols)) ycol_P <- cols[min(2, length(cols))]
                      if ("Box" %in% (input$plot_type %||% character(0)) && !("Point" %in% (input$plot_type %||% character(0)))) ycol_P <- input$Box_y
                      diff_ycols <- setdiff(cols, if (is.null(input$Point_x)) character(0) else input$Point_x)
                      updateSelectInput(session, "Point_y", choices = diff_ycols, selected = ycol_P)
                      
                      # Scatter color
                      fill_p <- cat_cols
                      updateSelectInput(
                        session, "fill_Point",
                        choices = c("None" = "", setNames(fill_p, fill_p)),
                        selected = if (!is.null(input$fill_Point) && input$fill_Point %in% fill_p) input$fill_Point else ""
                      )
                      
                      # Histogram bin width
                      x <- df[[xcol_His]]
                      x <- x[is.finite(x)]
                      if (length(x) > 1) {
                        span <- diff(range(x))
                        bw <- 2 * IQR(x) / (length(x)^(1/3))
                        if (!is.finite(bw) || bw <= 0) bw <- span / 30
                        if (!is.finite(bw) || bw <= 0) bw <- 1
                        min_bw <- max(span / 200, .Machine$double.eps)
                        max_bw <- max(span / 5, min_bw * 2)
                        step_bw <- signif((max_bw - min_bw) / 100, 2)
                        if (!is.finite(step_bw) || step_bw <= 0) step_bw <- 1
                        round_to_step <- function(v, step) round(v / step) * step
                        min_bw <- round_to_step(min_bw, step_bw)
                        max_bw <- round_to_step(max_bw, step_bw)
                        bw     <- round_to_step(bw, step_bw)
                        updateSliderInput(session, "His_Bin",
                                          min = min_bw, max = max_bw, value = bw, step = step_bw,
                                          label = paste0("Bin Width (", xcol_His, ")"))
                      }
                    }, ignoreInit = TRUE)
  
  # ---------- Summary outputs ----------
  output$tableSummary <- renderDT({
    df <- df_active()
    req(df)
    datatable(
      df, rownames = FALSE, filter = "top",
      options = list(pageLength = 5, lengthMenu = c(5, 10, 25, 50), scrollX = TRUE, autoWidth = TRUE),
      class = "nowrap"
    )
  })
  
  output$num_summary_text <- renderPrint({
    df <- df_active()
    req(df)
    num_df <- dplyr::select(df, where(is.numeric))
    validate(need(ncol(num_df) > 0, "No numeric columns found."))
    s <- summary(num_df)
    print(format(s, big.mark = ",", scientific = FALSE), quote = FALSE)
  })
  
  # ---------- Plots ----------
  output$plot1 <- renderPlot({
    req("His" %in% (input$plot_type %||% character(0)))
    df <- df_active(); req(df, input$His_x, input$His_Bin)
    ggplot(df, aes(x = .data[[input$His_x]])) +
      geom_histogram(binwidth = input$His_Bin, na.rm = TRUE, aes(fill = ..count..)) +
      labs(title = "Histogram")
  }, height = 400)
  
  output$plot2 <- renderPlot({
    req("Bar" %in% (input$plot_type %||% character(0)))
    df <- df_active(); req(df, input$Bar_x)
    fill_var <- if (is.null(input$fill_Bar)) "" else input$fill_Bar
    if (!nzchar(fill_var)) {
      ggplot(df, aes(x = .data[[input$Bar_x]])) + geom_bar() +
        ylab("Count") + labs(title = "Bar Chart")
    } else {
      validate(need(fill_var %in% names(df), "Select a valid Fill by column"))
      ggplot(df, aes(x = .data[[input$Bar_x]], fill = .data[[fill_var]])) + geom_bar() +
        ylab("Count") + labs(title = "Bar Chart")
    }
  }, height = 400)
  
  output$plot3 <- renderPlot({
    req("Box" %in% (input$plot_type %||% character(0)))
    df <- df_active(); req(df, input$Box_x, input$Box_y)
    show_outliers <- isTRUE(input[["outliers_Box"]])
    ggplot(df, aes(x = .data[[input$Box_x]], y = .data[[input$Box_y]])) +
      geom_boxplot(outlier.shape = if (show_outliers) 19 else NA) +
      labs(title = "Box Plot")
  }, height = 400)
  
  output$plot4 <- renderPlot({
    req("Point" %in% (input$plot_type %||% character(0)))
    df <- df_active(); req(df, input$Point_x, input$Point_y)
    pt_size  <- if (is.null(input$size_Point)) 2 else input$size_Point
    pt_alpha <- if (is.null(input$alpha_Point)) 0.5 else input$alpha_Point
    col_var  <- if (is.null(input$fill_Point)) "" else input$fill_Point
    if (!nzchar(col_var)) {
      ggplot(df, aes(x = .data[[input$Point_x]], y = .data[[input$Point_y]])) +
        geom_point(alpha = pt_alpha, size = pt_size, na.rm = TRUE) +
        labs(title = "Scatterplot")
    } else {
      validate(need(col_var %in% names(df), "Select a valid color column"))
      ggplot(df, aes(x = .data[[input$Point_x]], y = .data[[input$Point_y]], colour = .data[[col_var]])) +
        geom_point(alpha = pt_alpha, size = pt_size, na.rm = TRUE) +
        labs(title = "Scatterplot")
    }
  }, height = 400)
  
  output$plotMSG <- renderText({
    "Please select at least one plot type from the Plot Inputs panel."
  })
  
  # histogram brush selection table
  selection_his <- reactive({
    req(input$user_brush, input$His_x)
    brushedPoints(df_active(), brush = input$user_brush, xvar = input$His_x)
  })
  output$table1 <- renderDT({
    datatable(selection_his(), options = list(pageLength = 5, scrollX = TRUE))
  })
}

shinyApp(ui, server)
