#install.packages('shiny')
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(bslib) #for  page_sidebar
library(readxl) #to read excel
library(dplyr)
library(janitor)
source("R/login_ui.R")
source("R/auth_server.R")

options(scipen = 999)

server = function(input,output,session){
  page <- reactiveVal("login")
  observe({
    shinyjs::toggleState(
      "signup_btn",
      condition = isTRUE(input$agree_terms)
    )
  })
  observeEvent(input$go_signup, {
    page("signup")
  })
  
  observeEvent(input$back_to_login, {
    page("login")
  })
  output$auth_page <- renderUI({
    if (page() == "login") {
      login_ui(login_content())
    } else {
      login_ui(signup_content())
    }
  })
  
  auth <- auth_server(input, output, session)
  
  output$authed <- reactive({
    auth$authed()
  })
  observe({
    session$sendCustomMessage(
      "setLoginView",
      !auth$authed()
    )
  })
  outputOptions(output, "authed", suspendWhenHidden = FALSE)

  output$file_upload <- renderUI({
        column(
          width = 8, offset = 2,
          box(
            width = 12, status = "primary", solidHeader = TRUE,
            tags$h4("Welcome!", align = "center"),
            tags$p("To begin, please upload CSV or Excel files.", align = "center"),
            tags$div(
              id = "upload_drop_card",
              class = "dropzone_big",
              fileInput("file2", label = NULL, multiple = TRUE,
                        accept = c(".csv",".xlsx",".xls")),
              tags$div(class="hint",
                       "Drag & drop files anywhere in this box, or click to browse")
            )
          )
        )
      })
    
  
  output$file_mode_ui <- renderUI({
    files <- input$file2
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
  
  dfCountryMaster = reactive({
    data_files = input$file2
    req(data_files)
    
    dfs = lapply(seq_len(nrow(data_files)), function(i){
      path = data_files$datapath[i]
      name = data_files$name[i]
      ext  = tolower(tools::file_ext(name))
      
      df = if (ext == "csv") {
        read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
      } else if (ext %in% c("xlsx","xls")) {
        as.data.frame(readxl::read_excel(path), check.names = FALSE)
      } else {
        stop("Unsupported file: ", name)
      }
      
      names(df) <- janitor::make_clean_names(names(df))
      df
    })
    
    # common columns across all files (base R)
    common_cols <- Reduce(intersect, lapply(dfs, names))
    validate(need(length(common_cols) > 0,
                  "No common column found across uploaded files, so cannot merge."))
    
    # pick best key automatically (using first df as reference)
    score_key <- function(col) {
      vals <- as.character(dfs[[1]][[col]])
      miss <- mean(is.na(vals) | vals == "")
      uniq <- length(unique(na.omit(vals))) / max(1, nrow(dfs[[1]]))
      uniq - miss
    }
    
    key <- common_cols[which.max(vapply(common_cols, score_key, numeric(1)))]
    
    # merge wide (base R Reduce)
    merged <- Reduce(function(a, b) dplyr::full_join(a, b, by = key, suffix = c("", ".dup")), dfs)
    
    # coalesce duplicate columns created by join (base R grep/sub)
    dup_cols <- grep("__dup$", names(merged), value = TRUE)
    for (dc in dup_cols) {
      base <- sub("__dup$", "", dc)
      if (base %in% names(merged)) {
        merged[[base]] <- dplyr::coalesce(merged[[base]], merged[[dc]])
        merged[[dc]] <- NULL
      }
    }
    
    # drop columns that are completely empty (all NA / all "")
    is_empty_col <- function(x) all(is.na(x) | trimws(as.character(x)) == "")
    merged <- merged[, !vapply(merged, is_empty_col, logical(1)), drop = FALSE]
    
    attr(merged, "auto_join_key") <- key
    
    to_num <- function(x) {
      x2 <- gsub(",", "", trimws(as.character(x)))
      suppressWarnings(as.numeric(x2))
    }
    
    is_num_like <- function(x) {
      if (!is.character(x)) return(FALSE)
      y <- to_num(x)
      mean(!is.na(y)) >= 0.8   # 80% of values convert -> treat as numeric
    }
    
    for (nm in names(merged)) {
      if (is_num_like(merged[[nm]])) {
        merged[[nm]] <- to_num(merged[[nm]])
      }
    }
    
    merged
  })
  
  observeEvent(list(dfCountryMaster(), input$His_x, input$Bar_x, input$Box_x, 
                    input$Box_y, input$Point_x, input$Point_y, input$fill_Point), 
               {
                 df <- dfCountryMaster()
                 
                 cols <- names(df)[vapply(df, is.numeric, logical(1))]
                 req(length(cols) > 0)
                 
                 cat_cols_all <- names(df)[vapply(df, function(x) is.factor(x) || is.character(x), logical(1))]
                 req(length(cat_cols_all) > 0)
                 
                 # keep only columns with reasonable number of levels
                 max_levels <- 30
                 cat_cols <- Filter(function(col) {
                   n <- length(unique(na.omit(df[[col]])))
                   n >= 2 && n <= max_levels
                 }, cat_cols_all)
                 
                 # fallback if nothing left
                 if (length(cat_cols) == 0) cat_cols <- cat_cols_all
                 
                 xcol_His <- input$His_x
                 if (is.null(xcol_His) || !(xcol_His %in% cols)) xcol_His <- cols[1]
                 
                 if ("Point" %in% input$plot_type && !("His" %in% input$plot_type)) xcol_His <- input$Point_x
                 
                 updateSelectInput(session, "His_x", choices = cols, selected = xcol_His)
                 
                 xcol_Bar <- input$Bar_x
                 if (is.null(xcol_Bar) || !(xcol_Bar %in% cat_cols)) {
                   xcol_Bar <- cat_cols[1]
                 }
                 if ("Box" %in% input$plot_type && !("Bar" %in% input$plot_type)) xcol_Bar <- input$Box_x
                 updateSelectInput(session, "Bar_x", choices = cat_cols, selected = xcol_Bar)
                 
                 fill_choices <- setdiff(cat_cols, input$Bar_x %||% "")
                 updateSelectInput(
                   session, "fill_Bar",
                   choices = c("None" = "", setNames(fill_choices, fill_choices)),
                   selected = if ((input$fill_Bar %||% "") %in% fill_choices) input$fill_Bar else ""
                 )
                 
                 
                 xcol_Box <- input$Box_x
                 if (is.null(xcol_Box) || !(xcol_Box %in% cat_cols)) {
                   xcol_Box <- cat_cols[1]
                 }
                 if ("Bar" %in% input$plot_type && !("Box" %in% input$plot_type)) xcol_Box <- input$Bar_x
                 updateSelectInput(session, "Box_x", choices = cat_cols, selected = xcol_Box)
                 
                 ycol_Box <- input$Box_y
                 if (is.null(ycol_Box) || !(ycol_Box %in% cols)) {
                   ycol_Box <- cols[1]
                 }
                 if ("Point" %in% input$plot_type && !("Box" %in% input$plot_type)) ycol_Box <- input$Point_y
                 updateSelectInput(session, "Box_y", choices = cols, selected = ycol_Box)
                 
                 
                 xcol_P <- input$Point_x
                 if (is.null(xcol_P) || !(xcol_P %in% cols)) {
                   xcol_P <- cols[1]
                 }
                 diff_xcols <- setdiff(cols, input$Point_y %||% "")
                 if ("His" %in% input$plot_type && !("Point" %in% input$plot_type)) xcol_P <- input$His_x
                 updateSelectInput(session, "Point_x", choices = diff_xcols, selected = xcol_P)
                 
                 ycol_P <- input$Point_y
                 if (is.null(ycol_P) || !(ycol_P %in% cols)) {
                   ycol_P <- cols[2]
                 }
                 if ("Box" %in% input$plot_type && !("Point" %in% input$plot_type)) ycol_P <- input$Box_y
                 diff_ycols <- setdiff(cols, input$Point_x %||% "")
                 
                 updateSelectInput(session, "Point_y", choices = diff_ycols, selected = ycol_P)
                 
                 fill_p <- cat_cols
                 updateSelectInput(
                   session, "fill_Point",
                   choices = c("None" = "", setNames(fill_p, fill_p)),
                   selected = if ((input$fill_Point %||% "") %in% fill_p) input$fill_Point else ""
                 )
                 
                 x <- df[[xcol_His]]
                 x <- x[is.finite(x)]
                 req(length(x) > 1)
                 
                 span <- diff(range(x))
                 
                 bw <- 2 * IQR(x) / (length(x)^(1/3))
                 if (!is.finite(bw) || bw <= 0) bw <- span / 30
                 if (!is.finite(bw) || bw <= 0) bw <- 1
                 
                 min_bw <- max(span / 200, .Machine$double.eps)
                 max_bw <- max(span / 5, min_bw * 2)
                 
                 step_bw <- signif((max_bw - min_bw) / 100, 2)
                 if (!is.finite(step_bw) || step_bw <= 0) step_bw <- 1
                 
                 # round min/max/value to step
                 round_to_step <- function(v, step) round(v / step) * step
                 min_bw <- round_to_step(min_bw, step_bw)
                 max_bw <- round_to_step(max_bw, step_bw)
                 bw     <- round_to_step(bw, step_bw)
                 
                 updateSliderInput(
                   session, "His_Bin",
                   min = min_bw, max = max_bw, value = bw,step=step_bw,
                   label = paste0("Bin Width (", xcol_His, ")")
                 )
               })
  
  output$tableSummary <- DT::renderDT(dfCountryMaster(),
                                      rownames=F,
                                      filter = "top",
                                      options = list(
                                        pageLength = 5,
                                        lengthMenu = c(5, 10, 25, 50),
                                        scrollX = TRUE,
                                        autoWidth = TRUE
                                      ),
                                      class = "nowrap"
  )
  
  output$num_summary_text <- renderPrint({
    df <- dfCountryMaster()
    req(df)
    
    num_df <- dplyr::select(df, where(is.numeric))
    validate(need(ncol(num_df) > 0, "No numeric columns found."))
    
    s <- summary(num_df)
    
    print(format(s, big.mark = ",", scientific = FALSE), quote = FALSE)
  })
  
  output$plot1 <- renderPlot({
    req("His" %in% input$plot_type)
    req(dfCountryMaster())
    req(input$His_x, input$His_Bin)
    
    df <- dfCountryMaster()
    ggplot(df, aes(x = .data[[input$His_x]])) +
      geom_histogram(binwidth = input$His_Bin, na.rm = TRUE, aes(fill = ..count..)) +
      labs(title = "Histogram")
  },height = 400)
  
  output$plot2 <- renderPlot({
    req("Bar" %in% input$plot_type)
    req(dfCountryMaster(), input$Bar_x)
    
    df <- dfCountryMaster()
    fill_var <- input$fill_Bar %||% ""   
    
    if (!nzchar(fill_var)) {
      ggplot(df, aes(x = .data[[input$Bar_x]])) +
        geom_bar() +
        ylab("Number of countries") +
        labs(title = "Bar Chart")
    } else {
      validate(need(fill_var %in% names(df), "Select a valid Fill by column"))
      ggplot(df, aes(x = .data[[input$Bar_x]], fill = .data[[fill_var]])) +
        geom_bar() +
        ylab("Number of countries") +
        labs(title = "Bar Chart")
    }
  }, height = 400)
  
  output$plot3 <- renderPlot({
    req("Box" %in% input$plot_type)
    req(dfCountryMaster(), input$Box_x, input$Box_y)
    
    df <- dfCountryMaster()
    
    show_outliers <- isTRUE(input[["outliers_Box"]])
    
    ggplot(df,aes(x=.data[[input$Box_x]],y=.data[[input$Box_y]]))+
      geom_boxplot(outlier.shape = if (show_outliers) 19 else NA)+
      labs(title = 'Box Plot')
  },height = 400)
  
  output$plot4 <- renderPlot({
    req("Point" %in% input$plot_type)
    
    df <- dfCountryMaster()
    pt_size  <- input$size_Point  %||% 2
    pt_alpha <- input$alpha_Point %||% 0.5
    col_var  <- input$fill_Point %||% ""
    
    
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
  
  output$plotMSG <- renderText({"Please select at least one plot type from the sidebar."})
  
  selection_his <- reactive({
    req(input$user_brush)
    req(input$His_x)
    brushedPoints(
      dfCountryMaster(),
      brush = input$user_brush,
      xvar = input$His_x   # <-- string column name
    )
  })
  
  
  output$table1 <- DT::renderDataTable(DT::datatable(selection_his()))
  
  
  output$summary_cards <- renderUI({
    # show only when either upload is present
    req(input$file2)
    
    tagList(
      bslib::card(
        bslib::card_header("DATA Preview"),
        DT::dataTableOutput("tableSummary")
      ),
      bslib::card(
        bslib::card_header("Table Numeric summary"),
        verbatimTextOutput("num_summary_text")
      )
    )
  })

  
}



ui = dashboardPage(
  dashboardHeader(title = "Shiny Basics & Deployment",disable = TRUE),
  
  
  dashboardSidebar(

    sidebarMenu(id = 'tab',
    menuItem("Home",tabName = "Home", icon = icon("house")),
    menuItem("Summary",tabName = "Summary", icon = icon("clipboard-list") ),
    menuItem("Plots",tabName = "Plots", icon = icon("chart-area"))
    
    ),width = 100),
  
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("/*top bar*/.custom-topbar{
                      position: fixed;
                      top: 0; left: 0; right: 0;
                      height: 70px;
                      background: #1F2D3D;
                      color:#ffffff;
                      display: flex;
                      align-items: center;
                      padding: 0 16px;
                      z-index: 1050;
                      box-shadow: 0 1px 2px rgba(0,0,0,0.1);
                      font-weight:600;
                      }
                      
                      .content-wrapper, .right-side, .main-footer {
                        padding-top: 100px !important;
                      }
                      .main-sidebar, .left-side{
                        padding-top: 100px !important;
                      }")),
      tags$style(HTML("
                      .custom-topbar{
                      display: flex;
                      align-items: center;
                      justify-content: space-between;
                      gap: 12px;
                      }
                      .custom-topbar .left{
                      display: flex;
                      align-items: center;
                      gap: 12px;
                      min-width: 0;
                      }
                      .custom-topbar .right {
                            display: flex;
                            align-items: center;
                            gap: 8px;
                          }
                          /* Hamburger button */
                          .custom-topbar .hamburger {
                            display: inline-flex;
                            align-items: center;
                            justify-content: center;
                            width: 36px;
                            height: 36px;
                            border-radius: 0px;
                            border: 0px solid rgba(255,255,255,0.15);
                            background: rgba(255,255,255,0);
                            color: #ffffff;
                            cursor: pointer;
                          }
                          .custom-topbar .hamburger:hover {
                            background: rgba(255,255,255,0.15);
                          }
                          
                      .custom-topbar .logo {
                            width: 60px;
                            height: 60px;
                            border-radius: 4px;
                            background: rgba(255,255,255,0);
                            display: inline-flex;
                            align-items: center;
                            justify-content: center;
                            overflow: hidden;
                          }
                          .custom-topbar .logo img {
                            max-width: 100%;
                            max-height: 100%;
                            object-fit: contain;
                            display: block;
                          }
                          /* Title text should truncate if too long */
                          .custom-topbar .title {
                            font-weight: 600;
                            white-space: nowrap;
                            overflow: hidden;
                            text-overflow: ellipsis;
                          }


                      ")),
      tags$style(HTML("
                      .main-sidebar, .left-side {
                        position: fixed !important;
                        background-color: #1F2D3D !important;
    }

                      ")),
      tags$script(HTML("
          document.addEventListener('DOMContentLoaded', function() {
                var btn = document.getElementById('topbarToggleSidebar');
                if (btn) {
                  btn.addEventListener('click', function() {
                    // AdminLTE 2 (used by shinydashboard) collapses via 'sidebar-collapse' on <body>
                    document.body.classList.toggle('sidebar-collapse');
                    // Trigger resize so plots/layouts adjust
                    window.dispatchEvent(new Event('resize'));
                  });
                }
              });
            ")),
      tags$style(HTML("
      .user-avatar-btn {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        width: 36px;
        height: 36px;
        border-radius: 50%;
        border: 1px solid rgba(255,255,255,0.15);
        background: rgba(255,255,255,0.08);
        cursor: pointer;
        padding: 0;
        overflow: hidden;
        color: #fff;
      }
      .user-avatar-btn:hover {
        background: rgba(255,255,255,0.15);
      }
      .user-avatar-btn img {
        width: 100%; height: 100%; object-fit: cover; display: block;
      }
        .user-menu {
          position: absolute;
          top: 71px;               /* below 50px header; adjust if your header height changes */
          right: 2px;
          background: #ffffff;
            color: #111827;
            border: 1px solid #e5e7eb;
          border-radius: 8px;
          min-width: 200px;
          box-shadow: 0 10px 20px rgba(0,0,0,0.08);
          padding: 6px 0;
          z-index: 2000;
          display: none;           /* hidden by default */
        }
        .user-menu.open { display: block; }
        .user-menu .menu-header {
            display: block;              /* ensure it spans full width */
            text-align: center;          /* center the text */
            font-weight: 600;
            font-size: 13px;
            color: #374151;
            padding: 10px 12px 6px 12px;
            border-bottom: 1px solid #f1f5f9;
            margin-bottom: 6px;
        
            /* Handle long names gracefully */
            white-space: nowrap;
            overflow: hidden;
            text-overflow: ellipsis;
          }

        .user-menu .menu-item {
          display: flex;
          align-items: center;
          gap: 8px;
          width: 100%;
          padding: 10px 12px;
          background: transparent;
          border: 0;
          text-align: center;
          cursor: pointer;
          color: #111827;
            font-size: 15px;
        }
      .user-menu .menu-item:hover {
        background: #f3f4f6;
      }
      ")),
      
      tags$script(HTML("
          document.addEventListener('DOMContentLoaded', function() {
            var avatarBtn = document.getElementById('userAvatarBtn');
            var menu = document.getElementById('userMenu');
        
            // Guard in case elements aren't in DOM yet
            if (!avatarBtn || !menu) return;
        
            // Toggle on avatar click
            avatarBtn.addEventListener('click', function(e) {
              e.stopPropagation();                 // don't bubble to document
              menu.classList.toggle('open');       // toggles display
            });
        
            // Close when clicking outside the menu/avatar
            document.addEventListener('click', function(e) {
              var clickedInsideMenu = menu.contains(e.target);
              var clickedAvatar = avatarBtn.contains(e.target);
              if (!clickedInsideMenu && !clickedAvatar) {
                menu.classList.remove('open');
              }
            });
        
            // Optional: close on Escape key
            document.addEventListener('keydown', function(e) {
              if (e.key === 'Escape') {
                menu.classList.remove('open');
              }
            });
          });
        ")),
      tags$script(HTML("
        Shiny.addCustomMessageHandler('exitLoginView', function(x) {
          document.body.classList.remove('login-view');
      
          // force AdminLTE to recalc layout
          document.body.classList.remove('sidebar-collapse');
          window.dispatchEvent(new Event('resize'));
      
          setTimeout(function() {
            window.dispatchEvent(new Event('resize'));
          }, 100);
        });
      ")),
      tags$script(HTML("
        Shiny.addCustomMessageHandler('setLoginView', function(on) {
          if (on) {
            document.body.classList.add('login-view');
          } else {
            document.body.classList.remove('login-view');
          }
        });
      ")),
    
      tags$style(HTML("
                    .dropzone_big{
                      border:2px dashed #cfd4da; border-radius:20px;
                      padding:100px; background:#fafbfc; cursor:pointer;
                    }
                    .dropzone_big.dragover{ background:#f1f3f5; border-color:#adb5bd; }
                    .dropzone_big .form-group{ margin:0; }
                    .dropzone_big .hint{ margin-top:12px; color:#6c757d; text-align:center; }
                  ")),
      tags$script(HTML("
                  function bindDropzone(){
                    const dz = document.getElementById('upload_drop_card');
                    if(!dz || dz.dataset.bound === '1') return;
              
                    const fileInputEl = dz.querySelector('input[type=file]');
                    if(!fileInputEl) return;
              
                    dz.dataset.bound = '1';
              
                    dz.addEventListener('click', function(e){
                      
                      if(e.currentTarget !== e.target) return;
                      fileInputEl.click();
                    });
              
                    ['dragenter','dragover'].forEach(ev => {
                      dz.addEventListener(ev, function(e){
                        e.preventDefault(); e.stopPropagation();
                        dz.classList.add('dragover');
                      });
                    });
              
                    ['dragleave','drop'].forEach(ev => {
                      dz.addEventListener(ev, function(e){
                        e.preventDefault(); e.stopPropagation();
                        dz.classList.remove('dragover');
                      });
                    });
              
                    dz.addEventListener('drop', function(e){
                      const dt = e.dataTransfer;
                      if(!dt || !dt.files || dt.files.length === 0) return;
              
                      fileInputEl.files = dt.files;
                      fileInputEl.dispatchEvent(new Event('change', { bubbles: true }));
                    });
                  }
              
                  // run once now
                  document.addEventListener('DOMContentLoaded', function(){
                    bindDropzone();
              
                    // also watch for dynamic UI insertion (renderUI)
                    const obs = new MutationObserver(function(){
                      bindDropzone();
                    });
                    obs.observe(document.body, { childList: true, subtree: true });
                  });
                ")),

    
    div(class = "custom-topbar",
        div(class = "left",
            tags$button(
              id = "topbarToggleSidebar",
              class = "hamburger",
              title = "Toggle navigation",
              icon("bars")   # Font Awesome hamburger icon
            ),
            # Logo 
            div(class = "logo",
                tags$img(src = "logo.png", alt = "App logo")
            ),
            # Title
            div(class = "title", "Clinical Project")
        ),
        
        div(class = "right",
            # Avatar button (circle). 
            tags$button(
              id = "userAvatarBtn",
              class = "user-avatar-btn",
              title = "Account"
              ,
              
              # icon("user")   # <- uncomment to use icon instead of image
              tags$img(src = "avatar.png", alt = "User")  
            ),
            
            # The dropdown menu (initially hidden; toggled by JS)
            div(
              id = "userMenu",
              class = "user-menu",
              div(class = "menu-header", "Signed in as: Gunjan Tipe"),
              # Menu items
              #tags$button(class = "menu-item", icon("gear"), "Settings"),
              #tags$button(class = "menu-item", icon("circle-question"), "Help"),
              
              tags$button(
                id = "logoutBtn",
                class = "menu-item",
                onclick = "Shiny.setInputValue('logoutBtn', Date.now(), {priority: 'event'})",
                icon("right-from-bracket"), "Logout"
              )
              
            )
        )
        
    )),
    conditionalPanel(
      condition = "!output.authed",
      uiOutput("auth_page")
      
    ),
    conditionalPanel(
      condition = "output.authed",
    tabItems(
      tabItem(tabName = "Home",
              uiOutput("file_upload"),
              uiOutput("file_mode_ui")
      ),
      tabItem(tabName = "Summary",
              uiOutput("summary_cards")
      ),
      
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
                box(
                  title = "Histogram controls",width = 12,
                  selectInput("His_x", "X axis (numeric)", choices = NULL),
                  sliderInput("His_Bin", "Bin Width", min = 1, max = 100, value = 10, step = 1)
                )),
              # Bar controls
              conditionalPanel(
                "input.plot_type && input.plot_type.indexOf('Bar') !== -1",
                box(
                  title = "Bar chart controls",width = 12,
                  selectInput("Bar_x",  "X axis (categorical)", choices = NULL),
                  selectInput("fill_Bar", "Fill by", choices = c("None" = ""), selected = "")
                )),
              # Box controls
              conditionalPanel(
                "input.plot_type && input.plot_type.indexOf('Box') !== -1",
                box(
                  title = "Box plot controls",width = 12,
                  selectInput("Box_x", "X axis (categorical)", choices = NULL),
                  selectInput("Box_y", "Y axis (numeric)", choices = NULL),
                  checkboxInput("outliers_Box", "Show outliers", value = TRUE)
                ),
                # Scatter controls
                conditionalPanel(
                  "input.plot_type && input.plot_type.indexOf('Point') !== -1",
                  box(
                    title = "Scatterplot controls",width = 12,
                    selectInput("Point_x", "X axis (numeric)", choices = NULL),
                    selectInput("Point_y", "Y axis (numeric)", choices = NULL),
                    selectInput("fill_Point", "Color by", choices = c("None" = ""), selected = ""),
                    sliderInput("size_Point",  "Point size", min = 1, max = 6, value = 2, step = 0.5),
                    sliderInput("alpha_Point", "Transparency (alpha)", min = 0.1, max = 1, value = 0.5, step = 0.1)
                  )
                )
              ))),
          # Plot area (9 cols)
          column(
            width = 9,
            
            
            conditionalPanel(
              "input.plot_type && input.plot_type.indexOf('His') !== -1",
              box(
                title = "Histogram & Brush", width = 12, status = "primary", solidHeader = TRUE,
                plotOutput('plot1', brush = brushOpts(id = "user_brush", resetOnNew = TRUE, direction = "x")),
                DTOutput("table1")
              )),
            conditionalPanel(
              "input.plot_type && input.plot_type.indexOf('Bar') !== -1",
              box(
                title = "Bar Chart", width = 12, status = "primary", solidHeader = TRUE,
                plotOutput('plot2')
              )),
            conditionalPanel(
              "input.plot_type && input.plot_type.indexOf('Box') !== -1",
              box(
                title = "Box Plot", width = 12, status = "primary", solidHeader = TRUE,
                plotOutput('plot3')
              )),
            conditionalPanel(
              "input.plot_type && input.plot_type.indexOf('Point') !== -1",
              box(
                title = "Scatterplot", width = 12, status = "primary", solidHeader = TRUE,
                plotOutput('plot4')
              )),
            conditionalPanel('!(input.plot_type) || input.plot_type.length === 0',
                             box(
                               width = 12, status = "primary", solidHeader = TRUE,
                               h5(textOutput("plotMSG"))
                             ))
          )
        ))))
    
  ))

shinyApp(ui=ui,server = server)



