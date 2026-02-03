#install.packages('shiny')
library(shiny)
library(DT)
library(ggplot2)
library(bslib) #for  page_sidebar
library(readxl) #to read excel
library(dplyr)

server = function(input,output,session){
  output$file_upload = renderUI(
    {
      if (is.null(input$file1) && is.null(input$file2))
      {
        fileInput("file2", 
                  "Welcome!\n to begin please upload CSV or excel File here", 
                  accept = c(".csv",".xlsx"))
      }
      else(
        NULL
      )
    }
  )
  
  observe({
    if (is.null(input$file2)){
      sidebar_toggle("side",open = FALSE)
    }
    else{
      sidebar_toggle("side",open = TRUE)
    }
  })
  
  dfCountryMaster = reactive({  
    data_file = input$file1 %||% input$file2
    req(data_file)
    fileExt = tools::file_ext(data_file$datapath)
    if (fileExt == 'csv'){
      dfCountryMaster = read.csv(data_file$datapath)
    }
    else{
      dfCountryMaster = read_excel(data_file$datapath)
    }
    if ("X" %in% names(dfCountryMaster)) dfCountryMaster$X <- NULL
    dfCountryMaster$GDP.per.capita.2017 = as.numeric(gsub(",", "", dfCountryMaster$GDP.per.capita.2017))
    
    dfsummarise = dfCountryMaster %>% group_by(Region) %>% summarise(
      'Number of Countries' = n(),
      'Total population' = sum(Total.Population.2017,na.rm = TRUE)/1000000,
      'Average of GDP per capita'=mean(GDP.per.capita.2017,na.rm = TRUE),
      'Countries with low income.'= sum(IncomeGroup == 'Low income'),
      'Median GDP per capita'=median(GDP.per.capita.2017,na.rm = TRUE),
      'Min mortality rate under 5'=min(Under.5.Mortality.Rate.2017,na.rm = TRUE),
      'Max mortality rate under 5'=max(Under.5.Mortality.Rate.2017,na.rm = TRUE)) 
    
    dfsummarise[8,1]= 'Other'
    
    list(dfsummarise, dfCountryMaster)
  })
  
  observeEvent(list(dfCountryMaster(), input$His_x, input$Bar_x, input$Box_x, 
                    input$Box_y, input$Point_x, input$Point_y, input$fill_Point), 
               {
                 df <- dfCountryMaster()[[2]]
                 
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
                 if (is.null(xcol_His) || !(xcol_His %in% cols)) {
                   xcol_His <- cols[1]
                 }
                 
                 updateSelectInput(session, "His_x", choices = cols, selected = xcol_His)
                 
                 xcol_Bar <- input$Bar_x
                 if (is.null(xcol_Bar) || !(xcol_Bar %in% cat_cols)) {
                   xcol_Bar <- cat_cols[1]
                 }
                 
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
                 
                 updateSelectInput(session, "Box_x", choices = cat_cols, selected = xcol_Box)
                 
                 ycol_Box <- input$Box_y
                 if (is.null(ycol_Box) || !(ycol_Box %in% cols)) {
                   ycol_Box <- cols[1]
                 }
                 
                 updateSelectInput(session, "Box_y", choices = cols, selected = ycol_Box)
                 
                 
                 xcol_P <- input$Point_x
                 if (is.null(xcol_P) || !(xcol_P %in% cols)) {
                   xcol_P <- cols[1]
                 }
                 diff_xcols <- setdiff(cols, input$Point_y %||% "")
                 
                 updateSelectInput(session, "Point_x", choices = diff_xcols, selected = xcol_P)
                 
                 ycol_P <- input$Point_y
                 if (is.null(ycol_P) || !(ycol_P %in% cols)) {
                   ycol_P <- cols[2]
                 }
                 
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
  
  num_cols <- reactive({
    df <- dfCountryMaster()[[2]]
    names(df)[vapply(df, is.numeric, logical(1))]
  })
  
  output$tableSummary <- DT::renderDT(dfCountryMaster()[[1]],
                                      rownames=F,
                                      filter = "top")
  output$plot1 <- renderPlot({
    req("His" %in% input$plot_type)
    req(dfCountryMaster())
    req(input$His_x, input$His_Bin)
    
    df <- dfCountryMaster()[[2]]
    ggplot(df, aes(x = .data[[input$His_x]])) +
      geom_histogram(binwidth = input$His_Bin, na.rm = TRUE, aes(fill = ..count..)) +
      labs(title = "Histogram")
  },height = 400)
  
  output$plot2 <- renderPlot({
    req("Bar" %in% input$plot_type)
    req(dfCountryMaster(), input$Bar_x)
    
    df <- dfCountryMaster()[[2]]
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
    
    df <- dfCountryMaster()[[2]]
    
    show_outliers <- isTRUE(input[["outliers_Box"]])
    
    ggplot(df,aes(x=.data[[input$Box_x]],y=.data[[input$Box_y]]))+
      geom_boxplot(outlier.shape = if (show_outliers) 19 else NA)+
      labs(title = 'Box Plot')
  },height = 400)
  
  output$plot4 <- renderPlot({
    req("Point" %in% input$plot_type)
    
    df <- dfCountryMaster()[[2]]
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
  
  slection_his <- reactive({
    req(input$user_brush)
    req(input$His_x)
    brushedPoints(
      dfCountryMaster()[[2]],
      brush = input$user_brush,
      xvar = input$His_x   # <-- string column name
    )
  })
  
  
  output$table1 <- DT::renderDataTable(DT::datatable(slection_his()))
  
  output$plots_ui <- renderUI({
    
    if (is.null(input$plot_type) || length(input$plot_type) == 0) {
      return(textOutput('plotMSG'))
    }
    tagList(
      if ("His" %in% input$plot_type){
        #tagList(
        plotOutput('plot1', brush = brushOpts(id = "user_brush", resetOnNew = TRUE, direction = "x"))
      },
      dataTableOutput("table1"),
      if ("Bar" %in% input$plot_type)
        plotOutput('plot2',brush = brushOpts(id = "user_brush2", resetOnNew = TRUE, direction = "x")),
      if ("Box" %in% input$plot_type)
        plotOutput('plot3'),
      if ("Point" %in% input$plot_type)
        plotOutput('plot4'),
      
    )
  })
  
}

ui = page_navbar(
  title = "Shiny Basics & Deployment",
  id = 'Tab',
  theme = bs_theme(version = 5),
  sidebar = sidebar(
    id = "side",
    width = 350,
    accordion(open = F,
              accordion_panel("Import Data",
                              fileInput("file1", "Choose CSV or excel File", accept = c(".csv",".xlsx"))
              )),
    conditionalPanel(
      "input.Tab === 'Plots'",
      
      accordion(
        accordion_panel("Plot inputs",
                        checkboxGroupInput("plot_type", "Select Plots :",
                                           c("Histogram" = "His",
                                             "Bar Chart" = "Bar",
                                             "Box Plot" = "Box",
                                             "Scatterplot" = "Point")
                        )
        ),
        
        # Histogram controls
        conditionalPanel(
          "input.plot_type && input.plot_type.indexOf('His') !== -1",
          accordion_panel("Histogram controls",
                          selectInput("His_x", "X axis (numeric)", choices = NULL),
                          sliderInput("His_Bin", "Bin Width", min = 1000, max = 10000, value = 5000, step = 1000, width = "100%")
          )
        ),
        
        # Bar controls
        conditionalPanel(
          "input.plot_type && input.plot_type.indexOf('Bar') !== -1",
          accordion_panel("Bar chart controls",
                          selectInput("Bar_x", "X axis (categorical)", choices = NULL),
                          selectInput("fill_Bar", "Fill by", choices = c("None" = ""),
                                      selected = "")
          )
        ),
        
        # Box controls
        conditionalPanel(
          "input.plot_type && input.plot_type.indexOf('Box') !== -1",
          accordion_panel("Box plot controls",
                          
                          selectInput("Box_x", "X axis (categorical)", choices = NULL),
                          selectInput("Box_y", "Y axis (numeric)", choices = NULL),
                          checkboxInput("outliers_Box", "Show outliers", value = TRUE)
          )
        ),
        
        # Scatter controls
        conditionalPanel(
          "input.plot_type && input.plot_type.indexOf('Point') !== -1",
          accordion_panel("Scatterplot controls",
                          selectInput("Point_x", "X axis (numeric)", choices = NULL),
                          selectInput("Point_y", "Y axis (numeric)", choices = NULL),
                          selectInput("fill_Point", "Fill by", choices = c("None" = ""),
                                      selected = ""),
                          sliderInput("size_Point", "Point size", min = 1, 
                                      max = 6, value = 2, step = 0.5),
                          sliderInput("alpha_Point", "Transparency (alpha)", 
                                      min = 0.1, max = 1, value = 0.5, step = 0.1)
          )
        )
      )
    )),
  
  nav_panel("Summary", 
            uiOutput("file_upload"),
            DT::dataTableOutput("tableSummary")
  ),
  nav_panel("Plots",
            uiOutput("plots_ui"))
)

shinyApp(ui=ui,server = server)



