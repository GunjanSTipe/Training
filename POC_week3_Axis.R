#install.packages('shiny')
library(shiny)
library(DT)
library(ggplot2)
library(bslib) #for  page_sidebar
library(readxl) #to read excel
library(dplyr)
library(shinyWidgets)

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
  
  num_cols <- reactive({
    df <- dfCountryMaster()[[2]]
    names(df)[vapply(df, is.numeric, logical(1))]
  })
  
  output$tableSummary <- DT::renderDT(dfCountryMaster()[[1]],
                                      rownames=F,
                                      filter = "top")
  output$plot1 <- renderPlot({
    req("His" %in% input$plot_type)
    xvar <- input[["x_His"]]; if (is.null(xvar)) xvar <- "GDP.per.capita.2017"
    
    ggplot(dfCountryMaster()[[2]],aes(x=.data[[xvar]]))+
      geom_histogram(binwidth = 5000,na.rm = TRUE,aes(fill=..count..))
  },height = 400)
  
  output$plot2 <- renderPlot({
    req("Bar" %in% input$plot_type)
    
    ggplot(dfCountryMaster()[[2]],aes(x=IncomeGroup))+
      geom_bar(aes(fill=Region))+ylab('Number of countries')
  },height = 400)
  
  output$plot3 <- renderPlot({
    req("Box" %in% input$plot_type)
    
    ggplot(dfCountryMaster()[[2]],aes(x=Region,y=Under.5.Mortality.Rate.2017))+
      geom_boxplot()
  },height = 400)
  
  output$plot4 <- renderPlot({
    req("Point" %in% input$plot_type)
    
    ggplot(dfCountryMaster()[[2]],aes(x=GDP.per.capita.2017,y=Under.5.Mortality.Rate.2017))+
      geom_point(alpha=0.5,na.rm = TRUE,size=2,aes(colour = Region))+
      labs(title = 'Plot of mortality rate under 5 against GDP and region')
  },height = 400)
  output$plotMSG <- renderText({"Please select at least one plot type from the sidebar."})
  
  slection <- reactive({
    req(input$user_brush)
    brushedPoints(dfCountryMaster()[[2]], input$user_brush,xvar = "GDP.per.capita.2017")
  })
  
  
  output$table <- DT::renderDataTable(DT::datatable(slection()))
  
  output$plots_ui <- renderUI({
    
    if (is.null(input$plot_type) || length(input$plot_type) == 0) {
      return(textOutput('plotMSG'))
    }
    tagList(
      if ("His" %in% input$plot_type){
        tagList(
          plotOutput('plot1', brush = brushOpts(id = "user_brush", resetOnNew = TRUE, direction = "x")),
          dataTableOutput("table"))
      },
      if ("Bar" %in% input$plot_type)
        plotOutput('plot2'),
      if ("Box" %in% input$plot_type)
        plotOutput('plot3'),
      if ("Point" %in% input$plot_type)
        plotOutput('plot4'),
      
    )
  })
  
  
  output$plot_controls <- renderUI({
    # If no plots selected, show nothing.
    if (is.null(input$plot_type) || length(input$plot_type) == 0) {
      return(NULL)
    }
    
    
    plot_labels <- c(His = "Histogram", Bar = "Bar Chart", Box = "Box Plot", Point = "Scatterplot")
    
    panels <- lapply(input$plot_type, function(p) {
      # Give each control a unique ID namespace per plot type
      ns <- function(id) paste0(id, "_", p)
      
      if (p == "His") {
        accordion_panel(
          paste0(plot_labels[p], " Controls"),
          selectInput(ns("x"), "X axis (numeric)", choices = num_cols(), selected = "GDP.per.capita.2017")
          )
        
      } 
      
      else if (p == "Bar") {
        
        
        accordion_panel(
          paste0(plot_labels[p], " Controls"),
          selectInput(ns("x"), "X axis (categorical)", choices = cat_cols, selected = "Region"),
        )
      } 
      
      else if (p == "Box") {
        
        df <- dfCountryMaster()[[2]]
        cat_cols <- names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]
        
        accordion_panel(
          paste0(plot_labels[p], " Controls"),
          selectInput(ns("x"), "X axis (categorical)", choices = cat_cols, selected = "Region"),
          selectInput(ns("y"), "Y axis (numeric)", choices = num_cols(), selected = "Under.5.Mortality.Rate.2017"),
        )
      } 
      
      else if (p == "Point") {
        accordion_panel(
          paste0(plot_labels[p], " Controls"),
          selectInput(ns("x"), "X axis (numeric)", choices = num_cols(), selected = "GDP.per.capita.2017"),
          selectInput(ns("y"), "Y axis (numeric)", choices = num_cols(), selected = "Under.5.Mortality.Rate.2017")
        )
      }
    })
    
    
    htmltools::tagList(
      do.call(
        bslib::accordion,
        c(list(id = "per_plot_controls", open = FALSE), panels)
      )
    )
    
  })
}

ui = page_navbar(
  title = "Shiny Basics & Deployment",
  id = 'Tab',
  theme = bs_theme(version = 5),
  sidebar = sidebar(
    id = "side",
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
                                             "Scatterplot" = "Point")))),
      
      uiOutput("plot_controls")
    )),
  
  nav_panel("Summary", 
            uiOutput("file_upload"),
            DT::dataTableOutput("tableSummary")
  ),
  nav_panel("Plots",
            uiOutput("plots_ui"))
)

shinyApp(ui=ui,server = server)



