#install.packages('shiny')
library(shiny)
library(DT)
library(ggplot2)
library(shinythemes)


setwd('D:/ClinicalPOC')
CSV_file = './DS/Countries Population.csv'
file2 = './DS/Countries Region Mapping.xlsx'

dfRM = read_excel(file2)
CSV2 = './DS/Countries Indicators.csv'
dfCI = read.csv(CSV2)

#Create data frame "dfCountries", load countries population file in it.
dfCountries = read.csv(CSV_file)

#Merge the 3 datasets attached into 1 dataframe : "dfCountryMaster"
DS1_2 = full_join(dfCountries, dfRM, by = join_by("Country.Code"))
dfCountryMaster = full_join(DS1_2, dfCI, by = join_by("Country.Code"))


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

server = function(input,output,session){
  output$tableSummary <- DT::renderDT(dfsummarise,
                                      rownames=F,
                                      filter = "top")
  output$plot1 <- renderPlot({
  if (input$plot_type == "Histogram of gdp per capita"){
  
    ggplot(dfCountryMaster,aes(x=GDP.per.capita.2017))+
      geom_histogram(binwidth = 5000,na.rm = TRUE,aes(fill=..count..))+
      labs(title = 'Histogram of gdp per capita')
    
  }
  
  else if(input$plot_type == "Plot of income group by region"){
  
    ggplot(dfCountryMaster,aes(x=IncomeGroup))+geom_bar(aes(fill=Region))+
      ylab('Number of countries')+labs(title = 'Plot of income group by region')
  }
  
  
  else if(input$plot_type == "Plot of mortality rate under 5 by region"){
  
    ggplot(dfCountryMaster,aes(x=Region,y=Under.5.Mortality.Rate.2017))+
      geom_boxplot()+labs(title = 'Plot of mortality rate under 5 by region')
  }
  
  
  else if(input$plot_type == "Plot of mortality rate under 5 against GDP and region"){
  
    ggplot(dfCountryMaster,aes(x=GDP.per.capita.2017,y=Under.5.Mortality.Rate.2017))+
      geom_point(alpha=0.5,na.rm = TRUE,size=2,aes(colour = Region))+
      labs(title = 'Plot of mortality rate under 5 against GDP and region')
  }
    })
    
    slection <- reactive({
      
      user_click <- input$user_click
      #sel <- brushedPoints(dfCountryMaster, user_brush, yvar = NULL)
      sel <- nearPoints(dfCountryMaster, user_click, threshold = 10, maxpoints = 5)
      return(sel)
    })
    
    output$table <- DT::renderDataTable(DT::datatable(slection()))
  
}

ui = fluidPage(
  
  titlePanel("Shiny Basics & Deployment"), # our title
  
  sidebarLayout(
    
    sidebarPanel("POC Week 3",
                 conditionalPanel(
                   condition = "input.Tab == 'Plots'",
                 radioButtons("plot_type", "Select Plot :",
                              choices = c("Histogram of gdp per capita", 
                                          "Plot of income group by region", 
                                          "Plot of mortality rate under 5 by region",
                                          "Plot of mortality rate under 5 against GDP and region")))),
    
    mainPanel(
      tabsetPanel(id = 'Tab',
        tabPanel("Summary",
                 DT::dataTableOutput("tableSummary")),
        tabPanel("Plots",plotOutput('plot1',click = "user_click"),dataTableOutput("table"))
      )
    )
  ))

shinyApp(ui=ui,server = server)



