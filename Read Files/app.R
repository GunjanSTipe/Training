library(shiny)
library(shinyFiles)
library(DT)
source("ReadFileMiddleware.R")

volumes <- c(Home = fs::path_home(),
             D = "D:/")

ui <- fluidPage(
  shinyDirButton(
    id = "dir",
    label = "Choose Folder",
    title = "Select data folder"
  ),
  uiOutput("file_section"),
  DT::DTOutput("adam_table")
)

server <- function(input, output, session) {
  shinyDirChoose(
    input,
    id = "dir",
    roots = volumes,
    session = session
  )
  
  path <- reactive({
    req(input$dir)
    parseDirPath(volumes, input$dir)
  })
  
  files <- reactive({
    req(path())
    list.files(path(), full.names = TRUE)
  })
  
  output$file_section <- renderUI({
    req(files())
    selectInput(
      "files",
      "Select file",
      choices = files(),
      multiple = FALSE
    )
  })
  
  dataset <- reactive({
    req(input$files)
    read_file(input$files)
  })
  
  output$adam_table <- DT::renderDT({
    dataset()
  })
  
}

shinyApp(ui, server)
