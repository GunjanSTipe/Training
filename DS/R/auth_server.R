auth_server <- function(input, output, session) {
  users_rv <- reactiveVal(
    list(
      "gunjan@actalentservices.com" = "1234",
      "Gunjan" = "1234"
    )
  )
  
  authed <- reactiveVal(FALSE)
  
  
  observeEvent(input$login_user, {
    output$login_notice <- renderUI({ NULL })
  }, ignoreInit = TRUE)
  
  observeEvent(input$login_pass, {
    output$login_notice <- renderUI({ NULL })
  }, ignoreInit = TRUE)
  
  output$login_notice <- renderUI({ NULL })
  
  observeEvent(input$login_btn, {
    
    users <- users_rv()
    
    if (
      nzchar(input$login_user) &&
      nzchar(input$login_pass) &&
      !is.null(users[[input$login_user]]) &&
      users[[input$login_user]] == input$login_pass
    ) {
      authed(TRUE)
      output$login_notice <- renderUI({NULL})
    } else {
      
      output$login_notice <- renderUI({
        div(
          
          class = "shiny-notification",
          style = "
                  position: relative;
                  background-color: #FFD4D4;
                  color: #000000;
                  padding: 10px 14px;
                  border-radius: 0px;
                  margin-bottom: 12px;
                  font-size: 14px;
                ",
          icon("exclamation-circle"),
          "Invalid credentials"
          
        )
      })
      
    }
  })
  
  observeEvent(input$logoutBtn, {
    authed(FALSE)
  })
  signup_success <- reactiveVal(FALSE)
  observeEvent(input$signup_btn, {
    
    req(
      input$signup_name,
      input$signup_email,
      input$signup_password,
      input$signup_confirm
    )
    
    if (input$signup_password != input$signup_confirm) {
      showNotification("Passwords do not match", type = "error")
      return()
    }
    
    users <- users_rv()
    
    if (!is.null(users[[input$signup_email]])) {
      showNotification("User already exists", type = "error")
      return()
    }
    
    # Append new user
    users[[input$signup_email]] <- input$signup_password
    users_rv(users)
    
    showNotification("Signup successful! Please log in.", type = "message")
    
    # Switch back to login page
    authed(FALSE)
    session$sendInputMessage("back_to_login", list(value = TRUE))
    signup_success(TRUE)
  })
  return(list(
    authed = authed,
    signup_success = signup_success
  ))
}