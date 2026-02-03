library(shinyjs)

login_ui <- function(content) {
  tagList(
    
   
    tags$style(HTML("

      body.login-view {
        margin: 0 !important;
        padding: 0 !important;
        height:100%;
        overflow: hidden;
      }
      
      body.login-view .wrapper {
        min-height: 100vh !important;
      }


      body.login-view .content-wrapper {
        margin: 0 !important;
        padding: 0 !important;
        min-height: auto !important;
      }
      body.login-view .content-wrapper > .container-fluid {
        padding-left: 0 !important;
        padding-right: 0 !important;
      }
      
      body.login-view .row {
        margin-left: 0 !important;
        margin-right: 0 !important;
      }
      
      body.login-view .wrapper .main-sidebar,
      body.login-view .wrapper .left-side {
        display: none !important;
      }
      
      body.login-view .custom-topbar {
        display: none !important;
      }
      
      body.login-view .wrapper .content-wrapper,
      body.login-view .wrapper .right-side,
      body.login-view .wrapper .main-footer {
        margin-left: 0 !important;
        padding-top: 0 !important;
      }
      
      /* REMOVE FIXED HEADER GAP */
      body.login-view .wrapper {
        padding-top: 0 !important;
      }

      .login-container {
        height: 100vh;
      }
      
      /* LEFT SIDE (8 columns) */
      .login-left {
        position: relative;            /* REQUIRED */
        height:  100%;
        background-image: url('login_bg.svg');
        background-size: cover;
        background-position: center;
      }

      /* LOGO ON TOP OF BACKGROUND */
      .login-logo {
          position: absolute;
          top: 50%;
          left: 50%;
          transform: translate(-50%, -50%);
          z-index: 10;
        }

      .login-logo img {
          max-width: 800px; 
          width: 100%;
          height: auto;
          display: block;
        }

      /* RIGHT SIDE (4 columns) */
      .login-right {
        height:  100vh;
        display: flex;
        align-items: center;
        justify-content: center;
        background: #ffffff;
      }

      .login-card {
        width: 100%;
        max-width: 360px;
        padding: 32px;
      }
      
      
      .login-card .form-control {
        width: 100% !important;
        height: 46px;
        border-radius: 8px !important;
        align-items: center;
        justify-content: center;
      }
      
      .login-card .form-group {
        width: 100%;
        margin-bottom: 18px;
      }
      
      .login-card .login-btn {
        background-color: #43627D !important; 
        border-color: #43627D !important;
        color: #ffffff !important;
        border-radius: 8px !important;
      }
      .login-card .login-btn:hover,
      .login-card .login-btn:focus {
        background-color: #1f4fe0 !important;
        border-color: #1f4fe0 !important;
        color: #ffffff !important;
      }
      .login-card .login-btn:active {
        background-color: #173fb8 !important;
        border-color: #173fb8 !important;
      }
      .login-card .login-btn:disabled {
        opacity: 0.7;
      }

      .forgot-wrap {
        text-align: right;
        margin-bottom: 18px;
      }
      
      .forgot-link {
        font-size: 14px;
        color: #43627D;
        text-decoration: none;
      }
      
      .forgot-link:hover {
        text-decoration: underline;
      }
      
      /* Sign up */
      .signup-wrap {
        margin-top: 22px;
        text-align: center;
        font-size: 14px;
        color: #6c757d;
      }
      
      .signup-link {
        color: #43627D;
        font-weight: 500;
        text-decoration: none;
      }
      
      .signup-link:hover {
        text-decoration: underline;
      }
    ")),
    

    fluidRow(
      class = "login-container",
      
      ## ---- LEFT 8 COLUMNS ----
      column(
        width = 8,
        class = "login-left",
        
        # LOGO OVER BACKGROUND
        div(
          class = "login-logo",
          tags$img(
            src = "company_logo.svg",   # SVG WILL WORK
            alt = "Company Logo"
          )
        )
      ),
      column(
        width = 4,
        class = "login-right",
        content
      )
    )
  )
}
      
login_content <- function() {
        div(
          class = "login-card",
          uiOutput("login_notice"),
          h2("Welcome back"),
          p("Log in to continue"),
          
          textInput("login_user", "Username",
                    placeholder = "email@actalentservices.com"),
          passwordInput("login_pass", "Password"),
          
          div(
            class = "forgot-wrap",
            tags$a(
              href = "#",
              "Forgot password?",
              class = "forgot-link"
            )
          ),
          
          actionButton(
            "login_btn",
            "Log In",
            class = "btn login-btn",
            width = "30%"
          ),
          div(
            class = "signup-wrap",
            span("Don’t have an account? "),
            actionLink(
              "go_signup",
              "Sign up",
              class = "signup-link"
            )
          )
        )
}

signup_content <- function() {
  
  div(
    class = "login-card",
    
    h2("Sign up"),
    p("Sign up for free to access any of our product"),
    
    textInput(
      "signup_name",
      "Name",
      placeholder = "Your full name"
    ),
    
    textInput(
      "signup_email",
      "Email address",
      placeholder = "email@example.com"
    ),
    
    passwordInput(
      "signup_password",
      "Password"
    ),
    
    passwordInput(
      "signup_confirm",
      "Confirm password"
    ),
    checkboxInput(
      "agree_terms",
      label = HTML("I agree to the <a href='#' target='_blank'>Terms & Conditions</a>"),
      value = FALSE
    ),
    
    actionButton(
      "signup_btn",
      "Sign Up",
      class = "btn login-btn",
      width = "30%",
      disabled = TRUE
    ),
   
    div(
      class = "signup-wrap",
      span("Already have an account? "),
      actionLink("back_to_login", "Log in")
    )
  )
}

