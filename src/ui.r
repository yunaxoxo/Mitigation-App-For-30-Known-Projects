# ----- Import Libraries -----
library(shiny)
library(shinydashboard)
library(DT)
library(here)
# ------ Reference Files -----
source("server.r")

dashboardPage(
  
  dashboardHeader(title = "Mitigation App"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("New Project", tabName = "new_project", icon = icon("plus")),
      menuItem("User", tabName = "user", icon = icon("user"))
    )
  ),
  
  dashboardBody(
    # Custom CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    # Custom CSS for body
    tags$body(
      class = "custom-skin",
      style = "min-height: 100vh; background-color: #f0f0f0;"
    ),
    
    # Custom CSS for dashboard
    tags$script(HTML("
  $(document).ready(function() {
    $('body').removeClass('skin-blue').addClass('custom-skin');
  });
")),
    
    # Home Tab Content
    tabItems(
      tabItem(tabName = "home",
              
              # Welcome / introduction card
              fluidRow(
                column(
                  width = 6,
                  offset = 3,
                  box(
                    title = tagList(icon("info-circle"), "About the Program"),
                    width = NULL,
                    div(
                      class = "welcome-card",
                      h1(id = "welcome_header", "Welcome!"),
                      p(id = "desc", "Optimize your environmental projects efficiently.")
                    )
                  )
                )
              ),
              
              uiOutput("recent_activity_ui"),
              
              # How to use & Contact info cards side by side
              fluidRow(
                box(
                  title = tagList(icon("list-ul"), "How to Use"),
                  width = 6,
                  div(
                    class = "how-to-steps",
                    div(class = "step",
                        span(class = "step-number", "1."),
                        span("Go to New Project Tab.")
                    ),
                    div(class = "step",
                        span(class = "step-number", "2."),
                        span("Select one or more projects.")
                    ),
                    div(class = "step",
                        span(class = "step-number", "3."),
                        span("Click Solve to see optimal solution and costs.")
                    ),
                    div(class = "step",
                        span(class = "step-number", "4."),
                        span("Check tableau and iteration details.")
                    )
                  )
                ),
                box(
                  title = tagList(
                    icon("address-card"), 
                    "Contact Information"
                  ),
                  width = 6,
                  div(
                    class = "contact_card",
                    p(id = "author", "Made by: Frendzo Charles C. Pelagio"),
                    p(id = "email", "Email: fcpelagio@up.edu.ph"),
                    p(id = "version", "Version: 1.0")
                  )
                )
              )
      ),
      tabItem(tabName = "new_project",
              fluidRow(
                box(
                  width = 12,
                  # Project Choices
                  uiOutput("checkbox_ui"),
                  div(class = "project_buttons",
                      actionButton("select_all", "Select All"),
                      actionButton("reset", "Reset"),
                      actionButton("solve", "Solve"),
                      # Search Bar
                      textInput("search_input", NULL, placeholder = "Type to search...")
                  )
                )
              ),
              #  Cost Span Text
              fluidRow(
                box(
                  title = tagList(icon("calculator"), "Cost Breakdown"),
                  width = 6,
                  div(class = "text_results", 
                      uiOutput("result_ui"),
                      p(id = "optimized_cost_label",
                        "The Optimized Cost ($) is: ", 
                        textOutput("optimized_cost", inline = TRUE),
                      )
                  )
                ),
                # Export Box
                box(
                  title = tagList(icon("download"), "Export Options"),
                  width = 6,
                  uiOutput("export_buttons_ui")
                )
              ),
              # Results Tab
            uiOutput("results_tabs_ui")
      ),
      # User Manual
      tabItem(
        tabName = "user",
        fluidRow(
          column(
            width = 10,
            offset = 1,
            box(
              title = tagList(icon("book"), span("User Manual")),
              width = NULL,      
              class = "box user-manual-box", 
              div(
                class = "basic-solution-scroll user-manual-scroll",  # Scrollable Content
                
                # Section: Overview
                h3("Overview", class = "user-manual-heading"),
                p("This Mitigation App helps you optimize environmental projects efficiently. You can select multiple projects, compute optimal costs, and export reports."),
                
                # Section: How to Use
                h3("How to Use", class = "user-manual-heading"),
                tags$ol(
                  class = "user-manual-list",
                  tags$li("Go to the 'New Project' tab."),
                  tags$li("Select one or more mitigation projects."),
                  tags$li("Click 'Solve' to compute the optimal solution."),
                  tags$li("Check the cost breakdown and final solution in the dashboard."),
                  tags$li("Use the 'Export Options' to save your results as PDF or image.")
                ),
                
                # Section: Tips
                h3("Tips", class = "user-manual-heading"),
                tags$ul(
                  class = "user-manual-list",
                  tags$li("Use the search box to filter projects quickly."),
                  tags$li("Click 'Select All' to quickly select all projects."),
                  tags$li("Reset button clears all selections."),
                  tags$li("Hover over tables to highlight rows for better readability.")
                ),
                
                # Section: FAQ
                h3("FAQ", class = "user-manual-heading"),
                div(
                  class = "user-manual-faq",
                  strong("Q: Can I solve multiple projects at once?"),
                  p("A: Yes, you can select multiple projects and click 'Solve'."),
                  strong("Q: How can I contact the developer?"),
                  p("A: See 'Contact Information' in the Home tab for email details.")
                )
              )
            )
          )
        )
      )
    )
  )
)
