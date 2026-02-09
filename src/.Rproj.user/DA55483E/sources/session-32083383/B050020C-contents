library(here)
library(rmarkdown)
library(webshot)
source(here("R", "datasets.r"))
source(here("R", "simplex.r"))

server <- function(input, output, session) {
  
  all_projects <- project_data$Option
  
  # Reactive values: for results, UI flags, and persistent activity log
  rv <- reactiveValues(
    showResult = FALSE,
    errorMessage = "",
    all_selected = NULL,
    enableExport = FALSE,
    activity_log = NULL
  )
  
  # Load previous activity log from disk (if exists)
  recent_activity_file <- here::here("data", "recent_activity.rds")
  
  if (file.exists(recent_activity_file)) {
    # Load only the most recent activity
    rv$activity_log <- list(readRDS(recent_activity_file))
  } else {
    rv$activity_log <- list()
  }
  
  filteredProjects <- reactive({
    req(input$selected_projects) # wait until user selects something
    
    project_data[
      all_projects %in% input$selected_projects, , drop = FALSE
    ]
    
  })
  
  # ------------------- REACTIVE DATA ---------------------------------------- #
  
  filteredProjects <- reactive({
    req(input$selected_projects)
    project_data[all_projects %in% input$selected_projects, , drop = FALSE]
  })
  
  selectedProjects <- reactive({
    req(filteredProjects())
    selectedOptions <- filteredProjects()
    cost <- selectedOptions["Cost"]
    non_numeric_cols <- selectedOptions[, sapply(selectedOptions, function(x) !is.numeric(x)), drop = FALSE]
    cbind(non_numeric_cols, cost)
  })
  
  initialTableau <- eventReactive(input$solve,{
    req(filteredProjects())
    set_up_initial_tableau(build_initial_matrix(filteredProjects()))
  })
  
  simplexResult <- eventReactive(input$solve, {
    req(initialTableau())
    Simplex(initialTableau())
  })
  
  finalTableau <- reactive({ simplexResult()$finalTableau })
  finalSol     <- reactive({ simplexResult()$finalSolution })
  basicSolutions <- reactive({ simplexResult()$basicSolutionIterations })
  iterations <- reactive({ simplexResult()$history })
  
  selectedProjectsWithUnits <- reactive({
    req(finalSol(), selectedProjects())
    selected <- selectedProjects()
    cost <- selected[, "Cost", drop = FALSE]
    vars <- finalSol()[, substr(colnames(finalSol()), 1,1) != "S", drop = FALSE]
    vars <- t(vars[, -ncol(vars), drop = FALSE]) # remove Z column
    costEach <- cost * vars
    dimnames(vars) <- list(NULL, "Number of Project Units")
    cbind(selected[, "Option", drop = FALSE], vars, costEach)
  })
  
  
  # ---------------------------------------------------------------------------#
  # ------------------------------ OUTPUT ------------------------------------ #
  
  
  output$selected_projects_with_units <- renderDT({
    req(rv$showResult)  # only compute the table if Solve button was pressed
    datatable(
      selectedProjectsWithUnits(),
      options = list(
        responsive = TRUE,
        scrollX = TRUE,      # horizontal scroll if wide
        scrollY = "400px"   # sets a fixed height of the table
      ),
      rownames = FALSE,
      width = "100%"
    )
  })
  
  output$tableau_output <- renderDT({
    req(rv$showResult) 
    datatable(
      initialTableau(),
      options = list(
        dom = 'lrtip',  # removed 'f' so search bar disappears
        responsive = TRUE,
        scrollX = TRUE,      # horizontal scroll if wide
        scrollY = "400px"   # sets a fixed height of the table
      ),
      rownames = FALSE,
      width = "100%"
    )
  })
  
  output$optimized_cost <- renderText({
    req(rv$showResult)  # only show if solve button was pressed
    result <- simplexResult()
    
    if (result$status == "Infeasible") {
      "The problem is infeasible"
    } else {
      result$Z
    }
  })
  
  
  output$final_tableau <- renderDT({
    req(rv$showResult)
    datatable(
      finalTableau(),
      options = list(
        dom = 'lrtip',  # removed 'f' so search bar disappears
        responsive = TRUE,
        scrollX = TRUE,      # horizontal scroll if wide
        scrollY = "400px"   # sets a fixed height of the table
      ),
      rownames = FALSE,
      width = "100%"
    )
  })
  
  output$final_solution <- renderDT({
    req(rv$showResult)  # only show if solve button was pressed
    datatable(
      finalSol(),
      options = list(
        responsive = TRUE,
        scrollX = TRUE,      # horizontal scroll if wide
        scrollY = "400px"   # sets a fixed height of the table
      ),
      rownames = FALSE,
      width = "100%"
    )
  })
  
  output$iterations_ui <- renderUI({
    req(rv$showResult)
    
    if(length(iterations()) == 0) return(NULL)        # nothing to show
    
    # create one tabPanel per iteration
    tabs <- lapply(seq_along(iterations()), function(i) {
      
      output[[paste0("basic_sol_", i)]] <- renderTable({
        req(basicSolutions())
        basicSolutions()[[i]]
      })
      
      tabPanel(
        title = paste("Iteration", i),
        div(
          class = "iteration-box",
          DT::datatable(iterations()[[i]], options = list(
            dom = 't',
            scrollX = TRUE, # horizontal scroll
            scrollY = "400px", # vertical scroll
            paging = FALSE # show all rows without pagination
          ),
          rownames = FALSE,
          width = "100%"
          )
        ),
        div(
          class = "basic-solution-container",
          tags$h4("Basic Solution"), # title
          # scrollable wrapper 
          div(
            class = "basic-solution-scroll",
            tableOutput(paste0("basic_sol_", i)) 
          ) 
        )
      )
    })
    
    # combine all tabs into a tabset
    div(
      id = "simplex_iterations",
      box(
        width = 12,
        do.call(tabsetPanel,c(list(type = "tabs"), tabs))
      )  
    )
  })
  
  
  # handle if no project is selected
  output$result_ui <- renderUI({
    # There is an error
    if (!is.null(rv$errorMessage) && rv$errorMessage != "") {
      return(
        div(
          class = "error-box",
          strong("Error: "),
          rv$errorMessage
        )
      )
    }
    
    # No Error
    else{
      if(rv$showResult){
        return(
          DTOutput("selected_projects_with_units")) 
      }
      # show only the selected
      else if (!rv$showResult){
        return(
            DT::datatable(
              selectedProjects(),
              options = list(
                responsive = TRUE,
                scrollX = TRUE,
                scrollY = "400px"
              ),
              rownames = FALSE,
              width = "100%"
            )
          )
      }
      # Other case
      else{
        return(NULL)
      }
    }
    
  })
  
  
  # dynamically filters choices
  
  output$checkbox_ui <- renderUI({
    
    search_text <- input$search_input
    
    # Filter all projects based on search
    if (is.null(search_text) || search_text == "") {
      filtered <- all_projects
    } else {
      filtered <- all_projects[grepl(search_text, all_projects, ignore.case = TRUE)]
    }
    
    # Ensure previously selected items are always included
    choices_to_display <- unique(c(rv$all_selected, filtered))
    
    # Render the checkbox
    checkboxGroupInput(
      inputId = "selected_projects",
      label = "Select mitigation projects:",
      choices = choices_to_display,
      selected = rv$all_selected
    )
    
  })
  
  output$export_buttons_ui <- renderUI({
    
    div(
      class = "export_buttons",
      actionButton(
        inputId = "pdf_button",
        label = "Save as pdf",
        disabled = !rv$enableExport 
      ),
      actionButton(
        inputId = "img_button",
        label = "Save as Image",
        disabled = !rv$enableExport
      ) 
    )
    
  })
  
  output$results_tabs_ui <- renderUI({
    if (!rv$showResult) return(NULL)
    
    fluidRow(
      box(
        title = tagList(icon("bar-chart"), "Results"),
        width = 12,
        collapsible = TRUE,
        tabsetPanel(
          tabPanel("Initial Tableau", DTOutput("tableau_output")),
          tabPanel("Iterations", uiOutput("iterations_ui")),
          tabPanel("Final Tableau", DTOutput("final_tableau")),
          tabPanel("Final Solution", DTOutput("final_solution"))
        )
      )
    )
  })
  
  output$recent_activity_ui <- renderUI({
    req(rv$activity_log)
    
    # If there’s no recent activity, return nothing
    if (length(rv$activity_log) == 0) return(NULL)
    
    act <- rv$activity_log[[1]] 
    fluidRow(
      box(
        title = tagList(icon("clock-rotate-left"), "Recent Activity"),
        width = 12,           # Full width of the page
        collapsible = TRUE,   # Optional: allow collapsing
        div(
          class = "recent-activity-box",
          div(
            class = "recent-activity-flex",
            
            # ---- LEFT PANEL ----
            div(
              class = "recent-activity-input-container",
              div(
                class = "recent-activity-status",
                paste("Status:", act$status)
              ),
              tags$ul(
                class = "recent-activity-input",
                lapply(act$projects, function(p) tags$li(p))
              )
            ),
            
            # ---- RIGHT PANEL ----
            div(
              class = "recent-activity-table-container",
              tableOutput("recent_activity_table")
            )
          )
        )
      )
    )
  })
  
  output$recent_activity_table <- renderTable({
    req(rv$activity_log)
    rv$activity_log[[1]]$solution
  }, rownames = TRUE, colnames = TRUE)
  
  # ---------------------------------------------------------------------------#
  # ----------------------------- OBSERVE ------------------------------------ #
  
  # Freeze selected values in the check boxes
  observe({
    rv$all_selected <-  input$selected_projects
  })
  
  # ------------------------- OBSERVE EVENT -----------------------------------#
  # ------------------------- RECENT ACTIVITY LOG ---------------------------- #
  
  # Log only when Solve is clicked
  observeEvent(input$solve, {
    if (length(input$selected_projects) == 0) {
      rv$errorMessage <- "Please select a project!"
      rv$showResult <- FALSE
      rv$enableExport <- FALSE
    } else {
      rv$errorMessage <- ""
      rv$showResult <- TRUE
      rv$enableExport <- TRUE
      
      solution_table <- if (simplexResult()$status == "Infeasible") {
        data.frame(Message = "No feasible solution")
      } else {
        selectedProjectsWithUnits()
      }
      
      # Create new activity entry
      new_entry <- list(
        timestamp = Sys.time(),
        projects = input$selected_projects,
        status = simplexResult()$status,
        solution = solution_table
      )
      
      # Replace the old log with new entry
      rv$activity_log <- list(new_entry)
      
      # Save immediately
      saveRDS(new_entry, recent_activity_file)
    }
  })
  
  
  # Select All button
  observeEvent(input$select_all, {
    
    # Select all currently visible + previous selections
    search_text <- input$search_input
    if (is.null(search_text) || search_text == "") {
      filtered <- all_projects
    } else {
      filtered <- all_projects[grepl(search_text, all_projects, ignore.case = TRUE)]
    }
    
    choices_to_select <- unique(c(rv$all_selected, filtered))
    
    updateCheckboxGroupInput(
      session,
      "selected_projects",
      selected = choices_to_select
    )
    
    rv$all_selected <- choices_to_select
    
  })
  
  # Reset button
  observeEvent(input$reset, {
    rv$showResult <- FALSE
    rv$enableExport <- FALSE
    rv$errorMessage <- NULL
    updateCheckboxGroupInput(
      session,
      "selected_projects",
      selected = character(0)  # clear selection
    )
  })
  
  # Select Projects (hide results; wait for user to press solve button)
  observeEvent(input$selected_projects,{
    rv$showResult <- FALSE # cleans the result when user selects another
    rv$errorMessage <- NULL # remove error message
    rv$enableExport <- FALSE # reset export options
  })
  
  # PDF export using PhantomJS
  observeEvent(input$pdf_button, {
    
    # Determine solution table (NULL if infeasible)
    solution_table <- if (simplexResult()$status == "Infeasible") NULL else selectedProjectsWithUnits()
    
    # Temporary HTML file
    html_file <- tempfile(fileext = ".html")
    
    rmarkdown::render(
      input = here::here("data", "export_template.rmd"),
      output_file = html_file,
      params = list(
        cost = simplexResult()$Z,
        status = simplexResult()$status,
        selected_projects = selectedProjects(),
        solution_table = solution_table
      ),
      envir = new.env(parent = globalenv()),
      quiet = TRUE
    )
    
    # Output PDF file
    pdf_file <- file.path(tempdir(), "Mitigation_Report.pdf")
    
    # Convert HTML to PDF via PhantomJS
    webshot::webshot(
      url = html_file,
      file = pdf_file,
      vwidth = 1200,
      vheight = 1600
    )
    
    # Preview file
    browseURL(pdf_file)
  })
  
  # Image export
  observeEvent(input$img_button, {
    
    solution_table <- if (simplexResult()$status == "Infeasible") NULL else selectedProjectsWithUnits()
    
    html_file <- tempfile(fileext = ".html")
    
    rmarkdown::render(
      input = here::here("data", "export_template.rmd"),
      output_file = html_file,
      params = list(
        cost = simplexResult()$Z,
        status = simplexResult()$status,
        selected_projects = selectedProjects(),
        solution_table = solution_table
      ),
      envir = new.env(parent = globalenv()),
      quiet = TRUE
    )
    
    # Output image file
    img_file <- file.path(tempdir(), "Mitigation_Report.png")
    
    # Convert HTML to IMG via PhantomJS
    webshot::webshot(
      url = html_file,
      file = img_file,
      vwidth = 1200,
      vheight = 1600
    )
    
    browseURL(img_file)
  })
 
}

