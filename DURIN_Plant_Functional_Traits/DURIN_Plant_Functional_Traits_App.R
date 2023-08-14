library(shiny)
library(DT)
library(dplyr)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Plant Functional Trait Data Viewer"),
  sidebarLayout(
    sidebarPanel(
      h3("Welcome to the DURIN Project Plant Functional Trait Data Viewer! Here, we can import our cleaned trait data from our OSF account, and do exploratory data visualization."),
      fileInput("file1", "Upload Plant Functional Trait .csv",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Table",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("project", "Select Overarching Project", choices = c("All", "DURIN", "DroughtNet")),
                     conditionalPanel(
                       condition = "input.project == 'DURIN'",
                       uiOutput("durin_explanatory_vars")
                     ),
                     conditionalPanel(
                       condition = "input.project == 'DroughtNet'",
                       uiOutput("droughtnet_explanatory_vars")
                     ),
                     conditionalPanel(
                       condition = "input.project == 'All'",
                       uiOutput("all_explanatory_vars")
                     ),
                     uiOutput("traits")
                   ),
                   mainPanel(
                     DT::dataTableOutput("contents")
                   )
                 )),
        tabPanel("Boxplot",
                 sidebarLayout(
                   sidebarPanel(
                     h3("Data Filter Options"),
                     selectInput("box_project", "Select Overarching Project", choices = c("All", "DURIN", "DroughtNet")),
                     conditionalPanel(
                       condition = "input.box_project == 'DURIN'",
                       uiOutput("box_durin_explanatory_vars")
                     ),
                     conditionalPanel(
                       condition = "input.box_project == 'DroughtNet'",
                       uiOutput("box_droughtnet_explanatory_vars")
                     ),
                     conditionalPanel(
                       condition = "input.box_project == 'All'",
                       uiOutput("box_all_explanatory_vars")
                     ),
                     h3("Plotting Options"),
                     selectInput("box_response_var", "Select Response Variable",
                                 choices = NULL),
                     selectInput("box_predictor_var", "Select Predictor Variable",
                                 choices = NULL),
                     selectInput("box_grouping_var", "Select Grouping Variable",
                                 choices = NULL),
                     radioButtons("transformation", "Transformation",
                                  choices = c("None" = "",
                                              "Natural Log" = "log",
                                              "Log10" = "log10"))
                   ),
                   mainPanel(
                     plotOutput("box_plot")
                   )
                 ))
      )
    )
  )
)

server <- function(input, output, session) {
  unfiltered_data <- reactiveVal()

  observeEvent(input$file1, {
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    unfiltered_data(read.csv(inFile$datapath))
  })

  observe({
    req(unfiltered_data())

    updateSelectInput(session, "box_response_var", choices = names(unfiltered_data())[19:25])
    updateSelectInput(session, "box_predictor_var", choices = names(unfiltered_data())[c(1:4,6:11)])
    updateSelectInput(session, "box_grouping_var", choices = c("", names(unfiltered_data())[c(1:4,6:11)]))
  })

  filtered_data <- reactive({
    data <- unfiltered_data()
    if (is.null(data))
      return(NULL)

    if (input$project == 'DURIN') {
      data <- data[!is.na(data$DURIN_plot), ]

      lapply(c('project', 'siteID', 'habitat', 'species', 'leaf_age'), function(var) {
        input_name <- paste0(var, '_filter')
        if (!is.null(input[[input_name]]) && any(input[[input_name]] != '')) {
          data <<- data[data[[var]] %in% input[[input_name]], ]
        }
      })
    } else if (input$project == 'DroughtNet') {
      data <- data[!is.na(data$DroughtNet_plot), ]

      lapply(c('project', 'siteID', 'ageClass', 'DroughtTrt', 'species', 'leaf_age'), function(var) {
        input_name <- paste0(var, '_filter')
        if (!is.null(input[[input_name]]) && any(input[[input_name]] != '')) {
          data <<- data[data[[var]] %in% input[[input_name]], ]
        }
      })
    } else if (input$project == 'All') {
      lapply(c('project', 'siteID', 'habitat', 'ageClass', 'DroughtTrt', 'species', 'leaf_age'), function(var) {
        input_name <- paste0(var, '_filter')
        if (!is.null(input[[input_name]]) && any(input[[input_name]] != '')) {
          data <<- data[data[[var]] %in% input[[input_name]], ]
        }
      })
    }

    if (!is.null(input$traits)) {
      data <<- data[, c(names(data)[1:18], input$traits)]
    }

    data
  })

  output$durin_explanatory_vars <- renderUI({
    req(input$project)

    lapply(c('project', 'siteID', 'habitat', 'species', 'leaf_age'), function(var) {
      checkboxGroupInput(paste0(var, '_filter'), paste0(var, ' Filter'),
                         choices = unique(unfiltered_data()[[var]]))
    })
  })

  output$droughtnet_explanatory_vars <- renderUI({
    req(input$project)

    lapply(c('project', 'siteID', 'ageClass', 'DroughtTrt', 'species', 'leaf_age'), function(var) {
      checkboxGroupInput(paste0(var, '_filter'), paste0(var, ' Filter'),
                         choices = unique(unfiltered_data()[[var]]))
    })
  })

  output$all_explanatory_vars <- renderUI({
    req(input$project)

    lapply(c('project', 'siteID', 'habitat', 'ageClass', 'DroughtTrt', 'species', 'leaf_age'), function(var) {
      checkboxGroupInput(paste0(var, '_filter'), paste0(var, ' Filter'),
                         choices = unique(unfiltered_data()[[var]]))
    })
  })

  output$traits <- renderUI({
    req(input$project)

    checkboxGroupInput('traits', 'Select Traits',
                       choices = names(unfiltered_data())[19:25])
  })

  output$box_durin_explanatory_vars <- renderUI({
    req(input$box_project)

    lapply(c('project', 'siteID', 'habitat', 'species', 'leaf_age'), function(var) {
      checkboxGroupInput(paste0(var, '_box_filter'), paste0(var, ' Filter'),
                         choices = unique(unfiltered_data()[[var]]))
    })
  })

  output$box_droughtnet_explanatory_vars <- renderUI({
    req(input$box_project)

    lapply(c('project', 'siteID', 'ageClass', 'DroughtTrt', 'species', 'leaf_age'), function(var) {
      checkboxGroupInput(paste0(var, '_box_filter'), paste0(var, ' Filter'),
                         choices = unique(unfiltered_data()[[var]]))
    })
  })

  output$box_all_explanatory_vars <- renderUI({
    req(input$box_project)

    lapply(c('project', 'siteID', 'habitat', 'ageClass', 'DroughtTrt', 'species', 'leaf_age'), function(var) {
      checkboxGroupInput(paste0(var, '_box_filter'), paste0(var, ' Filter'),
                         choices = unique(unfiltered_data()[[var]]))
    })
  })

  output$contents <- DT::renderDataTable({
    filtered_data()
  })

  output$box_plot <- renderPlot({
    req(input$update)

    data <- unfiltered_data()

    if (input$box_project == 'DURIN') {
      data <- data[!is.na(data$DURIN_plot), ]

      lapply(c('project', 'siteID', 'habitat', 'species', 'leaf_age'), function(var) {
        input_name <- paste0(var, '_box_filter')
        if (!is.null(input[[input_name]]) && any(input[[input_name]] != '')) {
          data <<- data[data[[var]] %in% input[[input_name]], ]
        }
      })
    } else if (input$box_project == 'DroughtNet') {
      data <- data[!is.na(data$DroughtNet_plot), ]

      lapply(c('project', 'siteID', 'ageClass', 'DroughtTrt', 'species', 'leaf_age'), function(var) {
        input_name <- paste0(var, '_box_filter')
        if (!is.null(input[[input_name]]) && any(input[[input_name]] != '')) {
          data <<- data[data[[var]] %in% input[[input_name]], ]
        }
      })
    } else if (input$box_project == 'All') {
      lapply(c('project', 'siteID', 'habitat', 'ageClass', 'DroughtTrt', 'species', 'leaf_age'), function(var) {
        input_name <- paste0(var, '_box_filter')
        if (!is.null(input[[input_name]]) && any(input[[input_name]] != '')) {
          data <<- data[data[[var]] %in% input[[input_name]], ]
        }
      })
    }

    response_var <- input$box_response_var
    if (input$transformation != '') {
      response_var <- paste0(input$transformation, "(", response_var, ")")
    }

    ggplot(data, aes_string(x = input$box_predictor_var, y = response_var,
                            fill = input$box_grouping_var)) +
      geom_boxplot()
  })
}

shinyApp(ui, server)
