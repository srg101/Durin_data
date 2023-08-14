library(shiny)
library(lme4)
library(emmeans)


# Create a user interface for the App
ui <- fluidPage(
  titlePanel("DURIN Project Plant Functional Trait Data Viewer"),
  # Create a landing page, where we import the .csv file and conduct the filters of the dataset
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload Plant Functional Trait .csv"),
      # Provide an option to filter the dataset to our DURIN and DroughtNet Experimental Designs
      selectInput("project", "Select Overarching Project", choices = c("All", "DURIN", "DroughtNet"), selected = "All"),
      # If DURIN is selected, only display these filtering options
      conditionalPanel(
        condition = "input.project == 'DURIN'",
        checkboxGroupInput("project_filter", "Project", choices = NULL),
        checkboxGroupInput("siteID_filter", "Site ID", choices = NULL),
        checkboxGroupInput("habitat_filter", "Habitat", choices = NULL),
        checkboxGroupInput("species_filter", "Species", choices = NULL),
        checkboxGroupInput("leaf_age_filter", "Leaf Age", choices = NULL)
      ),
      # If DroughtNet is selected, only display these filtering options
      conditionalPanel(
        condition = "input.project == 'DroughtNet'",
        checkboxGroupInput("project_filter", "Project", choices = NULL),
        checkboxGroupInput("siteID_filter", "Site ID", choices = NULL),
        checkboxGroupInput("ageClass_filter", "Age Class", choices = NULL),
        checkboxGroupInput("DroughtTrt_filter", "Drought Treatment", choices = NULL),
        checkboxGroupInput("species_filter", "Species", choices = NULL),
        checkboxGroupInput("leaf_age_filter", "Leaf Age", choices = NULL)
      ),
      # If ALL is selected, display a combination of variables from the DURIN and DroughtNet experimental designs
      conditionalPanel(
        condition = "input.project == 'All'",
        checkboxGroupInput("project_filter", "Project", choices = NULL),
        checkboxGroupInput("siteID_filter", "Site ID", choices = NULL),
        checkboxGroupInput("habitat_filter", "Habitat", choices = NULL),
        checkboxGroupInput("ageClass_filter", "Age Class", choices = NULL),
        checkboxGroupInput("DroughtTrt_filter", "Drought Treatment", choices = NULL),
        checkboxGroupInput("species_filter", "Species", choices = NULL),
        checkboxGroupInput("leaf_age_filter", "Leaf Age", choices = NULL)
      )
    ),
    # Add welcome text
    mainPanel(
      textOutput("welcome_text"),
      # Set up tabs for our different data table, data plotting and data analysis actions
      tabsetPanel(
        # Set up a tab for displaying the data table after our selections have been accounted for
        tabPanel(
          title = 'Data Table',
          tableOutput('data_table')
        ),
        # Set up a boxplot visualization tab using our filter selections on the main page
        tabPanel(
          title = 'Boxplot',
          selectInput('response', 'Response Variable', c('plant_height', 'bulk_nr_leaves', 'wet_mass_g', 'leaf_thickness_1_mm', 'leaf_thickness_2_mm', 'leaf_thickness_3_mm', 'dry_mass_g', 'stomatal_conductance(gs)', 'spectromery')),
          selectInput('predictor', 'Predictor Variable', c('envelope_ID', 'day', 'month', 'year', 'siteID', 'species', 'project', 'habitat', 'plot_nr', 'DURIN_plot', 'ageClass', 'DroughtTrt', 'DroughtNet_plot', 'plant_nr', 'leaf_nr', 'leaf_age')),
          selectInput('grouping', 'Grouping Variable', c('envelope_ID', 'day', 'month', 'year', 'siteID', 'species', 'project', 'habitat', 'plot_nr', 'DURIN_plot', 'ageClass', 'DroughtTrt', 'DroughtNet_plot', 'plant_nr', 'leaf_nr')),
          radioButtons('transformation','Transformation:', c('Raw'='raw','Log'='log')),
          actionButton('create_boxplot','Create Plot'),
          plotOutput('boxplot')
        ),
        # Set up a scatterplot visualization tab using our filter selections on the main page
        tabPanel(
          title = 'Scatterplot',
          selectInput('response1','Response Variable 1:', c('plant_height','bulk_nr_leaves','wet_mass_g','leaf_thickness_1_mm','leaf_thickness_2_mm','leaf_thickness_3_mm','dry_mass_g','stomatal_conductance(gs)','spectromery')),
          selectInput('response2','Response Variable 2:', c('plant_height','bulk_nr_leaves','wet_mass_g','leaf_thickness_1_mm','leaf_thickness_2_mm','leaf_thickness_3_mm','dry_mass_g','stomatal_conductance(gs)','spectromery')),
          selectInput('predictor_scatterplot','Predictor Variable:', c('envelope_ID','day','month','year','siteID','species','project','habitat','plot_nr','DURIN_plot','ageClass','DroughtTrt','DroughtNet_plot','plant_nr','leaf_nr')),
          selectInput('grouping_scatterplot','Grouping Variable:', c('envelope_ID','day','month','year','siteID','species','project','habitat','plot_nr','DURIN_plot','ageClass','DroughtTrt','DroughtNet_plot','plant_nr','leaf_nr')),
          actionButton('create_scatterplot', 'Create Plot'),
          plotOutput('scatterplot')
        ),
        # Add a linear mixed effects analysis tab based on our filtering selections on the main page
        tabPanel(
          title = 'Linear Mixed Effects Analysis',
          textAreaInput('formulas', 'Mixed Effects Model Formulas', rows = 5),
          helpText("Enter one or more mixed effects model formulas, one per line, in the standard R formula format, which is response ~ predictor + (1|group). The response variable is the dependent variable, the predictor variable is the independent variable, and the group variable is the random effect."),
          actionButton('run_analysis', 'Run Analysis'),
          tableOutput('model_comparison_table'),
          verbatimTextOutput('best_model_output'),
          plotOutput('best_model_plot'),
          tableOutput('pairwise_comparisons_table'),
          plotOutput('diagnostic_plots')
        )
      )
    )
  )
)


# Create the server code the controls the UI components
server <- function(input, output, session) {
  data <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath)
  })

  # Create the welcome text
  output$welcome_text <- renderText({
    "Welcome to the DURIN Project Plant Functional Trait Data Viewer! Here, we can import our cleaned trait data from our OSF account, and do exploratory data visualization."
  })

  # On the main page, set the options for filtering the datasets
  filtered_data <- reactive({
    data <- data()
  # Define as DURIN if there are not NA values in the DURIN_plot column
    if (input$project == "DURIN") {
      data <- data[!is.na(data$DURIN_plot),]
  # Else, define as DroughtNet if there are no NA's in the DroughtNet_plot column
    } else if (input$project == "DroughtNet") {
      data <- data[!is.na(data$DroughtNet_plot),]
    }

    if (!is.null(input$project_filter)) {
      data <- data[data$project %in% input$project_filter,]
    }

    if (!is.null(input$siteID_filter)) {
      data <- data[data$siteID %in% input$siteID_filter,]
    }

    if (!is.null(input$habitat_filter)) {
      data <- data[data$habitat %in% input$habitat_filter,]
    }

    if (!is.null(input$ageClass_filter)) {
      data <- data[data$ageClass %in% input$ageClass_filter,]
    }

    if (!is.null(input$DroughtTrt_filter)) {
      data <- data[data$DroughtTrt %in% input$DroughtTrt_filter,]
    }

    if (!is.null(input$species_filter)) {
      data <- data[data$species %in% input$species_filter,]
    }

    if (!is.null(input$leaf_age_filter)) {
      data <- data[data$leaf_age %in% input$leaf_age_filter,]
    }

    return(data)
  })

  # For key variables: project, siteID, habitat, ageClass, DroughtTrt, species and leaf_age rovide the unique levels within each explanatory factor as section checkboxes.
  observeEvent(data(), {
    updateCheckboxGroupInput(session, "project_filter", choices = unique(data()$project))
    updateCheckboxGroupInput(session, "siteID_filter", choices = unique(data()$siteID))
    updateCheckboxGroupInput(session, "habitat_filter", choices = unique(data()$habitat))
    updateCheckboxGroupInput(session, "ageClass_filter", choices = unique(data()$ageClass))
    updateCheckboxGroupInput(session, "DroughtTrt_filter", choices = unique(data()$DroughtTrt))
    updateCheckboxGroupInput(session, "species_filter", choices = unique(data()$species))
    updateCheckboxGroupInput(session, "leaf_age_filter", choices = unique(data()$leaf_age))
  })

  # Render a table of data based on the selection options chosen from those defined above.
  output$data_table <- renderTable({
    filtered_data()
  })

  # Create the boxplot tab using the filtered data from the main page
  observeEvent(input$create_boxplot,{
    req(filtered_data())

  # Define response, predictor and grouping variables to be used for the plots
    response_var <- input$response
    predictor_var <- input$predictor
    grouping_var <- input$grouping

  # If the user chooses a transformation option, such as log transform, change the figure text
    if (input$transformation == 'log') {
      response_var <- paste0('log(',response_var,')')
      ylab_text <- paste0('log(',input$response,')')

  # Render a boxplot figure using the filtered data and chosen response, predictor and grouping variables. The grouping variable is also used to facet the plots.
      output$boxplot<-renderPlot({
        ggplot(filtered_data(), aes_string(x = predictor_var, y = response_var)) +
          geom_boxplot(aes_string(fill = grouping_var)) +
          facet_wrap(as.formula(paste0('~',grouping_var))) +
          labs(x = predictor_var, y = ylab_text)
      })
    } else {

      output$boxplot<-renderPlot({
        ggplot(filtered_data(), aes_string(x = predictor_var, y = response_var)) +
          geom_boxplot(aes_string(fill = grouping_var)) +
          facet_wrap(as.formula(paste0('~',grouping_var))) +
          labs(x = predictor_var, y = response_var)
      })
    }
  })

  # The scatterplot graphs using the filtered dataset
  observeEvent(input$create_scatterplot,{
    req(filtered_data())

  # Create the two numeric response variables, and provide options for predictor and grouping variables
    response1_var <- input$response1
    response2_var <- input$response2
    predictor_scatterplot_var <- input$predictor_scatterplot
    grouping_scatterplot_var <- input$grouping_scatterplot

  # Create a scatterplot using the defiend response, predictor and grouping variables.
    output$scatterplot<-renderPlot({
      ggplot(filtered_data(), aes_string(x=response1_var,y=response2_var,color=predictor_scatterplot_var))+
        geom_point()+
        facet_wrap(as.formula(paste0('~',grouping_scatterplot_var)))+
        labs(x=response1_var,y=response2_var)
    })
  })



  # Install relevant packages within the server code so that they can run.
  library(lme4)
  library(lmerTest)
  library(emmeans)
  library(multcomp)

  # Server code for Linear Mixed Effects Analysis tab, using the filtered dataset

  observeEvent(input$run_analysis, {
    req(input$formulas)
    req(filtered_data())

    model_formulas <- unlist(strsplit(input$formulas, "\n"))
 # Create a list of the models to populate
    models <- list()


    for (i in seq_along(model_formulas)) {
      model_formula <- as.formula(model_formulas[i])
      response_var <- all.vars(model_formula)[1]

      if (!response_var %in% colnames(filtered_data())) {
        stop(paste("Error: Response variable", response_var, "not found in data"))
      }

      model_name <- paste("model", i, sep = "")
      models[[model_name]] <- lmer(model_formula, data = filtered_data())
    }

    aicc_values <- sapply(models, function(model) {
      AIC(model) + (2 * length(fixef(model)) * (length(fixef(model)) + 1)) / (nrow(filtered_data()) - length(fixef(model)) - 1)
    })

    min_aicc_value <- min(aicc_values)
    best_model_name <- names(which.min(aicc_values))
    best_model <- models[[best_model_name]]

    model_comparison_results <- data.frame(
      model = names(aicc_values),
      AICc = aicc_values,
      delta_AICc = aicc_values - min_aicc_value,
      weight = exp(-0.5 * (aicc_values - min_aicc_value)) / sum(exp(-0.5 * (aicc_values - min_aicc_value)))
    )

    output$model_comparison_table <- renderTable({
      model_comparison_results
    })

    output$best_model_output <- renderPrint({
      cat("Best Model:", best_model_name, "\n")
      cat("Formula:", deparse(formula(best_model)), "\n")
      cat("AICc:", aicc_values[best_model_name], "\n")
      cat("\nANOVA:\n")
      print(anova(best_model))
      cat("\nSummary:\n")
      print(summary(best_model))
    })


    # Create table of the pairwise comparison table, with estimated means, standard errors etc
    # Currently, this doesn't work. Not sure why.
    # output$pairwise_comparisons_table <- renderTable({
    #   fixed_effects <- all.vars(update(formula(best_model), . ~ . - (1|group)))
    #
    #   if (length(fixed_effects) == 1) {
    #     fixed_effect_var <- fixed_effects[1]
    #
    #     emm <- emmeans(best_model, specs = fixed_effect_var)
    #
    #     contrast_matrix <- contrMat(levels(emm), method = "Tukey")
    #
    #     glht_result <- glht(best_model, linfct = mcp(fixed_effect_var = contrast_matrix))
    #
    #     summary(glht_result)$test
    #   } else {
    #     stop("Error: Cannot generate pairwise comparisons for model with more than one fixed effect")
    #   }
    # })

    # Create a series of diagnostic plots for the best model
    # Currently this doesn't seen to work as the 'resid' plot can't find the appropriate random effects structure, i.e. not 'group'
  #   output$diagnostic_plots <- renderPlot({
  #     par(mfrow = c(2, 2))
  #
  #     plot(resid(best_model), main = "Residuals Plot")
  #
  #     qqnorm(resid(best_model), main = "QQNorm Plot")
  #
  #     hist(resid(best_model), main = "Normality Plot")
  #
  #     plot(resid(best_model) ~ ranef(best_model)$group[,1], main = "Independence Plot")
  #
  #     par(mfrow = c(1, 1))
  #   })
   })

}

shinyApp(ui = ui, server = server)
