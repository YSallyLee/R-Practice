# Yen Jo (Sally) Lee


library(shiny)
library(tidyverse)
library(DT)
library(bslib)
library(broom)

source("./intro_html.R")


## Read in Data ------------------

energy_year <- readRDS("../data/energy_year.rds")

## Transform Data to Factors
energy_year <- energy_year %>%
  mutate(
    Ward = as.factor(Ward),
    Report_Year = as.factor(Report_Year),
    Type_SS = as.factor(Type_SS),
    Type_EPA = as.factor(Type_EPA),
    Metered_Energy = as.factor(Metered_Energy),
    Metered_Water = as.factor(Metered_Water),
    Era = case_when(
      Built < 1900 ~ "Pre-1900",
      Built < 1951 ~ "Early-Mid 20th",
      Built < 2000 ~ "Late 20th",
      Built < 2011 ~ "Aughts",
      TRUE ~ "Teens and later"
    ) %>% 
      factor(levels = c("Pre-1900", "Early-Mid 20th", "Late 20th", "Aughts", "Teens and later"))
  )

## Create t.test function

t_test_summary <- function(data_vector, null_value) {
  t_test_result <- t.test(data_vector, mu = null_value)
  
  tidy_result <- broom::tidy(t_test_result)
  
  tibble(
    Estimate = tidy_result$estimate,
    Null_Value = null_value,
    P_Value = tidy_result$p.value,
    Lower_CI = tidy_result$conf.low,
    Upper_CI = tidy_result$conf.high
  )
}

###
### Enter Business Logic before this line
###

###
## Begin User Interface Section ----------------
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "minty"),
   titlePanel("Analyzing Building Energy Performance Data"),

  navbarPage(
    title = "",
    position = "static-top",
    
# Introduction Tab
    tabPanel(
      title = "Introduction",
      fluidRow(
        column(12,intro_content)
        )
      ),
    
# Univariate Tab
tabPanel(
  title = "Univariate",
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Variable?",
                  choices = names(energy_year),
                  selected = "Energy_Star_Score"),
      checkboxInput("log_transform", "Log Transform?",
                    value = FALSE),
      checkboxInput("flip_coords", "Flip Coordinates on Factors?",
                    value = FALSE),
      sliderInput("num_bins", "Number of Bins",
                  min = 1, max = 100, value = 40),
      numericInput("null_value", "Null Value for t-test",
                   value = 0,
                   min = 0,
                   max = max(energy_year$Electricity_Grid_Usage, na.rm = TRUE)),
      checkboxGroupInput("years", "Select Years",
                         choices = sort(unique(energy_year$Report_Year)),
                         selected = "2022")
    ),
    mainPanel(
      plotOutput("single_var_plot"),
      tableOutput("t_test_results")
    )
  )
),
    
# Bivariate Tab
tabPanel(
  title = "Bivariate",
  sidebarLayout(
    sidebarPanel(
      selectInput("x_var", "X Variable",
                  choices = names(energy_year),
                  selected = "Source_EUI"),
      checkboxInput("log_x", "Log Transform?",
                    value = FALSE),
      selectInput("y_var", "Y Variable",
                  choices = names(energy_year),
                  selected = "Site_EUI"),
      checkboxInput("log_y", "Log Transform?",
                    value = FALSE),
      checkboxInput("add_lm", "Fit Linear Model?",
                    value = FALSE),
      checkboxInput("add_non_linear", "Fit a non-linear Smoother?",
                    value = FALSE),
      checkboxGroupInput("years", "Select Years",
                         choices = sort(unique(energy_year$Report_Year)),
                         selected = "2022")
    ),
    mainPanel(
      plotOutput("bivariate_plot"),
      verbatimTextOutput("lm_summary"),
      uiOutput("diagnostic_plots")
    )
  )
),


# Data Table Tab
tabPanel(
  title = "Data Table",
  fluidRow(
    column(12,
           checkboxInput("numeric_only", "Numeric Only?", value = FALSE),
           DT::dataTableOutput("data_table")
        )
      )
    )
  )
) #end of ui



server <- function(input, output, session) {
  
# Reactive element for univariate analysis
  df_year <- reactive({
    req(input$variable, input$years)
    
# Check if variable is numeric or factor
    validate(
      need(is.numeric(energy_year[[input$variable]]) || is.factor(energy_year[[input$variable]]),
           "Please select a numeric or factor variable")
    )
    
# Check log transform validity
    if(input$log_transform) {
      validate(
        need(is.numeric(energy_year[[input$variable]]), 
             "Log transform can only be applied to numeric variables"),
        need(all(energy_year[[input$variable]] > 0, na.rm = TRUE), 
             "Log transform cannot be applied to values <= 0")
      )
    }
    
    temp <- energy_year %>%
      select(Report_Year, all_of(input$variable)) %>%
      filter(Report_Year %in% input$years,
             !!sym(input$variable) != 0)
    
    validate(
      need(nrow(temp) > 0, "No non-zero values found for selected variable")
    )
    
    temp
  })
  
# Reactive element for bivariate analysis
  df_year_b <- reactive({
    req(input$x_var, input$y_var, input$years)
    
# Check if variables are numeric or factor
    validate(
      need(is.numeric(energy_year[[input$x_var]]) || is.factor(energy_year[[input$x_var]]),
           "X variable must be numeric or factor"),
      need(is.numeric(energy_year[[input$y_var]]) || is.factor(energy_year[[input$y_var]]),
           "Y variable must be numeric or factor")
    )
    
# Check log transform validity for X
    if(input$log_x) {
      validate(
        need(is.numeric(energy_year[[input$x_var]]), 
             "Log transform can only be applied to numeric X variables"),
        need(all(energy_year[[input$x_var]] > 0, na.rm = TRUE), 
             "Log transform cannot be applied to X values <= 0")
      )
    }
    
# Check log transform validity for Y
    if(input$log_y) {
      validate(
        need(is.numeric(energy_year[[input$y_var]]), 
             "Y variable is a factor so can't be logged")
        # need(all(energy_year[[input$y_var]] > 0, na.rm = TRUE), 
        #      "Log transform cannot be applied to Y values <= 0")
      )
    }
    
    temp <- energy_year %>%
      select(Report_Year, all_of(c(input$x_var, input$y_var))) %>%
      filter(Report_Year %in% input$years,
             !!sym(input$x_var) != 0,
             !!sym(input$y_var) != 0)
    
    validate(
      need(nrow(temp) > 0, "No non-zero values found for selected variables")
    )
    
    temp
  })
  
# Reactive element for linear model
  lmout <- reactive({
    req(input$add_lm, input$x_var, input$y_var)
    
    validate(
      need(is.numeric(energy_year[[input$y_var]]), 
           "Please Choose Numeric Variable for Y")
    )
    
    df <- df_year_b()
    
    x_data <- df[[input$x_var]]
    y_data <- df[[input$y_var]]
    
    if(input$log_x) x_data <- log(x_data)
    if(input$log_y) y_data <- log(y_data)
    
    lm(y_data ~ x_data)
  })
  
# Single variable plot using df_year
  output$single_var_plot <- renderPlot({
    df <- df_year()
    
    pl <- ggplot(df, aes(x = !!sym(input$variable)))
    
    if (is.numeric(df[[input$variable]])) {
      pl <- pl + geom_histogram(bins = input$num_bins)
      
      if (input$log_transform) {
        pl <- pl + scale_x_log10()
      }
    } else {
      pl <- pl + geom_bar()
      
      if (input$flip_coords) {
        pl <- pl + coord_flip()
      }
    }
    
    pl + facet_wrap(~Report_Year)
  })
  
# T-test results
  output$t_test_results <- renderTable({
    validate(
      need(is.numeric(energy_year[[input$variable]]), 
           "Variable is non-numeric so no t-test.")
    )
    
    df <- df_year()
    
    if (input$log_transform) {
      df[[input$variable]] <- log(df[[input$variable]])
    }
    
    t_test_summary(df[[input$variable]], input$null_value)
  })
  
  output$model_outputs <- renderUI({
    if (!input$add_lm && !input$add_non_linear) {
      return(NULL)
    }
    
    tagList(
      if (input$add_lm) {
        tagList(
          verbatimTextOutput("lm_summary"),
          plotOutput("residual_plot"),
          plotOutput("qq_plot")
        )
      }
    )
  })
# Bivariate plot using df_year_b
  output$bivariate_plot <- renderPlot({ 

        df <- df_year_b()
    
    isnx <- is.numeric(df[[input$x_var]])
    isny <- is.numeric(df[[input$y_var]])
    
    if (isnx && isny) {
      pl <- ggplot(df, aes(x = !!sym(input$x_var), 
                           y = !!sym(input$y_var), 
                           color = Report_Year)) + geom_point(alpha = 0.6)
      
      if (input$log_x) pl <- pl + scale_x_log10()
      if (input$log_y) pl <- pl + scale_y_log10()
      if (input$add_lm) pl <- pl + geom_smooth(method = "lm", se = FALSE)

      
    } else if (isnx) {
      pl <- ggplot(df, aes(x = !!sym(input$x_var), 
                           y = !!sym(input$y_var))) +
        geom_boxplot()
      
      if (input$log_x) pl <- pl + scale_x_log10()
      
    } else if (isny) {
      pl <- ggplot(df, aes(x = !!sym(input$x_var), 
                           y = !!sym(input$y_var))) +
        geom_boxplot()
      
      if (input$log_y) pl <- pl + scale_y_log10()
      
    } else {
      pl <- ggplot(df, aes(x = !!sym(input$x_var), 
                           y = !!sym(input$y_var))) +
        geom_jitter(width = 0.2, height = 0.2)
    }
    
    pl
  })
  
  
# Linear model outputs
  output$lm_summary <- renderPrint({
    req(input$add_lm)
    if (!input$add_lm) return(NULL)
    summary(lmout())
  })
  
  output$diagnostic_plots <- renderUI({
    req(input$x_var, input$y_var, input$add_lm)
    
    if (!input$add_lm) return(NULL)
    
    card(layout_column_wrap(
        width = 1/2,
        plotOutput("residual_plot"),
        plotOutput("qq_plot")
      )
    )
  })

# Residual plot  
  output$residual_plot <- renderPlot({
    req(input$add_lm)
    
    model <- lmout()
    
    plot_data <- tibble(
      fitted = fitted(model),
      residuals = residuals(model)
    )
    
    ggplot(plot_data, aes(x = fitted, y = residuals)) +
      geom_point() +
      geom_hline(yintercept = 0) +
      labs(title = "Residuals vs Fitted")
  })
  
#QQ plot
  output$qq_plot <- renderPlot({
    req(input$add_lm)
    
    model <- lmout()
    
    ggplot(tibble(residuals = residuals(model)), 
           aes(sample = residuals)) +
      geom_qq() +
      geom_qq_line() +
      labs(title = "QQ Plot")
  })

  
# Data table
  
  output$data_table <- DT::renderDataTable({
    if (input$numeric_only) {
      numeric_cols <- sapply(energy_year, is.numeric)
      datatable(energy_year[, numeric_cols, drop = FALSE],
                options = list(pageLength = 20),
                filter = 'top')
    } else {
      datatable(energy_year,
                options = list(pageLength = 20),
                filter = 'top')
    }
  })

}

shinyApp(ui, server)
