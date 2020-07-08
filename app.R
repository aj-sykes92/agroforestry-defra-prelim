
# source scenario functions
source("scenario-functions/row-agf-functions.R")
source("scenario-functions/shelter-belt-functions.R")
source("scenario-functions/fenceline-agf-functions.R")
source("scenario-functions/hedge-functions.R")

# source utility functions
source("scenario-functions/univ-functions.R")
source("scenario-functions/post-and-plot-functions.R")

# build ui
ui <- fluidPage(
  # app title ----
  titlePanel("Marginal Abatement Cost Model for Agroforestry"),
  # app subtitle
  h4(HTML("Prepared for Defra Project 'Clean Growth through Sustainable intensification'")),
  
  # busy spinner
  shinybusy::add_busy_spinner(position = "top-right", height = "80px", width = "80px"),
  
  # sidebar layout
  sidebarLayout(
    
    # sidebar panel
    sidebarPanel(
      
      #########################
      # global options
      #########################
      titlePanel(title = "Global options"),
      
      # inputs
      sliderInput(inputId = "discount_rate",
                  label = "Discount rate (%)",
                  min = 0.1, max = 10, value = 3.5, step = 0.1),
      
      # update
      actionButton(inputId = "run_all",
                   label = "Update all simulations"),
      
      #########################
      # row agroforestry
      #########################
      titlePanel(title = "Row agroforestry"),
      
      # inputs
      sliderInput(inputId = "felling_age",
                  label =  "Felling age (years)",
                  min = 50, max = 80, value = 60, step = 1),
      
      sliderInput(inputId = "row_spacing",
                  label =  "Inter-row spacing (m)",
                  min = 10, max = 40, value = 30, step = 1),
      
      sliderInput(inputId = "felling_age",
                  label =  "Felling age (years)",
                  min = 50, max = 80, value = 60, step = 1),
      
      # update
      actionButton(inputId = "run_row_agf",
                   label = "Update row agroforestry simulation"),
      
      
      #########################
      # shelter belt agroforestry
      #########################
      titlePanel(title = "Shelter belts"),
      
      # inputs
      selectInput(inputId = "spp",
                  label = "Tree species",
                  choices = list(Beech = "BE",
                                 Oak = "OK",
                                 `Sycamore, ash & birch` = "SAB",
                                 `Scots pine` = "SP",
                                 `Sitka spruce` = "SS"),
                  selected = "SAB"),

      sliderInput(inputId = "sb_felling_age",
                  label =  "Felling age (years)",
                  min = 40, max = 80, value = 60, step = 1),
      
      # update
      actionButton(inputId = "run_row_agf",
                   label = "Update row agroforestry simulation"),
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      h4(HTML("a")),
      plotOutput(outputId = "row_agf_macc"),
      
      h4(HTML("b")),
      tableOutput(outputId = "b")
      
    )
  )
)

# build server
server <- function(input, output) {
  
  # baseline values for pre-populating app
  sim <- reactiveValues(row_agf = read_rds("app-baseline-simulations/row-agf.rds"),
                        fl_agf = read_rds("app-baseline-simulations/fl-agf.rds"),
                        sb_agf = read_rds("app-baseline-simulations/sb-agf.rds"),
                        hdg_agf = read_rds("app-baseline-simulations/hdg-agf.rds"),
                        agf_ag = read_rds("app-baseline-simulations/agf-ag.rds"))
  #########################
  # row agroforestry
  #########################
  observeEvent(input$run_row_agf, {
    sim$row_agf <- build_row_agf(felling_age = input$felling_age,
                                 row_spacing = input$row_spacing,
                                 discount_rate = input$discount_rate * 10^-2)
  })
  
  # plot
  output$row_agf_macc <- renderPlot({
    build_macc_plot(sim$row_agf) +
      labs(title = "Row agroforestry",
           subtitle = "Maximum uptake")
  })
  
  #########################
  # shelter belt agroforestry
  #########################
  observeEvent(input$run_row_agf, {
    sim$row_agf <- build_sb_agf(felling_age = input$felling_age,
                                 row_spacing = input$row_spacing,
                                 discount_rate = input$discount_rate * 10^-2)
  })
  
  # plot
  output$row_agf_macc <- renderPlot({
    build_macc_plot(sim$row_agf) +
      labs(title = "Row agroforestry",
           subtitle = "Maximum uptake")
  })
  
  
  
  
  # fenceline agroforestry
  
  
  # hedges
  
  
  
}

# create shiny app
shinyApp(ui, server)