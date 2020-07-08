
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
  
  # app theme
  #theme = "bootstrap.css",
  
  # app title
  titlePanel("Marginal Abatement Cost Model for Agroforestry"),
  # app subtitle
  h4(HTML("Prepared for Defra Project 'Clean Growth through Sustainable intensification'")),
  
  # busy spinner
  shinybusy::add_busy_spinner(position = "top-right", height = "80px", width = "80px"),
  
  # app in sidebar-panel-main-panel layout 
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
      
      # break
      hr(),
      
      #########################
      # row agroforestry
      #########################
      titlePanel(title = "Row agroforestry"),
      
      # inputs
      sliderInput(inputId = "row_agf_felling_age",
                  label =  "Felling age (years)",
                  min = 50, max = 80, value = 60, step = 1),
      
      sliderInput(inputId = "row_agf_row_spacing",
                  label =  "Inter-row spacing (m)",
                  min = 10, max = 40, value = 30, step = 1),
      
      sliderInput(inputId = "row_agf_uptake",
                  label = "Measure uptake (%)",
                  min = 1, max = 100, value = 10, step = 1),
      
      # update
      actionButton(inputId = "run_row_agf",
                   label = "Update row agroforestry simulation"),
      
      # break
      hr(),
      
      #########################
      # shelter belt agroforestry
      #########################
      titlePanel(title = "Shelter belts"),
      
      # inputs
      selectInput(inputId = "sb_agf_spp",
                  label = "Tree species",
                  choices = list(Beech = "BE",
                                 Oak = "OK",
                                 `Sycamore, ash & birch` = "SAB",
                                 `Scots pine` = "SP",
                                 `Sitka spruce` = "SS"),
                  selected = "SAB"),
      
      sliderInput(inputId = "sb_agf_felling_age",
                  label =  "Felling age (years)",
                  min = 40, max = 80, value = 60, step = 1),
      
      sliderInput(inputId = "sb_agf_uptake",
                  label = "Measure uptake (%)",
                  min = 1, max = 100, value = 10, step = 1),
      
      # update
      actionButton(inputId = "run_sb_agf",
                   label = "Update shelter belt simulation"),
      
      # break
      hr(),
      
      #########################
      # fenceline agroforestry
      #########################
      titlePanel(title = "Fenceline agroforestry"),
      
      # inputs
      sliderInput(inputId = "fl_agf_felling_age",
                  label =  "Felling age (years)",
                  min = 40, max = 80, value = 60, step = 1),
      
      sliderInput(inputId = "fl_agf_uptake",
                  label = "Measure uptake (%)",
                  min = 1, max = 100, value = 10, step = 1),
      
      # update
      actionButton(inputId = "run_fl_agf",
                   label = "Update fenceline agroforestry simulation"),
      
      # break
      hr(),
      
      #########################
      # hedges
      #########################
      titlePanel(title = "Hedges"),
      
      # inputs
      selectInput(inputId = "hdg_agf_crop_spp",
                  label = "Applied on land use(s)",
                  multiple = TRUE,
                  choices = list(Barley = "barley",
                                 `Cereals, other` = "cereals_other",
                                 `Oil crops, other` = "oil_crops_other",
                                 Pasture = "pasture",
                                 `Pulses, other` = "pulses_other",
                                 Rapeseed = "rapeseed",
                                 Wheat = "wheat"),
                  selected = c("barley",
                               "cereals_other",
                               "oil_crops_other",
                               "pasture",
                               "pulses_other",
                               "rapeseed",
                               "wheat")),
      
      sliderInput(inputId = "hdg_agf_uptake",
                  label = "Measure uptake (%)",
                  min = 1, max = 100, value = 10, step = 1),
      
      # update
      actionButton(inputId = "run_hdg_agf",
                   label = "Update hedge simulation"),
      
    ),
    
    # main panel for outputs
    mainPanel(
      tabsetPanel(type = "tabs",
                  
                  ######################
                  # row agroforestry
                  ######################
                  tabPanel(
                    title = "Row agroforestry",
                    
                    # macc plot
                    plotOutput(outputId = "row_agf_macc")
                    
                    # map
                    
                    # table
                    #h4(HTML("b")),
                    #tableOutput(outputId = "b"),
                    
                  ),
                  
                  ######################
                  # shelter belts
                  ######################
                  tabPanel(
                    title = "Shelter belts",
                    
                    # macc plot
                    plotOutput(outputId = "sb_agf_macc"),
                    
                    # map
                    
                    # table
                    #h4(HTML("b")),
                    #tableOutput(outputId = "b"),
                    
                  ),
                  
                  ######################
                  # fenceline agroforestry
                  ######################
                  tabPanel(
                    title = "Fenceline planting",
                    
                    # macc plot
                    plotOutput(outputId = "fl_agf_macc"),
                    
                    # map
                    
                    # table
                    #h4(HTML("b")),
                    #tableOutput(outputId = "b"),
                    
                  ),
                  
                  ######################
                  # hedges
                  ######################
                  tabPanel(
                    title = "Hedges",
                    
                    # macc plot
                    plotOutput(outputId = "hdg_agf_macc"),
                    
                    # map
                    
                    # table
                    #h4(HTML("b")),
                    #tableOutput(outputId = "b"),
                    
                  ),
                  
                  ######################
                  # aggregated output
                  ######################
                  tabPanel(
                    title = "Aggregated output",
                    
                    # macc plot
                    #plotOutput(outputId = "row_agf_macc"),
                    
                    # map
                    
                    # table
                    #h4(HTML("b")),
                    #tableOutput(outputId = "b"),
                    
                  )
      )
      
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
  
  # sim run
  observeEvent(input$run_row_agf, {
    sim$row_agf <- build_row_agf(felling_age = input$row_agf_felling_age,
                                 row_spacing = input$row_agf_row_spacing,
                                 discount_rate = input$discount_rate * 10^-2) %>%
      cheap_scale(input$row_agf_uptake * 10^-2)
  })
  
  # plots
  output$row_agf_macc <- renderPlot({
    build_macc_plot(sim$row_agf)
  })
  
  # tables
  
  #########################
  # shelter belt agroforestry
  #########################
  
  # sim run
  observeEvent(input$run_sb_agf, {
    sim$sb_agf <- build_sb_agf(spp_short = input$sb_agf_spp,
                               felling_age = input$sb_agf_felling_age,
                               discount_rate = input$discount_rate * 10^-2) %>%
      even_scale(input$sb_agf_uptake * 10^-2)
  })
  
  # plots
  output$sb_agf_macc <- renderPlot({
    build_macc_plot(sim$sb_agf)
  })
  
  #########################
  # fenceline agroforestry
  #########################
  
  # sim run
  observeEvent(input$run_fl_agf, {
    sim$fl_agf <- build_fl_agf(felling_age = input$fl_agf_felling_age,
                               discount_rate = input$discount_rate * 10^-2) %>%
      cheap_scale(input$fl_agf_uptake  * 10^-2)
  })
  
  # plots
  output$fl_agf_macc <- renderPlot({
    build_macc_plot(sim$fl_agf)
  })
  
  #########################
  # hedges
  #########################
  
  # sim run
  observeEvent(input$run_hdg_agf, {
    sim$hdg_agf <- build_hdg_agf(discount_rate = input$discount_rate * 10^-2,
                                 applies_to = input$hdg_agf_crop_spp) %>%
      cheap_scale(input$hdg_agf_uptake * 10^-2)
  })
  
  # plots
  output$hdg_agf_macc <- renderPlot({
    build_macc_plot(sim$hdg_agf)
  })
  
}

# create shiny app
shinyApp(ui, server)