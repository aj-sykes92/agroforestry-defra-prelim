
# source scenario functions
source("scenario-functions/row-agf-functions.R")
source("scenario-functions/shelter-belt-functions.R")
source("scenario-functions/fenceline-agf-functions.R")
source("scenario-functions/hedge-functions.R")
source("scenario-functions/orchard-agf-functions.R")

# source utility functions
source("scenario-functions/univ-functions.R")
source("scenario-functions/post-and-plot-functions.R")

# build ui
ui <- fluidPage(
  
  # app theme and styling
  theme = shinythemes::shinytheme("lumen"),
  shinyWidgets::chooseSliderSkin("Nice"),
  shinyWidgets::setSliderColor(rep("#8FBC8F", 12), 1:12),
  
  # busy spinner
  shinybusy::add_busy_spinner(position = "top-right", height = "80px", width = "80px"),
  
  # app title
  titlePanel("Marginal Abatement Cost Model for Agroforestry in the United Kingdom"),
  # app subtitle
  h4(HTML("Prepared for Defra project <i>Clean Growth through Sustainable Intensification</i>")),
  
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
      
      sliderInput(inputId = "cost_ceiling",
                  label = HTML("Cost-effectiveness ceiling (£ tonne CO<sub>2</sub>-eq<sup>-1</sup>)"),
                  min = -100, max = 500, value = 500, step = 1),
      
      selectInput(inputId = "dev_adm",
                  label = "Select regions for simulation",
                  multiple = TRUE,
                  choices = c("England", "Scotland", "Wales", "Northern Ireland"),
                  selected = c("England", "Scotland", "Wales", "Northern Ireland")),
      
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
      
      # break
      hr(),
      
      #########################
      # orchard agroforestry
      #########################
      titlePanel(title = "Row orchards"),
      
      # inputs
      sliderInput(inputId = "orch_agf_row_spacing",
                  label =  "Inter-row spacing (m)",
                  min = 10, max = 40, value = 30, step = 1),
      
      sliderInput(inputId = "orch_agf_uptake",
                  label = "Measure uptake (%)",
                  min = 1, max = 100, value = 10, step = 1),
      
      # update
      actionButton(inputId = "run_orch_agf",
                   label = "Update row orchard simulation")
      
    ),
    
    # main panel for outputs
    mainPanel(
      tabsetPanel(type = "tabs",
                  
                  ######################
                  # intro/helper panel
                  ######################
                  tabPanel(
                    title = "Using the app",
                    hr(),
                    includeMarkdown("app-helper-data/using-the-app.Rmd")
                  ),
                  
                  ######################
                  # row agroforestry
                  ######################
                  tabPanel(
                    title = "Row agroforestry",
                    hr(),
                    
                    # macc plot
                    h4(HTML("Marginal abatement cost curve")),
                    plotOutput(outputId = "row_agf_macc"),
                    hr(),
                    
                    # map
                    h4(HTML("Spatial marginal abatement cost & rate")),
                    plotOutput(outputId = "row_agf_map"),
                    hr(),
                    
                    # table
                    h4(HTML("Simulation output data")),
                    tableOutput(outputId = "row_agf_table"),
                    hr()
                    
                  ),
                  
                  ######################
                  # shelter belts
                  ######################
                  tabPanel(
                    title = "Shelter belts",
                    
                    # macc plot
                    h4(HTML("Marginal abatement cost curve")),
                    plotOutput(outputId = "sb_agf_macc"),
                    hr(),
                    
                    # map
                    h4(HTML("Spatial marginal abatement cost & rate")),
                    plotOutput(outputId = "sb_agf_map"),
                    hr(),
                    
                    # table
                    h4(HTML("Simulation output data")),
                    tableOutput(outputId = "sb_agf_table"),
                    hr()
                    
                  ),
                  
                  ######################
                  # fenceline agroforestry
                  ######################
                  tabPanel(
                    title = "Fenceline planting",
                    
                    # macc plot
                    h4(HTML("Marginal abatement cost curve")),
                    plotOutput(outputId = "fl_agf_macc"),
                    hr(),
                    
                    # map
                    h4(HTML("Spatial marginal abatement cost & rate")),
                    plotOutput(outputId = "fl_agf_map"),
                    hr(),
                    
                    # table
                    h4(HTML("Simulation output data")),
                    tableOutput(outputId = "fl_agf_table"),
                    hr()
                    
                  ),
                  
                  ######################
                  # hedges
                  ######################
                  tabPanel(
                    title = "Hedge expansion",
                    hr(),
                    
                    # macc plot
                    h4(HTML("Marginal abatement cost curve")),
                    plotOutput(outputId = "hdg_agf_macc"),
                    hr(),
                    
                    # map
                    h4(HTML("Spatial marginal abatement cost & rate")),
                    plotOutput(outputId = "hdg_agf_map"),
                    hr(),
                    
                    # table
                    h4(HTML("Simulation output data")),
                    tableOutput(outputId = "hdg_agf_table"),
                    hr()
                    
                  ),

                  ######################
                  # orchards
                  ######################
                  tabPanel(
                    title = "Row orchards",
                    hr(),
                    
                    # macc plot
                    h4(HTML("Marginal abatement cost curve")),
                    plotOutput(outputId = "orch_agf_macc"),
                    hr(),
                    
                    # map
                    h4(HTML("Spatial marginal abatement cost & rate")),
                    plotOutput(outputId = "orch_agf_map"),
                    hr(),
                    
                    # table
                    h4(HTML("Simulation output data")),
                    tableOutput(outputId = "orch_agf_table"),
                    hr()
                    
                  ),
                  
                  ######################
                  # aggregated output
                  ######################
                  tabPanel(
                    title = "Aggregated output",
                    hr(),
                    
                    # macc plots
                    h4(HTML("Aggregate marginal abatement cost curve (crop-wise)")),
                    plotOutput(outputId = "ag_macc_crop"),
                    hr(),
                    h4(HTML("Aggregate marginal abatement cost curve (system-wise)")),
                    plotOutput(outputId = "ag_macc_sys"),
                    hr(),
                    
                    # maps
                    h4(HTML("Aggregate spatial marginal abatement cost & rate")),
                    plotOutput(outputId = "ag_map"),
                    hr(),
                    
                    h4(HTML("Spatial distribution of measure cost-effectiveness (system-wise)")),
                    plotOutput(outputId = "sys_map"),
                    hr(),
                    
                    # tables
                    h4(HTML("Aggregate output data (crop-wise)")),
                    tableOutput(outputId = "ag_table_crop"),
                    hr(),
                    h4(HTML("Aggregate output data (system-wise)")),
                    tableOutput(outputId = "ag_table_sys"),
                    hr()
                    
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
                        orch_agf = read_rds("app-baseline-simulations/orch-agf.rds"),
                        agf_ag = read_rds("app-baseline-simulations/agf-ag.rds"))
  
  #########################
  # aggregate / global options
  #########################
  
  # run all simulations
  observeEvent(input$run_all, {
    
    # row
    sim$row_agf <- build_row_agf(felling_age = input$row_agf_felling_age,
                                 row_spacing = input$row_agf_row_spacing,
                                 discount_rate = input$discount_rate * 10^-2,
                                 da = input$dev_adm) %>%
      cheap_scale(input$row_agf_uptake * 10^-2) %>%
      impose_cost_ceiling(input$cost_ceiling)
    
    # shelter belts
    sim$sb_agf <- build_sb_agf(spp_short = input$sb_agf_spp,
                               felling_age = input$sb_agf_felling_age,
                               discount_rate = input$discount_rate * 10^-2,
                               da = input$dev_adm) %>%
      check_strat_scale(input$sb_agf_uptake * 10^-2) %>%
      impose_cost_ceiling(input$cost_ceiling)
    
    # fenceline
    sim$fl_agf <- build_fl_agf(felling_age = input$fl_agf_felling_age,
                               discount_rate = input$discount_rate * 10^-2,
                               da = input$dev_adm) %>%
      cheap_scale(input$fl_agf_uptake  * 10^-2) %>%
      impose_cost_ceiling(input$cost_ceiling)
    
    
    # hedges
    sim$hdg_agf <- build_hdg_agf(discount_rate = input$discount_rate * 10^-2,
                                 applies_to = input$hdg_agf_crop_spp,
                                 da = input$dev_adm) %>%
      cheap_scale(input$hdg_agf_uptake * 10^-2) %>%
      impose_cost_ceiling(input$cost_ceiling)
    
    # orchards
    sim$orch_agf <- build_orch_agf(row_spacing = input$orch_agf_row_spacing,
                                   discount_rate = input$discount_rate * 10^-2,
                                   da = input$dev_adm) %>%
      cheap_scale(input$orch_agf_uptake  * 10^-2) %>%
      impose_cost_ceiling(input$cost_ceiling)
    
    # aggregate
    sim$agf_ag <- bind_rows(list(`Row agroforestry` = sim$row_agf,
                                 Shelterbelts = sim$sb_agf,
                                 `Fenceline planting` = sim$fl_agf,
                                 `Hedge expansion` = sim$hdg_agf,
                                 `Row orchards` = sim$orch_agf),
                            .id = "sys_type")
    
  })
  
  # maccs
  output$ag_macc_crop <- renderPlot({
    build_macc_plot(sim$agf_ag)
  })
  
  output$ag_macc_sys <- renderPlot({
    build_agmacc_plot(sim$agf_ag)
  })
  
  # maps
  output$ag_map <- renderPlot({
    build_paired_map(sim$agf_ag)
  })
  
  output$sys_map <- renderPlot({
    build_sys_map(sim$agf_ag)
  })
  
  # tables
  output$ag_table_crop <- renderTable({
    get_descriptives(sim$agf_ag, "crop")
  },
  sanitize.text.function = function(x) x)
  
  output$ag_table_sys <- renderTable({
    get_descriptives(sim$agf_ag, "sys_type")
  },
  sanitize.text.function = function(x) x)
  
  #########################
  # row agroforestry
  #########################
  
  # sim run
  observeEvent(input$run_row_agf, {
    sim$row_agf <- build_row_agf(felling_age = input$row_agf_felling_age,
                                 row_spacing = input$row_agf_row_spacing,
                                 discount_rate = input$discount_rate * 10^-2,
                                 da = input$dev_adm) %>%
      cheap_scale(input$row_agf_uptake * 10^-2) %>%
      impose_cost_ceiling(input$cost_ceiling)
    
    # re-aggregate
    sim$agf_ag <- bind_rows(list(`Row agroforestry` = sim$row_agf,
                                 Shelterbelts = sim$sb_agf,
                                 `Fenceline planting` = sim$fl_agf,
                                 `Hedge expansion` = sim$hdg_agf,
                                 `Row orchards` = sim$orch_agf),
                            .id = "sys_type")
  })
  
  # plots
  output$row_agf_macc <- renderPlot({
    build_macc_plot(sim$row_agf)
  })
  
  output$row_agf_map <- renderPlot({
    build_paired_map(sim$row_agf)
  })
  
  # table
  output$row_agf_table <- renderTable({
    get_descriptives(sim$row_agf, "crop")
  },
  sanitize.text.function = function(x) x)
  
  #########################
  # shelter belt agroforestry
  #########################
  
  # sim run
  observeEvent(input$run_sb_agf, {
    sim$sb_agf <- build_sb_agf(spp_short = input$sb_agf_spp,
                               felling_age = input$sb_agf_felling_age,
                               discount_rate = input$discount_rate * 10^-2,
                               da = input$dev_adm) %>%
      cheap_strat_scale(input$sb_agf_uptake * 10^-2) %>%
      impose_cost_ceiling(input$cost_ceiling)
    
    # re-aggregate
    sim$agf_ag <- bind_rows(list(`Row agroforestry` = sim$row_agf,
                                 Shelterbelts = sim$sb_agf,
                                 `Fenceline planting` = sim$fl_agf,
                                 `Hedge expansion` = sim$hdg_agf,
                                 `Row orchards` = sim$orch_agf),
                            .id = "sys_type")
  })
  
  # plots
  output$sb_agf_macc <- renderPlot({
    build_macc_plot(sim$sb_agf)
  })
  
  output$sb_agf_map <- renderPlot({
    build_paired_map(sim$sb_agf)
  })
  
  # table
  output$sb_agf_table <- renderTable({
    get_descriptives(sim$sb_agf, "crop")
  },
  sanitize.text.function = function(x) x)
  
  #########################
  # fenceline agroforestry
  #########################
  
  # sim run
  observeEvent(input$run_fl_agf, {
    sim$fl_agf <- build_fl_agf(felling_age = input$fl_agf_felling_age,
                               discount_rate = input$discount_rate * 10^-2,
                               da = input$dev_adm) %>%
      cheap_scale(input$fl_agf_uptake  * 10^-2) %>%
      impose_cost_ceiling(input$cost_ceiling)
    
    # re-aggregate
    sim$agf_ag <- bind_rows(list(`Row agroforestry` = sim$row_agf,
                                 Shelterbelts = sim$sb_agf,
                                 `Fenceline planting` = sim$fl_agf,
                                 `Hedge expansion` = sim$hdg_agf,
                                 `Row orchards` = sim$orch_agf),
                            .id = "sys_type")
  })
  
  # plots
  output$fl_agf_macc <- renderPlot({
    build_macc_plot(sim$fl_agf)
  })
  
  output$fl_agf_map <- renderPlot({
    build_paired_map(sim$fl_agf)
  })
  
  # table
  output$fl_agf_table <- renderTable({
    get_descriptives(sim$fl_agf, "crop")
  },
  sanitize.text.function = function(x) x)
  
  #########################
  # hedges
  #########################
  
  # sim run
  observeEvent(input$run_hdg_agf, {
    sim$hdg_agf <- build_hdg_agf(discount_rate = input$discount_rate * 10^-2,
                                 applies_to = input$hdg_agf_crop_spp,
                                 da = input$dev_adm) %>%
      cheap_scale(input$hdg_agf_uptake * 10^-2) %>%
      impose_cost_ceiling(input$cost_ceiling)
    
    # re-aggregate
    sim$agf_ag <- bind_rows(list(`Row agroforestry` = sim$row_agf,
                                 Shelterbelts = sim$sb_agf,
                                 `Fenceline planting` = sim$fl_agf,
                                 `Hedge expansion` = sim$hdg_agf,
                                 `Row orchards` = sim$orch_agf),
                            .id = "sys_type")
  })
  
  # plots
  output$hdg_agf_macc <- renderPlot({
    build_macc_plot(sim$hdg_agf)
  })
  
  output$hdg_agf_map <- renderPlot({
    build_paired_map(sim$hdg_agf)
  })
  
  # table
  output$hdg_agf_table <- renderTable({
    get_descriptives(sim$hdg_agf, "crop")
  },
  sanitize.text.function = function(x) x)
  
  #########################
  # orchards
  #########################
  
  # sim run
  observeEvent(input$run_orch_agf, {
    sim$orch_agf <- build_orch_agf(row_spacing = input$orch_agf_row_spacing,
                                   discount_rate = input$discount_rate * 10^-2,
                                   da = input$dev_adm) %>%
      cheap_scale(input$orch_agf_uptake * 10^-2) %>%
      impose_cost_ceiling(input$cost_ceiling)
    
    # re-aggregate
    sim$agf_ag <- bind_rows(list(`Row agroforestry` = sim$row_agf,
                                 Shelterbelts = sim$sb_agf,
                                 `Fenceline planting` = sim$fl_agf,
                                 `Hedge expansion` = sim$hdg_agf,
                                 `Row orchards` = sim$orch_agf),
                            .id = "sys_type")
  })
  
  # plots
  output$orch_agf_macc <- renderPlot({
    build_macc_plot(sim$orch_agf)
  })
  
  output$orch_agf_map <- renderPlot({
    build_paired_map(sim$orch_agf)
  })
  
  # table
  output$orch_agf_table <- renderTable({
    get_descriptives(sim$orch_agf, "crop")
  },
  sanitize.text.function = function(x) x)
  
}

# create shiny app
shinyApp(ui, server)