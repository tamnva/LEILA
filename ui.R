library(leaflet)
library(plotly)

navbarPage(
  "", id="nav",
  
  tabPanel(
    "Home",
    
    conditionalPanel("false", icon("bullseye")),
    
    shinybusy::add_busy_spinner(spin = "radar", position = c("bottom-right"),
                                margins = c(100, 100)),
    
    div(class="outer",
        tags$head(
          # Include our custom CSS
          includeCSS("styles.css"),
          includeScript("gomap.js")
        ),
        
        leafletOutput("map", width="100%", height="100%"),
        
        # Panel to display plots.
        absolutePanel(
          id = "controls", class = "panel panel-default", fixed = TRUE, 
          draggable = FALSE, top = 65, left = "auto", right = 15, 
          bottom = "auto", width = 500, height = "85%", overflow = "auto",
          
          bslib::navset_card_underline(
            id = "navset",
            title = NULL,
            
            # Select "targeted catchment"
            bslib::nav_panel(
              title = "1. Select period", selected = TRUE,
              tags$hr(class = "custom-line"), h5(),
              
              dateRangeInput("selectPeriod", 
                             "1. Select period for analysis",
                             start = as.Date("1990-01-01"),
                             end = as.Date("2020-12-31"),
                             min = as.Date("1980-01-01"),
                             max = as.Date("2020-12-31")),
              
              numericInput("maxQmissing", 
                           "2. Maximum allowable missing streamflow (%)",
                           min = 0, max = 99, value = 10, width = "85%"),
              
              h5("3. Run subseting catchments"),
              
              actionButton("dataSubset", 
                           "Select & derive streamflow statistics",
                           width = "68%"),
              h5(),
              h5("4. Derive other hydrological indicators..."),
            ),
            
            # Select "targeted catchment"
            bslib::nav_panel(
              title = "2. Select basin", selected = TRUE,
              tags$hr(class = "custom-line"), h5(),

              h5(tags$b("1. Near-natural conditions")),
              numericInput("maxNrDams", 
                           "Maximum number of dams",
                           min = 0, max = 50, value = 50, width = "85%"),
              
              numericInput("maxAgri", 
                           "Maximum agricultural land area (%)",
                           min = 0, max = 100, value = 100, width = "85%"),
              
              numericInput("maxUrban", 
                           "Maximum urban land area (%)",
                           min = 0, max = 100, value = 100, width = "85%"),
              
              numericInput("annualQTrend", 
                           "No Q trend (max |sen's slope|)",
                           min = 0.001, max = 0.1, value = 0.1, width = "85%"),
              
              h5(tags$b("2. Resilience to climate change")),
              selectInput("selectFlowRegime", "Select flow regime",
                          multiple = TRUE,
                          choices = c("None",
                                      "cvq_autumn > 1.1 (erratic)",
                                      "cvq_winter > 1.1 (erratic)",
                                      "cvq_spring > 1.1 (erratic)",
                                      "cvq_summer > 1.1 (erratic)"),
                          selected = "None"),
              
             
              h5(tags$b("3. Good ecological and water quality")),
              numericInput("nirtatePollutedArea", 
                           "Maxium GW nitrate polluted area (%)",
                           min = 0, max = 100, value = 25, width = "85%"),
              
              numericInput("protectedArea", 
                           "Minimum nature protected area (%)",
                           min = 0, max = 100, value = 0, width = "85%"),

              numericInput("soilMoisture", 
                           "Minimum duration soil moisture above PWP (%)",
                           min = 0, max = 100, value = 0, width = "85%"),

              #h5("9. No trend in groundwater levels"),
              checkboxInput("gwTrend", 
                            "No trend in groundwater level", FALSE),
              
              textInput("groundwater_quality", 
                           "Min. river segment with good quality (%)",
                           value = "No data available"),
              
              h5("11. Add more criteria here..."),
              
            ),
            
            # Regression to link hydrological indicator and catchment attributes
            bslib::nav_panel(
              title = "3.Regression", selected = TRUE,
              tags$hr(class = "custom-line"), h5(),
              
              selectInput("selectRegressionModel", "1. Select regression model",
                          multiple = FALSE, 
                          choices = c("Multiple Linear Regression",
                                      "Random forest",
                                      "TabICLv2"),
                          selected = "Multiple Linear Regression"),
              
              selectInput("selectIndepVar", "2. Select independent variable(s)",
                          multiple = TRUE, 
                          choices = NA,
                          selected = NA),
              
              selectInput("selectDepVar", "3. Select dependent variable(s)",
                          multiple = TRUE, 
                          choices = NA,
                          selected = NA),
              
              h5("4. Run regression model"),
              actionButton("runRegression", "Run", width = "68%"),
              
              h5(),
              div(
                style = "height: 300px; overflow-y: auto; 
                border: 1px solid #ccc; padding: 0px;",
                plotlyOutput("regression_plot", height = "300px", width = "300px"),
              ),
              
              h5("5. Calcualte near-nat. states for all catchments"),
              actionButton("calculate_near_nat", "Calculate", width = "68%"),
            ),
            
            bslib::nav_panel(
              title = "4.State", selected = TRUE,
              tags$hr(class = "custom-line"), h5(),
              
              selectInput("selectDiff", "1. Visualize distance to",
                          multiple = FALSE, 
                          choices = NA,
                          selected = NA),
              
              selectInput("selectBasinGroup", "2. Select basins",
                          multiple = FALSE,
                          choices = c("All",
                                      "Near natural basins",
                                      "Non near-natural basin"),
                          selected = "All"),
              
              h5("   "),
              h5("   "),
              h5("   "),
              h5("   "),
              h5("   "),
              h5("   "),
              
            ),
            
          ), 
        ),
    )
  ),
  
  tabPanel(
    "Einzugsgebiet Charakteristik",
    hr(),
    DT::dataTableOutput("catchment_attributes")
  ),
  
  tabPanel(
    "Hydrologische Indikatoren",
    hr(),
    DT::dataTableOutput("hydro_indicator")
  )
)
