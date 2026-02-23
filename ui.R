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
          bottom = "auto", width = 450, height = "85%", overflow = "auto",
          
          bslib::navset_card_underline(
            id = "navset",
            title = NULL,
            
            # Select "targeted catchment"
            bslib::nav_panel(
              title = "1. Select period", selected = TRUE,
              tags$hr(class = "custom-line"), h5(),
              
              dateRangeInput("selectPeriod", 
                             "1. Select period for analysis",
                             start = as.Date("2001-01-01"),
                             end = as.Date("2020-12-31"),
                             min = as.Date("1980-01-01"),
                             max = as.Date("2020-12-31")),
              
              numericInput("maxQmissing", 
                           "2. Maximum allowable missing streamflow (%)",
                           min = 0, max = 99, value = 5, width = "85%"),
              
              h5("3. Run subseting catchments"),
              
              actionButton("dataSubset", 
                           "Select & derive streamflow statistics",
                           width = "68%"),
            ),
            
            # Select "targeted catchment"
            bslib::nav_panel(
              title = "2. Select basin", selected = TRUE,
              tags$hr(class = "custom-line"), h5(),
              
             
              
              selectInput("selectFlowRegime", "1. Select flow regime",
                          multiple = TRUE,
                          choices = c("None",
                                      "cvq_autumn > 1.1 (erratic)",
                                      "cvq_winter > 1.1 (erratic)",
                                      "cvq_spring > 1.1 (erratic)",
                                      "cvq_summer > 1.1 (erratic)"),
                          selected = "None"),
              
              numericInput("maxNrDams", 
                           "2. Maximum number of dams",
                           min = 0, max = 50, value = 50, width = "85%"),
              
              numericInput("maxAgri", 
                           "3. Maximum agricultural land area (%)",
                           min = 0, max = 100, value = 100, width = "85%"),
              
              numericInput("maxUrban", 
                           "4. Maximum urban land area (%)",
                           min = 0, max = 100, value = 100, width = "85%"),
              
              numericInput("annualQTrend", 
                           "5. No Q trend (max |sen's slope|)",
                           min = 0.001, max = 0.1, value = 0.1, width = "85%"),
              
              numericInput("nirtatePollutedArea", 
                           "6. Maxium nitrate polluted area (%)",
                           min = 0, max = 100, value = 25, width = "85%"),
              
              textInput("groundwater_quality", 
                           "7. Aquifer area with good water quality (%)",
                           value = "No data available"),
              
              h5("8. Add more criteria here..."),
              
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
              )
              
            ),
            
            bslib::nav_panel(
              title = "4.State", selected = TRUE,
              tags$hr(class = "custom-line"), h5(),
              
              selectInput("selectHydro", "1. Select hydrological indicators",
                          multiple = FALSE, 
                          choices = NA,
                          selected = NA),
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
