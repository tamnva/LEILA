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
          bottom = "auto", width = 450, height = "auto",
          
          bslib::navset_card_underline(
            id = "navset",
            title = NULL,
            
            # First need to filter catchments with streamflow data
            bslib::nav_panel(
              title = "1.Data selection", selected = TRUE,
              tags$hr(class = "custom-line"), h5(),
              
              dateRangeInput("selectPeriod", 
                             "1. Select period for analysis",
                             start = as.Date("2001-01-01"),
                             end = as.Date("2020-12-31"),
                             min = as.Date("1980-01-01"),
                             max = as.Date("2020-12-31")),
              
              numericInput("maxQmissing", 
                           "2. Maximum allowable missing streamflow (%)",
                           min = 0, max = 99, value = 5),
              
              h5("3. Run subseting catchments"),
              actionButton("dataSubset", 
                           "Run and calculate streamflow statistics")
            ),
            
            # Now select "targeted catchment"
            bslib::nav_panel(
              title = "2.Targeted catchments", selected = TRUE,
              tags$hr(class = "custom-line"), h5(),
              
              selectInput("selectFlowRegime", "1. Select flow regime",
                          multiple = TRUE, 
                          choices = c("None",
                                      "CVQ_Autumn > 1.1 (erratic)",
                                      "CVQ_Winter > 1.1 (erratic)",
                                      "CVQ_Spring > 1.1 (erratic)",
                                      "CVQ_Summer > 1.1 (erratic)"),
                          selected = "None"),
              
              textInput("stream_wquality", 
                           "2. River length with good water quality (%)",
                           value = "No data available"),
              
              textInput("groundwater_quality", 
                           "3. Aquifer area with good water quality (%)",
                           value = "No data available"),
              
              h5("4. Add more criteria here..."),
              
            ),
            
            # Regression to link hydrological indicator and catchment attributes
            bslib::nav_panel(
              title = "3.Regression model", selected = TRUE,
              tags$hr(class = "custom-line"), h5(),
              
              selectInput("selectRegressionModel", "1. Select regression model",
                          multiple = FALSE, 
                          choices = c("Multiple Linear Regression",
                                      "Random forest",
                                      "Tableau Foundation"),
                          selected = "Multiple Linear Regression"),
              
              selectInput("selectIndepVar", "2. Select independent variable(s)",
                          multiple = TRUE, 
                          choices = colnames(attributes)[-c(1)]),
              
              selectInput("selectDepVar", "3. Select dependent variable(s)",
                          multiple = TRUE, 
                          choices = NA,
                          selected = NA),
              
              h5("4. Run regression model"),
              actionButton("runRegression", "Run", width = "70%"),
              
              h5(),
              div(
                style = "height: 300px; overflow-y: auto; 
                border: 1px solid #ccc; padding: 0px;",
                plotlyOutput("regression_plot", height = "300px", width = "300px"),
              )
              
            ),
            
            bslib::nav_panel(
              title = "4.Target & Curent state", selected = TRUE,
              tags$hr(class = "custom-line"), h5(),
              
              selectInput("selectHydro", "1. Select hydrological indicators",
                          multiple = TRUE, 
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
