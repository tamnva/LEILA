#==============================================================================#
#               The procedure of this R script is as follows:                  #
# 0. Load background maps and defaults values                                  #
# 1. Select catchment based on streamflow data availability (Data)             #
# 2. Select near-natural catchments                                            #
# 3. Regression (linking hydro. indicators with catchment attributes)          #
# 4. Visualization                                                             #
# 5. Other reactive functions                                                  #
#==============================================================================#


function(input, output, session) {

  # Stop the app when user close the browser
  session$onSessionEnded(function(){
    shiny::stopApp()
  })
  
  #============================================================================#
  #               0. Background + default maps/tables                          #
  #============================================================================#
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "OpenStreetMap") %>%
      addScaleBar(position = "bottomleft") %>%
      addRasterImage(huek, 
                     opacity = 0.7, 
                     group = "Hydrological unit") %>%
      addRasterImage(population_density,
                     opacity = 0.7, 
                     group = "Population density (people/km²)")  %>%
      addPolygons(data = subset(catchments),
                  stroke = TRUE,
                  fillColor = "#00000000",
                  color = "#C4C4C4",
                  weight = 1,
                  popup = ~ showPopup(gauge_id),
                  group = "Catchment boundary",
                  layerId = ~ gauge_id) %>%
      addPolygons(data = schutzgetbiet, 
                  fillColor = "#006400", 
                  fillOpacity = 0.6, 
                  color = "#006400", 
                  stroke = TRUE, 
                  weight = 1, 
                  group = "Nature protection area") %>%
      addPolygons(data = nitratbelastete_gebiete, 
                  fillColor = "#DC3220", 
                  fillOpacity = 0.6, 
                  color = "#DC3220", 
                  stroke = TRUE, 
                  weight = 1, 
                  group = "Nitrate pollutated area") %>% 
      addProviderTiles(provider = providers$CartoDB.PositronNoLabels,
                       group = "CartoDBPositronNolabel") %>%
      addProviderTiles(provider = providers$CartoDB.Positron,
                       group = "CartoDBPositron") %>%
      addProviderTiles(provider = providers$Esri.WorldImagery,
                       group = "WorldImagery") %>%
      addCircleMarkers(data = stations,
                       radius = 3,
                       group = "Streamgauge",
                       fillColor = "#FFC107",
                       fillOpacity = 0.8,
                       stroke = FALSE,
                       popup = ~ showPopup(gauge_id),
                       layerId = ~ gauge_id
      ) %>%
      addCircleMarkers(data = gw_wells,
                       radius = 2,
                       group = "Selected groundwater well",
                       fillColor = "#2798F5",
                       fillOpacity = 0.7,
                       stroke = FALSE,
                       popup = ~ UFZ.ID,
                       layerId = ~ UFZ.ID
      ) %>%
      addCircleMarkers(data = waste_water_discharge,
                       radius = 2,
                       group = "Wastewater discharge",
                       fillColor = "#DC267F",
                       fillOpacity = 0.7,
                       stroke = FALSE,
                       popup = ~ paste(durchschnitt_abwassermenge, 
                                       abwassermenge_einheit),
                       layerId = ~ durchschnitt_abwassermenge
      ) %>%
      addLayersControl(
        baseGroups = c("CartoDBPositron", "CartoDBPositronNolabel", 
                       "OpenStreetMap", "WorldImagery"),
        overlayGroups = c("Streamgauge", 
                          "Catchment boundary",
                          "Hydrological unit",
                          "Nature protection area",
                          "Nitrate pollutated area",
                          "Selected groundwater well",
                          "Population density (people/km²)",
                          "Wastewater discharge"),
        options = layersControlOptions(position = "bottomleft")
      )  %>%
      hideGroup(c("Hydrological unit", 
                  "Nature protection area",
                  "Nitrate pollutated area",
                  "Selected groundwater well",
                  "Catchment boundary",
                  "Population density (people/km²)",
                  "Wastewater discharge")) %>%
      setView(lng = 9, lat = 50, zoom = 5)
  })
  
  output$catchment_attributes <- DT::renderDataTable({
    showDataFrame(attributes, session, "catchment_attributes", NULL)
  })
  
  #============================================================================#
  #    1. Select catchment based on streamflow data availability (Data)        #
  #============================================================================#
  observeEvent(input$dataSubset, {

    attributes <<- read_csv("data/attributes.csv", show_col_types = FALSE)
    
    # Replace the default hydrological indicators if settings are changed
    if ((input$selectPeriod[1] == as.Date("1990-01-01") &
         input$selectPeriod[2] == as.Date("2020-12-31") &
         input$maxQmissing == 10)){
      
      hydro_indicator <<- readr::read_csv("data/hydro_indicator.csv",
                                          show_col_types = FALSE)
    } else {
      streamflow_statistic <<- getStreamflowStatistics(
        timeseries_camels_combine = timeseries_camels,
        variable_name = c("discharge_spec_obs", "precipitation_mean"),
        start_date    = input$selectPeriod[1],
        end_date      = input$selectPeriod[2],
        max_missing   = input$maxQmissing) %>%
        mutate(across(where(is.numeric), function(x) round(x, 3)))
      
      # Add lat, long to streamflow statistics to know location of stations
      hydro_indicator <<-  attributes %>% 
        filter(gauge_id %in% streamflow_statistic$gauge_id) %>%
        select(lat, long, gauge_id) %>% 
        left_join(streamflow_statistic, by = "gauge_id")
      
      # data.table::fwrite(hydro_indicator, "data/hydro_indicator.csv")
    }
    
    # Precipitation statistics should be in the attributes table
    attributes <<- attributes %>% 
      left_join(hydro_indicator %>% select(p_mean, p_std, gauge_id), 
                by = "gauge_id")
    
    hydro_indicator <<- hydro_indicator %>% 
      select(!c(p_mean, p_std)) 
    
    # Update the catchment attributes table
    output$catchment_attributes <- DT::renderDataTable({
      showDataFrame(attributes, session, "catchment_attributes", 
                    hydro_indicator$gauge_id)})
    
    # Update the hydrological indicator table
    output$hydro_indicator <- DT::renderDataTable({hydro_indicator})
    
    # Update map
    showGauge(stations, hydro_indicator$gauge_id)
    
    # Update regression select dependent variables
    if (!is.null(hydro_indicator)){
      updateSelectInput(session, "selectDepVar", 
                        choices = colnames(
                          hydro_indicator %>% select(!c(lat, long, gauge_id))
                          ))
    }
    
    # Update list of independent variables for regression
    updateSelectInput(session, "selectIndepVar", 
                      "2. Select independent variable(s)",
                      choices = colnames(
                        attributes %>% select(!c(gauge_id))
                        ))
    
    # Update visualize variables
    updateSelectInput(session, "visual_catchment_attr", "Select atribute",
                choices = names(attributes)[sapply(attributes, is.numeric)],
                selected = NA)
    }, ignoreNULL = TRUE)
  
  #============================================================================#
  #                         2. Select near-natural catchments                  #
  #============================================================================#
  observeEvent(
    c(input$selectFlowRegime,
      input$maxAgri,
      input$maxNrDams,
      input$maxUrban,
      input$annualQTrend,
      input$nirtatePollutedArea,
      input$protectedArea,
      input$popDensity,
      input$maxWasteWaterDischarge), 
    {
      
      if (!is.null(hydro_indicator)){

        #----------------------------------------------------------------------
        selected <- hydro_indicator
        
        if (!"None" %in% input$selectFlowRegime){
          for (condition in selectFlowRegime){
            colname <- strsplit(condition, " ")[[1]][1]
            selected <- selected %>% filter(!!sym(colname) > 1.1)
          }
        } 
        
        selected_basins <<- Reduce(intersect, list(
          selected$gauge_id,
          
          attributes %>% 
            filter(agricultural_areas_perc <= input$maxAgri) %>% 
            pull(gauge_id), 
          
          attributes %>% 
            filter(artificial_surfaces_perc <= input$maxUrban) %>% 
            pull(gauge_id),
          
          attributes %>% 
            filter(dams_num <= input$maxNrDams) %>% 
            pull(gauge_id),
          
          hydro_indicator %>% 
            filter(abs(hydro_indicator$q_annual_sens_slope) <= input$annualQTrend) %>% 
            pull(gauge_id),
          
          attributes %>% 
            filter(nitrate_polluted_area_fraction <= input$nirtatePollutedArea) %>% 
            pull(gauge_id),
          
          attributes %>% 
            filter(protected_area_fraction >= input$protectedArea) %>% 
            pull(gauge_id),
          
          attributes %>% 
            filter(popdens >= 10^3*input$popDensity[1],
                   popdens <= 10^3*input$popDensity[2]) %>% 
            pull(gauge_id),
          
          attributes %>% 
            filter(wastewater_discharge_m3_year <= 10^6*input$maxWasteWaterDischarge) %>% 
            pull(gauge_id)
        ))
        
        if (length(selected_basins) < 10){
          message("lengh = ", length(selected_basins))
          showNotification("Please relax the conditions and select some basins", 
                           type = "warning")}
        
        # Update map of selected stations
        showGauge(stations, selected_basins)
        
        # Update catchment attribute taböe
        output$catchment_attributes <- DT::renderDataTable({
          showDataFrame(attributes, session, "catchment_attributes", 
                        selected_basins)
        })
        
        # Update hydrological indicator talbe
        output$hydro_indicator <- DT::renderDataTable({
          showDataFrame(hydro_indicator %>% filter(gauge_id %in% selected_basins), 
                        session,  "hydro_indicator")
        })
        
      }
      
    }, ignoreInit = TRUE)
  
  #============================================================================#
  #      3. Regression (linking hydro. indicators with catchment attributes)   #
  #============================================================================#
  observeEvent(input$runRegression, {
    
    # Get data for regression
    selectDepVar <- input$selectDepVar
    selectIndepVar <- input$selectIndepVar
    
    regression_df <- hydro_indicator %>% 
      filter(gauge_id %in% selected_basins) %>%
      select(c(gauge_id, {{selectDepVar}})) %>%
      right_join(attributes %>% 
                   select(c(gauge_id, {{selectIndepVar}})) %>%
                   filter(gauge_id %in% selected_basins),
                 by = "gauge_id") %>%
      drop_na()

    model <<- reg_models(data = regression_df, 
                  dependent_var = input$selectDepVar,    # Dependent variables
                  independent_var = input$selectIndepVar,
                  model_name = input$selectRegressionModel,
                  n_train_samples = 80)  # Independent variables

    output$regression_plot <- renderPlotly(
      subplot(model$plt, nrows = length(model$plt), 
              titleX = TRUE, titleY = TRUE) %>%
        layout(height = 280*length(model$plt))
      )
    
    # Calcuate near natural states for all others variables
    # Get the data frame of independent variables
    temp <- attributes %>% 
      filter(gauge_id %in% hydro_indicator$gauge_id) %>%
      select(c(gauge_id, input$selectIndepVar))
    
    # Get the near natural state using regression equation
    for (var in input$selectDepVar){
      temp[[paste0(var, "_near_nat")]] <- predict(model[[var]], temp)
    }
    
    # Combine with current state
    temp <- temp %>%
      left_join(hydro_indicator %>% select(c(gauge_id, input$selectDepVar)),
                by = "gauge_id")
    
    # Calculate the differences between near natural and current states
    for (var in input$selectDepVar){
      temp <- temp %>%
        mutate(!!paste0(var, "_diff") :=  100*
                 (!!sym(var) - !!sym(paste0(var, "_near_nat")))/
                 (!!sym(paste0(var, "_near_nat"))))
    }
    
    # Assign back to global variable to use later
    near_nat_states <<- temp
    
    # Update select distance to near natural state for visualization
    choices <- paste0(input$selectDepVar, "_near_nat")
    updateSelectInput(session, "visual_distance_to_near_nat", "Distance to ",
                      choices = choices,
                      selected = choices[1])
    
  }, ignoreInit = TRUE)

  #----------------------------------------------------------------------------#
  #        Differences between current states and target indicators            #
  #----------------------------------------------------------------------------#  
  observeEvent(input$selectDepVar, {
    updateSelectInput(session, "visual_distance_to_near_nat", 
                      "1. Visualize distance to",
                      choices = paste0(input$selectDepVar, "_near_nat"),
                      selected = NA)
  }, ignoreInit = TRUE)
  
  
  #============================================================================#
  #                               4. Visualization                             #
  #============================================================================#
  observeEvent(input$visual_selected_var, {
  
    if (!is.null(near_nat_states)){
      new_stations <- stations %>% 
        filter(gauge_id %in% hydro_indicator$gauge_id) %>%
        left_join(near_nat_states, by = "gauge_id")
      
      if (input$selectBasinGroup == "Near natural basins"){
        show_gauge_id <- selected_basins
      } else if(input$selectBasinGroup == "Non near-natural basin") {
        show_gauge_id <- setdiff(hydro_indicator$gauge_id, selected_basins)
      } else {
        show_gauge_id <- new_stations$gauge_id
      }
    }
        
    # Get variables name
    if (!is.null(near_nat_states) & 
        input$visualType == "Distance to neat-natural states"){
      
      variable <- paste0(gsub("_near_nat", "", 
                              input$visual_distance_to_near_nat), "_diff")
      
      shinyCatch(showGauge(new_stations, show_gauge_id, colorby = variable),
                 blocking_level = "error")
      
      # Update hydrological indicator talbe
      shinyCatch(
        output$hydro_indicator <- DT::renderDataTable({
          showDataFrame(
            st_drop_geometry(new_stations) %>% 
              mutate(across(where(is.numeric), function(x) round(x, 2))),
            session,  "hydro_indicator")
        }),
        blocking_level = "error")
    } else {
      
      message("Cannot visualize")
      
    }
    }, ignoreInit = TRUE)
   
  #============================================================================#
  #                     5. Other reactive functions                            #
  #============================================================================#
  
  #----------------------------------------------------------------------------# 
  #                 Show catchment when click on table                         #
  #----------------------------------------------------------------------------# 
  observeEvent(input$goto, {
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.01
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  }, ignoreInit = TRUE)
  
  #----------------------------------------------------------------------------# 
  #                 Show catchment shape file when click on gauge              #
  #----------------------------------------------------------------------------# 
  observeEvent(input$map_marker_click, {
    if (!is.null(input$map_marker_click$id)){
      if (input$map_marker_click$id %in% catchments$gauge_id){
        leafletProxy("map") %>%
          clearGroup("Gewägktes Einzugsgebiet") %>%
          addPolygons(
            data = catchments %>% filter(gauge_id == input$map_marker_click$id),
            stroke = TRUE,
            fillColor = "#00000000",
            weight = 2,
            popup = ~ showPopup(gauge_id),
            group = "Gewägktes Einzugsgebiet",
            layerId = ~ gauge_id)}
    }
  }, ignoreInit = TRUE)
  
}
