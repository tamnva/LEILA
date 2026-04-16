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
      addPolygons(data = catchments,
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
  
  #============================================================================#
  #    1. Select catchment based on streamflow data availability (Data)        #
  #============================================================================#
  observeEvent(input$dataSubset, {
    
    # Replace the default hydrological indicators if settings are changed
    if (!(input$selectPeriod[1] == as.Date("1990-01-01") &
         input$selectPeriod[2] == as.Date("2020-12-31") &
         input$maxQmissing == 10)){
      
      streamflow_statistic <<- getStreamflowStatistics(
        timeseries_camels_combine = timeseries_camels,
        variable_name = c("discharge_spec_obs", "precipitation_mean"),
        start_date    = input$selectPeriod[1],
        end_date      = input$selectPeriod[2],
        max_missing   = input$maxQmissing) %>%
        mutate(across(where(is.numeric), function(x) round(x, 3)))
      
      # Update streamflow statistics in attributes
      att_hydro <<- read_csv("data/attributes_and_hydrological_indicator.csv", 
                             show_col_types = FALSE) %>%
        filter(gauge_id %in% streamflow_statistic$gauge_id) %>%
        rows_update(streamflow_statistic, by = "gauge_id")
    } else {
      att_hydro <<- att_hydro %>% drop_na(q_mean)
    }
    
    # Update the catchment attributes table
    output$catchment_attributes <- DT::renderDataTable({
      showDataFrame(att_hydro, session, "catchment_attributes", 
                    att_hydro$gauge_id)})
    
    # Update map
    showGauge(stations, att_hydro$gauge_id)
    
    # Update regression select dependent and independent variables
    column_names <- colnames(att_hydro %>% select(!c(lat, long, gauge_id)))
    updateSelectInput(session, "selectDepVar", choices = column_names)
    updateSelectInput(session, "selectIndepVar", choices = column_names)
    updateSelectInput(session, "visual_catchment_attr",choices = column_names)

    }, ignoreNULL = FALSE)
  
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
    
      selected <- att_hydro
      
      if (!"None" %in% input$selectFlowRegime){
        for (condition in selectFlowRegime){
          colname <- strsplit(condition, " ")[[1]][1]
          selected <- selected %>% filter(!!sym(colname) > 1.1)
        }
      } 
      
      selected_basins <<- Reduce(intersect, list(
        selected$gauge_id,
        
        att_hydro %>% 
          filter(agricultural_areas_perc <= input$maxAgri) %>% 
          pull(gauge_id), 
        
        att_hydro %>% 
          filter(artificial_surfaces_perc <= input$maxUrban) %>% 
          pull(gauge_id),
        
        att_hydro %>% 
          filter(dams_num <= input$maxNrDams) %>% 
          pull(gauge_id),
        
        att_hydro %>% 
          filter(abs(att_hydro$q_annual_sens_slope) <= 
                   input$annualQTrend) %>% 
          pull(gauge_id),
        
        att_hydro %>% 
          filter(nitrate_polluted_area_fraction <= 
                   input$nirtatePollutedArea) %>% 
          pull(gauge_id),
        
        att_hydro %>% 
          filter(protected_area_fraction >= input$protectedArea) %>% 
          pull(gauge_id),
        
        att_hydro %>% 
          filter(popdens >= 10^3*input$popDensity[1],
                 popdens <= 10^3*input$popDensity[2]) %>% 
          pull(gauge_id),
        
        att_hydro %>% 
          filter(wastewater_discharge_m3_year <= 
                   10^6*input$maxWasteWaterDischarge) %>% 
          pull(gauge_id)
      ))
      
      if (length(selected_basins) < 5){
        message("lengh = ", length(selected_basins))
        showNotification(paste0("Please relax the conditions to allow ",
                                "some basins to be selected"),
                         type = "error")}
      
      # Update map of selected stations
      showGauge(stations, selected_basins)
      
      # Update catchment attribute taböe
      output$catchment_attributes <- DT::renderDataTable({
        showDataFrame(att_hydro,session, "catchment_attributes",selected_basins)
      })
      
    }, ignoreInit = TRUE)
  
  #============================================================================#
  #      3. Regression (linking hydro. indicators with catchment attributes)   #
  #============================================================================#
  observeEvent(input$runRegression, {
    
    # Get data for regression
    selectDepVarIndepVar <- c(input$selectDepVar, input$selectIndepVar)
    
    regression_df <- att_hydro %>% 
      filter(gauge_id %in% selected_basins) %>%
      select(c(gauge_id, {{selectDepVarIndepVar}})) %>%
      drop_na()

    model <<- reg_models(data = regression_df, 
                  dependent_var = input$selectDepVar,
                  independent_var = input$selectIndepVar,
                  model_name = input$selectRegressionModel,
                  n_train_samples = 80)  

    output$regression_plot <- renderPlotly(
      subplot(model$plt, nrows = length(model$plt), 
              titleX = TRUE, titleY = TRUE) %>%
        layout(height = 280*length(model$plt))
      )
    
    # Calculate near natural states for all others variables
    for (var in input$selectDepVar){
      att_hydro[[paste0(var, "_near_nat")]] <<- predict(model[[var]], att_hydro)
    }

    # Calculate the differences between near natural and current states
    for (var in input$selectDepVar){
      att_hydro <<- att_hydro %>%
        mutate(!!paste0(var, "_diff") :=  100*
                 (!!sym(var) - !!sym(paste0(var, "_near_nat")))/
                 (!!sym(paste0(var, "_near_nat"))))
    }
    
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

    if (input$selectBasinGroup == "Near natural basins"){
      show_gauge_id <- selected_basins
    } else if(input$selectBasinGroup == "Non near-natural basin") {
      show_gauge_id <- setdiff(att_hydro$gauge_id, selected_basins)
    } else {
      show_gauge_id <- att_hydro$gauge_id
    }
      
  
    data.table::fwrite(att_hydro, "tam.csv")
    att_hydro <- data.table::fread("tam.csv")
    
    # Get variables name
    if (input$visualType == "Distance to neat-natural states"){
      
      variable <- paste0(gsub("_near_nat", "", 
                              input$visual_distance_to_near_nat), "_diff")
      
      shinyCatch(
        showGauge(st_as_sf(att_hydro, coords = c("long", "lat"), crs = 4326), 
                  show_gauge_id, colorby = variable),
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
  
  
  #----------------------------------------------------------------------------# 
  #                 Update select dependent variables                          #
  #----------------------------------------------------------------------------# 
  observeEvent(input$selectIndepVar, {
    column_names <- colnames(att_hydro %>% select(
      !c(lat, long, gauge_id, input$selectIndepVar)))
    
    updateSelectInput(session, "selectDepVar", 
                      choices = column_names)
  }, ignoreInit = TRUE)
  
}
