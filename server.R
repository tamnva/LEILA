

function(input, output, session) {

  # Stop the app when user close the browser
  session$onSessionEnded(function(){
    shiny::stopApp()
  })
  
  #----------------------------------------------------------------------------#
  #               Background + default maps/tables                             #
  #----------------------------------------------------------------------------#
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "OpenStreetMap") %>%
      addScaleBar(position = "bottomright") %>%
      addRasterImage(huek, opacity = 0.7, group = "Hydrogeologie") %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels,
                       group = "CartoDBPositronNolabel") %>%
      addProviderTiles(providers$CartoDB.Positron,
                       group = "CartoDBPositron") %>%
      addProviderTiles(providers$OpenTopoMap,
                       group = "OpenTopoMap") %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       group = "WorldImagery") %>%
      addCircleMarkers(data = stations,
                       radius = 3,
                       group = "Alle Einzugsgebiete",
                       fillColor = "#FFC107",
                       fillOpacity = 0.8,
                       stroke = FALSE,
                       popup = ~ showPopup(gauge_id),
                       layerId = ~ gauge_id
      ) %>%
      addLayersControl(
        baseGroups = c("CartoDBPositron", "CartoDBPositronNolabel", 
                       "OpenStreetMap", "OpenTopoMap", "WorldImagery"),
        overlayGroups = c("Alle Einzugsgebiete",
                          "Hydrogeologie", 
                          "Ökologischer Zustand der Fließgewässer",
                          "Grundwasserqualität",
                          "Grundwasser-Vulnerabilität"),
        options = layersControlOptions(position = "bottomleft")
      )  %>%
      hideGroup(c("Hydrogeologie", 
                  "Ökologischer Zustand der Fließgewässer",
                  "Grundwasserqualität",
                  "Grundwasser-Vulnerabilität")) %>%
      setView(lng = 9, lat = 50, zoom = 5)
  })
  
  
  output$catchment_attributes <- DT::renderDataTable({
    showDataFrame(attributes, session, "catchment_attributes", NULL)
  })
  
  #----------------------------------------------------------------------------#
  #    Select catchment based on streamflow data availability (Data)           #
  #----------------------------------------------------------------------------#
  observeEvent(input$dataSubset, {

    # Read streamflow data from CAMELS-DE
    streamflow_statistic <<- getStreamflowStatistics(
      timeseries_camels_combine = timeseries_camels_combine_file,
      variable_name = c("discharge_spec_obs", "precipitation_mean"),
      start_date = input$selectPeriod[1],
      end_date = input$selectPeriod[2],
      max_missing = input$maxQmissing)
    
    hydrologische_indikatoren <<-  attributes %>% 
      dplyr::filter(gauge_id %in% streamflow_statistic$gauge_id) %>%
      dplyr::select(lat, long, gauge_id) %>% 
      dplyr::left_join(streamflow_statistic, by = "gauge_id")
    
    # Display hydrological indicators
    output$hydrologische_indikatoren <- DT::renderDataTable({
      showDataFrame(attributes, session, "hydrologische_indikatoren", 
                    streamflow_statistic$gauge_id)
    })
    
    # Display catchment attributes
    output$catchment_attributes <- DT::renderDataTable({
      showDataFrame(attributes, session, "catchment_attributes", 
                    streamflow_statistic$gauge_id)})
    
    # Update map
    showGauge(stations, streamflow_statistic$gauge_id)

  })
  
  

  #----------------------------------------------------------------------------#
  #                 Show catchment when click on table                         #
  #----------------------------------------------------------------------------#
  observe({
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
  })
  
  #----------------------------------------------------------------------------#
  #                 Add catchment shape file when click on gauge               #
  #----------------------------------------------------------------------------#
  observeEvent(input$map_marker_click, {
    
    if (!is.null(input$map_marker_click$id)){
      leafletProxy("map") %>%
        clearGroup("Gewägktes Einzugsgebiet") %>%
        addPolygons(
          data = subset(catchments, gauge_id == input$map_marker_click$id),
          stroke = TRUE,
          fillColor = "#00000000",
          weight = 2,
          popup = ~ showPopup(gauge_id),
          group = "Gewägktes Einzugsgebiet",
          layerId = ~ gauge_id)}
  })
  
  #----------------------------------------------------------------------------#
  #    Select catchment based on streamflow data availability (Data)           #
  #----------------------------------------------------------------------------#
  observeEvent(input$selectFlowRegime, {
    if (!is.null(hydrologische_indikatoren)){
      
      update_hydrologische_indikatoren <<- hydrologische_indikatoren
      
      for (condition in input$selectFlowRegime){
        colname <- strsplit(condition, " ")[[1]][1]
        
        update_hydrologische_indikatoren <<- update_hydrologische_indikatoren %>%
          filter(!!sym(colname) > 1.1)
        
        message("colname = ", colname, " nstations = ", nrow(update_hydrologische_indikatoren))
      }
      
      # Display catchment attributes
      output$catchment_attributes <- DT::renderDataTable({
        showDataFrame(attributes, session, "catchment_attributes", 
                      update_hydrologische_indikatoren$gauge_id)
      })
      
      # Display hydrological indicators
      output$hydrologische_indikatoren <- DT::renderDataTable({
        showDataFrame(update_hydrologische_indikatoren, session, 
                      "hydrologische_indikatoren")
      })
      
      # Update map
      showGauge(stations, update_hydrologische_indikatoren$gauge_id)
    } 
    
  })
  
  
  #----------------------------------------------------------------------------#
  #    Select catchment based on streamflow data availability (Data)           #
  #----------------------------------------------------------------------------#
  
}
