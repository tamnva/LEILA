library(plotly)

function(input, output, session) {

  # Stop the app when user close the browser
  session$onSessionEnded(function(){
    shiny::stopApp()
  })
  
  #----------------------------------------------------------------------------#
  #               Background + default maps/tables                             #
  #----------------------------------------------------------------------------#
  output$map <- renderLeaflet({
    leaflet(schutzgetbiet) %>%
      addTiles(group = "OpenStreetMap") %>%
      addScaleBar(position = "bottomleft") %>%
      addRasterImage(huek, opacity = 0.7, group = "Hydrogeologie") %>%
      addPolygons(fillColor = "#006400", color = "#006400",
                  fillOpacity = 0.6, stroke = TRUE, weight = 1,
                  group = "Naturschutzgebiet") %>%
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
                          "Naturschutzgebiet",
                          "Ökologischer Zustand der Fließgewässer",
                          "Grundwasserqualität",
                          "Grundwasser-Vulnerabilität"),
        options = layersControlOptions(position = "bottomleft")
      )  %>%
      hideGroup(c("Hydrogeologie", 
                  "Naturschutzgebiet",
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

    # Replace the default hydrological indicator if settings are change
    if ((input$selectPeriod[1] == as.Date("2001-01-01") &
         input$selectPeriod[2] == as.Date("2020-12-31") &
         input$maxQmissing == 5)){
      hydro_indicator <<- readr::read_csv("data/hydro_indicator.csv",
                                          show_col_types = FALSE)
    } else {
      streamflow_statistic <<- getStreamflowStatistics(
        timeseries_camels_combine = timeseries_camels_combine_file,
        variable_name = c("discharge_spec_obs", "precipitation_mean"),
        start_date = input$selectPeriod[1],
        end_date = input$selectPeriod[2],
        max_missing = input$maxQmissing) %>%
        mutate(across(where(is.numeric), function(x) round(x, 3)))
      
      hydro_indicator <<-  attributes %>% 
        filter(gauge_id %in% streamflow_statistic$gauge_id) %>%
        select(lat, long, gauge_id) %>% 
        left_join(streamflow_statistic, by = "gauge_id")
      
      #data.table::fwrite(hydro_indicator, "data/hydro_indicator.csv")
    }
    
    # Display catchment attributes (add precipitation mean and std to this table)
    attributes <<- attributes %>% 
      left_join(hydro_indicator %>% select(p_mean, p_std, gauge_id), 
                       by = "gauge_id")
    
    output$catchment_attributes <- DT::renderDataTable({
      showDataFrame(attributes, session, "catchment_attributes", 
                    hydro_indicator$gauge_id)})
    
    # Display hydrological indicators, remove precipitation mean and std
    hydro_indicator <- hydro_indicator %>%
      select(!c(p_mean, p_std)) 
    
    output$hydro_indicator <- DT::renderDataTable({hydro_indicator})
    
    # Update map
    showGauge(stations, hydro_indicator$gauge_id)
    
    # Update regression select dependent variables
    if (!is.null(hydro_indicator)){
      updateSelectInput(session,
                        "selectDepVar", "3. Select dependent variable(s)",
                        choices = colnames(hydro_indicator %>% 
                                             select(!c(lat, long, gauge_id))))
    }
    
    # Update 
    updateSelectInput(session, "selectIndepVar", 
                      "2. Select independent variable(s)",
                      choices = colnames(attributes %>% select(!c(gauge_id))))

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
  observeEvent(c(input$selectFlowRegime,
                 input$maxAgri), {
    
    # Selected basins from flow regime
                   
    if (!("None" %in% input$selectFlowRegime) & 
         !is.null(hydro_indicator)){
      
      update_hydro_indicator <- hydro_indicator
      
      for (condition in input$selectFlowRegime){
        colname <- strsplit(condition, " ")[[1]][1]
        update_hydro_indicator <- update_hydro_indicator %>%
          filter(!!sym(colname) > 1.1)
      }

      # Display catchment attributes
      output$catchment_attributes <- DT::renderDataTable({
        showDataFrame(attributes, session, "catchment_attributes", 
                      update_hydro_indicator$gauge_id)
      })
      
      # Display hydrological indicators
      output$hydro_indicator <- DT::renderDataTable({
        showDataFrame(update_hydro_indicator, session, 
                      "hydro_indicator")
      })
      
      # Update map
      showGauge(stations, update_hydro_indicator$gauge_id)
      
      selected_catchment <<- update_hydro_indicator$gauge_id
      
    } else if (("None" %in% input$selectFlowRegime) &
               (!is.null(hydro_indicator))){
      
      # Display catchment attributes
      output$catchment_attributes <- DT::renderDataTable({
        showDataFrame(attributes, session, "catchment_attributes", 
                      hydro_indicator$gauge_id)
      })
      
      # Display hydrological indicators
      output$hydro_indicator <- DT::renderDataTable({
        showDataFrame(hydro_indicator, session, 
                      "hydro_indicator")
      })
      
      # Update map
      showGauge(stations, hydro_indicator$gauge_id)
    }
    
    # Update regression select dependent variables
    if (!is.null(hydro_indicator)){
      updateSelectInput(session,
                        "selectDepVar", "3. Select dependent variable(s)",
                        choices = colnames(hydro_indicator %>% 
                                             select(!c(lat, long, gauge_id))))
    }
    
  })
  
  #----------------------------------------------------------------------------#
  # Regression: linking hydrological indicators and catchment characteristics  #
  #----------------------------------------------------------------------------#
  observeEvent(input$runRegression, {
    
    # Currently this is for multi-linear regression model
    dependent_var <- input$selectDepVar  
    independent_var <- input$selectIndepVar 
    
    # Get data for regression
    regression_df <- hydro_indicator %>% 
      select(c(gauge_id, {{dependent_var}})) %>%
      right_join(attributes %>% 
                   select(c(gauge_id, {{independent_var}})) %>%
                   filter(gauge_id %in% hydro_indicator$gauge_id),
                 by = "gauge_id") %>%
      drop_na()
      
    model <<- multiLinearReg(regression_df, dependent_var, independent_var)
    
    output$regression_plot <- plotly::renderPlotly(
      subplot(model$plt, nrows = length(model$plt), 
              titleX = TRUE, titleY = TRUE) %>%
        layout(height = 280*length(model$plt))
      )
  })
  #----------------------------------------------------------------------------#
  #        Differences between current states and target indictors             #
  #----------------------------------------------------------------------------#  
  observeEvent(input$selectDepVar, {
    
    updateSelectInput(session, "selectHydro", 
                      "1. Select hydrological indicators",
                      choices = input$selectDepVar,
                      selected = NA)
    
  
  })
  
  
  #----------------------------------------------------------------------------#
  #    Select catchment based on streamflow data availability (Data)           #
  #----------------------------------------------------------------------------#  
  
}
