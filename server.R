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

    #streamflow_statistic <<- getStreamflowStatistics(
    #  timeseries_camels_combine = timeseries_camels_combine_file,
    #  variable_name = c("discharge_spec_obs", "precipitation_mean"),
    #  start_date = input$selectPeriod[1],
    #  end_date = input$selectPeriod[2],
    #  max_missing = input$maxQmissing) %>%
    #  mutate(across(where(is.numeric), function(x) round(x, 3)))
    
    #hydro_indicator <<-  attributes %>% 
    #  dplyr::filter(gauge_id %in% streamflow_statistic$gauge_id) %>%
    #  dplyr::select(lat, long, gauge_id) %>% 
    #  dplyr::left_join(streamflow_statistic, by = "gauge_id")
    
    hydro_indicator <<- readr::read_csv("data/hydro_indicator.csv",
                                        show_col_types = FALSE)
    
    # Display hydrological indicators
    output$hydro_indicator <- DT::renderDataTable({
      showDataFrame(attributes, session, "hydro_indicator", 
                    hydro_indicator$gauge_id)
    })
    
    # Display catchment attributes
    output$catchment_attributes <- DT::renderDataTable({
      showDataFrame(attributes, session, "catchment_attributes", 
                    hydro_indicator$gauge_id)})
    
    # Update map
    showGauge(stations, hydro_indicator$gauge_id)

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
    
    if (!("None" %in% input$selectFlowRegime) & 
         !is.null(hydro_indicator)){
      
      update_hydro_indicator <- hydro_indicator
      
      for (condition in input$selectFlowRegime){
        colname <- strsplit(condition, " ")[[1]][1]
        update_hydro_indicator <- update_hydro_indicator %>%
          filter(!!sym(colname) > 1.1)
      }
      
      showNotification(
        paste0("Number of targeted catchments: ", 
               nrow(update_hydro_indicator)),
               type = "message", 
               duration = 3
        )
      
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
    
    show_notification(input$runRegression)
    
    dependent_var <- input$selectDepVar #c("Q_5")
    independent_var <- input$selectIndepVar[1] #c("p_mean", "p_seasonality", "frac_snow", "high_prec_freq")
    
    # Get data for regression
    regression_df <- hydro_indicator %>% 
      select(c(gauge_id, {{dependent_var}})) %>%
      right_join(attributes %>% 
                   select(c(gauge_id, {{independent_var}})) %>%
                   filter(gauge_id %in% hydro_indicator$gauge_id),
                 by = "gauge_id") %>%
      drop_na()
      
    
    model <- multiLinearReg(regression_df, dependent_var, independent_var)
    
    fitted <- model$fitted.values
    actual <- regression_df[[dependent_var]]
    
    output$regression_plot <- plotly::renderPlotly(ggplotly(
      ggplot( ) + 
        geom_point(aes(x = fitted, y = actual), alpha = 0.4, size = 1, 
                   color = "#1E88E5")+
        labs(x = "Fitted values", y = "Actual values",
             title = paste0("Hydrological indicator: ", dependent_var))+
        theme_bw() +
        theme(plot.title = element_text(size = 11),
              axis.title = element_text(size = 10),
              text = element_text(family = "Arial"))
    ))
    
  })
  #----------------------------------------------------------------------------#
  #    Select catchment based on streamflow data availability (Data)           #
  #----------------------------------------------------------------------------#    
  
}
