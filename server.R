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
      addScaleBar(position = "bottomleft") %>%
      addRasterImage(huek, opacity = 0.7, group = "Hydrogeologie") %>%
      addPolygons(data = schutzgetbiet, fillColor = "#006400", 
                  color = "#006400", fillOpacity = 0.6, stroke = TRUE, 
                  weight = 1, group = "Naturschutzgebiet") %>%
      addPolygons(data = nitratbelastete_gebiete, fillColor = "#DC3220", 
                  color = "#DC3220", fillOpacity = 0.6, stroke = TRUE, 
                  weight = 1, group = "Nitratbelastete Gebiete") %>%
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
                       group = "Abflussmessstelle",
                       fillColor = "#FFC107",
                       fillOpacity = 0.8,
                       stroke = FALSE,
                       popup = ~ showPopup(gauge_id),
                       layerId = ~ gauge_id
      ) %>%
      addCircleMarkers(data = gw_wells,
                       radius = 2,
                       group = "Grundwassermessstelle",
                       fillColor = "#2798F5",
                       fillOpacity = 0.7,
                       stroke = FALSE,
                       popup = ~ UFZ.ID,
                       layerId = ~ UFZ.ID
      ) %>%
      addPolygons(
        data = subset(catchments),
        stroke = TRUE,
        fillColor = "#00000000",
        color = "#C4C4C4",
        weight = 1,
        popup = ~ showPopup(gauge_id),
        group = "Einzugsgebiete",
        layerId = ~ gauge_id) %>%
      addLayersControl(
        baseGroups = c("CartoDBPositron", "CartoDBPositronNolabel", 
                       "OpenStreetMap", "OpenTopoMap", "WorldImagery"),
        overlayGroups = c("Abflussmessstelle", 
                          "Einzugsgebiete",
                          "Hydrogeologie",
                          "Naturschutzgebiet",
                          "Nitratbelastete Gebiete",
                          "Grundwassermessstelle"),
        options = layersControlOptions(position = "bottomleft")
      )  %>%
      hideGroup(c("Hydrogeologie", 
                  "Naturschutzgebiet",
                  "Nitratbelastete Gebiete",
                  "Grundwassermessstelle",
                  "Einzugsgebiete")) %>%
      setView(lng = 9, lat = 50, zoom = 5)
  })
    
  
  output$catchment_attributes <- DT::renderDataTable({
    showDataFrame(attributes, session, "catchment_attributes", NULL)
  })
  
  #----------------------------------------------------------------------------#
  #    Select catchment based on streamflow data availability (Data)           #
  #----------------------------------------------------------------------------#
  observeEvent(input$dataSubset, {

    attributes <<- read_csv("data/attributes.csv", show_col_types = FALSE)
    
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
    hydro_indicator <<- hydro_indicator %>%
      select(!c(p_mean, p_std)) 
    
    output$hydro_indicator <- DT::renderDataTable({hydro_indicator})
    
    # Update map
    showGauge(stations, hydro_indicator$gauge_id)
    
    # Update regression select dependent variables
    if (!is.null(hydro_indicator)){
      updateSelectInput(session,
                        "selectDepVar", 
                        choices = colnames(hydro_indicator %>% 
                                             select(!c(lat, long, gauge_id))))
    }
    
    # Update 
    updateSelectInput(session, "selectIndepVar", 
                      "2. Select independent variable(s)",
                      choices = colnames(attributes %>% select(!c(gauge_id))))
  }, ignoreInit = TRUE)
  
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
  #                 Add catchment shape file when click on gauge               #
  #----------------------------------------------------------------------------#
  observeEvent(input$map_marker_click, {
    if (!is.null(input$map_marker_click$id)){
      if (input$map_marker_click$id %in% catchments$gauge_id){
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
      }
  }, ignoreInit = TRUE)
  
  #----------------------------------------------------------------------------#
  #    Select catchment based on streamflow data availability (Data)           #
  #----------------------------------------------------------------------------#
  observeEvent(
    c(input$selectFlowRegime,
      input$maxAgri,
      input$maxNrDams,
      input$maxUrban,
      input$annualQTrend,
      input$nirtatePollutedArea,
      input$protectedArea), 
    {
      
      
      if (!is.null(hydro_indicator)){

        # Selected gauge id
        selected <- hydro_indicator$gauge_id
        
        # Selected basins from flow regime
        if (!"None" %in% input$selectFlowRegime){
          temp <- hydro_indicator
          for (condition in input$selectFlowRegime){
            colname <- strsplit(condition, " ")[[1]][1]
            temp <- temp %>% filter(!!sym(colname) > 1.1)
          }
          selected <- intersect(selected, temp$gauge_id)
        }
        
        # Select basins from maximum agricultural area
        selected <- intersect(
          selected, 
          attributes$gauge_id[which(attributes$agricultural_areas_perc 
                                    <= input$maxAgri)]
        )
        
        # Select basins from maximum urban area 
        selected <- intersect(
          selected, 
          attributes$gauge_id[which(attributes$artificial_surfaces_perc 
                                    <= input$maxUrban)]
        )
        
        # Select basins from number of dams
        selected <- intersect(
          selected, 
          attributes$gauge_id[which(attributes$dams_num <= input$maxNrDams)]
        )
        
        # Select basins with sen's slope within a given range
        selected <- intersect(
          selected, 
          hydro_indicator$gauge_id[which(
            abs(hydro_indicator$q_annual_sens_slope) <= input$annualQTrend)]
        )
        
        # Select basins with nitrate polluted area smaller than certain values
        selected <- intersect(
          selected, 
          attributes$gauge_id[which(
            attributes$nitrate_polluted_area_fraction <= input$nirtatePollutedArea
          )]
        )
        
        # Select basins with nitrate polluted area smaller than certain values
        selected <- intersect(
          selected, 
          attributes$gauge_id[which(
            attributes$protected_area_fraction >= input$protectedArea
          )]
        )
        
        selected <- intersect(
          selected, 
          attributes$gauge_id[which(
            attributes$protected_area_fraction >= input$protectedArea
          )]
        )
        
        
        
        # Update map of selected stations
        showGauge(stations, selected)
        
        # Update catchment attribute taböe
        output$catchment_attributes <- DT::renderDataTable({
          showDataFrame(attributes, session, "catchment_attributes", 
                        selected)
        })
        
        # Update hydrological indicator talbe
        output$hydro_indicator <- DT::renderDataTable({
          showDataFrame(hydro_indicator %>% 
                          filter(gauge_id %in% selected), 
                        session,  "hydro_indicator")
        })
        
        # Assign to global variables so can be used later
        selected_gauge_id <<- selected
      }
      
    }, ignoreInit = TRUE)
  
  #----------------------------------------------------------------------------#
  # Regression: linking hydrological indicators and catchment characteristics  #
  #----------------------------------------------------------------------------#
  observeEvent(input$runRegression, {
    
    # Currently this is for multi-linear regression model
    dependent_var <- input$selectDepVar  
    independent_var <- input$selectIndepVar 
    
    # Get data for regression
    regression_df <- hydro_indicator %>% 
      filter(gauge_id %in% selected_gauge_id) %>%
      select(c(gauge_id, {{dependent_var}})) %>%
      right_join(attributes %>% 
                   select(c(gauge_id, {{independent_var}})) %>%
                   filter(gauge_id %in% selected_gauge_id),
                 by = "gauge_id") %>%
      drop_na()
      
    model <<- multiLinearReg(regression_df, dependent_var, independent_var)
    
    output$regression_plot <- plotly::renderPlotly(
      subplot(model$plt, nrows = length(model$plt), 
              titleX = TRUE, titleY = TRUE) %>%
        layout(height = 280*length(model$plt))
      )
  }, ignoreInit = TRUE)
  
  #----------------------------------------------------------------------------#
  #              Calculate near natural states of all catchments               #
  #----------------------------------------------------------------------------#
  observeEvent(input$calculate_near_nat, {
    
    if ((input$selectRegressionModel == "Multiple Linear Regression") &
        !is.null(hydro_indicator)){
      
      # Get the data frame of independent variables
      near_nat_states <<- attributes %>% 
        filter(gauge_id %in% hydro_indicator$gauge_id) %>%
        select(c(gauge_id, input$selectIndepVar))
      
      # Get the near natural state using regression equation
      for (var in input$selectDepVar){
        near_nat_states[[paste0(var, "_near_nat")]] <<- as.numeric(
          predict(model[[var]], near_nat_states)
          )
      }
      
      # Combine with current state
      near_nat_states <<- near_nat_states %>%
        left_join(hydro_indicator %>% 
                    select(c(gauge_id, input$selectDepVar)),
                  by = "gauge_id")

      # Calculate the differences between near natural and current states
      for (var in input$selectDepVar){ #c("q_mean","q_std")){ #input$selectDepVar){
        near_nat_states <<- near_nat_states %>%
          mutate(!!paste0(var, "_diff") := !!sym(paste0(var, "_near_nat")) - 
                   !!sym(var))
      }
    }
    
  }, ignoreInit = TRUE)
  
  #----------------------------------------------------------------------------#
  #        Differences between current states and target indictors             #
  #----------------------------------------------------------------------------#  
  observeEvent(input$selectDepVar, {
    updateSelectInput(session, "selectDiff", 
                      "1. Select hydrological indicators",
                      choices = paste0(input$selectDepVar, "_near_nat - ",
                                       input$selectDepVar),
                      selected = NA)
  }, ignoreInit = TRUE)
  
  #----------------------------------------------------------------------------#
  #              Calculate near natural states of all catchments               #
  #----------------------------------------------------------------------------#
  observeEvent(c(input$selectDiff, input$selectBasinGroup), {
    
    # Get variables name
    if (!is.null(near_nat_states)){
      variable <- paste0(strsplit(input$selectDiff, split = " - ")[[1]][2], 
                         "_diff")
      
      new_stations <- stations %>% 
        filter(gauge_id %in% hydro_indicator$gauge_id) %>%
        left_join(near_nat_states, by = "gauge_id")

      
      if (input$selectBasinGroup == "Near natural basins"){
        show_gauge_id <- selected_gauge_id
      } else if(input$selectBasinGroup == "Non near-natural basin") {
        show_gauge_id <- setdiff(hydro_indicator$gauge_id, selected_gauge_id)
      } else {
        show_gauge_id <- new_stations$gauge_id
      }
      
      showGauge(new_stations, show_gauge_id, colorby = variable)
      } 
    }, ignoreInit = TRUE)
   


  
  #----------------------------------------------------------------------------#
  #    Select catchment based on streamflow data availability (Data)           #
  #----------------------------------------------------------------------------#  
  
}
