# Define server logic required to draw a histogram

shinyServer(function(input, output) {

# -------- TAB 1
  
    output$q_Plot <- renderPlotly({

      q_Plot <- ggplot(temp_country2, aes(x=Year,y=Temp))+
        geom_point(aes(fill=Temp, text = glue("Highest Temperature:{round(Temp,2)} °C"))) +
        geom_smooth()+
        theme(plot.title = element_text(face = "bold"),
              panel.background = element_rect(fill = "#ffffff"))+
        labs(title = "Global Maximum Average Land Temperature by Country 1900-2013" )+
        scale_fill_gradient(low="blue", high="red")
      
      ggplotly(q_Plot, tooltip = "text")
    })
    
    output$bar_plot <- renderPlotly({
      temp_country3 <- temp_country_ %>% 
        filter(Year == input$year_id) %>%
        group_by(Country) %>% 
        summarise(max_temp = max(Temp)) %>% 
        arrange(desc(max_temp)) %>% 
        ungroup()
      
      bar_plot <- ggplot(data = temp_country3, mapping = aes(x = reorder(Country, max_temp), y = max_temp)) +
        geom_col(width = 0.2, aes(colour = max_temp, text = glue("Maximum Average Temperature:{round(max_temp,2)} °C"))) +
        labs(x = NULL, y = NULL) +
        theme(panel.background = element_rect(fill = "#ffffff"),
              axis.text.x = element_text(angle=60, size=10, hjust=5, vjust=8)) +
        scale_color_gradient(low="blue", high="red")
      
      ggplotly(bar_plot, tooltip = "text") %>% 
        config(displayModeBar = F)
    })
    
    output$bar_plot2 <- renderPlotly({
      temp_cool <- temp_less %>% 
        filter(Year == input$year_id) %>%
        group_by(Country) %>% 
        summarise(max_temp = max(Temp)) %>% 
        arrange(desc(max_temp)) %>% 
        ungroup()
      
      bar_plot2 <- ggplot(data = temp_cool, mapping = aes(x = reorder(Country, max_temp), y = max_temp)) +
        geom_col(width = 0.2, aes(colour = max_temp, text = glue("Maximum Average Temperature:{round(max_temp,2)} °C"))) +
        labs(x = NULL, y = NULL) +
        theme(panel.background = element_rect(fill = "#ffffff"),
              axis.text.x = element_text(angle=60, size=5, hjust=5, vjust=8)) +
        scale_color_gradient(low="blue", high="red")
      
      ggplotly(bar_plot2, tooltip = "text") %>% 
        config(displayModeBar = F)
    })
# -------- TAB 2
    
    output$leaflet <- renderLeaflet({
      
      m <- leaflet(shape) %>% 
        addProviderTiles("Esri.WorldImagery") %>%
        setView( lat=10, lng=0 , zoom=2) %>%
        # for choropleth
        addPolygons( 
          fillColor = ~mypalette(max_temp), 
          color = "white",
          dashArray = "3", 
          fillOpacity = 0.6,
          weight=1,
          label = mytext,
          labelOptions = labelOptions( 
            style = list("font-weight" = "normal", padding = "3px 8px"), 
            textsize = "13px", 
            direction = "auto"),
          popup = popup_shape
        ) %>%
        addLegend(pal=mypalette, 
                  values=~max_temp, opacity=0.9, 
                  title = paste("Maximum Temperature"), 
                  position = "bottomleft")
      m
    })

    output$point_plot <- renderPlotly({
      temp_line <- temp_hist %>% 
        filter(Country == input$country_id) %>% 
        group_by(Year) %>%
        summarise(max_temp = max(Temp)) %>% 
        arrange(desc(max_temp)) %>% 
        ungroup()
      
      point_plot <- ggplot(data = temp_line, mapping = aes(x=reorder(Year, max_temp), 
                                                          y = max_temp)) +
        geom_point(aes(colour = max_temp, text = glue("Maximum Average Temperature:{round(max_temp,2)} °C"), size = 1)) +
        labs(x = NULL, y = NULL) +
        theme(axis.text.x = element_text(angle=60, size=10, hjust=5, vjust=8))+
        scale_color_gradient(low="yellow", high="red")
      
      ggplotly(point_plot, tooltip = "text")
    })
    
    output$point2_plot <- renderPlotly({
     point_month <- temp_month %>% 
        filter(Country == input$country_id) %>% 
        group_by(Month) %>%
        summarise(max_temp = max(Temp)) %>% 
        arrange(desc(max_temp)) %>% 
        ungroup()
      
      point2_plot <- ggplot(data = point_month, mapping = aes(x=reorder(Month, max_temp), 
                                                           y = max_temp)) +
        geom_point(aes(colour = max_temp, text = glue("Maximum Average Temperature:{round(max_temp,2)} °C"), size = 1)) +
        labs(x = NULL, y = NULL) +
        theme(axis.text.x = element_text(size=12, hjust=5, vjust=8))+
        scale_color_gradient(low="yellow", high="red")
      
      ggplotly(point2_plot, tooltip = "text")
    })
    
# -------- TAB 3
    
    output$table <- DT::renderDataTable(datatable, options = list(scrollX = T))
})
