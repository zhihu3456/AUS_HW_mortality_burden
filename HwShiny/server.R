
# Define server logic required to draw a map and table
server <- function(input, output) {
  
  
  # Reactive expression to define the palette based on selected variable
  
  output$map <- renderLeaflet({
    
    # Ensure the reactive palette is used correctly
    pal <- colorNumeric("viridis", NULL, reverse=T)
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = 133.7751, lat = -25.2744, zoom = 4)
  })
  
  
  observe({
    varBy <- input$var
    
    colorData <- dat_plt[[varBy]]
    pal <- colorBin("viridis", colorData, 7, pretty = FALSE, reverse=T)
    
    leafletProxy("map", data = dat_plt) %>%
      clearShapes() %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                  fillColor = ~colorQuantile("viridis", n=10, colorData)(colorData),
                  label = ~paste0(SA2name, ": ", formatC(colorData, big.mark = ","))) %>%
      addLegend("topleft",pal = colorQuantile("viridis", n=10, colorData), 
                values = ~colorData,
                labels = c('0-10%','10-20%','20-30%','30-40%','40-50%','50-60%','60-70%','70-80%','80-90%','90-100%'),
                opacity = 1, title=varBy,
                layerId = 'colorLegend')
  })
  
  
  output$table <- renderDT({
    dat %>% 
      st_drop_geometry()
  },options = list(
    scrollX = TRUE  # Enable horizontal scrolling
  ))
}