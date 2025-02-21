#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(spData)
library(dplyr)
library(sf)
library(bslib)
library(DT)
library(tidyverse)

rm(list=ls());gc()
# setwd("yourworkspace")
dat <- read_sf('data/mapdata_simplify.shp')
# dat %>% 
#   st_simplify(dTolerance = 10) -> dat
dat %>% 
  mutate_if(is.numeric,round,3) -> dat

dat %>% 
  mutate(rr=ifelse(rr<1,1,rr),
         rrlow=ifelse(rrlow<1,1,rrlow),
         rrhigh=ifelse(rrhigh<1,1,rrhigh),
         an=ifelse(an<0,0,an),
         anlow=ifelse(anlow<0,0,anlow),
         anhigh=ifelse(anhigh<0,0,anhigh),
         af=ifelse(af<0,0,af),
         aflow=ifelse(aflow<0,0,aflow),
         afhigh=ifelse(afhigh<0,0,afhigh),
         amr=ifelse(amr<0,0,amr),
         amrlow=ifelse(amrlow<0,0,amrlow),
         amrhigh=ifelse(amrhigh<0,0,amrhigh),
         amr=ifelse(amr>quantile(amr,0.99),quantile(amr,0.99),amr),
         amrlow=ifelse(amrlow>quantile(amrlow,0.99),quantile(amrlow,0.99),amrlow),
         amrhigh=ifelse(amrhigh>quantile(amrhigh,0.99),quantile(amrhigh,0.99),amrhigh)) %>% 
  rename(RR=rr,RRlow=rrlow,RRhigh=rrhigh,
         AN=an,ANlow=anlow,ANhigh=anhigh,
         AF=af,AFlow=aflow,AFhigh=afhigh,
         AMR=amr,AMRlow=amrlow,AMRhigh=amrhigh) -> dat_plt

dat %>% 
  rename(RR=rr,RRlow=rrlow,RRhigh=rrhigh,
         AN=an,ANlow=anlow,ANhigh=anhigh,
         AF=af,AFlow=aflow,AFhigh=afhigh,
         AMR=amr,AMRlow=amrlow,AMRhigh=amrhigh) -> dat
dat_plt %>% 
  st_drop_geometry() -> cleantable


vars <- c(
  'RR','RRlow','RRhigh',
  'AN','ANlow','ANhigh',
  'AF','AFlow','AFhigh',
  'AMR','AMRlow','AMRhigh'
)
stename <- unique(dat_plt$STEname)
cityname <- unique(dat_plt$SA3name)
communityname <- unique(dat_plt$SA2name)


# Define server logic required to draw a map and table
server <- function(input, output, session) {
  
  
  # Reactive expression to define the palette based on selected variable
  
  output$map <- renderLeaflet({
    
    # Ensure the reactive palette is used correctly
    pal <- colorNumeric("Blues", NULL, reverse=T)
    
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
                  fillColor = ~colorBin("Blues", bins=9, colorData)(colorData),
                  label = ~paste0(SA2name, ": ", formatC(colorData, big.mark = ","))) %>% 
      addLegend("topleft",pal = colorBin("Blues", bins=9, colorData), 
                values = ~colorData,
                opacity = 1, title=varBy,
                layerId = 'colorLegend')
  })
  
  
  ## Data Explorer ###########################################
  
  observe({
    sa3 <- if (is.null(input$states)) character(0) else {
      dplyr::filter(cleantable, STEname %in% input$states) %>%
        `$`('SA3name') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$sa3[input$sa3 %in% sa3])
    updateSelectizeInput(session, "sa3", choices = sa3,
                         selected = stillSelected, server = TRUE)
  })
  
  observe({
    sa2 <- if (is.null(input$states)) character(0) else {
      cleantable %>%
        dplyr::filter(STEname %in% input$states,
                      is.null(input$sa3) | SA3name %in% input$sa3) %>%
        `$`('SA2name') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$sa2[input$sa2 %in% sa2])
    updateSelectizeInput(session, "sa2", choices = sa2,
                         selected = stillSelected, server = TRUE)
  })
  
  output$table <- renderDT({
    df <- cleantable %>%
      dplyr::filter(
        is.null(input$states) | STEname %in% input$states,
        is.null(input$sa3) | SA3name %in% input$sa3,
        is.null(input$sa2) | SA2name %in% input$sa2
      ) 
    action <- DT::dataTableAjax(session, df, outputId = "table")
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  },options = list(
    scrollX = TRUE  # Enable horizontal scrolling
  ))
}


ui <- navbarPage("HeatwaveAUS", id="nav",
                 
                 tabPanel("Interactive map",
                          div(class="outer",
                              
                              tags$head(
                                # Include our custom CSS
                                includeCSS("styles.css"),
                                includeScript("gomap.js")
                              ),
                              
                              # If not using custom CSS, set height of leafletOutput to a number instead of percent
                              leafletOutput("map", width="100%", height="100%"),
                              
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                            width = 330, height = "auto",
                                            
                                            h2("Australia explorer"),
                                            
                                            # Shiny versions prior to 0.11 should use class = "modal" instead.
                                            selectInput("var", "Var", vars, selected='RR')
                              ),
                              
                              
                              
                              tags$div(id="cite",
                                       'Data compiled for ', tags$em('Risk and Burden Map at SA2 level in Australia, 2016-2019'),
                                       ' by CARE Unit.'
                              )
                          )
                 ),
                 
                 tabPanel("Data explorer",
                          fluidRow(
                            column(3,
                                   selectInput("states", "STEname", c("All states"="", stename), multiple=TRUE)
                            ),
                            column(3,
                                   conditionalPanel("input.states",
                                                    selectInput("sa3", "SA3name", c("All SA3"=""), multiple=TRUE)
                                   )
                            ),
                            column(3,
                                   conditionalPanel("input.states",
                                                    selectInput("sa2", "SA2name", c("All SA2"=""), multiple=TRUE)
                                   )
                            )
                          ),
                          hr(),
                          DT::dataTableOutput("table")
                 ),
                 
                 conditionalPanel("false", icon("crosshair"))
)


# Run the application
shinyApp(ui = ui, server = server)




