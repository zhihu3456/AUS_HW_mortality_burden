library(leaflet)
vars <- c(
  'RR','RRlow','RRhigh',
  'AN','ANlow','ANhigh',
  'AF','AFlow','AFhigh',
  'AMR','AMRlow','AMRhigh'
)

navbarPage("HeatwaveAUS", id="nav",
           
           tabPanel("Interactive map",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h2("Australia explorer"),
                                      
                                      selectInput("var", "Var", vars, selected='rr')
                                      
                        ),
                        
                        tags$div(id="cite",
                                 'Data compiled for ', tags$em('Coming Apart: The SA2 of Australia, 2016-2019'), ' by CARE Unit.'
                        )
                    )
           ),
           
           tabPanel("Data explorer",
                    fluidRow(
                      column(3,
                             selectInput("states", "STEcode", c("All states"="",), multiple=TRUE)
                      ),
                      column(3,
                             conditionalPanel("input.states",
                                              selectInput("cities", "SA3code", c("All SA3"=""), multiple=TRUE)
                             )
                      ),
                      column(3,
                             conditionalPanel("input.states",
                                              selectInput("sa2", "SA2code", c("All SA2"=""), multiple=TRUE)
                             )
                      )
                    ),
                    hr(),
                    DT::dataTableOutput("table")
           ),
           
           conditionalPanel("false", icon("crosshair"))
)
