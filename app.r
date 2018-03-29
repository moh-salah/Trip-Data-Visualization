library(shiny)
library(shinydashboard)
library(plyr)
library(tidyverse)
library(plotly)
library(leaflet)
library(data.table)
library(DT)
library(googleway)
library(RColorBrewer)

#global variables ####

#google maps basemap tiles
map_url_google <-"https://mts1.google.com/vt/lyrs=m&hl=en&src=app&x={x}&y={y}&z={z}&s=G"
#map zoom level
zoom_level <- 11
#google api key for googleway
my_key <- ""
#link to source code 
githubrepo <- "https://github.com/moh-salah/Trip-Data-Visualization"

#read data
df <- fread("fake_data.txt",na.strings=c("","NA"))
#count number of days for each person
df <- ddply(df,.(person_id),transform, days = length(unique(day_number)))
#define mode for googleway input
df$g_mode <- ifelse(df$mode %in% c('Walk', 'Walking', 'On foot'), 'walking',
                    ifelse(df$mode %in% c('Bus', 'Train', 'Transit'), 'transit',
                                          ifelse(df$mode %in% c('Bike', 'Biking', 'Bicycling'), 'bicycling', 'driving')))
#define list of person ids and the number of travel days
#this is used later to define the conditional selection of days
pers_days <- unique(df[c("person_id", "days")])

# UI side ####

ui <- dashboardPage(
  dashboardHeader(title = "Trip Data Visualization", titleWidth = 250),
  dashboardSidebar(collapsed = TRUE,
                   sidebarMenu(
                     menuItem("Trips", tabName = "main_tab", icon = icon("map")),
                     menuItem("About", tabName = "about_tab", icon = icon("info-circle")),
                     menuItem("Source code", icon = icon("github"), 
                              href = githubrepo)
                   )
  ),
  
  dashboardBody(
    tabItems(
    tabItem(tabName = "main_tab",
      fluidRow(
      tags$div(tags$style(HTML( ".selectize-dropdown, .selectize-dropdown.form-control{z-index:10000;}"))), #to show dropdown menu in front of the map
      box(title = 'Select/Enter Person ID:', status = 'primary', solidHeader = TRUE,
        selectInput(
          inputId ='person_id'
          ,label =''
          ,choices = df$person_id
          ,selected= 1
        )
        
      ),
      
      box(title = 'Select Diary Day:', status = 'primary', solidHeader = TRUE,
        uiOutput('day_number') #use uiOutput along with renderUI to condition day_number choices on person_id
      ),
      
      box(width = 12,
          leafletOutput('map01', height = '400px')
      ),
      
      box(width = 12, title='Map Controls', collapsible = T, collapsed = T,
          actionButton("reset_button", "Reset Map View"),
          actionButton("clear_selection", "Clear Selections")
      ),
      
      box(width=12,
          div(style = 'overflow-x: scroll', DT::dataTableOutput('table01'))
      )
    )
    
    #adjusting actionButton alignment
    ,tags$style(type='text/css', "#reset_button {margin-left: 25px;}")
    ,tags$style(type='text/css', "#clear_selection {margin-left: 25px;}")
    ),
    
  tabItem(tabName = 'about_tab',
    fluidRow(
    tabBox(width=12, title= 'About', id='about', side = 'left',
        tabPanel("Description",
               includeMarkdown('description.Rmd')
                 ),
        tabPanel("Author",
                 includeMarkdown('author.Rmd')
                 )
       )
    #adjusting font size of the headers of the about tabs'
    ,tags$style(type='text/css', "#about {font-size: 20px} ")
      )
    )
  )
  )
)

# Server side ####

server <- function(input, output, session) { 
    
    #create a variable containg maxdays to condition day input choices
    maxdays <- reactive({
      pers_days$days[pers_days$person_id==input$person_id]
    })
    
    #creating day_number selectInput based on UiOutput
    output$day_number <- renderUI({
      selectInput(inputId ='day_x',label = '', choices=seq(1,maxdays()))
    })
    
  #the app is initiated with a NULL value for conditions based on renderUI  
  #create a 'day' variable to store the input day 
  #this is only needed for the mode attribute in googleway to work properly- all the indicies have to have a value  
  day <- reactive({
      if(is.null(input$day_x) || input$day_x>maxdays()) return(1) else return(input$day_x)
    })
  
  #subset data based on selection inputs
  df_sub <-  reactive({
    subset(df, df$person_id==input$person_id & df$day_number==day())
  })
  
  #create direction by mode using 'googleway'
  routes <-  reactive({
    lapply(1:(nrow(df_sub())), function(x){
     directions  <- google_directions(origin = df_sub()[x,c('start_lat','start_lon')],
                               destination = df_sub()[x, c('end_lat','end_lon')],
                               key = my_key,
                               #mode = 'driving',
                               mode = df_sub()[x,c('g_mode')],
                               simplify = TRUE)
     #there is a function to plot polylines directly but not for leaflet
     #decode the polyline and then plot the polyline in leaflet later
      points <- decode_pl(directions$routes$overview_polyline$points)

      return(points)
    }) %>% bind_rows(.id = "trip_id") #group points by trip_id so polylines can be plotted for each trip
  })

  #create a palette to color-code trips
  pal <- reactive({
    colorFactor(palette = 'Set2', routes()$trip_id)
  })
  
  output$table01 <- DT::renderDataTable({
    DT::datatable(
      df_sub()[c('household_id','person_id', 'trip_id', 'trip_sequence', 'start', 'end',
                 'start_time', 'end_time', 'mode', 'purpose')]
      ,selection = "single"
      ,rownames=FALSE
      ,options=list(stateSave = TRUE, dom = 't')
      ,colnames = c('Household Id', 'Person Id', 'Trip Id', 'Trip Sequence', 'Trip Origin','Trip Destination',
                    'Departure Time', 'Arrival Time', 'Travel Mode', 'Trip Purpose')
    )
  })
  
  
  output$map01 <- renderLeaflet({
    leaflet(data = df_sub()) %>%
      setView(lng = mean(df_sub()$start_lon), lat = mean(df_sub()$start_lat), zoom = zoom_level) %>%
      addTiles(urlTemplate= map_url_google, group = 'Google Maps')%>%
      #another option for the base layer (light background)
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = 'Esri Canvas') %>% #CartoDB.Positron
      addLayersControl(
        position = c("topright"),
        baseGroups = c("Google Maps", "Esri Canvas"),
        options = layersControlOptions(collapsed = T, autoZIndex = T)
      )
  })
  
  #update map to plot markers and polylines for each person_id/day
  observe({
    leafletProxy("map01") %>%
      clearShapes() %>% 
      addMarkers(data=df_sub()
                 ,lng= df_sub()$start_lon
                 ,lat = df_sub()$start_lat
                 ,layerId = df_sub()$trip_id
                 ,label = paste0("", df_sub()$start)
                 ,labelOptions= labelOptions(direction = 'auto', noHide=F)
      ) %>%
      
      {
        
        for (x in 1:(length(unique(routes()$trip_id)))){
          
          polylines <-   addPolylines(., data = subset(routes(), routes()$trip_id==x)
                              ,lng = ~lon
                              ,lat = ~lat
                              ,group = ~trip_id
                              ,layerId = ~trip_id
                              ,label= paste0('Trip #: ',df_sub()$trip_sequence,', Travel Mode: ', df_sub()$mode)
                              ,opacity = 1
                              ,color = ~pal()(x)
                              ,labelOptions= labelOptions(direction = 'auto', noHide=F)
                              ,highlightOptions = highlightOptions(
                                color=~pal()(x), opacity = 1, weight = 7, fillOpacity = 1,
                                bringToFront = TRUE, sendToBack = TRUE
                              )
          )
        }
        return(polylines)
      }
    
  })
  
  #create a mechanism to highlight markers and polylines when selected in DT
  start_icon = makeAwesomeIcon(icon = 'map-marker', markerColor = 'green', iconColor = 'white')
  end_icon = makeAwesomeIcon(icon = 'flag', markerColor = 'red', iconColor = 'white')
 
  #for some reason the selected row id doesn't print null when deselected when called from an observeEvent
  row_selected <- reactive({
    input$table01_rows_selected
  })
  
  observe({
  if(!is.null(row_selected())){
  observeEvent(input$table01_rows_selected, {
    row_selected <- df_sub()[input$table01_rows_selected,]
    dat_sub <- subset(routes(), routes()$trip_id==input$table01_rows_selected)
    mean_lon <-  mean(dat_sub$lon)
    mean_lat <-  mean(dat_sub$lat)
    
    leafletProxy("map01") %>%
      removeShape("highlighted_polyline") %>%
      addPolylines(data = dat_sub
                              ,lng = ~lon
                              ,lat = ~lat
                              ,layerId = "highlighted_polyline"
                              ,label= paste0('Trip #: ',df_sub()$trip_sequence, ', Travel Mode: ', df_sub()$mode)
                              ,weight=6
                              ,opacity = 1
                              ,color="black"
          ) %>%
      addAwesomeMarkers(data = subset(df_sub(), df_sub()$trip_id==input$table01_rows_selected)
                 ,lng= row_selected$start_lon
                 ,lat =row_selected$start_lat
                 ,layerId = "highlighted_marker_start"
                 ,label = paste0("", row_selected$start)
                 ,labelOptions= labelOptions(direction = 'auto', noHide=F)
                 ,icon=start_icon
                 ,options = markerOptions(riseOnHover = TRUE))  %>%
      addAwesomeMarkers(data = subset(df_sub(), df_sub()$trip_id==input$table01_rows_selected)
                        ,lng= row_selected$end_lon
                        ,lat =row_selected$end_lat
                        ,layerId = "highlighted_marker_end"
                        ,label = paste0("", row_selected$end)
                        ,labelOptions= labelOptions(direction = 'auto', noHide=F)
                        ,icon=end_icon
                        ,options = markerOptions(riseOnHover = TRUE))  %>%
    fitBounds(lng1 = min(row_selected$start_lon, row_selected$end_lon)*0.9996,
              lng2 = max(row_selected$start_lon, row_selected$end_lon)*1.0004,
              lat1 = min(row_selected$start_lat,  row_selected$end_lat)*0.9996,
              lat2 = max(row_selected$start_lat,  row_selected$end_lat)*1.0004)
  })
 
  }else{
    leafletProxy("map01") %>%
      removeShape("highlighted_polyline") %>%
      removeMarker("highlighted_marker_start") %>%
      removeMarker("highlighted_marker_end")
  }
    
  })
  
  #create a mechanism to highlight DT rows when polylines are selected on the map
  observeEvent(input$map01_shape_click, {
    clickId <- input$map01_shape_click$id
    dataTableProxy("table01") %>%
      selectRows(which(input$table01_rows_all == clickId)) %>%
      selectPage(which(input$table01_rows_all == clickId) %/% input$table01_state$length + 1)
  })
  
  #reset view
  observe({
    input$reset_button
    leafletProxy("map01") %>%
      setView(lng = mean(df_sub()$start_lon), lat = mean(df_sub()$start_lat), zoom = zoom_level)
  })
  
  #clear selection
  observe({
    input$clear_selection
    leafletProxy("map01")%>%
      removeShape("highlighted_polyline") %>%
      removeMarker("highlighted_marker_start") %>%
      removeMarker("highlighted_marker_end")
  })
  
}

# Run the app ####

shinyApp(ui = ui, server = server)
