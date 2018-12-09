
##load the needed libraries.
library(shiny)
library(rgdal)
library(leaflet)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(ggmap) # for Geocode.
library(wordcloud2)

# Data Loading
## Tab 1 data loading - Aniket 
load("Parks.spatial_Aniket.RData")
load("PF.spatial_Aniket.RData")
# Set the pal value
pal_aniket <- colorFactor(palette = 'Set1', domain =Parks.spatial_Aniket$Park_Type)
pal1_aniket <- colorFactor(palette = 'Set2', domain =PF.spatial_Aniket$POPL_TYPE)
## Tab 2 Data loading - Steve
# Load the data
load("Census_Steve.RData")
load("school_Steve.RData")
load("CensusVars_Steve.RData")
load("CensusLabels_Steve.RData")

## Tab 3 - Data loading - Patrick
load("cc_Patrick.RData")
load("enfors_Patrick.RData")
# Set city council district colors
cc$Num <- as.numeric(cc$Num)
qpal <- colorQuantile("Pastel2", domain = cc$Num, n = 6)
pal <- ~qpal(cc$Num)
mem <- as.character(cc$Council_Me)
# Set popups for city council districts
cc$popup <- paste("<b>",cc$Dist,"</b><br>",
                  "Council Member: ",cc$Council_Me,"<br>",
                  "Email : ", cc$Email, sep ="")
enfor$pop <- paste("<b>", "Status: ", enfor$Case_Status_Code_Description,"</b><br>",
                   "Case Number: ",enfor$Case_Number,"<br>",
                   "Code Type: ",enfor$Case_Type_Code_Description,"<br>",
                   "Date: ",enfor$Date,"<br>",
                   "Address : ", enfor$Street_Address, sep ="")

#Tab 4 - Data loading - Michael
load("districts_Michael.RData")
load("properties_Michael.RData")

#properties setting
pal_Michael <- colorFactor("Set1", districts_Michael$Dist)
properties_Michael$popup <- paste("<b>",paste(properties_Michael$Address_Nu,properties_Michael$Street_Nam),"</b><br>",
                          "Outcome: ",properties_Michael$Code_Enfor,"<br>", sep ="")

# Tab 5 Data loading - Vijay
 load("Contact_Management_final_Vijay.RData")
 load("Phone_Call_Log_final_Vijay.RData")
 load("SB_facilities_Vijay.RData" )

# Define UI for the application 
ui <- fluidPage(
   navbarPage("South Bend Executive Dashboard",
              tabPanel("Parks & Public facilities",         # Start - Aniket 
                       sidebarLayout(
                         sidebarPanel(
                           helpText('Map of South Bend is displayed. Toggle the checkboxes to see the location of each Park and Public Facility type'),
                           br(),
                           
                           checkboxGroupInput(inputId = "type", label = "Park Type",
                                              choices = list("Block Park", 
                                                             "Cemetery",
                                                             "Community Park",
                                                             "Golf Course",
                                                             "Memorial",
                                                             "Neighborhood Park",
                                                             "Special",
                                                             "Zoo"),
                                              selected = c("Block Park", "Cemetery")),
                           br(),br(),
                           
                           checkboxGroupInput(inputId = "PF_type", label = "Public facility Type",
                                              choices = list("Fire Station"="FIRE STATION", 
                                                             "Library"="LIBRARY"  ,
                                                             "Police Station"="POLICE STATION"),
                                              selected = c("Library"="LIBRARY" ))
                           
                         ),
                         mainPanel(leafletOutput("Parks"))
                       )
                       
              ),  # End - Aniket
              tabPanel("Census",  # Start - Steve 
                       sidebarLayout(
                         sidebarPanel(
                           selectInput(inputId = "DataValue",
                                       label = "Census Data to Display",
                                       choices = CensusVars_steve[13:73]
                           ),#end selectinupt       
                           br(),
                           selectInput(inputId = "tractID",
                                       label = "Census Tract to Display",
                                       choices = c("All", Census@data$TRACTCE),
                                       selected = "All",
                                       multiple = TRUE
                           )#end selectinput
                           
                         ),#end sidebarpanel
                         
                         mainPanel(leafletOutput("tracts",height = "80vh"))
                       )#end sidebarlayout -- Steve
              ), #End - Steve
              tabPanel("Code Enforcement",  # Start - Patrick
                       sidebarLayout(
                         sidebarPanel(
                           checkboxGroupInput(inputId = "status_patrick", label = "Case Status",
                                              choices = list("Active" = "Active", 
                                                             "Closed" = "Closed"),
                                              selected = c("Active", "Closed")),
                           checkboxGroupInput(inputId = "code_patrick", label = "Code Type",
                                              choices = list("ENVIRONMENTAL CLEANUP" = "ENVIRONMENTAL CLEANUP", 
                                                             "ENVIRONMENTAL MOWING" = "ENVIRONMENTAL MOWING",
                                                             "HOUSING REPAIR" = "HOUSING REPAIR",
                                                             "VEHICLE-PRIVATE" = "VEHICLE-PRIVATE", 
                                                             "VEHICLE-PUBLIC"= "VEHICLE-PUBLIC",
                                                             "ZONING VIOLATIONS" = "ZONING VIOLATIONS"),
                                              selected = c("ENVIRONMENTAL CLEANUP", "ENVIRONMENTAL MOWING", "HOUSING REPAIR",
                                                           "VEHICLE-PRIVATE", "VEHICLE-PUBLIC", "ZONING VIOLATIONS")),
                           dateRangeInput("date_patrick", label = "Select Date Range", start = "2013-01-02", end = "2014-06-17", min = "2013-01-02", max = "2014-06-17",
                                          format = "mm-dd-yyyy")),
                         mainPanel(
                           leafletOutput("map_patrick")
                         )
                       )), # End - Patric
                         
              tabPanel("Unresolved Abandoned Properties",  # Start - Michael
                       sidebarLayout(
                         sidebarPanel(
                           helpText('Abandoned properties with unresolved code enforcement violations are displayed. Select "City Council Districts" to highlight 
                                    an area on the map assigned to specific council Member. Select "Code Enforcement Status" to filter down properties to only those 
                                    of a specific type.'),br(),
                           radioButtons(inputId = "Districts_Michael",
                                        label = "City Council Districts:",
                                        choices = districts_Michael@data[,3],
                                        selected = 1301),
                        
                           br(),
                           radioButtons(inputId = "CodeEnforcement_Michael",
                                        label = "Code Enforcement Status:",
                                        choices = sort(unique(properties_Michael@data[,5]),decreasing = F),
                                        selected = "Demo Affirmed")
                           ),
                         
                         # Show a plot of the generated distribution
                         mainPanel(
                          leafletOutput("mymap_Michael"),
                          br(),br(),
                          h3("City Council Districts:"),
                          br(),
                          dataTableOutput("mydists_Michael"),
                          br(),br(),
                          h3("Code Enforcement Status:"),
                          br(),
                           dataTableOutput("myprops_Michael")
                           ))), # End - Michael
              
              navbarMenu("Reports",
                    tabPanel("Contact Management",  # Start - Vijay
                       sidebarLayout(
                         sidebarPanel(
                           
                           dateRangeInput(inputId = "dates", label = "Date range", startview = "year",start="2013-01-01"),
                           sliderInput("duration", "Duration:",
                                       min = 1, max = 1000,
                                       value = c(210,550))
                           
                         ),
                         mainPanel(
                           br(),
                           h3("Work Group vs Call duration"),
                            plotOutput("WGD"),
                           br(),br(),
                           h3("Call type code vs Call duration"),
                            plotOutput("CTC"),
                           br(),br(),br(),
                           h3("Call type code wordcloud"),
                            wordcloud2Output("wc_first")
                         )
                       )
              ),
              tabPanel("Phone call Report",
                       sidebarLayout(
                         sidebarPanel(
                           
                           dateRangeInput(inputId = "pc_dates", label = "Date range", startview = "year",start="2016-09-01",end = "2017-05-01"),
                           sliderInput("duration_pc", "Duration",
                                       min = 0, max = 2800,
                                       value = c(5,2800)),
                           checkboxGroupInput("departments", label = ("Departments:"), 
                                              choices = c("311 Office" , "Administration and Finance" ,"Animal Control" ,"Building","City Clerk","City of South Bend" ,"Code Enforcement - Department" ,"Community Investment","Engineering","Fire Department","Human Resources","Legal" ,"Mayor's Office","Organic Resources","Parks","Parks - Maintenance","Police Department" ,"Public Works","Safety and Risk","Solid Waste","Streets" ,"Waste Water","Water Works" ),
                                              selected = c("311 Office" , "Administration and Finance" ,"Animal Control" ,"Building","City Clerk","City of South Bend" ,"Code Enforcement - Department" ,"Community Investment","Engineering","Fire Department","Human Resources","Legal" ,"Mayor's Office","Organic Resources","Parks","Parks - Maintenance","Police Department" ,"Public Works","Safety and Risk","Solid Waste","Streets" ,"Waste Water","Water Works"))
                         ),
                         mainPanel(
                           br(),
                           h3("Department vs Average call duration"),
                            plotOutput("pc_first"),
                           br(),br(),
                           h3("Called About summary"),br(),
                           DT::dataTableOutput("pc_second")
                         )
                       )
              ),
              tabPanel("City Managed Properties",
                       br(),
                       h3("City Managed Properties"),br(),
                       DT::dataTableOutput("summary_first")
              )
              ),            ## End - Vijay
          
              tabPanel("About Us",
                       h1("Data Visualization Final Project - Fall 2018", align = "center"),
                       br(),
                       h2("East Group 4", align = "center"),
                       br(),
                       h3("Vijayasarathi Balasubramanian", align = "center"),
                       br(),
                       h3("Anikey Desai", align = "center"),
                       br(),
                       h3("Steven Lauretti", align = "center"),
                       br(),
                       h3("Patrick McCullough", align = "center"),
                       br(),
                       h3("Michael Sayer", align = "center")
                       
              )          
   )
)


# Define server logic required for app
server <- function(input, output) {
   # Aniket - Page 1
  output$Parks <- renderLeaflet({
    Park.temp <- Parks.spatial_Aniket[(Parks.spatial_Aniket@data$Park_Type %in% input$type),]
              PF.temp <- PF.spatial_Aniket[(PF.spatial_Aniket@data$POPL_TYPE %in% input$PF_type),]
      
      leaflet()  %>%
        addTiles() %>% 
        addCircleMarkers(data = Park.temp,
                         popup = ~Park.temp$Park_popup, 
                         color = ~pal_aniket(Park_Type), stroke = 0, fillOpacity = 1, radius = 4) %>%
        addCircleMarkers(data =PF.temp,
                         popup = ~PF.temp$PF_popup, 
                         color = ~pal1_aniket(POPL_TYPE), stroke = 0, fillOpacity = 1, radius = 4)
})
  
  # Steve - Tab 2
  tractdata <- eventReactive(input$tractID,{ if ("All" %in% input$tractID)
                                              { return(Census)} else { 
    return(Census[Census@data$TRACTCE %in% input$tractID,])
  } #end ifelse 
  }) #end eventreactive
  
  
  Census_pal <- eventReactive(input$DataValue,
                              { colorNumeric(palette = "YlGnBu",
                                             domain = as.numeric(Census@data[[input$DataValue]]))
                              }) #end reactive
  
    output$tracts <-  renderLeaflet({
    leaflet()%>%
      addProviderTiles(providers$OpenStreetMap) %>% 
      addPolygons(data=tractdata(),
                  fillColor = ~Census_pal()(as.numeric(Census@data[[input$DataValue]])),
                  fillOpacity = 0.5,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 2,
                                                      bringToFront = TRUE),
                  
                  label = paste(tractdata()@data$NAMELSAD,
                                CensusLabels_steve[input$DataValue],
                                tractdata()@data[[input$DataValue]],
                                sep=" - ")
                  
      ) %>%
      
      addLegend("bottomright", pal = Census_pal(),
                values = as.numeric(Census@data[[input$DataValue]]),
                title = paste(CensusLabels_steve[input$DataValue]),
                opacity = 1) %>% 
      addPolygons(data=school, color = "Black", group = "Schools") %>% 
      addLayersControl(overlayGroups = c("Schools"),
                       options = layersControlOptions(collapsed = FALSE)) %>% 
      hideGroup("Schools")                       
    
  }) #end renderleaflet
  
  
  #Pat- Tab 3
  output$map_patrick <- renderLeaflet({
    leaflet(data = filter(enfor, Case_Status_Code_Description %in% input$status_patrick
                          & Case_Type_Code_Description %in% input$code_patrick &
                            Date > input$date_patrick[1] &
                            Date < input$date_patrick[2])) %>%
     addProviderTiles("CartoDB.Positron")  %>%
      #addTiles() %>%
      addPolygons( data = cc, color = "black", opacity = .1,  fillColor = ~qpal(Num), fillOpacity = .6, group = "Show City Council Districts" ) %>%
      addLegend(position = "bottomleft", title = "City Council Member",
                labels = c("Tim Scott","Regina Williams","Sharon McBride","Jo M. Broden", "Dr. David Varner","Oliver Davis"),
                colors =  c("#B3E2CD", "#FDCDAC", "#CBD5E8", "#F4CAE4", "#E6F5C9", "#FFF2AE"), group = "Show City Council Districts")%>% 
      addLayersControl(overlayGroups = "Show City Council Districts", 
                       options = layersControlOptions(collapsed = F)) %>%
      addCircleMarkers(radius = .5, fillOpacity = 1,  popup = ~pop)
  })
  
  # Mike - Tab 4
  # District control - reaction
  dist.subset_Michael <- reactive({
    districts_Michael[districts_Michael@data[,3] == input$Districts_Michael,]
  })
  # Code enfrocements
  prop.subset_Michael  <- reactive({
    properties_Michael[properties_Michael@data[,5] == input$CodeEnforcement_Michael,]
  })
  # District map
  output$mymap_Michael  <- renderLeaflet(
    leaflet()%>%
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
      addPolygons(data = dist.subset_Michael(), color = ~pal_Michael(Dist), weight=.8, opacity = 1, fillOpacity = .2, dashArray = 1)%>%
      addPolygons(data = prop.subset_Michael(), popup = ~popup, group = "Code_Enfor", color = "black", fillOpacity = .5)
  )
  #District subset
  output$mydists_Michael  <- renderDataTable({ dist.subset_Michael()@data[,c(3,7,8)] })
  # Properties
  output$myprops_Michael  <- renderDataTable({ prop.subset_Michael()@data[,c(5,16,3,10,17)] })

  
  # Vijay - Tab 5
  
  # work Group definition
     output$WGD <- renderPlot({
       
       Contact_Management_final_new <- Contact_Management_final %>%
         filter(Entry_Date > input$dates[1])  %>%
         filter(Close_Date < input$dates[2]) %>%
        filter(duration > input$duration[1]) %>%
         filter(duration < input$duration[2]) 
       # Plot
       ggplot(Contact_Management_final_new,aes(x=Work_Group_Define,y=duration))+geom_boxplot()+
         #ggtitle("Word Group vs call duration")+
         ylab("Call duration")+xlab("Work group")+
         theme(axis.text.x = element_text(angle=90, hjust=1,size=12),
               plot.title = element_text(size = 15, face = "bold"),
               axis.title.y = element_text(size = rel(1.2)),
               axis.title.x = element_text(size = rel(1.2)))

     })
     
     # Call type code plot
     output$CTC <- renderPlot({
       Contact_Management_final_new <- Contact_Management_final %>%
         filter(Entry_Date > input$dates[1])  %>%
         filter(Close_Date < input$dates[2]) %>%
         filter(duration > input$duration[1]) %>%
         filter(duration < input$duration[2]) 
 # Plot
       ggplot(Contact_Management_final_new,aes(x=Call_Type_Code,y=duration))+geom_boxplot()+
         #ggtitle("Call type code vs call duration")+
         ylab("Call duration")+xlab("Call type code")+
         theme(axis.text.x = element_text(angle=90, hjust=1,size=12),
               plot.title = element_text(size = 15, face = "bold"),
               axis.title.y = element_text(size = rel(1.2)),
               axis.title.x = element_text(size = rel(1.2)))

     })
     
     # Word cloud - call type code
     output$wc_first <- renderWordcloud2({
      ctc_count <-  Contact_Management_final %>%
         filter(Entry_Date > input$dates[1])  %>%
         filter(Close_Date < input$dates[2]) %>%
         filter(duration > input$duration[1]) %>%
         filter(duration < input$duration[2]) %>%
          count(Call_Type_Code, sort = TRUE) %>%
         filter(n > 5)# Filter the rare appearing words 
      
        wc_plot1 <- wordcloud2(ctc_count,shape = "cardioid")
       return(wc_plot1)
     })
     
     # Phone call plot - control by department.
     output$pc_first <- renderPlot({
       Phone_Call_plot1 <-  Phone_Call_Log_final %>%
       filter( as.Date(Call_Date) > input$pc_dates[1]) %>%
      filter(as.Date(Call_Date) <  input$pc_dates[2]) %>%
         filter(duration_Seconds > input$duration_pc[1]) %>%
         filter(duration_Seconds < input$duration_pc[2]) %>%
       filter(Department %in% input$departments) %>%
       select(Department,duration_Seconds) %>%
       group_by(Department) %>%
       summarise(avg_call= mean(duration_Seconds,na.rm=T))
       
       ggplot(Phone_Call_plot1, aes(x=reorder(Department,avg_call),y=avg_call)) + geom_point()+
         #ggtitle("Department vs Average call duration")+
         ylab("Average call duration")+xlab("Department")+
         theme(axis.text.x = element_text(angle=60, hjust=1,size=12),
               plot.title = element_text(size = 15, face = "bold"),
               axis.title.y = element_text(size = rel(1.2)),
               axis.title.x = element_text(size = rel(1.2)))
       
     })
     # Phone call report - summary
     output$pc_second <- DT::renderDataTable({
       pc_count <-   Phone_Call_Log_final %>%
         filter( as.Date(Call_Date) > input$pc_dates[1]) %>%
         filter(as.Date(Call_Date) <  input$pc_dates[2]) %>%
         filter(duration_Seconds > input$duration_pc[1]) %>%
         filter(duration_Seconds < input$duration_pc[2]) %>%
         filter(Department %in% input$departments) %>%
         count(Called_About, sort = TRUE) 
       return(DT::datatable(pc_count,colnames=c("Called about","Count"),caption="Count per call type"))
     })
     
     # Summary report - SB properties
     
   output$summary_first <- DT::renderDataTable({
     DT::datatable(SB_facilities[order(-SB_facilities$Count),])
   })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

