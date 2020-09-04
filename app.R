library(shiny)
library(shinydashboard)
library(DBI)
library(odbc)
library(shinyjs)
library(shinyTime)
library(openxlsx)
library(maps)
library(ggmap)
library(ggplot2)
library(mapproj)
library(leaflet)
library(dplyr)
library(leaflet.minicharts)
library(DT)


## register key for googple api
register_google(key = '****')
getOption("ggmap")

conn <- DBI::dbConnect(odbc::odbc(),
                       Driver = "SQLServer",
                       Server = "****",
                       Database = "****",
                       UID = "****",
                       PWD = "****",
                       Port = ****)

CountsQuery = "SELECT [name] as Name,
                      [year] as Year,
                      [month] as Month,
                      [day] as Day,
                      [location] as Location,
                      [weather] as Weather,
                      [temperature] as Temperature,
                      [time] as Time,
                      [nbl] as NorthboundLeft,
                      [nbt] as NorthboundThrough,
                      [nbr] as NorthboundRight,
                      [sbl] as SouthboundLeft,
                      [sbt] as SouthboundThrough,
                      [sbr] as SouthbounfRight,
                      [ebl] as EastboundLeft,
                      [ebt] as EastboundThrough,
                      [ebr] as EastboundRight,
                      [wbl] as WestboundLeft,
                      [wbt] as WestboundThrough,
                      [wbr] as WestboundRight,
                      [hm] as HelmetMale,
                      [nhm] as NoHelmetMale,
                      [hf] as HelmetFemale,[nhf] as NoHelmetFemale,
                      [note] as Note
              FROM [Team06_W20].[dbo].[Counts]"
Counts = dbGetQuery(conn, CountsQuery)

LocationQuery = "SELECT * from Location"
Location = dbGetQuery(conn, LocationQuery)

SurveyQuery = "SELECT * from SUrvey"
Survey = dbGetQuery(conn, SurveyQuery)

x <- left_join(Location, Counts, by = "Location")

avg <- Counts %>%
  group_by(Location) %>%
  summarise(NoHelmetMale = mean(NoHelmetMale),
            HelmetMale = mean(HelmetMale),
            NoHelmetFemale = mean(NoHelmetFemale),
            HelmetFemale = mean(HelmetFemale)
  )
avg <- left_join(avg, Location, by ="Location")

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}


saveDataToDatabase <- function(data){
  table_columns <- dbListFields(conn, "Counts")
  colnames(data) <- as.character(table_columns)
  dbWriteTable(conn, "Counts", data, append = TRUE, overwrite = FALSE, row.names = FALSE)
}

saveDataToSurvey <- function(data){
  table_columns <- dbListFields(conn, "Survey")
  colnames(data) <- as.character(table_columns)
  dbWriteTable(conn, "Survey", data, append = TRUE, overwrite = FALSE, row.names = FALSE)
}


######################################UI############################################

ui <- dashboardPage(

  dashboardHeader(title="Final Project"),
  dashboardSidebar(

    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Map", tabName = "Map", icon = icon("map")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-line")),
      menuItem("Volunteer Input", tabName = "input", icon = icon("pen")),
      menuItem("Survey", tabName = "survey", icon = icon("poll")),
      menuItem("View/Download", tabName = "download", icon = icon("download"))
    )

  ),
  dashboardBody(
    useShinyjs(),
    tabItems(

      tabItem(tabName = "home",
              titlePanel("Team 6"),
              fluidRow(
                box(width = 12, Height = 4,
                    "Working with the city of Portland, we are organizing annual bicycle counts. Volunteers
                    can feel free to enter their counts in the input tab to improve the data to better serve
                    the City of Portland. Please make notes on the date, time, location, trip purpose, trip origin,
                    trip destination, and frequency of bicycle travels. Enjoy the app and happy cycling!"

                )
              ),
              fluidRow(
                valueBoxOutput(width = 6, "NoFemHel"),
                valueBoxOutput(width = 6, "NoMalHel")
              ),

              fluidRow(
                valueBoxOutput(width = 6, "FemHel"),
                valueBoxOutput(width = 6, "MalHel"),
              ),

              fluidRow(
                box(width = 12,
                    title = "Intersections with high traffic",
                    tableOutput("HighTable")
                )

              )

      ),

      tabItem(tabName = "Map",
              fluidRow(box(width = 12, title = "Map with all Intersections", leafletOutput("mapall"))),
              fluidRow(valueBoxOutput("test", width = 6),
                       box(selectInput("intersections", label = "View specific Intersection",
                                       choices = x$Location, selected = x$Location[1], multiple = F))),
              fluidRow(
                box(title = "Pie Chart", plotOutput ("plotintersection")),
                box(title = "Map with selected Intersections", leafletOutput("map"))
              )
      ),


      tabItem(tabName = "dashboard",
              fluidRow(
                column(4, div(
                  titlePanel("Bicycle Count Dashboard"),
                  p("A dashboard showing the count of bicyclists for each direction, also including the information about whether wearing helmet or not.")
                )),
                column(4, div(
                  selectInput("location", "Select the location", x$Location, selected = 1),
                  uiOutput("date_selection")
                )),
                column(4, div(
                  uiOutput("time_selection"),
                  selectInput("type", "Select the type of plot", c("Bar chart", "Pie chart"), selected = 1)

                ))
              ),
              fluidRow(
                column(4, plotOutput("barplot1")),
                column(4, plotOutput("barplot2")),
                column(4, plotOutput("barplot3"))
              ),
              fluidRow(
                column(4, plotOutput("barplot4")),
                column(4, plotOutput("barplot5")),
                column(4, plotOutput("barplot6"))
              )
      ),


      tabItem(tabName = "input",

              shinyjs::hidden(
                fluidRow(id = "thankyou_msg",
                         wellPanel(
                           div(h3("Thanks, your response was submitted successfully!"),
                               actionLink("submit_another", "Submit another response"))))
              ),

              fluidRow(
                wellPanel(id="form",
                          titlePanel("Basic Information"),
                          div(textInput("name",labelMandatory("Name"),""),
                              splitLayout(
                                numericInput("year", labelMandatory("Year"), format(Sys.Date(), "%Y"), min = 2014, max = format(Sys.Date(), "%Y")),
                                numericInput("month", labelMandatory("Month"), format(Sys.Date(), "%m"), min = 1, max = 12),
                                numericInput("day", labelMandatory("Day"), format(Sys.Date(), "%d"), min = 1, max = 31)
                              ),
                              selectInput("time", labelMandatory("Time Period"), c("", "7-9am", "4-6pm")),
                              textInput("location3",labelMandatory("Location (Ex. SW Oak St & SW Park Ave)"),""),
                              selectInput("weather", labelMandatory("Weather"),
                                          c("", "cloudy, windy", "light rain", "overcast", "overcast, no rain", "raining", "sunny, dry")),
                              sliderInput("temperature", labelMandatory(paste("Temperature (Fahrenheit)")),0,100,50),
                              splitLayout(
                                wellPanel(id="north",
                                          titlePanel("Northbound Input"),
                                          div(numericInput("nbl",labelMandatory("Northbound left turn:"),0, min = 0),
                                              numericInput("nbt",labelMandatory("Northbound through:"),0, min = 0),
                                              numericInput("nbr",labelMandatory("Northbound right turn:"),0, min = 0))
                                ),
                                wellPanel(id="south",
                                          titlePanel("Southbound Input"),
                                          div(numericInput("sbl",labelMandatory("Southbound left turn:"),0, min = 0),
                                              numericInput("sbt",labelMandatory("Southbound through:"),0, min = 0),
                                              numericInput("sbr",labelMandatory("Southbound right turn:"),0, min = 0))
                                )
                              ),
                              splitLayout(
                                wellPanel(id="east",
                                          titlePanel("Eastbound Input"),
                                          div(numericInput("ebl",labelMandatory("Eastbound left turn:"),0, min = 0),
                                              numericInput("ebt",labelMandatory("Eastbound through:"),0, min = 0),
                                              numericInput("ebr",labelMandatory("Eastbound right turn:"),0, min = 0))
                                ),
                                wellPanel(id="west",
                                          titlePanel("Westbound Input"),
                                          div(numericInput("wbl",labelMandatory("Westbound left turn:"),0, min = 0),
                                              numericInput("wbt",labelMandatory("Westbound through:"),0, min = 0),
                                              numericInput("wbr",labelMandatory("Westbound right turn:"),0, min = 0))
                                )
                              ),
                              splitLayout(
                                wellPanel(id="male",
                                          div(numericInput("hm",labelMandatory("Males with helmet:"),0, min = 0),
                                              numericInput("nhm",labelMandatory("Males with no helmet:"),0, min = 0))
                                ),
                                wellPanel(id="female",
                                          div(numericInput("hf",labelMandatory("Females with helmet:"),0, min = 0),
                                              numericInput("nhf",labelMandatory("Females with no helmet:"),0, min = 0))
                                )
                              ),
                              div(textInput("note","Description:")),
                              div(align = "right",
                                  actionButton("submit", "Submit", class = "btn-primary")
                              )
                          )
                )
              )
      ),

      tabItem(tabName = "survey",

              shinyjs::hidden(
                fluidRow(id = "thankyou_msg2",
                         wellPanel(
                           div(h3("Thanks, your response was submitted successfully!"),
                               actionLink("submit_another2", "Submit another response"))))
              ),

              fluidRow(
                wellPanel(id="surveydata",
                          titlePanel("Cycling Survey"),
                          div(textInput("name2",labelMandatory("Name"),""),
                              splitLayout(cellWidths = c("70%","15%","15%"),
                                          dateInput("date2", labelMandatory("Date"), value = Sys.Date()),
                                          numericInput("hour2",labelMandatory("Hour:"),0, min = 0, max = 23),
                                          numericInput("minute2",labelMandatory("Minute"),0, min = 0, max = 59)
                              ),
                              textInput("purpose2",labelMandatory("Trip Purpose"),""),
                              textInput("location2",labelMandatory("Location"),""),
                              splitLayout(
                                textInput("origin2",labelMandatory("Trip Origin"),""),
                                textInput("destination2",labelMandatory("Trip Destination"),"")
                              ),
                              textInput("frequency2",labelMandatory("Frequency of bicycle travel"),""),
                              div(align = "right",
                                  actionButton("submit2", "Submit", class = "btn-primary")
                              )
                          )
                )
              )
      ),


      tabItem(tabName = "download",
              titlePanel("View table"),
              tabsetPanel(
                tabPanel("Bicycle Counts", box(
                  width = NULL, status = "primary",
                  fluidRow(column(3,selectInput("Intersection", "Intersection:", c("All", unique(as.character(Counts$Location))))),
                           column(3,selectInput("Weather","Weather:", c("All", unique(as.character(Counts$Weather)))))),
                  div(style = 'overflow-x: scroll', DT::dataTableOutput('table1')))),
                tabPanel("Location", box(
                  width = NULL, status = "primary",
                  fluidRow(column(3,selectInput("TrafficClass", "Traffic Class:", c("All", unique(as.character(Location$TrafficClass)))))),
                  div(style = 'overflow-x: scroll', DT::dataTableOutput('table2')))),
                tabPanel("Survey", box(
                  width = NULL, status = "primary",
                  div(style = 'overflow-x: scroll', DT::dataTableOutput('table3')))),
                tabPanel("Download Data", box(
                  width = NULL, status = "primary",
                  selectInput("dataset", "Choose a dataset:",
                              choices = c("Bicycle Counts", "Location","Survey")),
                  downloadButton("downloadData", "Download")))
              )



      )
    )
  )
)


######################################Server############################################

server <- function(input, output){

  rvx <- reactiveValues(tableData = x)
  rvavg <- reactiveValues(tableData = avg)

  output$HighTable <- renderTable(filter(Location, TrafficClass == "high")
                                  %>% select(Location))
  var <- c(Counts %>% mutate(NoHelmetRatioF = (NoHelmetFemale/(NoHelmetFemale+HelmetFemale))) %>% top_n(n=1) %>% pull(Location))
  out1 <- ""
  for(i in 1:length(var)) {
    if(i == 1){
      out1 <- paste0(var[i])
    }
    else if(i < length(var)) {
      out1 <- paste0(out1, ", ", var[i])
    }
    else if(i == length(var)) {
      out1 <- paste0(out1, " and ", var[i])
    }
  }
  output$NoFemHel <- renderValueBox({
    valueBox(value = tags$p(out1, style = "font-size: 50%;"),
             "has the highest percentage of Females without a Helmet", icon = icon("dashboard"), color = "green")
  })

  yay <- c(x %>% mutate(NoHelmetRatioM = (NoHelmetMale/(NoHelmetMale+HelmetMale))) %>% top_n(n=1) %>% pull(Location))
  out2 <- ""
  for(i in 1:length(yay)) {
    if(i == 1){
      out2 <- paste0(yay[i])
    }
    else if(i < length(yay)) {
      out2 <- paste0(out2, ", ", yay[i])
    }
    else if(i == length(yay)) {
      out2 <- paste0(out2, " and ", yay[i])
    }
  }
  output$NoMalHel <- renderValueBox({
    valueBox(value = tags$p(out2, style = "font-size: 50%;"),
             "has the highest percentage of Males without a Helmet", icon = icon("dashboard"), color = "blue")
  })

  yhf <- c(Counts %>% mutate(HelmetRatioF = (HelmetFemale/(NoHelmetFemale+HelmetFemale))) %>% top_n(n=1) %>% pull(Location))
  out3 <- ""
  for(i in 1:length(yhf)) {
    if(i == 1){
      out3 <- paste0(yhf[i])
    }
    else if(i < length(yhf)) {
      out3 <- paste0(out3, ", ", yhf[i])
    }
    else if(i == length(yhf)) {
      out3 <- paste0(out3, " and ", yhf[i])
    }
  }
  output$FemHel <- renderValueBox({
    valueBox(value = tags$p(out3, style = "font-size: 50%;"),
             "has the highest percentage of Females with a Helmet", icon = icon("dashboard"), color = "red")
  })

  yhm <- c(Counts %>% mutate(HelmetRatioM = (HelmetMale/(NoHelmetMale+HelmetMale))) %>% top_n(n=1) %>% pull(Location))
  out4 <- ""
  for(i in 1:length(yhm)) {
    if(i == 1){
      out4 <- paste0(yhm[i])
    }
    else if(i < length(yhm)) {
      out4 <- paste0(out4, ", ", yhm[i])
    }
    else if(i == length(yhm)) {
      out4 <- paste0(out4, " and ", yhm[i])
    }
  }
  output$MalHel <- renderValueBox({
    valueBox(value = tags$p(out4, style = "font-size: 50%;"),
             "has the highest percentage of Males with a Helmet", icon = icon("dashboard"), color = "fuchsia")
  })


  ################################################################


  #update to database

  fieldsAll <- c("name", "year", "month", "day", "location3", "weather", "temperature", "time",
                 "nbl", "nbt", "nbr", "sbl", "sbt", "sbr",
                 "ebl", "ebt", "ebr", "wbl", "wbt", "wbr", "hm", "nhm", "hf", "nhf", "note")

  formData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
    data <- t(data)
    data <- as.data.frame(data)
  })


  insertQuery = "insert into Location (Location)
  values ((select Location from Counts
  where Location not in (select distinct Location from Location)))"
  locationidQuery = "update Location set LocationID = (select max(LocationID)+1 from Location)
  where LocationID is NULL"
  newcoordQuery = "select LocationID, Location from Location where lon is NULL"
  deleteQuery = "delete from Location where Location is NULL or Location = ''"

  VarName <- colnames(x)

  #Submit and submit another form
  observeEvent(input$submit, {
    saveDataToDatabase(formData())
    shinyjs::reset("form")
    shinyjs::hide("form")
    shinyjs::show("thankyou_msg")

    output$table1 <- renderDataTable(DT::datatable({
      data1 <- newCounts()
      if(input$Intersection!="All"){
        data1 <- filter(data1, Location == input$Intersection)
      }
      if(input$Weather!="All"){
        data1 <- filter(data1, Weather == input$Weather)
      }
      data1
    })
    )




    rvx$tableData <- left_join(newLocation(), newCounts(), by = "Location")

    rvavg$tableData <- newCounts() %>%
      group_by(Location) %>%
      summarise(NoHelmetMale = mean(NoHelmetMale),
                HelmetMale = mean(HelmetMale),
                NoHelmetFemale = mean(NoHelmetFemale),
                HelmetFemale = mean(HelmetFemale)
      )
    rvavg$tableData <- left_join(rvavg$tableData, newLocation(), by ="Location")


    #update Location
    dbSendStatement(conn, insertQuery)
    print(insertQuery)
    dbSendStatement(conn, locationidQuery)
    newcoord = dbGetQuery(conn, newcoordQuery)
    print(newcoord)
    newcoord = (data.frame(newcoord["LocationID"],
                           lapply(lapply(lapply(newcoord["Location"], function(x){gsub(" &", "@", x)}),
                                         function(x){gsub(" ", "+", x)}), function(x) {paste0(x,",+Portland,+OR")})))
    newcoord <- cbind(newcoord, geocode(as.character(newcoord[,2])))
    lonlatQuery = paste("update Location set Intersection = '", newcoord[1,2], "', lon =", newcoord[1,3], ", lat =", newcoord[1,4], "where LocationID =", newcoord[1,1])
    print(lonlatQuery)
    dbSendStatement(conn, lonlatQuery)
    dbSendQuery(conn, deleteQuery)
    newLocation = dbGetQuery(conn, LocationQuery)
    output$table2 <- renderDataTable(DT::datatable({
      data2 <- dbGetQuery(conn, LocationQuery)
      if(input$TrafficClass!="All"){
        data2 <- filter(data2, TrafficClass == input$TrafficClass)
      }
      data2
    })
    )

    output$HighTable <- renderTable(filter(newLocation(), TrafficClass == "high")
                                    %>% select(Location))
    var <- c(newCounts() %>% mutate(NoHelmetRatioF = (NoHelmetFemale/(NoHelmetFemale+HelmetFemale))) %>% top_n(n=1) %>% pull(Location))
    out1 <- ""
    for(i in 1:length(var)) {
      if(i == 1){
        out1 <- paste0(var[i])
      }
      else if(i < length(var)) {
        out1 <- paste0(out1, ", ", var[i])
      }
      else if(i == length(var)) {
        out1 <- paste0(out1, " and ", var[i])
      }
    }
    output$NoFemHel <- renderValueBox({
      valueBox(value = tags$p(out1, style = "font-size: 50%;"),
               "has the highest percentage of Females without a Helmet", icon = icon("dashboard"), color = "green")
    })

    yay <- c(newCounts() %>% mutate(NoHelmetRatioM = (NoHelmetMale/(NoHelmetMale+HelmetMale))) %>% top_n(n=1) %>% pull(Location))
    out2 <- ""
    for(i in 1:length(yay)) {
      if(i == 1){
        out2 <- paste0(yay[i])
      }
      else if(i < length(yay)) {
        out2 <- paste0(out2, ", ", yay[i])
      }
      else if(i == length(yay)) {
        out2 <- paste0(out2, " and ", yay[i])
      }
    }
    output$NoMalHel <- renderValueBox({
      valueBox(value = tags$p(out2, style = "font-size: 50%;"),
               "has the highest percentage of Males without a Helmet", icon = icon("dashboard"), color = "blue")
    })

    yhf <- c(newCounts() %>% mutate(HelmetRatioF = (HelmetFemale/(NoHelmetFemale+HelmetFemale))) %>% top_n(n=1) %>% pull(Location))
    out3 <- ""
    for(i in 1:length(yhf)) {
      if(i == 1){
        out3 <- paste0(yhf[i])
      }
      else if(i < length(yhf)) {
        out3 <- paste0(out3, ", ", yhf[i])
      }
      else if(i == length(yhf)) {
        out3 <- paste0(out3, " and ", yhf[i])
      }
    }
    output$FemHel <- renderValueBox({
      valueBox(value = tags$p(out3, style = "font-size: 50%;"),
               "has the highest percentage of Females with a Helmet", icon = icon("dashboard"), color = "red")
    })

    yhm <- c(newCounts() %>% mutate(HelmetRatioM = (HelmetMale/(NoHelmetMale+HelmetMale))) %>% top_n(n=1) %>% pull(Location))
    out4 <- ""
    for(i in 1:length(yhm)) {
      if(i == 1){
        out4 <- paste0(yhm[i])
      }
      else if(i < length(yhm)) {
        out4 <- paste0(out4, ", ", yhm[i])
      }
      else if(i == length(yhm)) {
        out4 <- paste0(out4, " and ", yhm[i])
      }
    }
    output$MalHel <- renderValueBox({
      valueBox(value = tags$p(out4, style = "font-size: 50%;"),
               "has the highest percentage of Males with a Helmet", icon = icon("dashboard"), color = "fuchsia")
    })





  }
  )


  observeEvent(input$submit_another, {
    shinyjs::show("form")
    shinyjs::hide("thankyou_msg")
  }
  )

  surveyAll <- c("name2", "date2", "hour2", "minute2", "purpose2", "location2", "origin2", "destination2", "frequency2")

  timestamp <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%OS")

  formSurvey <- reactive({
    data <- sapply(surveyAll, function(x) input[[x]])
    data <- c(data, timestamp = timestamp())
    data <- t(data)
    data <- as.data.frame(data)
  })


  observeEvent(input$submit2, {
    saveDataToSurvey(formSurvey())
    shinyjs::reset("surveydata")
    shinyjs::hide("surveydata")
    shinyjs::show("thankyou_msg2")
    updateDateQuery <- paste("update survey set date = '",input$date2,"' where timestamp = (select max(timestamp) from survey)")
    dbSendStatement(conn, updateDateQuery)
    output$table3 <- renderDataTable(DT::datatable({
      data3 <- dbGetQuery(conn, SurveyQuery)
      data3
    })
    )

  })



  observeEvent(input$submit_another2, {
    shinyjs::show("surveydata")
    shinyjs::hide("thankyou_msg2")
  }
  )

  #update new table
  newCounts <- eventReactive(input$submit, {
    new = dbGetQuery(conn, CountsQuery)
    new
  }
  )
  newLocation <- eventReactive(input$submit, {
    new = dbGetQuery(conn, LocationQuery)
    new
  }
  )
  newSurvey <- eventReactive(input$submit2, {
    new = dbGetQuery(conn, SurveyQuery)
    new
  }
  )
  output$table1 <- renderDataTable(DT::datatable({
    data1 <- Counts
    if(input$Intersection!="All"){
      data1 <- filter(data1, Location == input$Intersection)
    }
    if(input$Weather!="All"){
      data1 <- filter(data1, Weather == input$Weather)
    }
    data1
  })
  )
  output$table2 <- renderDataTable(DT::datatable({
    data2 <- Location
    if(input$TrafficClass!="All"){
      data2 <- filter(data2, TrafficClass == input$TrafficClass)
    }
    data2
  })
  )
  output$table3 <- renderDataTable(DT::datatable({
    Survey
  })
  )

  output$mapall <- renderLeaflet({leaflet()  %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(data = rvavg$tableData, ~as.numeric(lon), ~as.numeric(lat), popup =
                         paste0(
                           "<div>",
                           "<h3>",
                           rvavg$tableData$Location,
                           "</h3>",
                           "Male with helmet: ",
                           rvavg$tableData$HelmetMale,
                           "<br>",
                           "Female with Helmet: ",
                           rvavg$tableData$HelmetFemale,
                           "<br>",
                           "Male without helmet: ",
                           rvavg$tableData$NoHelmetMale,
                           "<br>",
                           "Female without helmet: ",
                           rvavg$tableData$NoHelmetFemale,
                           "<br>"
                         )
                       , radius = 2)})
  output$map <- renderLeaflet({rvavg$tableData %>%
      filter(Location %in% c(input$intersections)) %>% leaflet()  %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(~as.numeric(lon), ~as.numeric(lat),
                       popup = ~Location, radius = 2)})
  observe({
    coords <- input$map_bounds
    if (!is.null(coords)) {
      leafletProxy("mapall") %>%
        fitBounds(coords$west,
                  coords$south,
                  coords$east,
                  coords$north)
    }
  })

  observeEvent(input$mapall_marker_click,{
    clickedMarker <- input$mapall_marker_click
    z <- filter(rvx$tableData, lat == clickedMarker[3]) %>% filter(lon == clickedMarker[4])
    y <- data.frame(group = c("HelmetMale", "HelmetFemale", "NoHelmetMale", "NoHelmetFemale"),
                    value = c(mean(z[,26]), mean(z[,28]), mean(z[,27]), mean(z[,29])))
    #value = c(z[1,23], z[1,24], z[1,25], z[1,26]))

    output$plotintersection=renderPlot ({ ggplot(y, aes(x="", y=value, fill=group)) +
        geom_bar(stat="identity", width=1, color="white") +
        coord_polar("y", start=0) + theme_void()
    })
    out <- paste0(round(((mean(z[,26]) + mean(z[,28]))*100/(mean(z[,26]) + mean(z[,28]) + mean(z[,27]) + mean(z[,29]))), digits = 2), "%")

    output$test <- renderValueBox({
      valueBox(out, paste0("of the people wear helmets at ", z[1,5], " during the observed period."), icon = icon("map"), color = "green")
    })
  })


  ##### Dashboard #####


  location_index <- reactive({
    which(rvx$tableData$Location == input$location)
  })

  dates_index <- reactive({
    which(rvx$tableData$Location == input$location)
  })

  time_index <- reactive({
    which(rvx$tableData$Location == input$location & rvx$tableData$Year == strsplit(toString(input$date), "-")[[1]][1] & rvx$tableData$Month == strsplit(toString(input$date), "-")[[1]][2] & rvx$tableData$Day == strsplit(toString(input$date), "-")[[1]][3])
  })

  getFinalIndex <- reactive({
    which(rvx$tableData$Location == input$location & rvx$tableData$Year == strsplit(toString(input$date), "-")[[1]][1] & rvx$tableData$Month == strsplit(toString(input$date), "-")[[1]][2] & rvx$tableData$Day == strsplit(toString(input$date), "-")[[1]][3] & rvx$tableData$Time == input$timeperiod)
  })

  data <- reactive({
    data.frame(
      name_N=c(VarName[14], VarName[15], VarName[16]),

      value_N=c(rvx$tableData[getFinalIndex(), 14], rvx$tableData[getFinalIndex(), 15], rvx$tableData[getFinalIndex(), 16]),
      name_S=c(VarName[17], VarName[18], VarName[19]),
      value_S=c(rvx$tableData[getFinalIndex(), 17], rvx$tableData[getFinalIndex(), 18], rvx$tableData[getFinalIndex(), 19]),
      name_E=c(VarName[20], VarName[21], VarName[22]),
      value_E=c(rvx$tableData[getFinalIndex(), 20], rvx$tableData[getFinalIndex(), 21], rvx$tableData[getFinalIndex(), 22]),
      name_W=c(VarName[23], VarName[24], VarName[25]),
      value_W=c(rvx$tableData[getFinalIndex(), 23], rvx$tableData[getFinalIndex(), 24], rvx$tableData[getFinalIndex(), 25])
    )
  })

  helmet <- reactive({
    data.frame(
      name_h = c(VarName[26], VarName[28]),
      value_h = c(rvx$tableData[getFinalIndex(), 26], rvx$tableData[getFinalIndex(), 28]),
      name_no_h = c(VarName[27], VarName[29]),
      value_no_h = c(rvx$tableData[getFinalIndex(), 27], rvx$tableData[getFinalIndex(), 29])
    )
  })

  createStore <- function(arr, type) {
    store <- vector()
    for (i in arr) {
      if (type == "date") {
        store <- c(store, paste(toString(rvx$tableData$Year[i]), toString(rvx$tableData$Month[i]), toString(rvx$tableData$Day[i]), sep = "-"))
      } else if (type == "timeperiod") {
        store <- c(store, toString(rvx$tableData$Time[i]))
      }
    }
    return(store)
  }

  createDateStore <- reactive({
    createStore(dates_index(), "date")
  })

  createTimeStore <- reactive({
    createStore(time_index(), "timeperiod")
  })

  output$date_selection <- renderUI({
    selectInput("date", "Select the date", choices =  createDateStore(), selected = 1)
  })

  output$time_selection <- renderUI({
    selectInput("timeperiod", "Select the time period", choices = createTimeStore(), selected = 1)
  })

  observeEvent(input$location, {
    selectInput("date", "Select the date", choices =  createDateStore())
  })

  observeEvent(input$date, {
    selectInput("timeperiod", "Select the time period", choices = createTimeStore())
  })



  observeEvent(input$type, {

    if(input$type == "Bar chart") {

      output$barplot1 <- renderPlot({
        ggplot(data(), aes(x=name_N, y=value_N))  +
          geom_bar(stat = "identity", width=0.5, fill = "#0abab5") +
          xlab("") +
          ylab("Count") +
          ggtitle("Northbound") +
          theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15))
      })
      output$barplot2 <- renderPlot({
        ggplot(data(), aes(x=name_S, y=value_S)) +
          geom_bar(stat = "identity", width=0.5, fill = "#0abab5") +
          xlab("") +
          ylab("Count") +
          ggtitle("Southbound") +
          theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15))
      })
      output$barplot3 <- renderPlot({
        ggplot(helmet(), aes(x=name_h, y=value_h)) +
          geom_bar(stat = "identity", width=0.4, fill = "#06716e") +
          xlab("") +
          ylab("Count") +
          ggtitle("People with helmet") +
          theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15))
      })
      output$barplot4 <- renderPlot({
        ggplot(data(), aes(x=name_E, y=value_E)) +
          geom_bar(stat = "identity", width=0.5, fill = "#0abab5") +
          xlab("") +
          ylab("Count") +
          ggtitle("Eastbound") +
          theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15))
      })
      output$barplot5 <- renderPlot({
        ggplot(data(), aes(x=name_W, y=value_W)) +
          geom_bar(stat = "identity", width=0.5, fill = "#0abab5") +
          xlab("") +
          ylab("Count") +
          ggtitle("Westbound") +
          theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15))
      })
      output$barplot6 <- renderPlot({
        ggplot(helmet(), aes(x=name_no_h, y=value_no_h)) +
          geom_bar(stat = "identity", width=0.4, fill = "#06716e") +
          xlab("") +
          ylab("Count") +
          ggtitle("People without helmet") +
          theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15))
      })

    } else if(input$type == "Pie chart") {

      output$barplot1 <- renderPlot({
        ggplot(data(), aes(x="", y=value_N, fill=name_N))  +
          geom_bar(stat="identity", width=1) +
          coord_polar("y", start=0) +
          geom_text(aes(label = value_N), position = position_stack((vjust = 0.5))) +
          labs(x = NULL, y = NULL, title = "Northbound") +
          theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15))
      })

      output$barplot2 <- renderPlot({
        ggplot(data(), aes(x="", y=value_S, fill=name_S))  +
          geom_bar(stat="identity", width=1) +
          coord_polar("y", start=0) +
          geom_text(aes(label = value_S), position = position_stack((vjust = 0.5))) +
          labs(x = NULL, y = NULL, title = "Southbound") +
          theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15))
      })

      output$barplot3 <- renderPlot({
        ggplot(helmet(), aes(x="", y=value_h, fill=name_h))  +
          geom_bar(stat="identity", width=1) +
          coord_polar("y", start=0) +
          geom_text(aes(label = value_h), position = position_stack((vjust = 0.5))) +
          labs(x = NULL, y = NULL, title = "People with helmet") +
          theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15))
      })

      output$barplot4 <- renderPlot({
        ggplot(data(), aes(x="", y=value_E, fill=name_E))  +
          geom_bar(stat="identity", width=1) +
          coord_polar("y", start=0) +
          geom_text(aes(label = value_E), position = position_stack((vjust = 0.5))) +
          labs(x = NULL, y = NULL, title = "Eastbound") +
          theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15))
      })

      output$barplot5 <- renderPlot({
        ggplot(data(), aes(x="", y=value_W, fill=name_W))  +
          geom_bar(stat="identity", width=1) +
          coord_polar("y", start=0) +
          geom_text(aes(label = value_W), position = position_stack((vjust = 0.5))) +
          labs(x = NULL, y = NULL, title = "Westbound") +
          theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15))
      })

      output$barplot6 <- renderPlot({
        ggplot(helmet(), aes(x="", y=value_no_h, fill=name_no_h))  +
          geom_bar(stat="identity", width=1) +
          coord_polar("y", start=0) +
          geom_text(aes(label = value_no_h), position = position_stack((vjust = 0.5))) +
          labs(x = NULL, y = NULL, title = "People without helmet") +
          theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15))
      })

    }


  })

  datasetInput <- reactive({
    switch(input$dataset,
           "Bicycle Counts" = dbGetQuery(conn, CountsQuery),
           "Location" = dbGetQuery(conn, LocationQuery),
           "Survey" = dbGetQuery(conn, SurveyQuery))
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )





}

######################################Launch app############################################

shinyApp(ui=ui, server=server)
