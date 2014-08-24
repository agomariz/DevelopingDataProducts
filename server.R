library(shiny)
library(ggplot2)
library(data.table)
library(maps)
library(rCharts)
library(reshape2)
library(markdown)
library(mapproj)

states_map <- map_data("state")
dt <- fread('data/events_of_interest.csv')
dt$EVTYPE <- tolower(dt$EVTYPE)
evtypes <- sort(unique(dt$EVTYPE))

shinyServer(function(input, output) {
        
        dt.agg <- reactive({
                tmp <- merge(
                        data.table(STATE=sort(unique(dt$STATE))),
                        dt[
                                YEAR >= input$range[1] & YEAR <= input$range[2] & EVTYPE %in% input$evtypes,
                                list(
                                        COUNT=sum(COUNT),
                                        INJURIES=sum(INJURIES),
                                        FATALITIES=sum(FATALITIES),
                                        PROPERTY.VALUE=round(sum(PROPERTY.VALUE)/1000000, 2),
                                        CROP.VALUE=round(sum(CROP.VALUE)/1000000, 2)
                                ),
                                by=list(STATE)],
                        by=c('STATE'), all=TRUE
                )
                tmp[is.na(tmp)] <- 0
                tmp
        })
        
        dt.agg.year <- reactive({
                dt[
                        YEAR >= input$range[1] & YEAR <= input$range[2] & EVTYPE %in% input$evtypes,
                        list(
                                COUNT=sum(COUNT),
                                INJURIES=sum(INJURIES),
                                PROPERTY.VALUE=round(sum(PROPERTY.VALUE)/1000000, 2),
                                FATALITIES=sum(FATALITIES),
                                CROP.VALUE=round(sum(CROP.VALUE)/1000000, 2)
                        ),
                        by=list(YEAR)
                        ]
        })
        
        dt.agg.year.event <- reactive({
                dt[
                        YEAR >= input$range[1] & YEAR <= input$range[2] & EVTYPE %in% input$evtypes,
                        list(
                                COUNT=sum(COUNT),
                                INJURIES=sum(INJURIES),
                                PROPERTY.VALUE=round(sum(PROPERTY.VALUE)/1000000, 2),
                                FATALITIES=sum(FATALITIES),
                                CROP.VALUE=round(sum(CROP.VALUE)/1000000, 2)
                        ),
                        by=list(YEAR,EVTYPE)
                        ]
        })
        
        
        output$damagedPopulationByState <- renderPlot({
                data <- dt.agg()
                if(input$populationCategory == 'both') {
                        data$Affected <- data$INJURIES + data$FATALITIES
                } else if(input$populationCategory == 'fatalities') {
                        data$Affected <- data$FATALITIES
                } else {
                        data$Affected <-data$INJURIES
                }
                
                title <- paste("Damaged population", input$range[1], "-", input$range[2], "(number of affected)")
                p <- ggplot(data, aes(map_id = STATE))
                p <- p + geom_map(aes(fill = Affected), map = states_map, colour='black') + expand_limits(x = states_map$long, y = states_map$lat) + scale_fill_gradient(high = "red", low = "blue", guide = "colorbar")
                p <- p + coord_map() + theme_bw()
                p <- p + labs(x = "Long", y = "Lat", title = title)
                print(p)
        })
        
        output$economicDamageByState <- renderPlot({
                data <- dt.agg()
                
                if(input$economicCategory == 'both') {
                        data$Damages <- data$PROPERTY.VALUE + data$CROP.VALUE
                } else if(input$economicCategory == 'crops') {
                        data$Damages <- data$CROP.VALUE
                } else {
                        data$Damages <- data$PROPERTY.VALUE
                }
                
                title <- paste("Economic damage", input$range[1], "-", input$range[2], "(x10^6 USD)")
                p <- ggplot(data, aes(map_id = STATE))
                p <- p + geom_map(aes(fill = Damages), map = states_map, colour='black') + expand_limits(x = states_map$long, y = states_map$lat) + scale_fill_gradient(high = "red", low = "blue", guide = "colorbar")
                p <- p + coord_map() + theme_bw()
                p <- p + labs(x = "Long", y = "Lat", title = title)
                print(p)
        })
        
        output$evtypeControls <- renderUI({
                if(1) {
                        checkboxGroupInput('evtypes', 'Event types', evtypes, selected=evtypes)
                }
        })
        
        output$eventsByYearAndEvent <- renderChart({
                data <- melt(
                        dt.agg.year.event()[, list(Year=YEAR, Count=COUNT, Event=EVTYPE)],
                        id=c('Year','Event')
                )
                
                eventsByYearAndEvent <- nPlot(
                        value ~ Year,
                        group = 'Event',
                        data = data[order(-Year, variable, decreasing = T)],
                        type = "lineChart", dom = 'eventsByYearAndEvent', width = 650
                )
                
                eventsByYearAndEvent$chart(margin = list(left = 100))
                eventsByYearAndEvent$yAxis( axisLabel = "Count", width = 80)
                eventsByYearAndEvent$xAxis( axisLabel = "Year", width = 70)
                return(eventsByYearAndEvent)
        })
        
        output$populationDamage <- renderChart({
                data <- melt(
                        dt.agg.year()[, list(Year=YEAR, Injuries=INJURIES, Fatalities=FATALITIES)],
                        id='Year'
                )
                populationDamage <- nPlot(
                        value ~ Year, group = 'variable', data = data[order(-Year, variable, decreasing = T)],
                        type = 'stackedAreaChart', dom = 'populationDamage', width = 650
                )
                
                populationDamage$chart(margin = list(left = 100))
                populationDamage$yAxis( axisLabel = "Affected", width = 80)
                populationDamage$xAxis( axisLabel = "Year", width = 70)
                
                return(populationDamage)
        })
        
        output$economicDamage <- renderChart({
                data <- melt(
                        dt.agg.year()[, list(Year=YEAR, Property=PROPERTY.VALUE, Crops=CROP.VALUE)],
                        id='Year'
                )
                economicDamage <- nPlot(
                        value ~ Year, group = 'variable', data = data[order(-Year, variable, decreasing = T)],
                        type = 'stackedAreaChart', dom = 'economicDamage', width = 650
                )
                economicDamage$chart(margin = list(left = 100))
                economicDamage$yAxis( axisLabel = "Total damage (Million USD)", width = 80)
                economicDamage$xAxis( axisLabel = "Year", width = 70)
                
                return(economicDamage)
        })
        
        #     output$downloadData <- downloadHandler(
        #         filename = 'data.csv',
        #         content = function(file) {
        #             write.csv(dataTable(), file, row.names=FALSE)
        #         }
        #     )
})


