library(shiny)
library(rCharts)
library(base64enc)

shinyUI(
    navbarPage("Most dangerous phenomena in USA",
        tabPanel("Plot",
                sidebarPanel(
                    sliderInput("range",
                        "Time range:",
                        min = 1950,
                        max = 2011,
                        value = c(2001, 2011),
                        format="####"),
                    uiOutput("evtypeControls")
                ),
  
                mainPanel(
                    tabsetPanel(
                        
                        # Visualize data by States
                        tabPanel('By states',
                            column(3,
                                wellPanel(
                                    radioButtons(
                                        "populationCategory",
                                        "Damaged population:",
                                        c("Both" = "both", "Injuries" = "injuries", "Fatalities" = "fatalities"))
                                )
                            ),
                            column(3,
                                wellPanel(
                                    radioButtons(
                                        "economicCategory",
                                        "Economic damage:",
                                        c("Both" = "both", "Property damage" = "property", "Crops damage" = "crops"))
                                )
                            ),
                            column(7,
                                plotOutput("damagedPopulationByState"),
                                plotOutput("economicDamageByState")
                            )

                        ),
                        
                        # Visualize data by year (Time series)
                        tabPanel('By years',
                                 h4('Event evolution by year', align = "center"),
                                 showOutput("eventsByYearAndEvent", "nvd3"),
                                 h4('Damaged population by year', align = "center"),
                                 showOutput("populationDamage", "nvd3"),
                                 h4('Economic damage by year', align = "center"),
                                 showOutput("economicDamage", "nvd3")
                         )
                    )
                )
            
        ),
        
        tabPanel("Info",
            mainPanel(
                includeMarkdown("include.md")
            )
        )
    )
)


