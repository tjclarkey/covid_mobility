library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
source("107script.R", local = TRUE)
source("uicomponents/header.R", local = TRUE)
source("uicomponents/body.R", local = TRUE)
source("uicomponents/rightsidebar.R", local = TRUE)
source("serverobjects/MobilityPlot.R", local = TRUE)
source("serverobjects/graphic1.R", local = TRUE)
source("uicomponents/body1.R", local = TRUE)

ui <- dashboardPagePlus(
        header = hdr,
        sidebar = dashboardSidebar(),
        body = body1,
        rightsidebar = rsb,
        title = "Covid Mobility")
        
server = function(input, output) {
        
        output$histL <- renderPlot({
                hist(rnorm(input$obs), 
                     main = paste("Histogram of", input$obs, "Observations of Random Normal", sep = " "),
                     col = "#3a8be8", border = "#3ae8a8")
        })
        
        output$histM <- renderPlot({
                hist(rnorm(27))
        })
        
        output$histU <- renderPlot({
                hist(rnorm(27))
        })
        
        output$graphic1 <- renderPlot({
                graphic1(input$country_g1, input$type_g1)
        })
}

shinyApp(ui = ui, server = server)

