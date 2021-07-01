#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(purrr)
library(shinyjs)


config_generator <- function() {
    Sys.sleep(2)
    shinyjs::runjs(
        'Shiny.setInputValue("loading", 1, {priority: "event"});'
    )
    return(
        list(
            merge_test = "y",
            merge_test3 = "xxy",
            merge_test4 = "xxxy",
            merge_test5 = "xxxy",
            merge_test6 = "xxxy",
            merge_test7 = "xxxy",
            merge_test8 = "xxxy",
            merge_test9 = "xxxy",
            merge_test10 = "xxxy",
            merge_test11 = "xxxy",
            merge_test12 = "xxxy",
            merge_test13 = "xxxy",
            merge_test14 = "xxxy",
            merge_test15 = "xxxy",
            merge_test16 = "xxxy",
            merge_test17 = "xxxy"
        )
    )
}

source(file = "ui_by_json.R");

# Define UI for application that draws a histogram
ui <- fluidPage(
    useShinyjs(),
    # Application title
    titlePanel("Old Faithful Geyser Data"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            # Loading header
            tags$head(tags$style(type="text/css", "
             #loadmessage {
               position: fixed;
               top: 0px;
               left: 0px;
               width: 100%;
               padding: 5px 0px 5px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #000000;
               background-color: #CCFF66;
               z-index: 105;
             }
          ")),
            div(style="display:none", numericInput(inputId = "loading", label = "label", value = 0, width = "0")),
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            conditionalPanel(condition="input.loading < 1",
                             tags$div("Loading...",id="loadmessage"))
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"),
            # UI described by configufile
            uiByJSONUi("configuredUI")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    callModule(uiByJSON,"configuredUI", config_generator())
}

# Run the application
shinyApp(ui = ui, server = server)