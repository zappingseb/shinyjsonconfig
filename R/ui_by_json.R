#' JSON Importer UI
#' 
#' @description UI fitting to \link{uiByJSON}
#' 
#' @param id (\code{string}) Id for the namespace
#' 
#' @examples 
#'
#' #Define UI for application that draws a histogram
#' ui <- fluidPage(
#'   # Application title
#'   titlePanel("Old Faithful Geyser Data"),
#'   
#'   # Sidebar with a slider input for number of bins
#'   sidebarLayout(
#'     sidebarPanel(
#'       # Loading header
#'       tags$head(tags$style(type="text/css", "
#'              #loadmessage {
#'                position: fixed;
#'                top: 0px;
#'                left: 0px;
#'                width: 100%;
#'                padding: 5px 0px 5px 0px;
#'                text-align: center;
#'                font-weight: bold;
#'                font-size: 100%;
#'                color: #000000;
#'                background-color: #CCFF66;
#'                z-index: 105;
#'              }
#'           ")),
#'       div(style="display:none", numericInput(inputId = "loading", label = "label", value = 0, width = "0")),
#'       sliderInput("bins",
#'                   "Number of bins:",
#'                   min = 1,
#'                   max = 50,
#'                   value = 30),
#'       conditionalPanel(condition="input.loading < 1",
#'                        tags$div("Loading...",id="loadmessage"))
#'     ),
#'     
#'     # Show a plot of the generated distribution
#' mainPanel(
#'       plotOutput("distPlot"),
#'       # UI described by configufile
#'       uiByJSONUi("configuredUI")
#'     )
#'   )
#' )
#' 
#' # Define server logic required to draw a histogram
#' server <- function(input, output) {
#'   
#'   output$distPlot <- renderPlot({
#'     # generate bins based on input$bins from ui.R
#'     x    <- faithful[, 2]
#'     bins <- seq(min(x), max(x), length.out = input$bins + 1)
#'     
#'     # draw the histogram with the specified number of bins
#'     hist(x, breaks = bins, col = 'darkgray', border = 'white')
#'   })
#'   
#'   callModule(uiByJSON,"configuredUI", helper_config_generator())
#' }
#' if (interactive()) {
#'   # Run the application
#'   shinyApp(ui = ui, server = server)
#' }
#' 
#' @import shiny
#' @export
#'  
uiByJSONUi <- function(id) {
  ns <- NS(id)
  
  return(
    uiOutput(ns("xx"))
  )
}

#' JSON importer server
#' 
#' @import shiny
#' @importFrom purrr map
#' @export
uiByJSON <- function(input, output, session, config) {
  ## Render an input for each single file in config
  output$xx <- renderUI(purrr::map(
    names(config),
    ~shiny::textInput(inputId = .x, label=.x, value=config[[.x]])
  ))
}

