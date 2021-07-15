#' Module for limited values
#' @param id (`character`) UI ID
#' @param value (`numeric`) default value of the input field
#'
#' renders an input field with an additional error
#' field to show if the numeric value is inside
#' or outside of a range of desired values
#'
#' for a useful example see \link{module_value_limit}
#'
#' @return \link[shiny]{tagList} with a numeric input field
#' @seealso module_value_limit
#' @author Sebastian Engel-Wolf \link{mailto:sebastian@@mail-wolf.de}
#' @md
module_ui_value_limit <- function(id, value) {

  stopifnot(is.character(id))
  stopifnot(is.numeric(value))

  ns <- NS(id)

  return(
    tagList(
      numericInput(ns("value"), value = value, label = "Value"),
      div(style = "color:red", textOutput(outputId = ns("error_text")))
    )
  )
}

#' Module for limited values - server
#'
#' @param input (from shiny)
#' @param output (from shiny)
#' @param session (from shiny)
#' @param border_value (`numeric`) vector of length 2 that
#'   defines the lower and upper limit of values allowed
#'   inside the input
#'
#' @return shiny module server function ony functional with
#'   \link{module_ui_value_limit}
#' @seealso module_ui_value_limit
#' @import shiny
#' @examples
#'
#' ui <- shiny::fluidPage(
#'   tags$span("Only works with values between 1 and 6"),
#'   module_ui_value_limit(id = "noID", value = 5),
#'   textOutput(outputId = "out")
#' )
#' server <- function(input, output, session) {
#'   val <- reactive({
#'     shiny::callModule(
#'      module = module_value_limit,
#'      id = "noID",
#'      border_value = c(1, 6)
#'     )
#'   })
#'   output$out <- shiny::renderText({val()})
#' }
#'
#' if (interactive()) {
#'   shiny::shinyApp(ui, server)
#' }
#'
#' @author Sebastian Engel-Wolf \link{mailto:sebastian@@mail-wolf.de}
#' @md
module_value_limit <- function(input, output, session, border_value) {

  stopifnot(is.numeric(border_value))
  stopifnot(length(border_value) == 2)
  stopifnot(border_value[1] <= border_value[2])

  output$error_text <- renderText({
    if (length(input$value) > 0) {
      if (is.na(input$value)) {
        "Field cannot be left empty"
      } else {
        if (input$value > border_value[2]) {
          paste("Please enter a value <=", border_value[2])
        } else if (input$value < border_value[1]) {
          paste("Please enter a value >=", border_value[1])
        } else {
          NULL
        }
      }
    } else {
      NULL
    }
  })

  if (length(input$value) > 0) {

    if (is.na(input$value)) {
      return(NA)
    } else {
      if (input$value > border_value[2]) {
        return(NA)
      } else if (input$value < border_value[1]) {
        return(NA)
      } else {
        return(input$value)
      }
    }
  } else {
    return(NA)
  }
}
