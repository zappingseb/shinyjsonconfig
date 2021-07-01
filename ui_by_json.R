uiByJSONUi <- function(id) {
  ns <- NS(id)
  
  return(
    uiOutput(ns("xx"))
  )
}

uiByJSON <- function(input, output, session, config) {
  ## Render an input for each single file in config
  output$xx <- renderUI(purrr::map(
    names(config),
    ~shiny::textInput(inputId = .x, label=.x, value=config[[.x]])
  ))
}
