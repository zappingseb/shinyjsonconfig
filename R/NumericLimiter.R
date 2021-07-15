#' Single numeric filter - ui and server
#'
#' @import R6
#' @import shiny
#' @importFrom uuid UUIDgenerate
#'
#' @details found the way to construct this class at StackOverflow
#' https://stackoverflow.com/a/60801574/4663704
#'
#' @examples
#'
#' appclass <- NumericLimiter$new(min = 0, max = 1)
#'
#'
#' # UI
#' # Here we only have to call the UI methods.
#' ui <- fluidPage(
#'
#'   appclass$get_ui(),
#'   tags$hr(),
#'   verbatimTextOutput("exampleoutput")
#'
#' )
#'
#' # And here we just have to call the server methods.
#' server <- function(input, output, session) {
#'
#'   output$exampleoutput <- renderText({
#'     x <- appclass$get_server()
#'     paste(unlist(x()))
#'   })
#'
#' }
#' if (interactive()) {
#'   shinyApp(ui, server)
#' }
#'
#' @export
NumericLimiter <- R6Class(
  "NumericLimiter",
  public = list(
    #' @description
    #' Create a NumericLimiter object.
    #' @param min (`numeric`) The lowest value the filter allows
    #' @param max (`numeric`) The highest value the filter allows
    #' @md
    #' @return A new `Person` object.
    initialize = function(min = 0, max = 1) {
      stopifnot(is.numeric(min) && !is.na(min))
      stopifnot(is.numeric(max) && !is.na(max))
      private$id_input <- uuid::UUIDgenerate()
      private$min$value <- min
      private$max$value <- max
    },
    #' @description
    #' numeric filter user interace
    #' @param ns (`function`) Namespace function that gets used
    #'   internally
    #' @return `shiny::fluidRow` painting the
    #'   filter to select a minimum / maximum value
    #' @md
    get_ui = function(ns = NS(NULL)) {
      ns <- shiny::NS(ns(private$id_input))

      print(paste("INUI:", ns("type")))
      base_id <- ns("");

      shiny::fluidRow(
        id = substr(base_id, 1, nchar(base_id) - 1),
        column(4,
               selectInput(
                 inputId = ns("type"),
                 choices = list(
                   "no filtering" = "no-filter",
                   "equal to" = "eq",
                   "greater than" = "gt",
                   "lower than" = "lt",
                   "inside range" = "range"
                 ),
                 label = "Filter method"
               )
        ),
        # minimum value selector
        conditionalPanel(
          condition = paste0(paste0("[", toString(paste0("'",
            c("eq", "gt", "range"), "'")), "]"), ".includes(input.type)"),
          ns = ns,
          column(
            4,
            conditionalPanel(
              condition = paste0(paste0("[", toString(paste0("'",
                c("gt", "range"), "'")), "]"), ".includes(input.type)"),
              ns = ns,
              selectInput(ns("min_select"),
                          choices = c(">", ">="),
                          selected = c(">"),
                          multiple = FALSE,
                          label = paste("Select comparison")
                          )
            ),
            module_ui_value_limit(id = ns("min_value"),
                                  value = private$min$value)
          )
        ),
        # maximum value selector
        conditionalPanel(
          condition = paste0(paste0("[", toString(paste0("'",
            c("lt", "range"), "'")), "]"), ".includes(input.type)"),
          ns = ns,
          column(
            4,
            selectInput(ns("max_select"),
                        choices = c("<", "<="),
                        selected = "<",
                        multiple = FALSE,
                        label = paste("Select comparison")
            ),
            module_ui_value_limit(id = ns("max_value"),
                                  value = private$max$value)
          )
        )
      )

    },
    #' @description
    #' shiny server function calling the module
    #'
    #' @return a `shiny::reactive` with the following
    #' list
    #' * **type** - inputtype
    #' * **min** - lower value selected
    #' * **min** - lower comparison selected
    #' * **max** - upper value selected
    #' * **max** - upper comparison selected
    #' @md
    get_server = function(session) {
      moduleServer(
        id = private$id_input,
        module = private$server,
        session = session
      )
    }
  ),
  private = list(
    type = "eq",
    min = list(
      type = "eq",
      value = 0
    ),
    max = list(
      type = "eq",
      value = 1
    ),
    id_input = NULL,
    server = function(input, output, session) {

      print(paste("INserver:", session$ns("type")))
      print(names(input))
      min_value <- callModule(
        module_value_limit,
        id = "min_value",
        border_value = c(private$min$value, private$max$value)
      );
      max_value <- callModule(
        module_value_limit,
        id = "max_value",
        border_value = c(private$min$value, private$max$value)
      );

      has_min <- reactive(input$type %in% c("eq", "gt", "range"))
      has_min_comp <- reactive(input$type %in% c("gt", "range"))
      has_max <- reactive(input$type %in% c("lt", "range"))

      return(
          reactive({
            print(paste("inputtype", input[[session$ns("type")]]))
            if (length(input$type) > 0) {
              list(
                type = input$type,
                min = ifelse(has_min(), min_value, private$min$value),
                min_compare = ifelse(has_min_comp(), input$min_select, ifelse(
                  input$type == "eq", "=", ">=")),
                max = ifelse(has_max(), max_value, private$max$value),
                max_compare = ifelse(has_max(), input$max_select, "<=")
              )
            }
          })
      )

    }
  )
)
