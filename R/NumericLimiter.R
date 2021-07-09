#' @import R6
#' @import shiny
#' @importFrom uuid UUIDgenerate
#'
#' @examples
#'
#' appclass <- NumericLimiter$new(min = 0, max = 1, type = "range")
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
    initialize = function(type = "eq", min = 0, max = 1){
      stopifnot(type %in% c("eq", "gt", "lt", "range"))

      private$id_input <- uuid::UUIDgenerate()
      private$min$value <- min
      private$max$value <- max
      private$type <- type
      private$has_min <- (type %in% c("eq", "lt", "range"))
      private$has_min_comp <- (type %in% c("lt", "range"))
      private$has_max <- (type %in% c("gt", "range"))
    },
    get_ui = function(ns = NS(NULL)){
      ns <- shiny::NS(ns(private$id_input))

      shiny::fluidRow(
        if (private$has_min) {
          column(
            6,
            if (private$has_min_comp) {

              selectInput(ns("min_select"),
                          choices = c(">", ">="),
                          selected = c(">"),
                          multiple = FALSE,
                          label=paste("Select", ifelse(private$type=="range", "lower", ""), "comparison")
                          )
            },
              numericInput(ns("min_value"), value = private$min$value, label = "Value")
          )
        },

        if (private$has_max) {
          column(
            6,
            selectInput(ns("max_select"),
                        choices = c("<", "<="),
                        selected = "<",
                        multiple = FALSE,
                        label=paste("Select", ifelse(private$type=="range", "upper", ""), "comparison")
            ),
            numericInput(ns("max_value"), value = private$max$value, label = "Value")
          )
        }
      )

    },
    get_server = function(){

      callModule(private$server, id = private$id_input)

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
    has_min = TRUE,
    has_min_comp = FALSE,
    has_max = FALSE,
    id_input = NULL,
    server = function(input, output, session) {

      return(
        reactive({
          list(
            type = private$type,
            min = ifelse(private$has_min, input$min_value, -Inf),
            min_compare = ifelse(private$has_min_comp, input$min_select, ""),
            max = ifelse(private$has_max, input$max_value, Inf),
            max_compare = ifelse(private$has_max, input$max_select, "")
          )

        })
      )

    }
  ),
  active = list(
    name = function(){
      return(private$name)
    }
  )
)
