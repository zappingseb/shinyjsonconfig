#' Numeric Filter
#'
#'
#'
#' @import R6
#' @import shiny
#' @importFrom uuid UUIDgenerate
#'
#' @examples
#' appclass <- FilterNumeric$new(
#'   min = 0,
#'   max = 1,
#'   name="myfilter",
#'   subfilters = 3
#' )
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
#'  output$exampleoutput <- renderText({
#'  x <-  appclass$get_server(session)
#'
#'     paste(unlist(
#'        lapply(x(), function(y) {
#'          paste(unlist(y()))
#'        })
#'    ), collapse = ' - ')
#'  })
#' }
#' if (interactive()) {
#'   shinyApp(ui, server)
#' }
#' @export
FilterNumeric <- R6Class(
  "FilterNumeric",
  inherit = GenericFilter,
  public = list(
    #' @description
    #' Constructor for a numeric filter
    #' @param active (`logical`) Whether the
    #'   filter should be active per-se
    #' @param name (`character`) Name of
    #'   the Filter. normally the column name
    #'   of the column the filter is applied to
    #' @param min (`numeric`) minimum value of the filter
    #' @param max (`numeric`) maximum value of the filter
    #' @param subfilters (`numeric`) how many OR combined filters are allowed
    #'   for the numeric filter
    #' @md
    initialize = function(
      active = FALSE,
      name = NULL,
      min,
      max,
      subfilters = 3) {
      stopifnot(is.logical(active))
      stopifnot(!is.null(name))
      stopifnot(is.numeric(min) && !is.na(min))
      stopifnot(is.numeric(max) && !is.na(max))
      stopifnot(is.numeric(subfilters) && !is.na(subfilters))

      private$id_input <- uuid::UUIDgenerate()
      private$isactive <- active
      private$name_val <- name
      private$min_val <- min
      private$max_val <- max
      private$subfilters <- subfilters
      private$subfilter_objects <- setNames(
        lapply(1:subfilters, function(i) {
          return(NumericLimiter$new(min = min, max = max))
        }),
        private$filtername(1:subfilters)
      )
    },
    get_ui = function(ns = NS(NULL)) {
      ns <- shiny::NS(ns(private$id_input))


      shiny::fluidRow(
        column(12,
               sliderInput(
                 inputId = ns("nr_subfilters"),
                 label = "Number of OR combined filters",
                 min = 1,
                 max = private$subfilters,
                 step = 1,
                 value = 1
               )
        ),
        do.call(tagList, lapply(1:private$subfilters, function(i) {
          conditionalPanel(
            condition = paste0("input.nr_subfilters >= ", i),
            private$subfilter_objects[[private$filtername(i)]]$get_ui(ns = ns),
            ns = ns
          )
        }))
      )
    },
    get_server = function(session) {
      callModule(private$server, id = private$id_input, session = session)
    }
  ),
  private = list(
    isactive = FALSE,
    name_val = NULL,
    min_val = NA,
    max_val = NA,
    subfilters = 3,
    id_input = NULL,
    subfilter_objects = NULL,
    filtername = function(id) {
      paste0("filter", id)
    },
    server = function(input, output, session) {

      return(reactive({
        lapply(1:input$nr_subfilters, function(i) {
          private$subfilter_objects[[private$filtername(i)]]$get_server(session)
        })
      }))
    }
  )
)
