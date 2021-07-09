#' @import R6
#' @export
FilterNumeric <- R6Class(
  "Filter",
  inherit = Filter,
  public = list(
    initialize = function(active = FALSE, name = NULL, min, max){
      stopifnot(is.logical(active))
      stopifnot(!is.null(name))

      private$active <- active
      private$name <- name
    },
    get_ui = function(){
      return(
        function(id) {
          shiny::div(shiny::tags$span("This is not implemented for the parent class, please use a child class"))
        }
      )
    },
    get_server = function() {
      return(
        function(input, output, session) {

        }
      )
    }
  ),
  private = list(
    active = FALSE,
    name = NULL,
    subfilters = c(
      NumericLimiter$new()
    )
  ),
  active = list(
    name = function(){
      return(private$name)
    }
  )
)
