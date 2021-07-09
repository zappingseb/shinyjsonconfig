#' @import R6
#' @export
Filter <- R6Class(
  "Filter",
  public = list(
    initialize = function(active = FALSE, name = NULL){
      stopifnot(is.logical(active))
      stopifnot(!is.null(name))

      privateäactive <- active
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
    name = NULL
  ),
  active = list(
    name = function(){
      return(private$name)
    }
  )
)
