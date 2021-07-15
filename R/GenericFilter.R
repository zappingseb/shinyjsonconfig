#' Column Filtering class
#'
#' @import R6
#' @export
GenericFilter <- R6Class(
  "GenericFilter",
  public = list(
    #' @description
    #' Constructor for a gerneric filter
    #' @param active (`logical`) Whether the
    #'   filter should be active per-se
    #' @param name (`character`) Name of
    #'   the Filter. normally the column name
    #'   of the column the filter is applied to
    #' @md
    initialize = function(active = FALSE, name = NULL){
      stopifnot(is.logical(active))
      stopifnot(!is.null(name))

      private$isactive <- active
      private$name_val <- name
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
    isactive = FALSE,
    name_val = NULL
  ),
  active = list(
    name = function(){
      return(private$name_val)
    }
  )
)
