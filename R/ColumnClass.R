#' @import R6
#' @export
ColumnClass <- R6Class(
  "ColumnClass",
  public = list(

    #' @description
    #' Create a new person object.
    #' @param name Name.
    #' @param hair Hair color.
    #' @return A new `Person` object.
    initialize = function(x = NULL, y = NULL, name) {
      stopifnot(!is.null(x))
      stopifnot(!is.null(y))
      stopifnot(helper_is.Date(x))
      stopifnot(is.character(name))

      private$x <- x
      private$y <- y
      private$name <- name
    },
    set_x = function(x) {
      stopifnot(helper_is.Date(x))
      private$x <- x
    },
    set_y = function(y) {
      private$y <- y
    },
    get_filter_module = function() {
      return(
        list(
          ui = private$filter$get_ui(),
          server = private$filter$get_server()
        )
      )
    }
  ),
  private = list(
    x = NULL,
    y = NULL,
    name = NULL,
    filter = Filter$new()
  ),
  active = list(
    x = function() {return(private$x)},
    y = function() {return(private$y)}
  )
)
