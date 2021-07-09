#' @import R6
#' @export
ColumnClassNumeric <- R6Class(
  "ColumnClassNumeric",
  inherit = ColumnClass,
  public = list(

   #' @description
   #' Create a new person object.
   #' @param name Name.
   #' @param hair Hair color.
   #' @return A new `Person` object.
   initialize = function(x = NULL, y = NULL, name = NULL) {
     stopifnot(!is.null(x))
     stopifnot(!is.null(y))
     stopifnot(is.character(name))

     stopifnot(helper_is.Date(x))
     stopifnot(is.numeric(y))


     private$x <- x
     private$y <- y
     private$name <- name
     private$filter <- FilterNumeric$new(
       active = FALSE,
       name = paste0(private$name, "-filter"),
       min = min(private$y, na.rm = TRUE),
       max = max(private$y, na.rm = TRUE)
     )
   },
   get_x_filter = function() {
     indeces <- private$filter$get_filtered_indeces(private$y)
     return(private$x[indeces])
   },
   set_y = function(y) {
     stopifnot(is.numeric(y))
     private$y <- y
   }
 )
)
