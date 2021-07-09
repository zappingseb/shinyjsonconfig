#' Configuration Generator for examples
#'
#' @export
helper_config_generator <- function() {
  Sys.sleep(2)
  shinyjs::runjs(
    'Shiny.setInputValue("loading", 1, {priority: "event"});'
  )
  return(
    list(
      merge_test = "y",
      merge_test3 = "xxy",
      merge_test4 = "xxxy",
      merge_test5 = "xxxy",
      merge_test6 = "xxxy",
      merge_test7 = "xxxy",
      merge_test8 = "xxxy",
      merge_test9 = "xxxy",
      merge_test10 = "xxxy",
      merge_test11 = "xxxy",
      merge_test12 = "xxxy",
      merge_test13 = "xxxy",
      merge_test14 = "xxxy",
      merge_test15 = "xxxy",
      merge_test16 = "xxxy",
      merge_test17 = "xxxy"
    )
  )
}

helper_is.Date_single <- function(x){
  is.na(as.Date(as.character(x), format="%d/%m/%Y"))
}

helper_is.Date <- function(x) {
  if (length(x) > 1) {
    return(all(vapply(x, helper_is.Date_single, rep(TRUE, length(x)))))
  } else {
    return(helper_is.Date_single(x))
  }
}
