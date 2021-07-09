#' Derive economics data
#'
#' @return economics data from ggplot2
#'
#' @importFrom ggplot2 economics
#' @export
#' @author Sebastian Engel-Wolf\url{mailto:sebastian@@mail-wolf.de}
example_data_plot <- function(){
  return(ggplot2::economics)
}

#' Derive flag data
#' @param active_filtered_flags (\code{character}) vector that
#'   may only contain \code{error}, \code{warn}, \code{info}
#' @import tidyr tibble
example_data_flag <- function(active_filtered_flags) {

  # check values of active_filtered flags
  if (length(active_filtered_flags) > 0) {
    stopifnot(
      length(
        setdiff(c("error","info", "warn"), active_filtered_flags)
      ) > 0
    )
  }

  # create flag data with errors, warnings and infos
  flags <- c("error","warn","warn","info","info","warn","warn","info","error","error")

  flag_data = tidyr::tibble(data.frame(
    date=vapply(c(
      "1970-01-01",
      "1975-01-01",
      "1980-01-01",
      "1985-01-01",
      "1990-01-01",
      "1995-01-01",
      "2000-01-01",
      "2005-01-01",
      "2010-01-01",
      "2015-01-01"
    ), as.Date, FUN.VALUE = as.Date("1970-01-01")),
    from =vapply(c(
      "1967-07-01",
      "1975-01-01",
      "1980-01-01",
      "1985-01-01",
      "1990-01-01",
      "1995-01-01",
      "2000-01-01",
      "2005-01-01",
      "2010-01-01",
      "2013-07-01"
    ), as.Date, FUN.VALUE = as.Date("1970-01-01")),
    to = vapply(c(
      "1973-06-30",
      "1975-01-01",
      "1980-01-01",
      "1985-01-01",
      "1990-01-01",
      "1995-01-01",
      "2000-01-01",
      "2005-01-01",
      "2010-01-01",
      "2017-06-30"
    ), as.Date, FUN.VALUE = as.Date("1970-01-01")),
    value=rep(10,10),
    variable=rep("flag",10),
    text=flags,
    visible=unlist(lapply(flags, function(x){
      ifelse(x %in% active_filtered_flags, "legendonly", TRUE)
    })),
    id=rep(6, 10)
  ))

  # format the date correctly again
  flag_data$date <- as.Date(flag_data$date,origin = "1970-01-01")

  return(flag_data)
}
