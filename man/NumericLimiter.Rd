% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NumericLimiter.R
\name{NumericLimiter}
\alias{NumericLimiter}
\title{Single numeric filter - ui and server}
\description{
Single numeric filter - ui and server

Single numeric filter - ui and server
}
\details{
found the way to construct this class at StackOverflow
https://stackoverflow.com/a/60801574/4663704
}
\examples{

appclass <- NumericLimiter$new(min = 0, max = 1, type = "range")


# UI
# Here we only have to call the UI methods.
ui <- fluidPage(

  appclass$get_ui(),
  tags$hr(),
  verbatimTextOutput("exampleoutput")

)

# And here we just have to call the server methods.
server <- function(input, output, session) {

  output$exampleoutput <- renderText({
    x <- appclass$get_server()
    paste(unlist(x()))
  })

}
if (interactive()) {
  shinyApp(ui, server)
}

}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{NumericLimiter$new()}}
\item \href{#method-get_ui}{\code{NumericLimiter$get_ui()}}
\item \href{#method-get_server}{\code{NumericLimiter$get_server()}}
\item \href{#method-clone}{\code{NumericLimiter$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Create a NumericLimiter object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NumericLimiter$new(type = "eq", min = 0, max = 1)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{type}}{(\code{character}) needs to be \code{eq}, \code{gt}, \code{lt} or \code{range}}

\item{\code{min}}{(\code{numeric}) The lowest value the filter allows}

\item{\code{max}}{(\code{numeric}) The highest value the filter allows}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{Person} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_ui"></a>}}
\if{latex}{\out{\hypertarget{method-get_ui}{}}}
\subsection{Method \code{get_ui()}}{
numeric filter user interace
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NumericLimiter$get_ui(ns = NS(NULL))}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ns}}{(\code{function}) Namespace function that gets used
internally}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\code{shiny::fluidRow} painting the
filter to select a minimum / maximum value
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_server"></a>}}
\if{latex}{\out{\hypertarget{method-get_server}{}}}
\subsection{Method \code{get_server()}}{
shiny server function calling the module
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NumericLimiter$get_server()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
a \code{shiny::reactive} with the following
list
\itemize{
\item \strong{type} - inputtype
\item \strong{min} - lower value selected
\item \strong{min} - lower comparison selected
\item \strong{max} - upper value selected
\item \strong{max} - upper comparison selected
}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NumericLimiter$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
