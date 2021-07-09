library(shiny)
library(shinydashboard)
library(plotly)
library(shinyjs)

source("plotly_module.R")

ui <- shiny::fluidPage(
  useShinyjs(),
  fluidRow(
    column(4,
           shiny::fluidRow(
             shiny::column(12,
                           shiny::selectInput(
                             multiple = TRUE,
                             inputId="filterRange",
                             label="flags",
                             choices=c("error","info","warn"),
                             selected=c("error","info","warn")
                             )
                           ),
             shiny::column(6, verbatimTextOutput("legendclickevent")),
             shiny::column(6, verbatimTextOutput("activefilters")),
             shiny::column(6, verbatimTextOutput("selected"))
           )
           ),
    column(8,
           plotlyUI("final_plot")
           )
  )

)

clicks<-reactiveValues(legend = NULL, plot=NULL)


server <- shinyServer(function(input, output, session) {
  # Example from plotly
  fig <- economics

  color_palette <- RColorBrewer::brewer.pal(dim(economics)[2], "Dark2")

  fig <- fig %>% tidyr::gather(variable, value, -date)

  fig <- fig %>% transform(id = as.integer(factor(variable)))

  all_plots <- reactive({

    if (!is.null(filtering$all_filters)) {
      for (filterclass in names(filtering$all_filters)) {
        if (length(filtering$all_filters[[filterclass]]) > 0) {
          for (filtersubclass in filtering$all_filters[[filterclass]]) {

            if (filtersubclass$uid == "range") {
              for (i in seq_along(filtersubclass$x)) {
                smaller <- as.Date(filtersubclass$ids[i], origin = "1970-01-01")
                bigger <- as.Date(filtersubclass$customdata[i], origin = "1970-01-01")
                fig <- fig %>%
                  dplyr::filter(!(date > smaller & date < bigger))

              }
            }

          }
        }
      }
      figure_data <- fig
    } else {
      figure_data <- fig
    }
    # make a list of plots, because else we cannot combine it with
    # the flag plot
    figure_list <- lapply(unique(figure_data$id),function(i) {
      return(figure_data %>% dplyr::filter(id == i) %>%
               plot_ly(x = ~date, y = ~value) %>%
               add_lines(
                 line=list(color = color_palette[i]), name=unique(figure_data$variable)[i])
      )
    })

    flag_data <- example_data_flag(names(filtering$all_filters$range))



    # create a marker plot

    color_arr = c(error = "red",info = "blue",warn = "orange")
    figure_list2 <- lapply(unique(flag_data$text), function(flag_value){

      filtered_data <- flag_data %>% dplyr::filter(text == flag_value);

    fig1 <- plot_ly(
      data = filtered_data,
      x = ~date,
      y = ~value,
      text=~text,
      # color=~text,
      visible=~visible,
      name=flag_value,
      # colors=color_arr,
      type="scatter",
      mode="markers+text",
      marker = list(
        color = '#ffffff',
        size = 20,
        line = list(
          width = 1,
          color=color_arr[flag_value]
        ),
        symbol = 'square'
      ),
      uid="range",
      ids=~from,
      customdata=~to,
      textposition = "middle center",
      textfont = list(
        family = "sans serif",
        size = 10,
        color=toRGB("grey50")
      )
    ) %>% layout(yaxis=list(
      visible=FALSE

    ))
    })



    #---- plotting ----
    return(c(figure_list, figure_list2))

  })


  filtering <- callModule(module = plotlyModule, id = "final_plot", all_plots());

  output$legendclickevent <- renderPrint({
    event_data("plotly_legendclick")
  })

  output$selected <- renderPrint({
    event_data("plotly_relayout")
  })

  output$activefilters <- renderPrint({

    paste(vapply(names(filtering$all_filters), function(x){
      paste(paste0(x,": !"),paste(names(filtering$all_filters[[x]]), collapse=", !"))
    }, rep("", length(filtering$all_filters))), collapse = "\n")

  })

})

shinyApp(ui, server)
