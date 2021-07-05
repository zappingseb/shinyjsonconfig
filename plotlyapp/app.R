library(shiny)
library(plotly)
library(shinyjs)

ui <- shinyUI(
  fluidPage(
    useShinyjs(),
    # code to reset plotlys event_data("plotly_click", source="A") to NULL -> executed upon action button click
    # note that "A" needs to be replaced with plotly source string if used
    extendShinyjs(text = "shinyjs.resetClick = function() { Shiny.onInputChange('.clientValue-plotly_legendclick-A', 'null'); }"),
    actionButton("reset", "Reset plotly click value"),
    plotlyOutput("plot"),
    shiny::fluidRow(
      
      shiny::column(6, verbatimTextOutput("clickevent")),
      shiny::column(6, verbatimTextOutput("legendclickevent"))
    )
    
  )
)

clicks<-reactiveValues(legend = NULL, plot=NULL)

filtering <- reactiveValues(warn=NULL)

server <- shinyServer(function(input, output) {
  
  
  # Example from plotly
  fig <- economics
  
  color_paxlette <- RColorBrewer::brewer.pal(dim(economics)[2], "Dark2")
  
  fig <- fig %>% tidyr::gather(variable, value, -date)
  
  fig <- fig %>% transform(id = as.integer(factor(variable)))
  
  output$plot <- renderPlotly({
    
    if (!is.null(filtering$warn)) {
      for (date_clicked in filtering$warn$x) {
        smaller <- as.Date(date_clicked, origin = "1970-01-01") - (2.5*365)
        bigger <- as.Date(date_clicked, origin = "1970-01-01") + (2.5*365)
        fig <- fig %>%
          dplyr::filter(!(date > smaller & date < bigger))
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
    
    # create flag data with errors, warnings and infos
    
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
      value=rep(10,10),
      variable=rep("flag",10),
      text=c("error","warn","warn","info","info","warn","warn","info","error","error"),
      id=rep(6, 10)
    ))
    # format the date correctly again
    flag_data$date <- as.Date(flag_data$date,origin = "1970-01-01")
    
      # create a marker plot
    fig1 <- flag_data %>% plot_ly(
      x = ~date,
      y = ~value,
      text=~text,
      color=~text,
      colors=c("red","blue","orange"),
      type='scatter',
      mode="markers+text",
      marker = list(
        color = '#ffffff',
        size = 20,
        line = list(
          width = 1
        ),
        symbol = 'square'
      ),
      textposition = "center center",
      textfont = list(
        family = "sans serif",
        size = 10,
        name="flag",
        color=toRGB("grey50")
      ) %>% add_trace()
    )
    
    #---- plotting ----
    all_plots <- c(figure_list, list(fig1))
    fig_sub <- subplot(all_plots, nrows=6, shareX = TRUE)
    
    return(fig_sub %>% event_register("plotly_legendclick"))
  })
  
  observeEvent(event_data("plotly_legendclick"), {
    if (is.null(filtering$warn)) {
      filtering$warn <- event_data("plotly_legendclick")
    } else {
      filtering$warn <- NULL
    }
  })
  
  output$legendclickevent <- renderPrint({
    event_data("plotly_legendclick")
  })
  
  output$clickevent <- renderPrint({
    event_data("plotly_click")
  })  
  
  observeEvent(input$reset, {
    js$resetClick()
  })
})

shinyApp(ui, server)