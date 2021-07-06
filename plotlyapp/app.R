library(shiny)
library(shinydashboard)
library(plotly)
library(shinyjs)

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
           plotlyOutput("plot")
           )
  )
  
)

clicks<-reactiveValues(legend = NULL, plot=NULL)

filtering <- reactiveValues(all_filters=NULL, registered=0)

server <- shinyServer(function(input, output, session) {
  # Example from plotly
  fig <- economics
  
  color_palette <- RColorBrewer::brewer.pal(dim(economics)[2], "Dark2")
  
  fig <- fig %>% tidyr::gather(variable, value, -date)
  
  fig <- fig %>% transform(id = as.integer(factor(variable)))
  
  output$plot <- renderPlotly({
    
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
        ifelse(x %in% names(filtering$all_filters$range), "legendonly", TRUE)
      })),
      id=rep(6, 10)
    ))
    
    # format the date correctly again
    flag_data$date <- as.Date(flag_data$date,origin = "1970-01-01")
    # flag_data$from <- as.Date(flag_data$from,origin = "1970-01-01")
    # flag_data$to <- as.Date(flag_data$to,origin = "1970-01-01")
    
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
    all_plots <- c(figure_list, figure_list2)
    fig_sub <- subplot(all_plots, nrows=8, shareX = TRUE)
    # cat(filtering$registered)
    # if (filtering$registered == 2) {
      # return(fig_sub)
    # } else {
      # filtering$registered <- filtering$registered + 1
      return(fig_sub %>% event_register("plotly_legendclick") %>% event_register("plotly_relayout"))
    # }
    
  })
  
  observeEvent(event_data("plotly_legendclick"), {
      
      click_data <- event_data("plotly_legendclick");
      
      if (click_data$mode == "markers+text") {
        
        class = unique(click_data$uid);
        subclass = unique(click_data$text);
        
        # ANY FILTERS OF THIS CLASS?
        if(!is.null(filtering$all_filters[[class]])){
          # no filter for subclass
          if (is.null(filtering$all_filters[[class]][[subclass]])){
            cat("no filtering for subclass", subclass)
            filtering$all_filters[[class]][[subclass]] <- click_data
          # filter for subclass
          } else {
            # RESET SPECIFIC SUBFILTER
            filtering$all_filters[[class]][[subclass]] <- NULL
          }
        # set up the filtering for this class
        } else {
          filtering$all_filters[[class]] <- c()
          filtering$all_filters[[class]][[subclass]] <- click_data
        }
        
        if (class == "range") {
          input$filterRange
          updateSelectInput(session, "filterRange",
                            selected = names(filtering$all_filters[[class]]))
        }
        
      }
      
  })
  
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
  
  observeEvent(input$reset, {
    js$resetClick()
  })
})

shinyApp(ui, server)