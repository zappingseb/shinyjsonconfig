plotlyUI <- function(id) {
  ns <- NS(id)
  return(
    tagList(
      plotlyOutput(ns("plot"))
      )
    )
}



plotlyModule <- function(input, output, session, all_plots) {
  
  filtering <- reactiveValues(all_filters=NULL, registered=0)
  
  output$plot <- plotly::renderPlotly({
    fig_sub <- subplot(all_plots, nrows=8, shareX = TRUE)
    # cat(filtering$registered)
    # if (filtering$registered == 2) {
    # return(fig_sub)
    # } else {
    # filtering$registered <- filtering$registered + 1
    return(fig_sub %>% event_register("plotly_legendclick") %>% event_register("plotly_relayout"))
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
  
  return(filtering)
}