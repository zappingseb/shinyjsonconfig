library(plotly)

#-------- subplots -------

# Example from plotly
fig <- economics

color_palette <- RColorBrewer::brewer.pal(dim(economics)[2], "Dark2")

fig <- fig %>% tidyr::gather(variable, value, -date)



fig <- fig %>% transform(id = as.integer(factor(variable)))

# make a list of plots, because else we cannot combine it with 
# the flag plot
figure_list <- lapply(unique(fig$id),function(i) {
  return(fig %>% dplyr::filter(id == i) %>% 
           plot_ly(x = ~date, y = ~value) %>%
           add_lines(
             line=list(color = color_palette[i]), name=unique(fig$variable)[i])
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
  text=c(rep("error",5),rep("warn",2),rep("info",3)),
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

fig_sub
