# Plotting an overview of support for all hypotheses

# Comments:
# directly using plotly because ggplotly does not work well with the pie chart
# piecharts do not work in plotly if I want to have subplots.https://github.com/plotly/plotly.R/issues/655 
# It's weird, so I changed the type of plot

plot_piechart_overview <-  function(df = total_df) {
  
  library(plotly)
  library(ggplot2)
  library(dplyr)
  try(if(is.null(df)) stop("No data"))

  df <- total_df %>% 
    group_by(hypothesis) %>% 
    count(support_for_hypothesis, sort = FALSE, .drop = FALSE) %>% 
    mutate(
      total = sum(n),
      fraction = n/total,
      ymax = cumsum(fraction),
      ymin = c(0,ymax[1:2]),
      text = paste(round(fraction*100,digits = 2),
                    '% (',n,')', sep = ""),
      y_lab = (ymax + ymin) /2,
      x_lab = 3.5
    )
  
  p <- ggplot(df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3,
                 fill=support_for_hypothesis)) +
    geom_rect() +
    geom_label(data = df, mapping = aes(x = x_lab, y = y_lab, label = text), label.size = 0.2) +
    coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
    xlim(c(2, 4))+ # Try to remove that to see how to make a pie chart
    theme_void(  base_size = 13) +
    scale_fill_manual(values = c("#5DEDB7","#DFDBD7","#CC6D7B","#27596B")) +
    facet_wrap(vars(hypothesis))

  
  p
  return(p)
}

