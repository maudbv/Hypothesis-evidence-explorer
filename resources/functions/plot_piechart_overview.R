# donut plot for summary stats on hypothesis support
# directly using plotly


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
      ymin = c(0,ymax[1:2])
    )
  
  p <- ggplot(df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3,
                 fill=support_for_hypothesis)) +
    geom_rect() +
    coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
    xlim(c(2, 4))+ # Try to remove that to see how to make a pie chart
    theme_void() +
    scale_fill_manual(values = c("#5DEDB7","#DFDBD7","#CC6D7B","#27596B")) +
    facet_wrap(vars(hypothesis))

  return(fig)
}



plot_piechart_overview <-  function(df = total_df) {
  
  library(plotly)
  library(ggplot2)
  library(dplyr)
  try(if(is.null(df)) stop("No data"))
  
  plot_piechart <-  function(df) {
    
    library(plotly)
    library(dplyr)
    try(if(is.null(df)) stop("No data"))
    
    counts <- df %>% count(support_for_hypothesis, sort = FALSE, .drop = FALSE)
    
    fig <- counts %>% plot_ly(labels = ~support_for_hypothesis, values = ~n)
    fig <- fig %>% add_pie(hole = 0.6,
                           marker = list( 
                             colors = setNames(hi_colors$cols,nm = hi_colors$col_names),
                             line = list(color = '#FFFFFF', width = 1),
                             pull = 0.1
                           ),
                           domain = list(x = c(0, 0,5), y = c(0, 1))
                           #The 'pull' attribute can also be used to create space between the sectors
    )
    fig <- fig %>% layout(showlegend = FALSE,
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    return(fig)
  }
  
  
  df <- total_df %>% 
    group_by(hypothesis)
  
  f <- do(df, figs = plot_piechart(.))
  fig <- f$figs %>% subplot(nrows = 2)  ####### NOT WORKInG: no grid


  return(fig)
}

  