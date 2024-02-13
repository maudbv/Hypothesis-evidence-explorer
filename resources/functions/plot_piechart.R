# donut plot for summary stats on hypothesis support
# directly using plotly

plot_piechart <-  function(df) {
  
  library(plotly)
  library(dplyr)
  try(if(is.null(df)) stop("No data"))
  
  counts <- df %>% count(support_for_hypothesis, sort = FALSE, .drop = FALSE)
  
  fig <- counts %>% plot_ly(labels = ~support_for_hypothesis, values = ~n)
  fig <- fig %>% add_pie(hole = 0.6,
                         marker = list( 
                           colors = setNames(hi_colors$cols,nm = hi_colors$col_names),
                           line = list(color = '#FFFFFF', width = 2),
                           pull = 0.1
                         ),
                         domain = list(x = c(0, 0,5), y = c(0, 1))
                         #The 'pull' attribute can also be used to create space between the sectors
  )
  fig <- fig %>% layout(showlegend = FALSE,
                        margin = list(t  = 1, b=1, l = 1, r = 1),
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  return(fig)
}
