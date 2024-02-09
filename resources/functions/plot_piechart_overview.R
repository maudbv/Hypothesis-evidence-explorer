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
    theme_void() +
    scale_fill_manual(values = c("#5DEDB7","#DFDBD7","#CC6D7B","#27596B")) +
    facet_wrap(vars(hypothesis))

  return(p)
}


# 
# plot_overview <-  function(df = total_df) {
#   
#   library(plotly)
#   library(ggplot2)
#   library(dplyr)
#   try(if(is.null(df)) stop("No data"))
#   
#   plot_piechart <-  function(df) {
#     plot_ly(df, labels = ~support_for_hypothesis, values = ~n) %>% 
#       add_pie(hole = 0.6,
#                            marker = list( 
#                              colors = setNames(hi_colors$cols,nm = hi_colors$col_names),
#                              line = list(color = '#FFFFFF', width = 1),
#                              pull = 0.1
#                            )) # %>%
#                          #  domain = list(x = c(0, 0,5), y = c(0, 1))) %>% 
#               
#                            #The 'pull' attribute can also be used to create space between the sectors
#       # layout(showlegend = FALSE,
#       #                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#       #                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
#     
#   }
#   
#   
#   df <- total_df %>% 
#     count(support_for_hypothesis, sort = FALSE, .drop = FALSE)  %>%
#     group_by(hypothesis) %>%
#     group_map(~plot_piechart(.))
#   
#   list_fig <- list()
#   for (i in 1:length(unique(df$hypothesis))) {
#   subdf = 
#   f <- do(df, figs = plot_piechart(.))
#   f <- deframe(df, plot_piechart(.))
#   
# subplot(df[[1]],df[[2]], nrows = 1)  ####### NOT WORKInG: no grid
# 
# 
# list_df = total_df  %>%
#   split(.$hypothesis) %>%
#   lapply(function(d) count(d,support_for_hypothesis, sort = FALSE, .drop = FALSE))
#   
# p = plot_ly(list_df[[1]], labels = ~support_for_hypothesis, values = ~n) %>% 
#   add_pie(hole = 0.6,
#           marker = list( 
#             colors = setNames(hi_colors$cols,nm = hi_colors$col_names),
#             line = list(color = '#FFFFFF', width = 1),
#             pull = 0.1
#           )) 
# 
# subplot(p,p,p,p,
#         margin = 0.05,
#         nrows=2,
#         which_layout =1
# ) %>% layout(showlegend = FALSE)
# 
# 
#   # or pass a list
# total_df  %>%
#   split(.$hypothesis) %>%
#   lapply(function(d) count(d,support_for_hypothesis, sort = FALSE, .drop = FALSE))  %>%
#   lapply(function(d)  plot_piechart(d)) %>%
#   subplot(nrows =3)
# 
#   return(fig)
# }

  