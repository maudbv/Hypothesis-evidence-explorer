

plot_overview <-  function(df = total_df) {

  library(plotly)
  library(ggplot2)
  library(dplyr)

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
  
  library(tidyr)
#   data = df %>%
#     pivot_wider( names_from = c(support_for_hypothesis), 
#                  id_cols = hypothesis,
#                  values_from = c(fraction,n)) %>%
#     arrange(fraction_Supported, desc(fraction_Questioned),.by_group = FALSE)
#  
#   
#   # plot figure 
#   
# fig <- plot_ly(data, type = 'bar',orientation = 'h') %>%
#     add_trace( x = ~ fraction_Supported,
#                y = ~ hypothesis,
#                marker = list(color = hi_colors$cols[1]) ) %>%
#   add_trace( x = ~ fraction_Undecided,
#              y = ~ hypothesis,
#              marker = list(color = hi_colors$cols[2])) %>%
#   add_trace( x = ~ fraction_Questioned,
#              y = ~ hypothesis,
#              marker = list(color = hi_colors$cols[3]))%>%
#   layout(barmode = 'stack',
#                       xaxis = list(title = ""),
#                       yaxis = list(title ="Fraction of papers"),
#          showlegend = FALSE,
#          margin = 0.01
#          )

# By absolute support
data = df %>%
pivot_wider( names_from = c(support_for_hypothesis), 
             id_cols = hypothesis,
             values_from = c(fraction,n)) %>%
  arrange(fraction_Supported, desc(fraction_Questioned),.by_group = FALSE)



fig <- plot_ly(data, type = 'bar',orientation = 'h') %>%
  add_trace( x = ~ n_Supported,
             y = ~ hypothesis,
             marker = list(color = hi_colors$cols[1]),
             name = "Supporting") %>%
  add_trace( x = ~ n_Undecided,
             y = ~ hypothesis,
             marker = list(color = hi_colors$cols[2]),
             name = "Undecided") %>%
  add_trace( x = ~ n_Questioned,
             y = ~ hypothesis,
             marker = list(color = hi_colors$cols[3]),
             name = "Questioning")%>%
  layout(barmode = 'stack',
         xaxis = list(title = "Number of studies"),
         yaxis = list(title =""),
         title = list(text = "Evidence for ten major hypotheses in Invasion Biology",
                      font = list(size = 20),
                      pad = list(b = 0, l = 1, r = 1, t= 1),
                      x = 0.01,
                      y = 0.95),
         legend =  list(text = 'Evidence for the hypothesis'),
         showlegend = TRUE,
         margin = 0.01
  )
  
  return(fig)
}

