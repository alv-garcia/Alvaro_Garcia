



library(tidyverse)
library(plotly)



CO2<-read.csv("CO2.csv",sep =",")



fig <- plot_ly(CO2, type = "scatter" , mode = "lines") %>% 
  add_trace( x = CO2$Year , y = CO2$CO2.concentrations) %>% 
  layout(showlegend = F, title='Concentra�ao de CO2 na hist�ria do planeta',
         xaxis = list(rangeslider = list(visible = T)))


fig <- fig %>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6', width = 900)


fig



