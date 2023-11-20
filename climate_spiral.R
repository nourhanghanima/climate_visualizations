install.packages("plotly")
install.packages("htmlwidgets")
library(plotly)
library(tidyverse)
library(glue)
library(htmlwidgets)

#loading the data
#month.abb is built in
interactive_data <- read_csv("/Users/nourhanghanima/downloads/GLB.Ts+dSST(2).csv", skip=1, na = "***") |> 
  select(Year, all_of(month.abb)) |>
  drop_na() |>

#tidying the data
 pivot_longer(-Year, names_to = "month", 
values_to = "t_diff") |> 
#changing month from character to factor 
mutate(month = factor(month, levels = month.abb)) |>
#creating radius 
                              mutate(month_number = as.numeric(month),
                                     radius = t_diff + 1.5,
                                     theta = 2*pi*(month_number - 1)/12, 
                                     x = radius *sin(theta), 
                                     y = radius * cos(theta), 
                                     z = Year, 
                                     label = glue("{month} {Year}\n{t_diff}\u00B0 C"))


#creating the plot 


axx <- list(
  title = "",
  showgrid = FALSE,
  zeroline = FALSE,
  showticklabels = FALSE
)

axy <- list(
  title = "",
  showgrid = FALSE,
  zeroline = FALSE,
  showticklabels = FALSE
)

axz <- list(
  title = ""
)


p<- plot_ly(interactive_data, 
        x = ~x, 
        y = ~y, 
        z = ~z, 
        text = ~label,
        hoverinfo = "text",
        type = 'scatter3d', mode = 'lines',
        line = list(width = 4, color = ~t_diff,
                    cmid = 0, cmin = min(interactive_data$t_diff), cmax = max(interactive_data$t_diff),
                    colorscale = list(c(0,'#0000FF'),
                                 c(0.5, "#FFFFFF"),
                                 c(1,'#FF0000')))) |>
       layout(scene = list(xaxis=axx,
                      yaxis=axy,
                      zaxis=axz))

saveWidget(p, "downloads/climate_spiral_plotly.html")
