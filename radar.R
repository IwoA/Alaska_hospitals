dat <- data.frame(a=5, b=8, c=4, d=-1, e=4, f=2, g=5, h=2)

library(ggradar)

ggradar(dat)

mtcars %>%
     rownames_to_column( var = "group" ) %>%
     tail(4) %>% select(1:10) -> mtcars_radar

ggradar(dat, grid.min = 0, grid.max = 10) 

library(plotly)

plot_ly(
     type = 'scatterpolar',
     r = as.numeric(dat[1,]),
     theta = colnames(dat),
     fill = 'toself'
) %>%
     layout(
          polar = list(
               radialaxis = list(
                    visible = T,
                    range = c(-5,10)
               )
          ),
          showlegend = F
     )


##### przygotowanie danych
out <- matrix(NA, nrow = 3, ncol = 8)

for (i in 1:length(levels(ph1$measure_title))) {
     tmp <- filter(ph1, measure_title == levels(ph1$measure_title)[i]) %>% 
          group_by(state) %>% 
          summarise(performance = mean(measure_performance_rate, na.rm = TRUE)) %>% 
          droplevels(.)
     
     tmp$performance <- coalesce(tmp$performance, 0) #remove of NAs
     
     first <- levels(reorder(tmp$state, -tmp$performance))[1]
     first <- filter(tmp, state == first)
     
     second <- levels(reorder(tmp$state, -tmp$performance))[2]
     second <- filter(tmp, state == second)
     
     third <- levels(reorder(tmp$state, -tmp$performance))[3]
     third <- filter(tmp, state == third)
     
     al <- filter(tmp, state == "AL")
     
     out[1,i] <- round(first$performance, digits = 0) - round(al$performance, digits = 0)
     out[2,i] <- round(second$performance, digits = 0) - round(al$performance, digits = 0)
     out[3,i] <- round(third$performance, digits = 0) - round(al$performance, digits = 0)
}
colnames(out) <- levels(ph1$measure_title)
#rownames(out) <- "Distance to 3rd state"

m <- list(
     l = 10,
     r = 10,
     b = 100,
     t = 100,
     pad = 4
)

t <-  colnames(out)
t[5] <- "Getting timely care"

plot_ly(
     type = 'scatterpolar',
     fill = 'toself'
)%>%
     add_trace(
          r = out[1,],
          theta = t,
          name = 'Distance to 1st state'
     ) %>%
     add_trace(
          r = out[2,],
          theta = t,
          name = 'Distance to 2nd state'
     ) %>%
     add_trace(
          r = out[3,],
          theta = t,
          name = 'Distance to 3rd state'
     ) %>%
     layout(
          polar = list(
               radialaxis = list(
                    visible = T,
                    range = c(-1,25)
               )
          ),
          title = "Alaska overview - physicians' assessment",
          autosize = F,
          margin = m
     )