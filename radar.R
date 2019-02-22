library(dplyr)
library(plotly)


##### przygotowanie danych
ph1 <- read.csv("physician.csv")

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


##### Hospitals
h1 <- read.csv("hospitals.csv")
h <- h1 %>% select(- Overall_rating, - Ownership, - Type)

states <- unique(h$state) %>% as.character() %>% sort() 
out <- NULL 
tmp <- NULL 
hospitals <- NULL 
for (s in 1:length(states)) { 
  state <- filter(h, state == states[s])
  for (i in 2:length(state)) { 
    tmp <- table(state[,i]) %>% as.data.frame(.) %>% mutate(category = colnames(state)[i]) 
    out <- rbind(out, tmp) 
    
  } 
  out$state <- states[s]  
  if (s == 1) { 
    hospitals <- out 
  } else { 
    hospitals<- rbind(hospitals, out) 
  }  
  out <- NULL 
  tmp <- NULL 
}  

hospitals$category <- as.factor(hospitals$category) 
hospitals$state    <- as.factor(hospitals$state) 
colnames(hospitals)[1] <- "Values"

hospitals <- hospitals %>% filter(Values == "Below the national average" & state == "AL")

hradar <- hospitals %>% select(category, Freq) %>% t(.)

m <- list(
  l = 10,
  r = 10,
  b = 100,
  t = 100,
  pad = 4
)

plot_ly(
  type = 'scatterpolar',
  fill = 'toself',
  marker = list(color = "red", opacity = .5),
  fillcolor = list(color = "red", opacity = .5),
    r = hradar[2,],
    theta = hradar[1,]
  ) %>%
   layout(
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(0,5)
      )
    ),
    title = "Number of hospitals below national average",
    autosize = F,
    margin = m,
    showlegend = F
  )