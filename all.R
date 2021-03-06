library(dplyr) 
library(ggplot2)
library(plotly)

h <- read.csv("https://data.medicare.gov/resource/rbry-mqwu.csv") 
h1 <- h %>%  
  select(state, 
         Type = hospital_type, 
         Ownership = hospital_ownership, 
         'Overall_rating' = hospital_overall_rating, 
         Mortality = mortality_national_comparison,
         'Safety_of_care' = safety_of_care_national_comparison,
         Readmission = readmission_national_comparison,
         'Patient_experience' = patient_experience_national_comparison,
         'Effectiveness_of_Care' = effectiveness_of_care_national_comparison,
         'Timeliness_of_Care' = timeliness_of_care_national_comparison,
         'Medical_imaging' = efficient_use_of_medical_imaging_national_comparison
  ) 

 write.csv(h1, "hospitals.csv", row.names = FALSE)

#### Dataset in percentage

states <- unique(h1$state) %>% as.character() %>% sort() 
out <- NULL 
tmp <- NULL 
hospitals <- NULL 
for (s in 1:length(states)) { 
  state <- filter(h1, state == states[s]) 
  for (i in 2:length(state)) { 
    tmp <- table(state[,i]) %>% as.data.frame(.) %>% mutate(category = colnames(state)[i]) 
    tmp$Freq <- tmp$Freq/sum(tmp$Freq)
    out <- rbind(out, tmp) 
    
  } 
  out$state <- states[s] 
  if (s == 1) { 
    hospitals <- out 
  } else { 
    hospitals <- rbind(hospitals, out) 
  } 
  out <- NULL 
  tmp <- NULL 
} 

hospitals$category <- as.factor(hospitals$category) 
hospitals$state    <- as.factor(hospitals$state) 
colnames(hospitals)[1] <- "Values"

 write.csv(hospitals, "hospitals_perc.csv", row.names = FALSE)
### End of dataset in percentage




#### Dataset in nominal values


h2 <- h1
h2 <- h2 %>% group_by(state) %>% count(Mortality)
h2 <- group_by(state) %>% mutate(above = ifelse(Mortality=="Above the national average"), n, 0)

h2 <- h2 %>%group_by(state) %>% 
  mutate(n = n())

for (i in 5:11) {  #correct order of levels
  h2[[i]] <- factor(h2[[i]], levels(h1[[i]])[c(3,4,2,1)])
}
h2$Overall_rating <- factor(h2$Overall_rating, levels(h1$Overall_rating)[c(6, 1:5)]) #correct order of levels
 


### End of dataset in  nominal values


#### Plot of hospital types, potentially sortable, in plotly
g1 <- table(h2$state, h2$Type) %>% as.data.frame(.) %>% tidyr::spread( Var2, Freq)
al <- filter(g1, Var1 == "AL")
al$sum <- sum(al[2:4])
a <- list(
  y = al$Var1,
  x = al$sum,
  text = "AL",
  xref = "x",
  yref = "y",
  showarrow = TRUE,
  arrowhead = 1,
  ax = 30,
  ay = 0
)

library(plotly)
plot_ly(g1, y = ~reorder(g1$Var1, g1$`Critical Access Hospitals`), x = g1$`Acute Care Hospitals`, type = "bar", orientation = 'h', name = 'Acute Care Hospitals') %>%
  add_trace(x = ~g1$`Childrens`, name ="Childrens") %>% 
  add_trace(x = ~g1$`Critical Access Hospitals`, name ="Critical Access Hospitals") %>% 
  layout(xaxis = list(title = "No of hospitals", tickmode = "auto", nticks = 20), 
         yaxis = list(title = "State", ticklen = 5, tickcolor = toRGB("white")), barmode = 'stack') %>%
  layout(hovermode = 'compare') %>% 
  layout(annotations = a)

### End of plotly hospital types
