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

### End of dataset in percentage


#### Percentage plot
tmp <- subset(hospitals, category == "Mortality")
tmp <- droplevels(tmp)
tmp$Values <- factor(tmp$Values, levels(tmp$Values)[c(1,3,4,2)])  #correct order of levels
tmp[tmp$Values=="Below the national average",2] <- tmp[tmp$Values=="Below the national average",2] *-1
tmp <- tmp %>% group_by(state) %>% 
  mutate(above = if_else(Values == "Above the national average", Freq, 0)) %>% 
  arrange(state)

ggplot(tmp, aes(x=reorder(state, -above), y = Freq)) +
  geom_col(aes(fill = Values)) +
  scale_fill_manual(values = c("lightgray", "red", "orange", "green")) +
  geom_vline(aes(xintercept = which(levels(reorder(state, -above)) %in% "AL")), color = "grey") +
  theme_minimal()

### End of percentage plot

#### Dataset in nominal values

h2 <- h1
h2 <- h2 %>%group_by(state) %>% 
  mutate(n = n())

for (i in 5:11) {  #correct order of levels
  h2[[i]] <- factor(h2[[i]], levels(h1[[i]])[c(3,4,2,1)])
}
h2$Overall_rating <- factor(h2$Overall_rating, levels(h1$Overall_rating)[c(6, 1:5)]) #correct order of levels
 
#write.csv(h2, "hospitals.csv", row.names = FALSE)

### End of dataset in  nominal values

#### Plot in nominal values

graph <- function(data, tab = h2){  # helper plotting graphs
  if (data == "Overall_rating") {
    ggplot(tab, aes(x=reorder(state,-n))) +
      geom_bar(aes(fill=`Overall_rating`)) +   # Overall rating
      scale_fill_manual(values = c("#d7191c","#fdae61", "#ffffbf", "#a6d96a", "#1a9641", "lightgray")) + #colors - http://colorbrewer2.org/#type=diverging&scheme=RdYlGn&n=5
      geom_vline(aes(xintercept = which(levels(reorder(state, -n)) %in% "AL")), color = "grey") +
      theme_minimal() + labs( x = "State", y = "Number of hospitals", title = "Overall rating")
  } else if (data == "Type") {
    ggplot(tab, aes(x=reorder(state,-n))) +
      geom_bar(aes(fill=Type)) +   # Type
      geom_vline(aes(xintercept = which(levels(reorder(state, -n)) %in% "AL")), color = "grey") +
      theme_minimal() + labs( x = "State", y = "Number of hospitals", title = data)
  } else {
    ggplot(tab, aes(x=reorder(state,-n))) +
      geom_bar(aes_string(fill=data)) +   
      scale_fill_manual("", values = c("lightgray","orange","red", "green")) +
      geom_vline(aes(xintercept = which(levels(reorder(state, -n)) %in% "AL"))) +
      theme_minimal() + labs( x = "State", y = "Number of hospitals", title = gsub("_", " ", data)) +
      theme(legend.position = c(0.7, 0.8))
  }

}


ggplot(h2, aes(x=reorder(state,-n))) +
  geom_bar(aes_string(fill=data)) +   
  scale_fill_manual(values = c("lightgray","orange","red", "green")) +
  geom_vline(aes(xintercept = which(levels(reorder(state, -n)) %in% "AL"))) +
  theme_minimal() 





ggplot(h2, aes(x=reorder(state,-n))) +
  geom_bar(aes(fill=Type)) +   # Type
  #scale_fill_manual(values = c("lightgray","#d7191c","#fdae61", "#ffffbf", "#a6d96a", "#1a9641")) + #colors - http://colorbrewer2.org/#type=diverging&scheme=RdYlGn&n=5
  geom_vline(aes(xintercept = which(levels(reorder(state, -n)) %in% "AL")), color = "grey") +
  theme_minimal()

ggplot(h2, aes(x=reorder(state,-n))) +
  geom_bar(aes(fill=Ownership)) +   # Ownership
  #scale_fill_manual(values = c("lightgray","#d7191c","#fdae61", "#ffffbf", "#a6d96a", "#1a9641")) + #colors - http://colorbrewer2.org/#type=diverging&scheme=RdYlGn&n=5
  geom_vline(aes(xintercept = which(levels(reorder(state, -n)) %in% "AL")), color = "grey") +
  theme_minimal()
### End of plot in nominal values


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
