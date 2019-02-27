library(dplyr)
library(ggplot2)


##################

graph_hh <- function(title, data = hh) {
  
  hh1 <- hh %>% select(state, title)
  colnames(hh1) <- c("state", "V1")
  third <- levels(reorder(hh1$state, -hh1$V1))[3]
  third <- filter(hh1, state == third)
  
  
  
  ggplot(hh1, aes(x=reorder(state, -V1), y = V1)) +
    geom_col(aes(fill = ifelse(state == "AK",1,0))) +
    geom_hline(aes(yintercept = third$V1), color = "grey") +
    #     geom_segment(x = 18, y = 28, xend = 18, yend = third$performance, col = "red") +
    geom_text(data = filter(hh1, state == "AK"), aes(label = V1), vjust = 1.5) +
    geom_text(data = filter(hh1, state == "AK"), aes(label = round(third$V1, digits = 0)),  y = third$V1+1.5) +
    theme_minimal() + labs( x = "State", y = "%", title = gsub("_", " ", title)) +
    guides(fill=FALSE)
  
}