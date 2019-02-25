graph_perc <- function(cat, data = hospitals) { # helper function plotting graphs for percentage values
  # cat - category of hospitals measure
  # h - datatable
  
     hospitals <- data  # "starting" the argument. Otherwise next line would file due to recursive issues.
     tmp <- filter(hospitals, category == cat)
     tmp <- droplevels(tmp)
     
     if (cat =="Overall_rating") {
          tmp$Values <- factor(tmp$Values, levels(tmp$Values)[c(6, 1:5)])  #correct order of levels
          #  tmp[tmp$Values=="1",2] <- tmp[tmp$Values=="1",2] *-1
          tmp <- tmp %>% group_by(state) %>% 
               mutate(above = if_else(Values == "5", Freq, 0)) %>% 
               arrange(state)
          
          ggplot(tmp, aes(x=reorder(state, -above), y = Freq*100)) +
               geom_col(aes(fill = Values)) +
               scale_fill_manual("Score", values = c("lightgray", "#d7191c","#fdae61", "#ffffbf", "#a6d96a", "#1a9641")) + #colors - http://colorbrewer2.org/#type=diverging&scheme=RdYlGn&n=5
               geom_vline(aes(xintercept = which(levels(reorder(state, -above)) %in% "AL"))) +
               theme_minimal()+ labs( x = "State", y = "% of hospitals", title = "Overall rating")
          
     } else if (cat == "Type") {
          ggplot(tmp, aes(x=state, y = Freq*100)) +
               geom_col(aes(fill = Values)) +
               # scale_fill_manual("") +
               geom_vline(aes(xintercept = which(levels(state) %in% "AL")), color = "grey") +
               theme_minimal()+ labs( x = "State", y = "% of hospitals", title = "Type")
          
     } else {
          tmp$Values <- factor(tmp$Values, levels(tmp$Values)[c(1, 4, 3, 2)])  #correct order of levels
          tmp[tmp$Values=="Below the national average",2] <- tmp[tmp$Values=="Below the national average",2] *-1
          tmp <- tmp %>% group_by(state) %>% 
               mutate(above = if_else(Values == "Above the national average", Freq, 0)) %>% 
               arrange(state)
          
          ggplot(tmp, aes(x=reorder(state, -above), y = Freq*100)) +
               geom_col(aes(fill = Values)) +
               scale_fill_manual("Score", values = c("lightgray","orange","red", "green")) +
               geom_vline(aes(xintercept = which(levels(reorder(state, -above)) %in% "AL"))) +
               theme_minimal()+ labs( x = "State", y = "% of hospitals", title = gsub("_", " ", cat)) +
                  theme(legend.position = "bottom")
     }
}
########################## end of graph_perc function
