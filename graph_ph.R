graph_ph <- function (title, data = ph1) {  # helper function plotting graphs for pysician dataset
  # title - category of physicians measure and title of a graph
  # data - datatable
  
  ph1 <- data
  tmp <- filter(ph1, measure_title == title) %>% 
    group_by(state) %>% 
    summarise(performance = mean(measure_performance_rate, na.rm = TRUE)) %>% 
    droplevels(.)
  
  tmp$performance <- coalesce(tmp$performance, 0) #remove of NAs
  
  third <- levels(reorder(tmp$state, -tmp$performance))[3]
  third <- filter(tmp, state == third)
  
  ggplot(tmp, aes(x=reorder(state, -performance), y = performance)) +
    geom_col(aes(fill = ifelse(state == "AK",1,0))) +
    geom_hline(aes(yintercept = third$performance), color = "grey") +
    #     geom_segment(x = 18, y = 28, xend = 18, yend = third$performance, col = "red") +
    geom_text(data = filter(tmp, state == "AK"), aes(label = performance), vjust = 1.5) +
    geom_text(data = filter(tmp, state == "AK"), aes(label = round(third$performance, digits = 0)),  y = third$performance+1.5) +     
    theme_minimal() + labs( x = "State", y = "Measure of performance", title = title) +
    guides(fill=FALSE)}
