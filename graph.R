graph <- function(cat, tab = h){  # helper function plotting graphs for nominal values
  # cat - category of hospitals measure
  # tab - datatable
  
     if (cat == "Overall_rating") {
          ggplot(tab, aes(x=reorder(state,-n))) +
               geom_bar(aes(fill=`Overall_rating`)) +   # Overall rating
               scale_fill_manual("Score", values = c("lightgray", "#d7191c","#fdae61", "#ffffbf", "#a6d96a", "#1a9641")) + #colors - http://colorbrewer2.org/#type=diverging&scheme=RdYlGn&n=5
               geom_vline(aes(xintercept = which(levels(reorder(state, -n)) %in% "AK")), color = "grey") +
               theme_minimal() + labs( x = "State", y = "Number of hospitals") +
               theme(legend.position = c(0.8, 0.7))
     } else if (cat == "Type") {
          ggplot(tab, aes(x=reorder(state,-n))) +
               geom_bar(aes(fill=Type)) +   # Type
               geom_vline(aes(xintercept = which(levels(reorder(state, -n)) %in% "AK")), color = "grey") +
               theme_minimal() + labs( x = "State", y = "Number of hospitals")
     } else {
          ggplot(tab, aes(x=reorder(state,-n))) +
               geom_bar(aes_string(fill=cat)) +   
               scale_fill_manual("Score", values = c("lightgray","orange","red", "green")) +
               geom_vline(aes(xintercept = which(levels(reorder(state, -n)) %in% "AK"))) +
               theme_minimal() + labs( x = "State", y = "Number of hospitals") +
               theme(legend.position = c(0.7, 0.7))
     }
     
}