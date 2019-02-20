#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

h1 <- read.csv("hospitals.csv")
ph1 <- read.csv("physician.csv")

source("graph_perc.R")
source("graph.R")
source("graph_ph.R")

ui <- dashboardPage(
  dashboardHeader(title = "Quality of Healthcare"),
  dashboardSidebar(
  sidebarMenu(
    menuItem("Hospitals", tabName = "hospitals"),
    menuItem("Physicians", tabName = "physicians")
  )  
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "hospitals",
              
              fluidRow(
                box(      selectInput("gov", label = "Ownership",
                                      choices = c("Government - Federal",
                                                  "Government - Hospital District or Authority",
                                                  "Government - Local",
                                                  "Government - State",
                                                  "Physician",
                                                  "Proprietary",
                                                  "Tribal",
                                                  "Voluntary non-profit - Church",
                                                  "Voluntary non-profit - Other",
                                                  "Voluntary non-profit - Private"),
                                      selected = c("Government - State", "Government - Local"), multiple = TRUE)),
                
                box(      selectInput("category", label =  "Category",
                                      choices = c(
                                        "Overall_rating",
                                        "Type", 
                                        "Mortality",
                                        "Safety_of_care" ,
                                        "Readmission" ,
                                        "Patient_experience" ,
                                        "Effectiveness_of_Care",
                                        "Timeliness_of_Care",
                                        "Medical_imaging"),
                                      selected = "Overall_rating"))
              ),
              plotOutput("category_perc"),
              plotOutput("category")
              ),
      
      tabItem(tabName = "physicians",
              plotOutput("physicians"),
              plotOutput("physicians1"),
              plotOutput("physicians2"),
              plotOutput("physicians3"),
              plotOutput("physicians4"),    
              plotOutput("physicians5"),
              plotOutput("physicians6"),
              plotOutput("physicians7"),
              plotOutput("physicians8")
      )
    )
    
  )
  

  )






# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$category_perc <- renderPlot({
     h1$gov <- h1$Ownership %in% as.character(input$gov) #
     
     states <- unique(h1$state) %>% as.character() %>% sort() 
     out <- NULL 
     tmp <- NULL 
     hospitals_gov <- NULL 
     for (s in 1:length(states)) { 
       state <- filter(h1, state == states[s] & gov == TRUE) %>% select(-gov) # !!!!!! true or false
       for (i in 2:length(state)) { 
         tmp <- table(state[,i]) %>% as.data.frame(.) %>% mutate(category = colnames(state)[i]) 
         tmp$Freq <- tmp$Freq/sum(tmp$Freq)
         out <- rbind(out, tmp) 
         
       } 
       out$state <- states[s]  
       if (s == 1) { 
         hospitals <- out 
       } else { 
         hospitals_gov <- rbind(hospitals_gov, out) 
       }  
       out <- NULL 
       tmp <- NULL 
     }  
     
     hospitals_gov$category <- as.factor(hospitals_gov$category) 
     hospitals_gov$state    <- as.factor(hospitals_gov$state) 
     colnames(hospitals_gov)[1] <- "Values"
     
     
     graph_perc(as.character(input$category), h = hospitals_gov)
   })
   
   output$category <- renderPlot({
     
     h1$gov <- h1$Ownership %in% as.character(input$gov)
     h1 <- h1 %>%group_by(state) %>% 
       mutate(n = n())
     
     h1 <- filter(h1, gov == TRUE)
     
     for (i in 5:11) {  
       h1[[i]] <- factor(h1[[i]], levels(h1[[i]])[c(3,4,2,1)])
     }
     h1$Overall_rating <- factor(h1$Overall_rating, levels(h1$Overall_rating)[c(6, 1:5)]) 
     
     
     graph(as.character(input$category), tab = h1)
   })
   
   output$physicians <- renderPlot({
     tmp <- ph1 %>% group_by(state) %>% 
       summarise(performance = mean(measure_performance_rate, na.rm = TRUE)) %>% 
       droplevels(.)

     
     third <- levels(reorder(tmp$state, -tmp$performance))[3]
     third <- filter(tmp, state == third)
     
     ggplot(tmp, aes(x=reorder(state, -performance), y = performance)) +
       geom_col(aes(fill = ifelse(state == "AL",1,0))) +
       geom_hline(aes(yintercept = third$performance), color = "grey") +
       #     geom_segment(x = 18, y = 28, xend = 18, yend = third$performance, col = "red") +
       geom_text(data = filter(tmp, state == "AL"), aes(label = round(performance, digits = 0)), vjust = 1.5) +
       geom_text(data = filter(tmp, state == "AL"), aes(label = round(third$performance, digits = 0)),  y = third$performance+1.5) +     
       theme_minimal() + labs( x = "State", y = "Measure of performance", title = "Average score") +
       guides(fill=FALSE) + theme(plot.title = element_text(colour = "red", size = 15))
   })
   
   output$physicians1 <- renderPlot({
     graph_ph("Attention to patient medicine cost." )
      })
   
   output$physicians2 <- renderPlot({
     graph_ph("Between visit communication." )
   })
   
   output$physicians3 <- renderPlot({
     graph_ph("Clinicians working together for your care." )
   })
   
   output$physicians4 <- renderPlot({
     graph_ph("Courteous and helpful office staff." )
   })
   
   output$physicians5 <- renderPlot({
     graph_ph("Getting timely care, appointments, and information." )
   })
   
   output$physicians6 <- renderPlot({
     graph_ph("Health promotion and education." )
   })
   
   output$physicians7 <- renderPlot({
     graph_ph("How well clinicians communicate." )
   })
   
   output$physicians8 <- renderPlot({
     graph_ph("Patients' rating of clinicians." )
   })

}


# Run the application 
shinyApp(ui = ui, server = server)

