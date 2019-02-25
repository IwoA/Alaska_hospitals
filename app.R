#### Alaska healthcare dashboard

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)

ui <- dashboardPage(
  dashboardHeader(title = "Quality of Healthcare"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("KPIs", tabName = "kpi", icon = icon("dashboard")),
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Hospitals", tabName = "hospitals", icon = icon("chart-bar")),
      menuItem("Physicians", tabName = "physicians", icon = icon("signal"))
    )  
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "kpi",
              fluidRow(
                box(plotOutput("overall"), width = 12, title = "Hospitals")                
              ),
              fluidRow(
                box(plotOutput("physicians_kpi"), width = 12, title = "Physicians") 
              )
              
      ),
      
      
      tabItem(tabName = "overview",
              plotlyOutput("radar_h"),
              plotlyOutput("radar")
              
      ),
      
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
  
  source("graph_perc.R")
  source("graph.R")
  source("graph_ph.R")
  
  h1 <- read.csv("hospitals.csv")
  ph1 <- read.csv("physician.csv")
  
  output$overall <- renderPlot({
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
    
    
    graph_perc("Overall_rating", data = hospitals)
  })
  
  output$physicians_kpi <- renderPlot({
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
      theme_minimal() + labs( x = "State", y = "Measure of performance", title = "Physicians Average Score") +
      guides(fill=FALSE) 
  })
  
  output$radar <- renderPlotly({
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
        title = "Physicians' assessment",
        #   autosize = F,
        margin = m
      )
  })
  
  output$radar_h <- renderPlotly({
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
        #  autosize = F,
        margin = m,
        showlegend = F
      )
  })
  
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
        hospitals_gov <- out 
      } else { 
        hospitals_gov <- rbind(hospitals_gov, out) 
      }  
      out <- NULL 
      tmp <- NULL 
    }  
    
    hospitals_gov$category <- as.factor(hospitals_gov$category) 
    hospitals_gov$state    <- as.factor(hospitals_gov$state) 
    colnames(hospitals_gov)[1] <- "Values"
    
    
    graph_perc(as.character(input$category), data = hospitals_gov)
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
    graph_ph("Attention to patient medicine cost.", data = ph1 )
  })
  
  output$physicians2 <- renderPlot({
    graph_ph("Between visit communication.", data = ph1)
  })
  
  output$physicians3 <- renderPlot({
    graph_ph("Clinicians working together for your care.", data = ph1 )
  })
  
  output$physicians4 <- renderPlot({
    graph_ph("Courteous and helpful office staff.", data = ph1 )
  })
  
  output$physicians5 <- renderPlot({
    graph_ph("Getting timely care, appointments, and information.", data = ph1 )
  })
  
  output$physicians6 <- renderPlot({
    graph_ph("Health promotion and education.", data = ph1 )
  })
  
  output$physicians7 <- renderPlot({
    graph_ph("How well clinicians communicate.", data = ph1 )
  })
  
  output$physicians8 <- renderPlot({
    graph_ph("Patients' rating of clinicians.", data = ph1 )
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)

