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
      menuItem("Home Health", tabName = "home", icon = icon("signal"))
    )  
  ),
  dashboardBody(
    tags$style(type='text/css', '#below {background-color: rgba(255,255,0,0.40); color: red; font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;}'),
    tabItems(
      tabItem(tabName = "kpi",
              fluidRow(
                box(plotOutput("overall"), width = 12, title = "Hospitals")                
              ),
              fluidRow(
                box(plotOutput("home_kpi"), width = 12, title = "Home Health Care") 
              )
              
      ),
      
      
      tabItem(tabName = "overview",
              plotlyOutput("radar_h"),
              verbatimTextOutput("below"),
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
      
      tabItem(tabName = "home",
              plotOutput("home"),
              plotOutput("home1"),
              plotOutput("home2"),
              plotOutput("home3"),
              plotOutput("home4"),    
              plotOutput("home5")

      )
    )
    
  )
  
  
)






# Define server logic required to draw a histogram
server <- function(input, output) {
  
  source("graph_perc.R")
  source("graph.R")
  source("graph_hh.R")
  
  h1 <- read.csv("hospitals.csv")
  hh <- read.csv("home_health.csv") 
  hh <- select(hh, -X) 
  
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
  
  output$home_kpi <- renderPlot({
    tmp <- hh %>% group_by(state) %>% tidyr::gather("answers", "values", 1:5) %>% summarise(avg = mean(values))
    
    third <- levels(reorder(tmp$state, -tmp$avg))[3]
    third <- filter(tmp, state == third)
    
    ggplot(tmp, aes(x=reorder(state, -avg), y = avg)) +
      geom_col(aes(fill = ifelse(state == "AK",1,0))) +
      geom_hline(aes(yintercept = third$avg), color = "grey") +
      #     geom_segment(x = 18, y = 28, xend = 18, yend = third$avg, col = "red") +
      geom_text(data = filter(tmp, state == "AK"), aes(label = round(avg, digits = 0)), vjust = 1.5) +
      geom_text(data = filter(tmp, state == "AK"), aes(label = round(third$avg, digits = 0)),  y = third$avg+1.5) +     
      theme_minimal() + labs( x = "State", y = "%", title = "Average score") +
      guides(fill=FALSE) + theme(plot.title = element_text(colour = "red", size = 15))
  })
  
  output$radar <- renderPlotly({
    radar <- hh %>% filter(state == "AK") %>% select(-state)
    mx <- hh %>% select(-state) %>% sapply(., max)
    
    t <-  c("Rating 9 or 10", "Good communication", "Discussed medicine, pain, etc.", "Professional", "Recommend to friends")
    
    m <- list(
      l = 10,
      r = 10,
      b = 100,
      t = 100,
      pad = 4
    )
    
    plot_ly(
      type = 'scatterpolar',
      fill = 'toself'
    )%>%
      add_trace(
        marker = list(color = "red", opacity = .5),
        fillcolor = list(color = "red", opacity = .5),
        r = t(radar),
        theta = t,
        name = 'Alaska'
      ) %>%
      add_trace(
        marker = list(color = "lightgray", opacity = .5),
        fillcolor = list(color = "lightgray", opacity = .5),
        r = t(mx),
        theta = t,
        name = 'Max'
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,100)
          )
        ),
        title = "Home Health Care. % of positive answers.",
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
    
    hospitals <- hospitals %>% filter(Values == "Same as the national average" & state == "AK")
    
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
        title = "Number of hospitals same as  national average",
        #  autosize = F,
        margin = m,
        showlegend = F
      )
  })
  
  output$below <- renderText("Number of hospitals below the national average is 0!")
  
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
  
  output$home <- renderPlot({
    tmp <- hh %>% group_by(state) %>% tidyr::gather("answers", "values", 1:5) %>% summarise(avg = mean(values))
    
    third <- levels(reorder(tmp$state, -tmp$avg))[3]
    third <- filter(tmp, state == third)
    
    ggplot(tmp, aes(x=reorder(state, -avg), y = avg)) +
      geom_col(aes(fill = ifelse(state == "AK",1,0))) +
      geom_hline(aes(yintercept = third$avg), color = "grey") +
      #     geom_segment(x = 18, y = 28, xend = 18, yend = third$avg, col = "red") +
      geom_text(data = filter(tmp, state == "AK"), aes(label = round(avg, digits = 0)), vjust = 1.5) +
      geom_text(data = filter(tmp, state == "AK"), aes(label = round(third$avg, digits = 0)),  y = third$avg+1.5) +     
      theme_minimal() + labs( x = "State", y = "%", title = "Average score") +
      guides(fill=FALSE) + theme(plot.title = element_text(colour = "red", size = 15))
  })
  
  output$home1 <- renderPlot({
    graph_hh(colnames(hh)[1], data = hh )
  })
  
  output$home2 <- renderPlot({
    graph_hh(colnames(hh)[2], data = hh)
  })
  
  output$home3 <- renderPlot({
    graph_hh(colnames(hh)[3], data = hh )
  })
  
  output$home4 <- renderPlot({
    graph_hh(colnames(hh)[4], data = hh )
  })
  
  output$home5 <- renderPlot({
    graph_hh(colnames(hh)[5], data = hh )
  })
  

  
}


# Run the application 
shinyApp(ui = ui, server = server)

