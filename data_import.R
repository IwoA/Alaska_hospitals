## Download and preprocess data about hospitals.
##
## Data about physicians are downloaded via physicians.R script.



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

