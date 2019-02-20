library(ggplot2)
library(dplyr)

h1 <- read.csv("hospitals.csv")
ph1 <- read.csv("physician.csv")

x <- select(h1, state, Overall_rating)

columns <- colnames(x)

df1 <- x %>%
  group_by_at(vars(one_of(columns))) %>%
  summarize(Value = n())
