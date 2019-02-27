# Home Health Care - Patient survey  https://data.medicare.gov/Home-Health-Compare/Home-Health-Care-Patient-survey-HHCAHPS-State-Data/m5jg-jg7i

hh <- read.csv("https://data.medicare.gov/resource/pn26-9qfp.csv")

hh <- hh[,-c(1,7)]

hh <- filter(hh, is.na(percent_of_patients_who_gave_their_home_health_agency_a_rating_of_9_or_10_on_a_scale_from_0_lowest_1) == FALSE)

write.csv(hh, "home_health.csv")