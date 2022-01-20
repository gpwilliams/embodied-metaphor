# load libraries for data cleaning
library(tidyverse)
library(here)

# read data
full_data <- read_csv(here("data", "02_cleaned", "cleaned_data_r.csv"))

# make by participants data

# tense long format ----

tense_by_participants_long <- full_data %>% 
  group_by(participant, tense) %>% 
  summarise(
    clicked_up_prop = mean(clicked_up),
    clicked_up_sum = sum(clicked_up),
    clicked_right_prop = mean(clicked_right),
    clicked_right_sum = sum(clicked_right),
    n = n(),
    depression_sum = unique(depression_sum),
    anxiety_sum = unique(anxiety_sum),
    stress_sum = unique(stress_sum),
    alexithymia_sum = unique(alexithymia_sum),
    depression_group = unique(depression_group),
    anxiety_group = unique(anxiety_group),
    alexithymia_group = unique(alexithymia_group)
  )

# tense wide format ----

tense_by_participants_wide <- tense_by_participants_long %>% 
  pivot_wider(
    names_from = tense, 
    values_from = c(
      clicked_up_prop, 
      clicked_up_sum, 
      clicked_right_prop,
      clicked_right_sum
    )
  )

# emotion long format ----

emotion_by_participants_long <- full_data %>% 
  group_by(participant, emotion) %>% 
  summarise(
    clicked_up_prop = mean(clicked_up),
    clicked_up_sum = sum(clicked_up),
    clicked_right_prop = mean(clicked_right),
    clicked_right_sum = sum(clicked_right),
    n = n(),
    depression_sum = unique(depression_sum),
    anxiety_sum = unique(anxiety_sum),
    stress_sum = unique(stress_sum),
    alexithymia_sum = unique(alexithymia_sum),
    depression_group = unique(depression_group),
    anxiety_group = unique(anxiety_group),
    alexithymia_group = unique(alexithymia_group)
  )

# emotion wide format ----

emotion_by_participants_wide <- emotion_by_participants_long %>% 
  pivot_wider(
    names_from = emotion, 
    values_from = c(
      clicked_up_prop, 
      clicked_up_sum, 
      clicked_right_prop,
      clicked_right_sum
    )
  )

# save data
write_csv(
  tense_by_participants_long, 
  here("data", "02_cleaned", "by_participants", "tense_long_r.csv")
)
write_csv(
  tense_by_participants_wide, 
  here("data", "02_cleaned", "by_participants", "tense_wide_r.csv")
)

write_csv(
  emotion_by_participants_long, 
  here("data", "02_cleaned", "by_participants", "emotion_long_r.csv")
)
write_csv(
  emotion_by_participants_wide, 
  here("data", "02_cleaned", "by_participants", "emotion_wide_r.csv")
)
