# load libraries for data cleaning
library(tidyverse)
library(here)

# read data
tense_by_participants_long <- read_csv(here(
  "data", "02_cleaned", "by_participants", "tense_long_r.csv"
))
emotion_by_participants_long <- read_csv(here(
  "data", "02_cleaned", "by_participants", "emotion_long_r.csv"
))

# make descriptives

# by tense
tense_by_participants_long %>% 
  group_by(tense) %>% 
  drop_na() %>% 
  summarise(
    clicked_up_prop = mean(clicked_up_prop),
    clicked_right_prop = mean(clicked_right_prop),
    depression_mean = mean(depression_sum),
    anxiety_mean = mean(anxiety_sum),
    stress_mean = mean(stress_sum),
    alexithymia_mean = mean(alexithymia_sum),
    n = length(unique(participant))
  )

# by emotion
emotion_by_participants_long %>% 
  group_by(emotion) %>% 
  drop_na() %>% 
  summarise(
    clicked_up_prop = mean(clicked_up_prop),
    clicked_right_prop = mean(clicked_right_prop),
    depression_mean = mean(depression_sum),
    anxiety_mean = mean(anxiety_sum),
    stress_mean = mean(stress_sum),
    alexithymia_mean = mean(alexithymia_sum),
    n = length(unique(participant))
  )