
# load libraries for data cleaning
library(tidyverse)
library(here)
library(janitor)

# load all functions
r_function_list <- list.files(
  path = here("R", "00_functions"), 
  pattern = "R$",
  full.names = TRUE
)
purrr::walk(r_function_list, source)

# experiment data
experiment <- map_files_to_list(
  here("data", "01_raw", "pavlovia"), 
  readr::read_csv
  ) %>% 
  bind_rows() %>%  
  mutate(trials_n = coalesce(trials.thisN, trials_2.thisN)) %>% 
  select(
    participant,
    list,
    item,
    trials_n,
    emotion:sentence,
    correct,
    target_position,
    distractor_position,
    mouse_x:mouse.y,
    mouse.time:mouse.clicked_name,
    target_x:distract_y,
    date,
    frameRate
  ) %>% 
  rename(
    mouse_x_clicked = mouse.x,
    mouse_y_clicked = mouse.y,
    mouse_time_clicked = mouse.time
  ) %>% 
  janitor::clean_names() %>% 
  mutate(
    emotion = case_when(
      emotion == "depress" ~ "depression",
      TRUE ~ emotion
    ),
    clicked_up = case_when(
      mouse_y_clicked > 0 ~ 1,
      TRUE ~ 0
    ),
    clicked_right = case_when(
      mouse_x_clicked > 0 ~ 1,
      TRUE ~ 0
    ),
    mouse_time_clicked = mouse_time_clicked * 1000
  ) %>% 
  filter(!is.na(trials_n)) %>% 
  type_convert()

# read in and simplify consent
consent <- read_csv(here(
  "data", "01_raw", "qualtrics", "consent_data.csv"
  )) %>% 
  slice(3:n()) %>% 
  type_convert() %>% 
  janitor::clean_names() %>% 
  select(participant, progress, q5:q8) %>% 
  rename(
    consent = q5,
    age = q7,
    gender = q8
  ) %>% 
  mutate(
    gender = factor(
      gender,
      levels = c(1, 2, 3, 4),
      labels = c("male", "female", "non_binary", "prefer_not_to_say")
    )
  )

# get questionnaire data
questionnaire <- read_csv(here(
  "data", "01_raw", "qualtrics", "questionnaire_data.csv"
  )) %>% 
  slice(3:n()) %>% 
  type_convert() %>% 
  janitor::clean_names() %>% 
  group_by(participant) %>% 
  rowwise() %>% 
  summarise(
    depression_sum = sum(c_across(das_1:das_7)),
    anxiety_sum = sum(c_across(das_8:das_14)),
    stress_sum = sum(c_across(das_15:das_21)),
    alexithymia_sum = sum(c_across(toronto_1:toronto_20))
  )

# merge data, filter to full consent, add anxiety and depression groups
full_data <- left_join(experiment, consent, by = "participant") %>% 
  left_join(., questionnaire, by = "participant") %>% 
  filter(consent == 1234567) %>% 
  mutate(
    anxiety_group = case_when(
      is.na(anxiety_sum) ~ NA_character_,
      anxiety_sum >= 10 ~ "high",
      TRUE ~ "low"
    ),
    depression_group = case_when(
      is.na(depression_sum) ~ NA_character_,
      depression_sum >= 13 ~ "high",
      TRUE ~ "low"
    ),
    alexithymia_group = case_when(
      is.na(alexithymia_sum) ~ NA_character_,
      alexithymia_sum <= 51 ~ "no_alexithymia",
      alexithymia_sum > 51 & alexithymia_sum <= 60 ~ "possible_alexithymia",
      alexithymia_sum > 60 ~ "alexithymia"
    )
  )

# save data
write_csv(full_data, here("data", "02_cleaned", "cleaned_data_r.csv"))
