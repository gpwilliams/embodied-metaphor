library(tidyverse)
library(here)
library(rlang)

# read data
demo_data <- read_csv(here("data", "02_cleaned", "cleaned_data_r.csv")) %>% 
  group_by(participant) %>% 
  summarise(
    gender = unique(gender), 
    age = unique(age),
    depression_group = unique(depression_group), 
    anxiety_group = unique(anxiety_group), 
    alexithymia_group = unique(alexithymia_group)
  ) %>% 
  ungroup()
  
# ages
age_data <- demo_data %>% 
  summarise(
    mean_age = mean(age),
    min_age = min(age),
    max_age = max(age),
    sd_age = sd(age),
    n = n()
  )

# gender counts
gender_data <- demo_data %>% 
  group_by(gender) %>% 
  summarise(n = n())

# clinical groups
vars <- c(quo(anxiety_group), quo(depression_group), quo(alexithymia_group))
clinical_summary <- list()
for(i in seq_along(vars)) {
  clinical_summary[[i]] <- demo_data %>% 
    group_by(!!vars[[i]]) %>% 
    summarise(n = n())
}

quo_name(vars)

# save data ----

write_csv(
  age_data, 
  here("data", "02_cleaned", "demographics", "age_data.csv")
)
write_csv(
  gender_data, 
  here("data", "02_cleaned", "demographics", "gender_data.csv")
)

for(i in seq_along(clinical_summary)) {
  write_csv(
    clinical_summary[[i]], 
    here(
      "data", 
      "02_cleaned",
      "demographics", 
      paste0(
        as_name(vars[[i]]),
        "_count.csv")
    )
  )
}
