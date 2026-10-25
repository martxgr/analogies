library(tidyverse)

demographics <- read.csv("raw/study1_prolific.csv") %>%
  filter(Status == "APPROVED")

trials <- read.csv("stimuli/study1_trials.csv")
analogies <- read.csv("stimuli/study1_analogies.csv")

qualtrics <- read.csv("raw/study1_qualtrics.csv") %>%
  mutate(id = trimws(id)) %>%
  filter(id %in% demographics$Participant.id) %>%
  filter(attention == "6")

scored_toward <- analogies %>%
  filter(base_domain %in% c("mechanical", "biological")) %>%
  pull(id)

study1 <- qualtrics %>%
  select(id, matches("^X?\\d+_version\\d+$")) %>% 
  rename_with(~ sub("^X", "", .x), matches("^X\\d+")) %>% 
  pivot_longer(-id, names_to = "response_col", values_to = "choice", values_drop_na = TRUE) %>%
  mutate(choice = as.numeric(choice)) %>%
  inner_join(trials, by = "response_col") %>% 
  mutate(analogy_a = if_else(top_id %in% scored_toward, top_id, bottom_id),
         analogy_b = if_else(top_id %in% scored_toward, bottom_id, top_id),
         pref = if_else(top_id == analogy_a, 4 - choice, choice - 4)) %>%
  left_join(analogies %>% select(analogy_a = id, comparison), by = "analogy_a") %>%
  select(id, comparison, analogy_a, analogy_b, top_id, choice, pref)

write.csv(study1, "data/study1.csv")
write.csv(demographics, "data/study1_demographics.csv")
