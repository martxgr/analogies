library(tidyverse)

analogies <- read.csv("stimuli/study4_analogies.csv")

demographics <- read.csv("raw/study4_prolific.csv") %>%
  filter(Status == "APPROVED")

qualtrics <- read.csv("raw/study4_qualtrics.csv") %>%
  mutate(id = trimws(id)) %>% 
  filter(id %in% demographics$Participant.id) %>%
  filter(attention == "6")

questions <- distinct(analogies, mech_item, domain) %>%
  mutate(question = paste(mech_item, c(chemical = "chem", biological = "bio", economic = "econ")[domain],
                          sep = "_"))

choices <- qualtrics %>%
  select(ResponseId, id, list, all_of(questions$question)) %>%
  pivot_longer(all_of(questions$question), names_to = "question", values_to = "choice")

orders <- qualtrics %>%
  select(id, all_of(paste0(questions$question, "_DO"))) %>%
  pivot_longer(-id, names_to = "question", values_to = "order") %>%
  mutate(question = str_remove(question, "_DO$"))

study4 <- choices %>%
  left_join(orders, by = c("id", "question")) %>%
  filter(!is.na(choice), !is.na(order)) %>%
  left_join(questions, by = "question") %>%
  mutate(choice = as.numeric(choice),
         mech_on_top = str_starts(order, paste0(question, "_TF")),
         pref = if_else(mech_on_top, 4 - choice, choice - 4)) %>%
  select(id, list, mech_item, domain, mech_on_top, choice, pref)

write.csv(study4, "data/study4.csv")
write.csv(demographics, "data/study4_demographics.csv")
