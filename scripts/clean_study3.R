library(tidyverse)

phenomena <- read.csv("stimuli/study3_phenomena.csv")

demographics <- read.csv("raw/study3_prolific.csv") %>%
  filter(Status == "APPROVED")

qualtrics <- read.csv("raw/study3_qualtrics.csv") %>%
  mutate(id = trimws(id)) %>% 
  filter(id %in% demographics$Participant.id) %>% 
  mutate(across(c(explanandum, explanans_mech, explanans_psych, rating, attention), as.numeric)) %>%
  filter(!is.na(explanandum)) %>%
  filter(attention == 2)

# GPT's coding
coding <- read.csv("raw/study3_gpt_coding.csv") %>%
  transmute(id = trimws(id), coded_base = if_else(base == "mech", "mechanical", "psychological"))

# do targets and bases
study3 <- qualtrics %>%
  left_join(coding, by = "id") %>%
  mutate(chose_base = if_else(!is.na(explanans_mech), "mechanical", "psychological"),
         target_n = if_else(explanandum > 5, explanandum - 5, explanandum),
         base_n = coalesce(explanans_mech, explanans_psych),
         mech_code = paste0("mech", if_else(chose_base == "mechanical", base_n, target_n)),
         psych_code = paste0("psych", if_else(chose_base == "mechanical", target_n, base_n)),
         analogy = trimws(response)) %>%
  left_join(phenomena %>% select(mech_code = code, mech_item = name), by = "mech_code") %>%
  left_join(phenomena %>% select(psych_code = code, psych_item = name), by = "psych_code") %>%
  select(id, chose_base, mech_item, psych_item, analogy, rating, coded_base)

write.csv(study3, "data/study3.csv")
write.csv(demographics, "data/study3_demographics.csv")
