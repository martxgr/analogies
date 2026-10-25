library(tidyverse)

demographics <- read.csv("raw/study2_prolific.csv") %>%
  filter(Status == "APPROVED")

questions <- tribble(~pair, ~order_col,
                     "psych_ld", "lever.decision_DO",
                     "psych_pa", "pulley.attention_DO",
                     "psych_be", "bicycle.emotion_DO",
                     "psych_gh", "gear.habit_DO",
                     "psych_ss", "scissor.social_DO",
                     "psych_sm", "spring.memory_DO")

surveys <- c(GPT = "raw/study2_qualtrics_gpt.csv",
             Claude = "raw/study2_qualtrics_claude.csv",
             Gemini = "raw/study2_qualtrics_gemini.csv")

qualtrics <- map_dfr(surveys, ~ read.csv(.x), .id = "llm") %>%
  mutate(id = trimws(id)) %>%
  filter(id %in% demographics$Participant.id) %>%
  filter(attention == "6")

choices <- qualtrics %>%
  select(ResponseId, llm, id, all_of(questions$pair)) %>%
  pivot_longer(all_of(questions$pair), names_to = "pair", values_to = "choice")

orders <- qualtrics %>%
  select(ResponseId, all_of(questions$order_col)) %>%
  pivot_longer(-ResponseId, names_to = "order_col", values_to = "order") %>%
  left_join(questions, by = "order_col") %>%
  select(ResponseId, pair, order)

study2 <- choices %>%
  left_join(orders, by = c("ResponseId", "pair")) %>%
  filter(!is.na(choice), !is.na(order)) %>%
  mutate(choice = as.numeric(choice),
         mech_on_top = str_detect(order, "^top\\|bottom"),
         pref = if_else(mech_on_top, 4 - choice, choice - 4)) %>%
  select(id, llm, pair, mech_on_top, choice, pref)

write.csv(study2, "data/study2.csv")
write.csv(demographics, "data/study2_demographics.csv")
