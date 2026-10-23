# Study 2 cleaning: random mechanical-psychological pairings, analogies written by
# GPT, Claude, and Gemini (one survey per LLM). Reads raw/ (not shared), writes data/.

library(tidyverse)

prolific <- read.csv("raw/study2_prolific.csv", check.names = FALSE, na.strings = c("", "NA"), fileEncoding = "UTF-8-BOM") %>%
  filter(Status == "APPROVED")

ids <- tibble(id = sort(unique(prolific$`Participant id`))) %>%
  mutate(pid = sprintf("P%03d", row_number()))

# Each pairing is one Qualtrics question. Its "_DO" column records display order:
# "top|bottom" means the mechanical-base analogy was shown on top.
questions <- tribble(
  ~pair,      ~order_col,
  "psych_ld", "lever-decision_DO",
  "psych_pa", "pulley-attention_DO",
  "psych_be", "bicycle-emotion_DO",
  "psych_gh", "gear-habit_DO",
  "psych_ss", "scissor-social_DO",
  "psych_sm", "spring-memory_DO"
)

surveys <- c(GPT = "raw/study2_qualtrics_gpt.csv",
             Claude = "raw/study2_qualtrics_claude.csv",
             Gemini = "raw/study2_qualtrics_gemini.csv")

qualtrics <- map_dfr(surveys, ~ read.csv(.x, check.names = FALSE, na.strings = c("", "NA"), fileEncoding = "UTF-8-BOM", colClasses = "character"),
                     .id = "llm") %>%
  mutate(id = trimws(id)) %>%                            # a few ids carry stray spaces
  filter(str_detect(ResponseId, "^R_"), id %in% ids$id)

passed <- qualtrics %>% filter(attention == "6")
cat("Approved on Prolific:", nrow(ids), "| survey responses:", nrow(qualtrics),
    "| failed the attention check:", nrow(qualtrics) - nrow(passed), "| analysed:", nrow(passed), "\n")

choices <- passed %>%
  select(ResponseId, llm, id, all_of(questions$pair)) %>%
  pivot_longer(all_of(questions$pair), names_to = "pair", values_to = "choice")

orders <- passed %>%
  select(ResponseId, all_of(questions$order_col)) %>%
  pivot_longer(-ResponseId, names_to = "order_col", values_to = "order") %>%
  left_join(questions, by = "order_col")

study2 <- choices %>%
  left_join(orders %>% select(ResponseId, pair, order), by = c("ResponseId", "pair")) %>%
  filter(!is.na(choice), !is.na(order)) %>%
  mutate(choice = as.numeric(choice),                       # 1 = top much better, 7 = bottom much better
         mech_on_top = str_detect(order, "^top\\|bottom"),
         pref = if_else(mech_on_top, 4 - choice, choice - 4)) %>%
  left_join(ids, by = "id") %>%
  select(pid, llm, pair, mech_on_top, choice, pref) %>%
  arrange(llm, pid)

demographics <- prolific %>%
  left_join(ids, by = c("Participant id" = "id")) %>%
  transmute(pid, age = suppressWarnings(as.numeric(Age)), sex = Sex,
            ethnicity = `Ethnicity simplified`, analysed = pid %in% study2$pid) %>%
  arrange(pid)

write.csv(study2, "data/study2.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(demographics, "data/study2_demographics.csv", row.names = FALSE, fileEncoding = "UTF-8")
