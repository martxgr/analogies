# Study 1 cleaning: the "best" GPT analogies in each direction
library(tidyverse)

trials <- read.csv("stimuli/study1_trials.csv", fileEncoding = "UTF-8")
analogies <- read.csv("stimuli/study1_analogies.csv", fileEncoding = "UTF-8")

# participants approved on Prolific, each given an anonymous id
prolific <- read.csv("raw/study1_prolific.csv", check.names = FALSE, na.strings = c("", "NA"), fileEncoding = "UTF-8-BOM") %>%
  filter(Status == "APPROVED")

ids <- tibble(id = sort(unique(prolific$`Participant id`))) %>%
  mutate(pid = sprintf("P%03d", row_number()))

qualtrics <- read.csv("raw/study1_qualtrics.csv", check.names = FALSE, na.strings = c("", "NA"), fileEncoding = "UTF-8-BOM") %>%
  mutate(id = trimws(id)) %>%                            # a few ids carry stray spaces
  filter(str_detect(ResponseId, "^R_"), DistributionChannel != "preview",
         id %in% ids$id, Finished == "1")

passed <- qualtrics %>% filter(attention == "6")
cat("Approved on Prolific:", nrow(ids), "| survey responses:", nrow(qualtrics),
    "| failed the attention check:", nrow(qualtrics) - nrow(passed), "| analysed:", nrow(passed), "\n")

# each Qualtrics column holds one trial; preference is scored toward the analogy
# with a mechanical base (or, for filler trials, an economic base)
scored_toward <- analogies %>%
  filter(base_domain %in% c("mechanical", "economic")) %>%
  pull(id)

study1 <- passed %>%
  select(id, matches("^\\d+_version\\d+$")) %>%
  pivot_longer(-id, names_to = "response_col", values_to = "choice", values_drop_na = TRUE) %>%
  mutate(choice = as.numeric(choice)) %>%                    # 1 = top much better, 7 = bottom much better
  inner_join(trials, by = "response_col") %>%
  mutate(analogy_a = if_else(top_id %in% scored_toward, top_id, bottom_id),
         analogy_b = if_else(top_id %in% scored_toward, bottom_id, top_id),
         pref = if_else(top_id == analogy_a, 4 - choice, choice - 4)) %>%
  left_join(analogies %>% select(analogy_a = id, comparison), by = "analogy_a") %>%
  left_join(ids, by = "id") %>%
  select(pid, comparison, analogy_a, analogy_b, top_id, choice, pref) %>%
  arrange(pid)

demographics <- prolific %>%
  left_join(ids, by = c("Participant id" = "id")) %>%
  transmute(pid, age = suppressWarnings(as.numeric(Age)), sex = Sex,
            ethnicity = `Ethnicity simplified`, analysed = pid %in% study1$pid) %>%
  arrange(pid)

write.csv(study1, "data/study1.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(demographics, "data/study1_demographics.csv", row.names = FALSE, fileEncoding = "UTF-8")
