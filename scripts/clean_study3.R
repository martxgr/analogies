# Study 3 cleaning: participants chose a target and a base, wrote their own analogy,
# and rated it. GPT later coded which domain each written analogy actually used as
# its base. Reads raw/ (not shared), writes data/.

library(tidyverse)

phenomena <- read.csv("stimuli/study3_phenomena.csv", fileEncoding = "UTF-8")

prolific <- read.csv("raw/study3_prolific.csv", check.names = FALSE, na.strings = c("", "NA"), fileEncoding = "UTF-8-BOM") %>%
  filter(Status == "APPROVED")

ids <- tibble(id = sort(unique(prolific$`Participant id`))) %>%
  mutate(pid = sprintf("P%03d", row_number()))

qualtrics <- read.csv("raw/study3_qualtrics.csv", check.names = FALSE, na.strings = c("", "NA"), fileEncoding = "UTF-8-BOM") %>%
  mutate(id = trimws(id)) %>%                            # a few ids carry stray spaces
  filter(str_detect(ResponseId, "^R_"), id %in% ids$id) %>%
  mutate(across(c(explanandum, explanans_mech, explanans_psych, rating, attention), as.numeric)) %>%
  filter(!is.na(explanandum))

passed <- qualtrics %>% filter(attention == 2)
cat("Approved on Prolific:", nrow(ids), "| survey responses:", nrow(qualtrics),
    "| failed the attention check:", nrow(qualtrics) - nrow(passed), "| analysed:", nrow(passed), "\n")


# GPT's coding of each written analogy: "mech" or "psych" base
# (trimws: one id in the coding file carries a trailing space)
coding <- read.csv("raw/study3_gpt_coding.csv", check.names = FALSE, na.strings = c("", "NA"), fileEncoding = "UTF-8-BOM") %>%
  transmute(id = trimws(id), coded_base = if_else(base == "mech", "mechanical", "psychological"))

# Target codes 1-5 are the mechanical phenomena and 6-10 the psychological ones;
# the base was picked from whichever list was not the target's.
study3 <- passed %>%
  left_join(coding, by = "id") %>%
  left_join(ids, by = "id") %>%
  mutate(chose_base = if_else(!is.na(explanans_mech), "mechanical", "psychological"),
         target_n = if_else(explanandum > 5, explanandum - 5, explanandum),
         base_n = coalesce(explanans_mech, explanans_psych),
         mech_code = paste0("mech", if_else(chose_base == "mechanical", base_n, target_n)),
         psych_code = paste0("psych", if_else(chose_base == "mechanical", target_n, base_n)),
         analogy = trimws(response)) %>%
  left_join(phenomena %>% select(mech_code = code, mech_item = name), by = "mech_code") %>%
  left_join(phenomena %>% select(psych_code = code, psych_item = name), by = "psych_code") %>%
  select(pid, chose_base, mech_item, psych_item, analogy, rating, coded_base) %>%
  arrange(pid)

cat("Analogies without a GPT code:", sum(is.na(study3$coded_base)), "\n")

demographics <- prolific %>%
  left_join(ids, by = c("Participant id" = "id")) %>%
  transmute(pid, age = suppressWarnings(as.numeric(Age)), sex = Sex,
            ethnicity = `Ethnicity simplified`, analysed = pid %in% study3$pid) %>%
  arrange(pid)

write.csv(study3, "data/study3.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(demographics, "data/study3_demographics.csv", row.names = FALSE, fileEncoding = "UTF-8")
