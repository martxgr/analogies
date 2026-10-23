# Study 4 cleaning: mechanical analogies against chemical, biological, and economic
# ones. Reads raw/ (not shared), writes data/.

library(tidyverse)

analogies <- read.csv("stimuli/study4_analogies.csv", fileEncoding = "UTF-8")

prolific <- read.csv("raw/study4_prolific.csv", check.names = FALSE, na.strings = c("", "NA"), fileEncoding = "UTF-8-BOM") %>%
  filter(Status == "APPROVED")

ids <- tibble(id = sort(unique(prolific$`Participant id`))) %>%
  mutate(pid = sprintf("P%03d", row_number()))

qualtrics <- read.csv("raw/study4_qualtrics.csv", check.names = FALSE, na.strings = c("", "NA"), fileEncoding = "UTF-8-BOM",
                      colClasses = "character") %>%
  mutate(id = trimws(id)) %>%                            # a few ids carry stray spaces
  filter(str_detect(ResponseId, "^R_"), id %in% ids$id)

passed <- qualtrics %>% filter(attention == "6")
cat("Approved on Prolific:", nrow(ids), "| survey responses:", nrow(qualtrics),
    "| failed the attention check:", nrow(qualtrics) - nrow(passed), "| analysed:", nrow(passed), "\n")

# Each question is named <mechanical item>_<domain> (e.g. hinge_bio). Its "_DO" column
# records display order: starting with "<question>_TF" means the analogy with the
# mechanical base ("[target] is like [mechanical]") was shown on top.
questions <- distinct(analogies, mech_item, domain) %>%
  mutate(question = paste(mech_item, c(chemical = "chem", biological = "bio", economic = "econ")[domain],
                          sep = "_"))

choices <- passed %>%
  select(ResponseId, id, list, all_of(questions$question)) %>%
  pivot_longer(all_of(questions$question), names_to = "question", values_to = "choice")

orders <- passed %>%
  select(ResponseId, all_of(paste0(questions$question, "_DO"))) %>%
  pivot_longer(-ResponseId, names_to = "question", values_to = "order") %>%
  mutate(question = str_remove(question, "_DO$"))

study4 <- choices %>%
  left_join(orders, by = c("ResponseId", "question")) %>%
  filter(!is.na(choice), !is.na(order)) %>%
  left_join(questions, by = "question") %>%
  mutate(choice = as.numeric(choice),                       # 1 = top much better, 7 = bottom much better
         mech_on_top = str_starts(order, paste0(question, "_TF")),
         pref = if_else(mech_on_top, 4 - choice, choice - 4)) %>%
  left_join(ids, by = "id") %>%
  select(pid, list, mech_item, domain, mech_on_top, choice, pref) %>%
  arrange(pid)

demographics <- prolific %>%
  left_join(ids, by = c("Participant id" = "id")) %>%
  transmute(pid, age = suppressWarnings(as.numeric(Age)), sex = Sex,
            ethnicity = `Ethnicity simplified`, analysed = pid %in% study4$pid) %>%
  arrange(pid)

write.csv(study4, "data/study4.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(demographics, "data/study4_demographics.csv", row.names = FALSE, fileEncoding = "UTF-8")
