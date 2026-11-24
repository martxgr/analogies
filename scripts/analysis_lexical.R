# Supplement S1
library(tidyverse)
library(lmerTest)
library(emmeans)
library(stringi)
library(tidytext)
library(LexOPS)

norms <- lexops %>%
  mutate(word = str_to_lower(string), conc = CNC.Brysbaert, freq = Zipf.SUBTLEX_US) %>%
  select(word, conc, freq) %>%
  distinct(word, .keep_all = TRUE)

# content words
stop <- c(filter(stop_words, lexicon == "snowball")$word, "like")

##################
# phenomena: mechanical phenomenon minus the other phenomenon, for each analogy
##################

phen <- read.csv("stimuli/analogy_phenomena.csv", fileEncoding = "UTF-8") %>%
  pivot_longer(c(target, base), names_to = "role", values_to = "phenomenon") %>%
  mutate(domain = if_else(role == "target", target_domain, base_domain)) %>%
  unnest_tokens(word, phenomenon) %>%
  filter(!word %in% stop) %>%
  mutate(freq = coalesce(norms$freq[match(word, norms$word)], norms$freq[match(sub("s$", "", word), norms$word)]),
         conc = coalesce(norms$conc[match(word, norms$word)], norms$conc[match(sub("s$", "", word), norms$word)])) %>%
  group_by(study, set, item, domain) %>%
  summarise(freq = mean(freq, na.rm = TRUE), conc = mean(conc, na.rm = TRUE)) %>%
  group_by(study, set, item) %>%
  summarise(phen_freq = freq[domain == "mechanical"] - freq[domain != "mechanical"],
            phen_conc = conc[domain == "mechanical"] - conc[domain != "mechanical"])

##################
# Study 1 (and post-test)
##################

text1 <- read.csv("stimuli/study1_analogies.csv", fileEncoding = "UTF-8") %>%
  filter(comparison == "mechanical vs psychological") %>%
  mutate(length = stri_count_words(text)) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop) %>%
  mutate(freq = coalesce(norms$freq[match(word, norms$word)], norms$freq[match(sub("s$", "", word), norms$word)]),
         conc = coalesce(norms$conc[match(word, norms$word)], norms$conc[match(sub("s$", "", word), norms$word)])) %>%
  group_by(id, base_domain, length) %>%
  summarise(freq = mean(freq, na.rm = TRUE), conc = mean(conc, na.rm = TRUE))

phen1 <- phen %>% filter(study == 1)

# each pairing
pairs1 <- read.csv("data/study1.csv") %>%
  filter(comparison == "mechanical vs psychological") %>%
  distinct(mech_base = analogy_a, psych_base = analogy_b) %>%
  mutate(text_length = text1$length[match(mech_base, text1$id)] - text1$length[match(psych_base, text1$id)],
         text_freq = text1$freq[match(mech_base, text1$id)] - text1$freq[match(psych_base, text1$id)],
         text_conc = text1$conc[match(mech_base, text1$id)] - text1$conc[match(psych_base, text1$id)],
         phen_freq = (phen1$phen_freq[match(mech_base, phen1$item)] + phen1$phen_freq[match(psych_base, phen1$item)]) / 2,
         phen_conc = (phen1$phen_conc[match(mech_base, phen1$item)] + phen1$phen_conc[match(psych_base, phen1$item)]) / 2)

d1 <- read.csv("data/study1.csv") %>%
  filter(comparison == "mechanical vs psychological") %>%
  rename(mech_base = analogy_a, psych_base = analogy_b) %>%
  left_join(pairs1, by = c("mech_base", "psych_base"))

d1p <- read.csv("data/study1_posttest.csv") %>%
  filter(comparison == "mechanical vs psychological") %>%
  rename(mech_base = analogy_a, psych_base = analogy_b) %>%
  left_join(pairs1, by = c("mech_base", "psych_base"))

# do the two sides differ?
t.test(length ~ base_domain, text1)
t.test(freq ~ base_domain, text1)
t.test(conc ~ base_domain, text1)
t.test(phen1$phen_freq)
t.test(phen1$phen_conc)

# controls
m1_length <- lmer(pref ~ 1 + text_length + (1 | id) + (1 | mech_base) + (1 | psych_base), d1)
summary(m1_length)
confint(m1_length)

m1_freq <- lmer(pref ~ 1 + text_freq + (1 | id) + (1 | mech_base) + (1 | psych_base), d1)
summary(m1_freq)
confint(m1_freq)

m1_conc <- lmer(pref ~ 1 + text_conc + (1 | id) + (1 | mech_base) + (1 | psych_base), d1)
summary(m1_conc)
confint(m1_conc)

m1_phen_freq <- lmer(pref ~ 1 + phen_freq + (1 | id) + (1 | mech_base) + (1 | psych_base), d1)
summary(m1_phen_freq)
confint(m1_phen_freq)

m1_phen_conc <- lmer(pref ~ 1 + phen_conc + (1 | id) + (1 | mech_base) + (1 | psych_base), d1)
summary(m1_phen_conc)
confint(m1_phen_conc)

# controls for post-test
m1p_length <- lmer(pref ~ 1 + text_length + (1 | id) + (1 | mech_base) + (1 | psych_base), d1p)
summary(m1p_length)
confint(m1p_length)

m1p_freq <- lmer(pref ~ 1 + text_freq + (1 | id) + (1 | mech_base) + (1 | psych_base), d1p)
summary(m1p_freq)
confint(m1p_freq)

m1p_conc <- lmer(pref ~ 1 + text_conc + (1 | id) + (1 | mech_base) + (1 | psych_base), d1p)
summary(m1p_conc)
confint(m1p_conc)

m1p_phen_freq <- lmer(pref ~ 1 + phen_freq + (1 | id) + (1 | mech_base) + (1 | psych_base), d1p)
summary(m1p_phen_freq)
confint(m1p_phen_freq)

m1p_phen_conc <- lmer(pref ~ 1 + phen_conc + (1 | id) + (1 | mech_base) + (1 | psych_base), d1p)
summary(m1p_phen_conc)
confint(m1p_phen_conc)

##################
# Study 2
##################

pairs2 <- read.csv("stimuli/study2_analogies.csv", fileEncoding = "UTF-8") %>%
  mutate(length = stri_count_words(text)) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop) %>%
  mutate(freq = coalesce(norms$freq[match(word, norms$word)], norms$freq[match(sub("s$", "", word), norms$word)]),
         conc = coalesce(norms$conc[match(word, norms$word)], norms$conc[match(sub("s$", "", word), norms$word)])) %>%
  group_by(llm, pair, base_domain, length) %>%
  summarise(freq = mean(freq, na.rm = TRUE), conc = mean(conc, na.rm = TRUE)) %>%
  group_by(llm, pair) %>%
  summarise(text_length = length[base_domain == "mechanical"] - length[base_domain != "mechanical"],
            text_freq = freq[base_domain == "mechanical"] - freq[base_domain != "mechanical"],
            text_conc = conc[base_domain == "mechanical"] - conc[base_domain != "mechanical"]) %>%
  left_join(phen %>% filter(study == 2) %>% ungroup() %>% select(llm = set, pair = item, phen_freq, phen_conc),
            by = c("llm", "pair")) %>%
  mutate(llm = factor(llm, c("GPT", "Claude", "Gemini")))

d2 <- read.csv("data/study2.csv") %>%
  mutate(llm = factor(llm, c("GPT", "Claude", "Gemini"))) %>%
  left_join(pairs2, by = c("llm", "pair"))

# do the two sides differ?
summary(emmeans(lm(text_length ~ llm, pairs2), ~ 1), infer = TRUE)
summary(emmeans(lm(text_freq ~ llm, pairs2), ~ 1), infer = TRUE)
summary(emmeans(lm(text_conc ~ llm, pairs2), ~ 1), infer = TRUE)
summary(emmeans(lm(phen_freq ~ llm, pairs2), ~ 1), infer = TRUE)
summary(emmeans(lm(phen_conc ~ llm, pairs2), ~ 1), infer = TRUE)

# controls
m2_length <- lmer(pref ~ 0 + llm + text_length + (1 | id) + (1 | pair) + (1 | pair:llm), d2)
summary(m2_length)
confint(m2_length)
summary(emmeans(m2_length, ~ 1, at = list(text_length = 0)), infer = TRUE)
summary(emmeans(m2_length, ~ llm, at = list(text_length = 0)), infer = TRUE)

m2_freq <- lmer(pref ~ 0 + llm + text_freq + (1 | id) + (1 | pair) + (1 | pair:llm), d2)
summary(m2_freq)
confint(m2_freq)
summary(emmeans(m2_freq, ~ 1, at = list(text_freq = 0)), infer = TRUE)
summary(emmeans(m2_freq, ~ llm, at = list(text_freq = 0)), infer = TRUE)

m2_conc <- lmer(pref ~ 0 + llm + text_conc + (1 | id) + (1 | pair) + (1 | pair:llm), d2)
summary(m2_conc)
confint(m2_conc)
summary(emmeans(m2_conc, ~ 1, at = list(text_conc = 0)), infer = TRUE)
summary(emmeans(m2_conc, ~ llm, at = list(text_conc = 0)), infer = TRUE)

m2_phen_freq <- lmer(pref ~ 0 + llm + phen_freq + (1 | id) + (1 | pair) + (1 | pair:llm), d2)
summary(m2_phen_freq)
confint(m2_phen_freq)
summary(emmeans(m2_phen_freq, ~ 1, at = list(phen_freq = 0)), infer = TRUE)
summary(emmeans(m2_phen_freq, ~ llm, at = list(phen_freq = 0)), infer = TRUE)

m2_phen_conc <- lmer(pref ~ 0 + llm + phen_conc + (1 | id) + (1 | pair) + (1 | pair:llm), d2)
summary(m2_phen_conc)
confint(m2_phen_conc)
summary(emmeans(m2_phen_conc, ~ 1, at = list(phen_conc = 0)), infer = TRUE)
summary(emmeans(m2_phen_conc, ~ llm, at = list(phen_conc = 0)), infer = TRUE)

##################
# Study 4
##################

pairs4 <- read.csv("stimuli/study4_analogies.csv", fileEncoding = "UTF-8") %>%
  mutate(length = stri_count_words(text)) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop) %>%
  mutate(freq = coalesce(norms$freq[match(word, norms$word)], norms$freq[match(sub("s$", "", word), norms$word)]),
         conc = coalesce(norms$conc[match(word, norms$word)], norms$conc[match(sub("s$", "", word), norms$word)])) %>%
  group_by(mech_item, domain, base_domain, length) %>%
  summarise(freq = mean(freq, na.rm = TRUE), conc = mean(conc, na.rm = TRUE)) %>%
  group_by(mech_item, domain) %>%
  summarise(text_length = length[base_domain == "mechanical"] - length[base_domain != "mechanical"],
            text_freq = freq[base_domain == "mechanical"] - freq[base_domain != "mechanical"],
            text_conc = conc[base_domain == "mechanical"] - conc[base_domain != "mechanical"]) %>%
  left_join(phen %>% filter(study == 4) %>% ungroup() %>% select(domain = set, mech_item = item, phen_freq, phen_conc),
            by = c("mech_item", "domain")) %>%
  mutate(domain = factor(domain, c("chemical", "biological", "economic")))

d4 <- read.csv("data/study4.csv") %>%
  mutate(domain = factor(domain, c("chemical", "biological", "economic"))) %>%
  left_join(pairs4, by = c("mech_item", "domain"))

# do the two sides differ?
summary(emmeans(lmer(text_length ~ domain + (1 | mech_item), pairs4), ~ 1), infer = TRUE)
summary(emmeans(lmer(text_freq ~ domain + (1 | mech_item), pairs4), ~ 1), infer = TRUE)
summary(emmeans(lmer(text_conc ~ domain + (1 | mech_item), pairs4), ~ 1), infer = TRUE)
summary(emmeans(lmer(phen_freq ~ domain + (1 | mech_item), pairs4), ~ 1), infer = TRUE)
summary(emmeans(lmer(phen_conc ~ domain + (1 | mech_item), pairs4), ~ 1), infer = TRUE)

# controls
m4_length <- lmer(pref ~ 0 + domain + text_length + (1 | id) + (1 | mech_item) + (1 | mech_item:domain), d4)
summary(m4_length)
confint(m4_length)
summary(emmeans(m4_length, ~ 1, at = list(text_length = 0)), infer = TRUE)
summary(emmeans(m4_length, ~ domain, at = list(text_length = 0)), infer = TRUE)

m4_freq <- lmer(pref ~ 0 + domain + text_freq + (1 | id) + (1 | mech_item) + (1 | mech_item:domain), d4)
summary(m4_freq)
confint(m4_freq)
summary(emmeans(m4_freq, ~ 1, at = list(text_freq = 0)), infer = TRUE)
summary(emmeans(m4_freq, ~ domain, at = list(text_freq = 0)), infer = TRUE)

m4_conc <- lmer(pref ~ 0 + domain + text_conc + (1 | id) + (1 | mech_item) + (1 | mech_item:domain), d4)
summary(m4_conc)
confint(m4_conc)
summary(emmeans(m4_conc, ~ 1, at = list(text_conc = 0)), infer = TRUE)
summary(emmeans(m4_conc, ~ domain, at = list(text_conc = 0)), infer = TRUE)

m4_phen_freq <- lmer(pref ~ 0 + domain + phen_freq + (1 | id) + (1 | mech_item) + (1 | mech_item:domain), d4)
summary(m4_phen_freq)
confint(m4_phen_freq)
summary(emmeans(m4_phen_freq, ~ 1, at = list(phen_freq = 0)), infer = TRUE)
summary(emmeans(m4_phen_freq, ~ domain, at = list(phen_freq = 0)), infer = TRUE)

m4_phen_conc <- lmer(pref ~ 0 + domain + phen_conc + (1 | id) + (1 | mech_item) + (1 | mech_item:domain), d4)
summary(m4_phen_conc)
confint(m4_phen_conc)
summary(emmeans(m4_phen_conc, ~ 1, at = list(phen_conc = 0)), infer = TRUE)
summary(emmeans(m4_phen_conc, ~ domain, at = list(phen_conc = 0)), infer = TRUE)
