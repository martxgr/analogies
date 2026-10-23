# Study 4 analysis: analogies with a mechanical base vs analogies with a chemical,
# biological, or economic base.
# pref runs from -3 to 3; positive = prefers the analogy with the mechanical base.

library(tidyverse)
library(lmerTest)
library(emmeans)
library(stringi)
library(tidytext)
library(LexOPS)

d <- read.csv("data/study4.csv", fileEncoding = "UTF-8") %>%
  mutate(domain = factor(domain, c("chemical", "biological", "economic")))

analogies <- read.csv("stimuli/study4_analogies.csv", fileEncoding = "UTF-8")

d %>% group_by(domain) %>% summarise(participants = n_distinct(pid), trials = n())

##################
# main analyses
##################

# all three domains together: the overall effect, and whether the domains differ
m_all <- lmer(pref ~ 0 + domain + (1 | pid) + (1 | mech_item) + (1 | mech_item:domain), d)
summary(emmeans(m_all, ~ 1), infer = TRUE)
summary(emmeans(m_all, ~ domain), infer = TRUE)
eff_size(emmeans(m_all, ~ domain), sigma = sigma(m_all), edf = df.residual(m_all))
joint_tests(m_all)

# one model per domain
m_chemical <- lmer(pref ~ 1 + (1 | pid) + (1 | mech_item), filter(d, domain == "chemical"))
summary(m_chemical)
confint(m_chemical)

m_biological <- lmer(pref ~ 1 + (1 | pid) + (1 | mech_item), filter(d, domain == "biological"))
summary(m_biological)
confint(m_biological)

m_economic <- lmer(pref ~ 1 + (1 | pid) + (1 | mech_item), filter(d, domain == "economic"))
summary(m_economic)
confint(m_economic)

##################
# exploratory linguistic controls
##################

# word frequency: mean SUBTLEX-US Zipf value of each analogy's words
freq <- LexOPS::lexops %>%
  transmute(word = str_to_lower(string), zipf = Zipf.SUBTLEX_US) %>%
  filter(!is.na(word), !is.na(zipf)) %>%
  distinct(word, .keep_all = TRUE)

stim <- analogies %>%
  mutate(base = if_else(base_domain == "mechanical", "mech", "other"),
         n_words = stri_count_words(text))

stim <- stim %>%
  left_join(stim %>%
              unnest_tokens(word, text) %>%
              left_join(freq, by = "word") %>%
              group_by(mech_item, domain, base) %>%
              summarise(mean_zipf = mean(zipf, na.rm = TRUE), .groups = "drop"),
            by = c("mech_item", "domain", "base"))

# one row per pairing: mechanical-base minus other-base version
pair_cov <- stim %>%
  select(mech_item, domain, base, n_words, mean_zipf) %>%
  pivot_wider(names_from = base, values_from = c(n_words, mean_zipf)) %>%
  transmute(mech_item, domain = factor(domain, levels(d$domain)),
            length_diff = n_words_mech - n_words_other,
            freq_diff = mean_zipf_mech - mean_zipf_other) %>%
  group_by(domain) %>%
  mutate(length_diff_z = as.numeric(scale(length_diff)),
         freq_diff_z = as.numeric(scale(freq_diff))) %>%
  ungroup()

# main model with both differences as covariates: are the effects still significant?
d_control <- d %>%
  left_join(pair_cov %>% select(mech_item, domain, length_diff_z, freq_diff_z),
            by = c("mech_item", "domain"))

m_control <- lmer(pref ~ 0 + domain + length_diff_z + freq_diff_z +
                    (1 | pid) + (1 | mech_item) + (1 | mech_item:domain), d_control)
summary(m_control)
summary(emmeans(m_control, ~ 1), infer = TRUE)
summary(emmeans(m_control, ~ domain), infer = TRUE)
