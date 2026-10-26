library(tidyverse)
library(lmerTest)
library(emmeans)
library(stringi)
library(tidytext)
library(LexOPS)

d <- read.csv("data/study4.csv") %>%
  mutate(domain = factor(domain, c("chemical", "biological", "economic")))

analogies <- read.csv("stimuli/study4_analogies.csv")

# main analyses

# one model per domain
m_chemical <- lmer(pref ~ 1 + (1 | id) + (1 | mech_item), filter(d, domain == "chemical"))
summary(m_chemical)
confint(m_chemical)

m_biological <- lmer(pref ~ 1 + (1 | id) + (1 | mech_item), filter(d, domain == "biological"))
summary(m_biological)
confint(m_biological)

m_economic <- lmer(pref ~ 1 + (1 | id) + (1 | mech_item), filter(d, domain == "economic"))
summary(m_economic)
confint(m_economic)

# all three
m_all <- lmer(pref ~ 0 + domain + (1 | id) + (1 | mech_item) + (1 | mech_item:domain), d)
# overall
summary(emmeans(m_all, ~ 1), infer = TRUE)
# simple slopes
summary(emmeans(m_all, ~ domain), infer = TRUE)
# effect sizes
eff_size(emmeans(m_all, ~ domain), sigma = sigma(m_all), edf = df.residual(m_all))
# do the domains differ?
joint_tests(m_all)

# length and word frequency
freq <- lexops %>%
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

# do the two versions differ in length or frequency?
m_length <- lmer(length_diff ~ domain + (1 | mech_item), pair_cov)
summary(emmeans(m_length, ~ 1), infer = TRUE)
m_freq <- lmer(freq_diff ~ domain + (1 | mech_item), pair_cov)
summary(emmeans(m_freq, ~ 1), infer = TRUE)

# control model
d_control <- d %>%
  left_join(pair_cov %>% select(mech_item, domain, length_diff_z, freq_diff_z),
            by = c("mech_item", "domain"))

m_control <- lmer(pref ~ 0 + domain + length_diff_z + freq_diff_z +
                    (1 | id) + (1 | mech_item) + (1 | mech_item:domain), d_control)
summary(m_control)
summary(emmeans(m_control, ~ 1), infer = TRUE)
summary(emmeans(m_control, ~ domain), infer = TRUE)
