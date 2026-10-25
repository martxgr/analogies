# Study 1 analysis
library(tidyverse)
library(lmerTest)
library(stringi)
library(tidytext)
library(LexOPS)

d <- read.csv("data/study1.csv") %>%
  filter(comparison == "mechanical vs psychological") %>%
  rename(mech_base = analogy_a, psych_base = analogy_b)

analogies <- read.csv("stimuli/study1_analogies.csv") %>%
  filter(comparison == "mechanical vs psychological")

# main analysis

m_main <- lmer(pref ~ 1 + (1 | id) + (1 | mech_base) + (1 | psych_base), d)
summary(m_main)
confint(m_main)

# word frequency
freq <- lexops %>%
  transmute(word = str_to_lower(string), zipf = Zipf.SUBTLEX_US) %>%
  filter(!is.na(word), !is.na(zipf)) %>%
  distinct(word, .keep_all = TRUE)

stim <- analogies %>%
  transmute(id, base = factor(base_domain, c("psychological", "mechanical")), text,
            n_words = stri_count_words(text))

stim <- stim %>%
  left_join(stim %>%
              unnest_tokens(word, text) %>%
              left_join(freq, by = "word") %>%
              group_by(id) %>%
              summarise(mean_zipf = mean(zipf, na.rm = TRUE)),
            by = "id")

# do analogies with mechanical and psychological bases differ in length or frequency?
t.test(n_words ~ base, data = stim)
t.test(mean_zipf ~ base, data = stim)

# main model with each pairing's length and frequency difference (standardized) as covariates
pair_cov <- d %>%
  distinct(mech_base, psych_base) %>%
  left_join(stim %>% select(mech_base = id, mech_words = n_words, mech_zipf = mean_zipf), by = "mech_base") %>%
  left_join(stim %>% select(psych_base = id, psych_words = n_words, psych_zipf = mean_zipf), by = "psych_base") %>%
  mutate(length_diff_z = as.numeric(scale(mech_words - psych_words)),
         freq_diff_z = as.numeric(scale(mech_zipf - psych_zipf)))

d_control <- d %>% left_join(pair_cov, by = c("mech_base", "psych_base"))

m_control <- lmer(pref ~ length_diff_z + freq_diff_z + (1 | pid) + (1 | mech_base) + (1 | psych_base),
                  d_control)
summary(m_control)
confint(m_control)
