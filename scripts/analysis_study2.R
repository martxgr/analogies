library(tidyverse)
library(lmerTest)
library(emmeans)
library(stringi)
library(tidytext)
library(LexOPS)

d <- read.csv("data/study2.csv") %>%
  mutate(llm = factor(llm, c("GPT", "Claude", "Gemini")))

analogies <- read.csv("stimuli/study2_analogies.csv")

# main analyses
# one model per LLM
m_gpt <- lmer(pref ~ 1 + (1 | id) + (1 | pair), filter(d, llm == "GPT"))
summary(m_gpt)
confint(m_gpt)

m_claude <- lmer(pref ~ 1 + (1 | id) + (1 | pair), filter(d, llm == "Claude"))
summary(m_claude)
confint(m_claude)

m_gemini <- lmer(pref ~ 1 + (1 | id) + (1 | pair), filter(d, llm == "Gemini"))
summary(m_gemini)
confint(m_gemini)

# all three
m_all <- lmer(pref ~ 0 + llm + (1 | id) + (1 | pair) + (1 | pair:llm), d)
# overall
summary(emmeans(m_all, ~ llm), infer = TRUE)
# effect size
eff_size(emmeans(m_all, ~ llm), sigma = sigma(m_all), edf = df.residual(m_all))
# differ
joint_tests(m_all)
# simple slopes
summary(emmeans(m_all, ~ 1, weights = "equal"), infer = c(TRUE, TRUE))

# exploratory linguistic controls: length and word frequency (mean SUBTLEX-US Zipf)
freq <- lexops %>%
  transmute(word = str_to_lower(string), zipf = Zipf.SUBTLEX_US) %>%
  filter(!is.na(word), !is.na(zipf)) %>%
  distinct(word, .keep_all = TRUE)

stim <- analogies %>%
  mutate(base = if_else(base_domain == "mechanical", "mech", "psych"),
         n_words = stri_count_words(text))

stim <- stim %>%
  left_join(stim %>%
              unnest_tokens(word, text) %>%
              left_join(freq, by = "word") %>%
              group_by(llm, pair, base) %>%
              summarise(mean_zipf = mean(zipf, na.rm = TRUE), .groups = "drop"),
            by = c("llm", "pair", "base"))

# one row per pairing: mechanical-base minus psychological-base version
pair_cov <- stim %>%
  select(llm, pair, base, n_words, mean_zipf) %>%
  pivot_wider(names_from = base, values_from = c(n_words, mean_zipf)) %>%
  transmute(llm = factor(llm, levels(d$llm)), pair,
            length_diff = n_words_mech - n_words_psych,
            freq_diff = mean_zipf_mech - mean_zipf_psych) %>%
  group_by(llm) %>%
  mutate(length_diff_z = as.numeric(scale(length_diff)),
         freq_diff_z = as.numeric(scale(freq_diff))) %>%
  ungroup()

# do the two versions differ in length or frequency?
m_length <- lmer(length_diff ~ llm + (1 | pair), pair_cov)
summary(emmeans(m_length, ~ 1), infer = TRUE)
m_freq <- lmer(freq_diff ~ llm + (1 | pair), pair_cov)
summary(emmeans(m_freq, ~ 1), infer = TRUE)

# main model with both differences as covariates
d_control <- d %>%
  left_join(pair_cov %>% select(llm, pair, length_diff_z, freq_diff_z), by = c("llm", "pair"))

m_control <- lmer(pref ~ 0 + llm + length_diff_z + freq_diff_z +
                    (1 | id) + (1 | pair) + (1 | pair:llm), d_control)
summary(m_control)
