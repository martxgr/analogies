library(tidyverse)
library(lmerTest)
library(emmeans)

d <- read.csv("data/study2.csv") %>%
  mutate(llm = factor(llm, c("GPT", "Claude", "Gemini")))

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
# simple slopes
summary(emmeans(m_all, ~ llm), infer = TRUE)
# effect size
eff_size(emmeans(m_all, ~ llm), sigma = sigma(m_all), edf = df.residual(m_all))
# differ
joint_tests(m_all)
# overall
summary(emmeans(m_all, ~ 1, weights = "equal"), infer = c(TRUE, TRUE))

# per-pairing means and 95% CIs
d %>% group_by(llm, pair) %>%
  summarise(mean = mean(pref, na.rm = TRUE), lo = t.test(pref)$conf.int[1], hi = t.test(pref)$conf.int[2])

# length, frequency and concreteness controls: see analysis_lexical.R
