# Study 1 analysis
library(tidyverse)
library(lmerTest)
library(broom.mixed)

d <- read.csv("data/study1.csv") %>%
  filter(comparison == "mechanical vs psychological") %>%
  rename(mech_base = analogy_a, psych_base = analogy_b)

# main analysis

m_main <- lmer(pref ~ 1 + (1 | id) + (1 | mech_base) + (1 | psych_base), d)
summary(m_main)
confint(m_main)

# per-pairing 95% CIs
d %>% group_by(mech_base, psych_base) %>%
  summarise(mean = mean(pref, na.rm = TRUE), lo = t.test(pref)$conf.int[1], hi = t.test(pref)$conf.int[2])