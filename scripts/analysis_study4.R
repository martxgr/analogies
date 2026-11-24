library(tidyverse)
library(lmerTest)
library(emmeans)

d <- read.csv("data/study4.csv") %>%
  mutate(domain = factor(domain, c("chemical", "biological", "economic")))

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

# per-pairing means and 95% CIs
d %>% group_by(domain, mech_item) %>%
  summarise(mean = mean(pref, na.rm = TRUE), lo = t.test(pref)$conf.int[1], hi = t.test(pref)$conf.int[2])
