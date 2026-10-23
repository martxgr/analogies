# Study 3 analysis: participants chose a target and a base, wrote an analogy, and
# rated it (1-7). GPT then coded which domain each analogy actually used as its base.

library(tidyverse)
library(lmerTest)

d <- read.csv("data/study3.csv", fileEncoding = "UTF-8") %>%
  mutate(chose_base = factor(chose_base, c("mechanical", "psychological")),
         coded_base = factor(coded_base, c("mechanical", "psychological")))

##################
# what participants chose to write
##################

count(d, chose_base)
binom.test(sum(d$chose_base == "mechanical"), nrow(d), p = .5)

# did the chosen direction change how good writers thought their analogy was?
d %>% group_by(chose_base) %>% summarise(n = n(), mean = mean(rating), sd = sd(rating))
m_chose <- lmer(rating ~ chose_base + (1 | mech_item) + (1 | psych_item), d)
summary(m_chose)
confint(m_chose, parm = "beta_", method = "Wald")

##################
# what participants actually wrote (GPT coding)
##################

count(d, coded_base)
binom.test(sum(d$coded_base == "mechanical"), nrow(d), p = .5)

# how often did writers end up using the other domain as the base?
switching <- table(chose = d$chose_base, switched = d$chose_base != d$coded_base)
switching
prop.table(switching, 1)
fisher.test(switching)

##################
# exploratory: self-ratings by the base actually written
##################

d %>% group_by(coded_base) %>% summarise(n = n(), mean = mean(rating), sd = sd(rating))
m_coded <- lmer(rating ~ coded_base + (1 | mech_item) + (1 | psych_item), d)
summary(m_coded)
confint(m_coded, parm = "beta_", method = "Wald")
