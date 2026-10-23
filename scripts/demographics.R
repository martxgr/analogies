# Demographics of the participants approved on Prolific in each study, and how many
# were excluded for failing the attention check.

library(tidyverse)

files <- c(`Study 1` = "data/study1_demographics.csv",
           `Study 2` = "data/study2_demographics.csv",
           `Study 3` = "data/study3_demographics.csv",
           `Study 4` = "data/study4_demographics.csv")

demographics <- map_dfr(files, read.csv, .id = "study")

demographics %>%
  group_by(study) %>%
  summarise(recruited = n(),
            excluded = sum(!analysed),
            analysed = sum(analysed),
            pct_female = round(100 * mean(sex == "Female", na.rm = TRUE)),
            pct_white = round(100 * mean(ethnicity == "White", na.rm = TRUE)),
            mean_age = round(mean(age, na.rm = TRUE), 1),
            sd_age = round(sd(age, na.rm = TRUE), 1))

# full breakdowns
demographics %>% count(study, sex) %>% pivot_wider(names_from = study, values_from = n)
demographics %>% count(study, ethnicity) %>% pivot_wider(names_from = study, values_from = n)
