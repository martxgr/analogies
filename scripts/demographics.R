library(tidyverse)

files <- c(`Study 1` = "data/study1_demographics.csv",
           `Study 2` = "data/study2_demographics.csv",
           `Study 3` = "data/study3_demographics.csv",
           `Study 4` = "data/study4_demographics.csv")

demographics <- map_dfr(files, read.csv, .id = "study")

demographics %>%
  group_by(study) %>%
  summarise(recruited = n(),
            pct_female = 100 * mean(Sex == "Female", na.rm = TRUE),
            pct_white = 100 * mean(Ethnicity.simplified == "White", na.rm = TRUE),
            mean_age = mean(Age, na.rm = TRUE),
            sd_age = sd(Age, na.rm = TRUE), 1)
