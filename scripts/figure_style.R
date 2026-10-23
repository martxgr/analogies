# Basic figure design; going for a "rust" style!@

library(tidyverse)
library(patchwork)

mech_col <- "#B4501F"
psych_col <- "#2A6F9E"
zero_col <- "#9E9D97"
est_col  <- "#2C2C2A" # CI line
shroud_col <- "#888780" # shroud
ref_col <- "#5F5E5A" # 0 line

fig_width <- 183 # mm
lwd_mm <- 1 / .75 # linewidth

theme_paper <- theme_minimal(base_size = 7, base_family = "Arial") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = .15 * lwd_mm, colour = "grey90"),
        axis.title = element_text(size = 7),
        axis.text = element_text(size = 6.5, colour = "black"),
        strip.text = element_text(size = 7, face = "bold", hjust = 0, margin = margin(0, 0, 1, 0, "mm")),
        plot.title = element_text(size = 7, face = "bold", hjust = 0, margin = margin(2, 0, 2, 0, "mm")),
        plot.tag = element_text(size = 9, face = "bold"),
        plot.margin = margin(1, 2, 1, 1, "mm"))