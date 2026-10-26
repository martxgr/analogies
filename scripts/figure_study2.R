# Figure 2 (Study 2). A: each participant's average preference, with the 95% CI shroud
# and estimate from the model pooling all three LLMs. B: each pairing's average
# preference with its 95% CI, grouped by the LLM that wrote it; each block's shroud
# comes from that LLM's own model.

library(lmerTest)
source("scripts/figure_style.R")

d <- read.csv("data/study2.csv", fileEncoding = "UTF-8") %>%
  filter(!is.na(pref)) %>%                               # trials left blank
  mutate(llm = factor(llm, c("GPT", "Claude", "Gemini")))

pair_labels <- read.csv("stimuli/study2_analogies.csv", fileEncoding = "UTF-8") %>%
  distinct(llm, pair, label)

# model estimate and 95% CI (Satterthwaite) for a fitted model's intercept
intercept_ci <- function(m) {
  coefs <- summary(m)$coefficients
  tibble(est = coefs[1, "Estimate"],
         lo = est - qt(.975, coefs[1, "df"]) * coefs[1, "Std. Error"],
         hi = est + qt(.975, coefs[1, "df"]) * coefs[1, "Std. Error"])
}

est_all <- intercept_ci(lmer(pref ~ 1 + (1 | id) + (1 | llm:pair), d))

est_llm <- bind_rows(
  GPT    = intercept_ci(lmer(pref ~ 1 + (1 | id) + (1 | pair), filter(d, llm == "GPT"))),
  Claude = intercept_ci(lmer(pref ~ 1 + (1 | id) + (1 | pair), filter(d, llm == "Claude"))),
  Gemini = intercept_ci(lmer(pref ~ 1 + (1 | id) + (1 | pair), filter(d, llm == "Gemini"))),
  .id = "llm") %>%
  mutate(llm = factor(llm, levels(d$llm)))

##################
# Panel A: participant averages
##################

a_mm <- 42                 # panel height; the y axis is in mm so dots stay round
x_lim <- c(-2.65, 3.15)    # trimmed to the range of the data

participants <- d %>%
  group_by(id, llm) %>%
  summarise(m = round(mean(pref), 6), .groups = "drop") %>%
  mutate(colour = case_when(m > 0 ~ mech_col, m < 0 ~ psych_col, TRUE ~ zero_col))

# dot plot: participants with the same average sit in rows of three, rows
# alternating above and below the first. Each LLM gets its own thin band
# (GPT lowest, Gemini highest) so the three shapes stay apart.
dots <- participants %>%
  arrange(llm, m) %>%
  group_by(llm, m) %>%
  mutate(k = row_number() - 1, row = k %/% 3, col = k %% 3,
         in_row = pmin(3, n() - 3 * row),
         x = m + (col - (in_row - 1) / 2) * .045,
         level = ceiling(row / 2) * if_else(row %% 2 == 1, 1, -1)) %>%
  ungroup() %>%
  mutate(band_height = 2 * max(abs(level)) + 1.6,         # tallest stack plus a small gap
         y = 1.5 + level + max(abs(level)) + (as.integer(llm) - 1) * band_height)

# density of participant averages, split at zero
dens <- density(participants$m, from = x_lim[1], to = x_lim[2], n = 512)
curve <- tibble(x = sort(c(dens$x, 0))) %>%
  mutate(h = approx(dens$x, dens$y, x)$y / max(dens$y) * (a_mm - 2))
curve <- bind_rows(filter(curve, x <= 0) %>% mutate(side = psych_col),
                   filter(curve, x >= 0) %>% mutate(side = mech_col))

pa <- ggplot() +
  geom_ribbon(data = curve, aes(x, ymin = 0, ymax = h, fill = side), alpha = .14) +
  geom_line(data = curve, aes(x, h, colour = side), linewidth = .25 * lwd_mm, alpha = .6) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = ref_col, linewidth = .3 * lwd_mm) +
  geom_point(data = dots, aes(x, y, colour = colour, shape = llm), stroke = 0, size = 1.5, alpha = .7) +
  annotate("rect", xmin = est_all$lo, xmax = est_all$hi, ymin = 0, ymax = a_mm, fill = shroud_col, alpha = .18) +
  annotate("segment", x = est_all$est, xend = est_all$est, y = 0, yend = a_mm,
           colour = est_col, linewidth = .3 * lwd_mm) +
  annotate("text", x = x_lim, y = -4.2, label = c("Prefers target domain", "Prefers mechanical"),
           hjust = c(0, 1), vjust = 1, size = 6.5 / .pt, family = "Arial", colour = "grey25") +
  scale_colour_identity() +
  scale_fill_identity() +
  scale_shape_manual(values = c(GPT = 16, Claude = 17, Gemini = 15), name = NULL,
                     guide = guide_legend(override.aes = list(colour = "grey35", alpha = 1, size = 1.8))) +
  scale_x_continuous(breaks = -3:3, labels = abs(-3:3)) +
  scale_y_continuous(breaks = NULL) +
  coord_cartesian(xlim = x_lim, ylim = c(0, a_mm), expand = FALSE, clip = "off") +
  labs(x = NULL, y = NULL, title = "Participant average response") +
  theme_paper +
  theme(plot.title.position = "panel", plot.margin = margin(1, 2, 5, 1, "mm"),
        legend.position = "inside", legend.position.inside = c(0, 1), legend.justification = c(0, 1),
        legend.direction = "horizontal", legend.key.size = unit(3, "mm"), legend.text = element_text(size = 6.5))

##################
# Panel B: pairing averages, one block per LLM
##################

pairings <- d %>%
  group_by(llm, pair) %>%
  summarise(m = mean(pref), n = n(), half = qt(.975, n - 1) * sd(pref) / sqrt(n), .groups = "drop") %>%
  left_join(pair_labels %>% mutate(llm = factor(llm, levels(d$llm))), by = c("llm", "pair")) %>%
  arrange(llm, m) %>%
  mutate(label = fct_inorder(label),
         colour = if_else(m > 0, mech_col, psych_col),
         # labels are stored "mechanical / psychological"; shown psychological (left) /
         # mechanical (right) to match the x axis
         mech_term = str_extract(as.character(label), "^[^/]+(?= / )"),
         psych_term = str_remove(as.character(label), "^[^/]+ / "),
         psych_term = paste0(toupper(substr(psych_term, 1, 1)), substr(psych_term, 2, nchar(psych_term))),
         mech_term = paste0(tolower(substr(mech_term, 1, 1)), substr(mech_term, 2, nchar(mech_term))))

# the labels sit in their own plot to the left, aligned on the slash (positions in mm)
width_mm <- function(x) max(systemfonts::string_width(x, family = "Arial", size = 6.5)) * 25.4 / 72
slash_x <- width_mm(pairings$psych_term) + 1.5
label_mm <- slash_x + 1.5 + width_mm(pairings$mech_term) + 3

pb_labels <- ggplot(pairings, aes(y = label)) +
  geom_text(aes(x = slash_x - 1.2, label = psych_term), colour = psych_col, hjust = 1,
            size = 6.5 / .pt, family = "Arial") +
  geom_text(aes(x = slash_x, label = "/"), colour = "grey55", size = 6.5 / .pt, family = "Arial") +
  geom_text(aes(x = slash_x + 1.2, label = mech_term), colour = mech_col, hjust = 0,
            size = 6.5 / .pt, family = "Arial") +
  facet_wrap(~ llm, ncol = 1, scales = "free_y") +
  scale_x_continuous(limits = c(0, label_mm), expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(add = .7)) +
  labs(title = "Item average response") +
  theme_void(base_size = 7, base_family = "Arial") +
  theme(plot.title = theme_paper$plot.title, plot.tag = theme_paper$plot.tag,
        strip.text = theme_paper$strip.text, panel.spacing.y = unit(2.5, "mm"),
        plot.margin = margin(1, 0, 1, 1, "mm"))

pb <- ggplot(pairings, aes(y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = ref_col, linewidth = .3 * lwd_mm) +
  geom_segment(aes(x = m - half, xend = m + half, yend = label), colour = est_col, linewidth = .2 * lwd_mm) +
  geom_point(aes(x = m, colour = colour), shape = 16, stroke = 0, size = 1.7) +
  geom_rect(data = est_llm, aes(xmin = lo, xmax = hi, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = shroud_col, alpha = .18) +
  geom_vline(data = est_llm, aes(xintercept = est), colour = est_col, linewidth = .3 * lwd_mm) +
  facet_wrap(~ llm, ncol = 1, scales = "free_y") +
  scale_colour_identity() +
  scale_x_continuous(breaks = -3:3, labels = abs(-3:3), expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(add = .7)) +
  coord_cartesian(xlim = c(-.65, 2.15)) +
  labs(x = NULL, y = NULL, title = " ") +
  theme_paper +
  theme(axis.text.y = element_blank(), strip.text = element_text(colour = NA),
        panel.spacing.y = unit(2.5, "mm"))

##################
# assemble and save
##################

b_mm <- nrow(pairings) * 4.2 + 3 * 5
fig <- (free(pa) / ((pb_labels | pb) + plot_layout(widths = unit(c(label_mm, 1), c("mm", "null"))))) +
  plot_layout(heights = unit(c(a_mm, 1), c("mm", "null"))) +
  plot_annotation(tag_levels = list(c("A", "B", "")))

ggsave("figures/figure2.pdf", fig, width = fig_width, height = a_mm + b_mm + 23, units = "mm",
       device = cairo_pdf)
ggsave("figures/figure2.png", fig, width = fig_width, height = a_mm + b_mm + 23, units = "mm",
       dpi = 600, device = ragg::agg_png, bg = "white")
