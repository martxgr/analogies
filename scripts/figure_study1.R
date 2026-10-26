# Figure 1 (Study 1). A: each participant's average preference, with the model's
# 95% CI shroud and estimate. B: each pairing's average preference with its 95% CI.

library(lmerTest)
source("scripts/figure_style.R")

d <- read.csv("data/study1.csv", fileEncoding = "UTF-8") %>%
  filter(!is.na(pref)) %>%                               # trials left blank
  filter(comparison == "mechanical vs psychological") %>%
  rename(mech_base = analogy_a, psych_base = analogy_b)

analogies <- read.csv("stimuli/study1_analogies.csv", fileEncoding = "UTF-8")

# model estimate and 95% CI (Satterthwaite) for the shrouds
m <- lmer(pref ~ 1 + (1 | id) + (1 | mech_base) + (1 | psych_base), d)
coefs <- summary(m)$coefficients
est <- tibble(est = coefs[1, "Estimate"],
              lo = est - qt(.975, coefs[1, "df"]) * coefs[1, "Std. Error"],
              hi = est + qt(.975, coefs[1, "df"]) * coefs[1, "Std. Error"])

##################
# Panel A: participant averages
##################

a_mm <- 42                 # panel height; the y axis is in mm so dots stay round
x_lim <- c(-3.15, 3.15)

participants <- d %>%
  group_by(id) %>%
  summarise(m = round(mean(pref), 6)) %>%
  mutate(colour = case_when(m > 0 ~ mech_col, m < 0 ~ psych_col, TRUE ~ zero_col))

# dot plot: participants with the same average sit in rows of three, rows
# alternating above and below the first
dots <- participants %>%
  arrange(m) %>%
  group_by(m) %>%
  mutate(k = row_number() - 1, row = k %/% 3, col = k %% 3,
         in_row = pmin(3, n() - 3 * row),
         x = m + (col - (in_row - 1) / 2) * .035,
         level = ceiling(row / 2) * if_else(row %% 2 == 1, 1, -1)) %>%
  ungroup() %>%
  mutate(y = level + max(abs(level)) + 1.5)

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
  geom_point(data = dots, aes(x, y, colour = colour), shape = 16, stroke = 0, size = 1.36, alpha = .7) +
  annotate("rect", xmin = est$lo, xmax = est$hi, ymin = 0, ymax = a_mm, fill = shroud_col, alpha = .18) +
  annotate("segment", x = est$est, xend = est$est, y = 0, yend = a_mm,
           colour = est_col, linewidth = .3 * lwd_mm) +
  annotate("text", x = x_lim, y = -4.2, label = c("Prefers target domain", "Prefers mechanical"),
           hjust = c(0, 1), vjust = 1, size = 6.5 / .pt, family = "Arial", colour = "grey25") +
  scale_colour_identity() +
  scale_fill_identity() +
  scale_x_continuous(breaks = -3:3, labels = abs(-3:3)) +
  scale_y_continuous(breaks = NULL) +
  coord_cartesian(xlim = x_lim, ylim = c(0, a_mm), expand = FALSE, clip = "off") +
  labs(x = NULL, y = NULL, title = "Participant average response") +
  theme_paper +
  theme(plot.title.position = "panel", plot.margin = margin(1, 2, 5, 1, "mm"))

##################
# Panel B: pairing averages
##################

# each analogy is written "explanans -> explanandum", each side coloured by its domain
domain_col <- c(mechanical = mech_col, psychological = psych_col)
lower_first <- function(x) paste0(tolower(substr(x, 1, 1)), substr(x, 2, nchar(x)))

analogy_text <- analogies %>%
  transmute(id,
            explanans = str_to_sentence(label_base), explanans_col = domain_col[base_domain],
            explanandum = lower_first(label_target), explanandum_col = domain_col[target_domain])

pairings <- d %>%
  group_by(mech_base, psych_base) %>%
  summarise(m = mean(pref), n = n(), half = qt(.975, n - 1) * sd(pref) / sqrt(n), .groups = "drop") %>%
  mutate(label = fct_reorder(paste(mech_base, psych_base), m),
         colour = if_else(m > 0, mech_col, psych_col))

# The labels sit in their own plot to the left, as two aligned columns (mechanical-base
# analogy, then psychological-base analogy). Positions are in mm.
width_mm <- function(x) max(systemfonts::string_width(x, family = "Arial", size = 6.5)) * 25.4 / 72
mech_text <- analogy_text %>% filter(id %in% pairings$mech_base)
psych_text <- analogy_text %>% filter(id %in% pairings$psych_base)

arrow_1 <- width_mm(mech_text$explanans) + 2.5
vs_x <- arrow_1 + 2.5 + width_mm(mech_text$explanandum) + 4.5
arrow_2 <- vs_x + 4.5 + width_mm(psych_text$explanans) + 2.5
label_mm <- arrow_2 + 2.5 + width_mm(psych_text$explanandum) + 3

label_cells <- bind_rows(
  pairings %>% left_join(analogy_text, by = c("mech_base" = "id")) %>%
    transmute(label, x = arrow_1, explanans, explanans_col, explanandum, explanandum_col),
  pairings %>% left_join(analogy_text, by = c("psych_base" = "id")) %>%
    transmute(label, x = arrow_2, explanans, explanans_col, explanandum, explanandum_col))

y_expand <- expansion(add = c(.7, 1.8))   # room above the first row for the column headers

pb_labels <- ggplot(label_cells, aes(y = label)) +
  geom_text(aes(x = x - 2.5, label = explanans, colour = explanans_col), hjust = 1,
            size = 6.5 / .pt, family = "Arial") +
  geom_text(aes(x = x, label = "\u2192"), colour = "grey35", size = 6.5 / .pt, family = "Arial") +
  geom_text(aes(x = x + 2.5, label = explanandum, colour = explanandum_col), hjust = 0,
            size = 6.5 / .pt, family = "Arial") +
  geom_text(aes(x = vs_x, label = "vs."), colour = "grey55", size = 6 / .pt, family = "Arial") +
  annotate("text", x = c(arrow_1, arrow_2), y = nrow(pairings) + 1.2,
           label = c("Mechanical base", "Psychological base"),
           size = 6.5 / .pt, family = "Arial", colour = "grey35") +
  scale_colour_identity() +
  scale_x_continuous(limits = c(0, label_mm), expand = c(0, 0)) +
  scale_y_discrete(expand = y_expand) +
  coord_cartesian(clip = "off") +
  labs(title = "Item average response") +
  theme_void(base_size = 7, base_family = "Arial") +
  theme(plot.title = theme_paper$plot.title, plot.tag = theme_paper$plot.tag,
        plot.margin = margin(1, 0, 1, 1, "mm"))

pb <- ggplot(pairings, aes(y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = ref_col, linewidth = .3 * lwd_mm) +
  geom_segment(aes(x = m - half, xend = m + half, yend = label), colour = est_col, linewidth = .2 * lwd_mm) +
  geom_point(aes(x = m, colour = colour), shape = 16, stroke = 0, size = 1.7) +
  annotate("rect", xmin = est$lo, xmax = est$hi, ymin = -Inf, ymax = Inf, fill = shroud_col, alpha = .18) +
  geom_vline(xintercept = est$est, colour = est_col, linewidth = .3 * lwd_mm) +
  scale_colour_identity() +
  scale_x_continuous(breaks = -3:3, labels = abs(-3:3), expand = c(0, 0)) +
  scale_y_discrete(expand = y_expand) +
  coord_cartesian(xlim = c(-1.15, 3.15)) +
  labs(x = NULL, y = NULL, title = " ") +
  theme_paper +
  theme(axis.text.y = element_blank())

##################
# assemble and save
##################

b_mm <- nrow(pairings) * 4.2 + 4
fig <- (free(pa) / ((pb_labels | pb) + plot_layout(widths = unit(c(label_mm, 1), c("mm", "null"))))) +
  plot_layout(heights = unit(c(a_mm, 1), c("mm", "null"))) +
  plot_annotation(tag_levels = list(c("A", "B", "")))

ggsave("figures/figure1.pdf", fig, width = fig_width, height = a_mm + b_mm + 23, units = "mm",
       device = cairo_pdf)
ggsave("figures/figure1.png", fig, width = fig_width, height = a_mm + b_mm + 23, units = "mm",
       dpi = 600, device = ragg::agg_png, bg = "white")
