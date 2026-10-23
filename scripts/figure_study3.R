# Figure 3 (Study 3). A: the base each participant chose to write with (left) and the
# base GPT coded their analogy as actually using (right). B: self-rated quality of the
# analogies, grouped by the base chosen and by the base written, with each group's
# model estimate and 95% CI shroud.

library(lmerTest)
source("scripts/figure_style.R")

d <- read.csv("data/study3.csv", fileEncoding = "UTF-8")

base_cols <- c(mechanical = mech_col, psychological = psych_col)

##################
# Panel A: chose -> wrote
##################

gap <- 10        # vertical space between the two bars in a column (in participants)
bar_w <- .05     # bar width (x runs from 0 = "Chose" to 1 = "Wrote")

# stacked bars: mechanical on top, psychological below
bars <- bind_rows(count(d, base = chose_base) %>% mutate(column = "Chose"),
                  count(d, base = coded_base) %>% mutate(column = "Wrote")) %>%
  group_by(column) %>%
  mutate(top = c(0, n[1] + gap), bottom = top + n,
         xmin = if_else(column == "Chose", 0, 1 - bar_w), xmax = xmin + bar_w) %>%
  ungroup()

# one band per chose -> wrote combination, stacked within each bar
flows <- count(d, chose_base, coded_base) %>%
  group_by(chose_base) %>%
  mutate(start = cumsum(n) - n) %>%
  group_by(coded_base) %>%
  mutate(end = cumsum(n) - n) %>%
  ungroup() %>%
  left_join(bars %>% filter(column == "Chose") %>% select(chose_base = base, start_bar = top), by = "chose_base") %>%
  left_join(bars %>% filter(column == "Wrote") %>% select(coded_base = base, end_bar = top), by = "coded_base") %>%
  mutate(start = start + start_bar, end = end + end_bar, flow = row_number())

# each band follows an S-curve from its place in the left bar to its place in the right
flow_curves <- flows %>%
  crossing(t = seq(0, 1, length.out = 60)) %>%
  mutate(x = bar_w + (1 - 2 * bar_w) * t,
         top = start + (end - start) * (3 * t^2 - 2 * t^3),
         bottom = top + n)

pa <- ggplot() +
  geom_ribbon(data = flow_curves, aes(x, ymin = top, ymax = bottom, group = flow, fill = coded_base),
              alpha = .45) +
  geom_rect(data = bars, aes(xmin = xmin, xmax = xmax, ymin = top, ymax = bottom, fill = base)) +
  geom_text(data = bars, aes(x = if_else(column == "Chose", -.04, 1.04), y = (top + bottom) / 2,
                             label = str_to_sentence(base), hjust = if_else(column == "Chose", 1, 0)),
            size = 6.5 / .pt, family = "Arial") +
  annotate("text", x = c(bar_w / 2, 1 - bar_w / 2), y = -6, label = c("Chose", "Wrote"),
           vjust = 0, fontface = "bold", size = 7 / .pt, family = "Arial") +
  scale_fill_manual(values = base_cols, guide = "none") +
  scale_x_continuous(breaks = NULL) +
  scale_y_reverse(breaks = NULL) +
  coord_cartesian(xlim = c(-.65, 1.65), clip = "off") +
  labs(x = NULL, y = NULL) +
  theme_paper +
  theme(panel.grid = element_blank())

##################
# Panel B: self-rated quality
##################

# model means and 95% CIs (Satterthwaite), by the base chosen and by the base written
m_chose <- lmer(rating ~ 0 + chose_base + (1 | mech_item) + (1 | psych_item), d)
m_coded <- lmer(rating ~ 0 + coded_base + (1 | mech_item) + (1 | psych_item), d)

estimates <- bind_rows(
  as_tibble(summary(m_chose)$coefficients, rownames = "term") %>% mutate(set = "Chose"),
  as_tibble(summary(m_coded)$coefficients, rownames = "term") %>% mutate(set = "Wrote")) %>%
  transmute(set, base = str_remove(term, "chose_base|coded_base"),
            est = Estimate,
            lo = est - qt(.975, df) * `Std. Error`,
            hi = est + qt(.975, df) * `Std. Error`)

fig_h <- 70                      # figure height (mm)
col_mm <- 22                     # width of each column of dots (mm); x is in mm
rating_mm <- (fig_h - 18) / 7    # height of one rating step (mm)
centres <- c(mechanical = 11, psychological = 33)

ratings <- bind_rows(d %>% transmute(set = "Chose", base = chose_base, rating),
                     d %>% transmute(set = "Wrote", base = coded_base, rating))

# at each rating, dots sit in rows of three (spread 1 mm apart along the rating axis),
# rows alternating left and right of the column centre
dots <- ratings %>%
  arrange(set, base, rating) %>%
  group_by(set, base, rating) %>%
  mutate(k = row_number() - 1, row = k %/% 3, col = k %% 3,
         in_row = pmin(3, n() - 3 * row),
         y = rating + (col - (in_row - 1) / 2) / rating_mm,
         x = centres[base] + ceiling(row / 2) * if_else(row %% 2 == 1, 1, -1)) %>%
  ungroup()

estimates <- estimates %>%
  mutate(xmin = centres[base] - col_mm / 2 + 1, xmax = centres[base] + col_mm / 2 - 1)

pb <- ggplot() +
  geom_point(data = dots, aes(x, y, colour = base), shape = 16, stroke = 0, size = 1.36, alpha = .7) +
  geom_rect(data = estimates, aes(xmin = xmin, xmax = xmax, ymin = lo, ymax = hi),
            fill = shroud_col, alpha = .18) +
  geom_segment(data = estimates, aes(x = xmin, xend = xmax, y = est, yend = est),
               colour = est_col, linewidth = .3 * lwd_mm) +
  facet_wrap(~ set, nrow = 1) +
  scale_colour_manual(values = base_cols, guide = "none") +
  scale_x_continuous(breaks = centres, labels = c("Mechanical", "Psychological")) +
  scale_y_continuous(breaks = 1:7) +
  coord_cartesian(xlim = c(0, 2 * col_mm), ylim = c(.5, 7.5), expand = FALSE) +
  labs(x = NULL, y = "Self-rated quality") +
  theme_paper +
  theme(panel.grid.major.x = element_blank(), panel.spacing.x = unit(5, "mm"))

##################
# assemble and save
##################

fig <- (pa | pb) + plot_annotation(tag_levels = "A")

ggsave("figures/figure3.pdf", fig, width = fig_width, height = fig_h, units = "mm", device = cairo_pdf)
ggsave("figures/figure3.png", fig, width = fig_width, height = fig_h, units = "mm",
       dpi = 600, device = ragg::agg_png, bg = "white")
