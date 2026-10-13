# DTAN-505 Group Project: three regional charts from NOAA Climate at a Glance data.
#
#   Chart 1: Warming rate by continent since 1980 (ranked horizontal bar)
#   Chart 2: Northern Hemisphere land, month by year since 1950 (heatmap)
#   Chart 3: Change by region, 1850-1900 vs 2016-2025 (slopegraph)
#
# Before running, set `proj` to the project folder (the one that contains data/).
# Missing packages are installed automatically. Charts are saved to output/charts
# as 300 dpi PNGs. Style matches noaa.R (one red for the point, gray for context).

# ---- Packages and paths -----------------------------------------------------

packages <- c("ggplot2", "dplyr", "tidyr", "readr", "scales")
missing  <- packages[!packages %in% rownames(installed.packages())]
if (length(missing) > 0) install.packages(missing, repos = "https://cloud.r-project.org")
invisible(lapply(packages, library, character.only = TRUE))

proj <- Sys.getenv("PROJ", unset = "~/Documents/DTAN-505 6.4 Group Project")

annual  <- read_csv(file.path(proj, "data/noaa_annual_anomalies_1850_2025.csv"),  show_col_types = FALSE)
monthly <- read_csv(file.path(proj, "data/noaa_monthly_anomalies_1850_2026.csv"), show_col_types = FALSE)

chart_dir <- file.path(proj, "output/charts")
dir.create(chart_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Shared style (same as noaa.R) ------------------------------------------

red        <- "#c0392b"
gray_line  <- "#a6a6a6"
gray_text  <- "#7f7f7f"
gray_axis  <- "#bfbfbf"
dark_text  <- "#404040"
deg        <- "\u00b0C"

theme_swd <- function() {
  theme_classic(base_size = 11) +
    theme(
      plot.title            = element_text(colour = dark_text, size = 14, face = "bold",
                                           margin = margin(b = 4)),
      plot.subtitle         = element_text(colour = gray_text, size = 10, margin = margin(b = 12)),
      plot.caption          = element_text(colour = gray_text, size = 8, hjust = 0,
                                           margin = margin(t = 10)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      axis.line             = element_line(colour = gray_axis, linewidth = 0.4),
      axis.ticks            = element_line(colour = gray_axis, linewidth = 0.4),
      axis.text             = element_text(colour = gray_text, size = 9),
      axis.title            = element_text(colour = gray_text, size = 9),
      axis.title.x          = element_text(hjust = 0, margin = margin(t = 8)),
      axis.title.y          = element_text(hjust = 1, margin = margin(r = 8)),
      legend.position       = "none",
      plot.margin           = margin(14, 20, 10, 14)
    )
}

# Wrap long takeaway titles so they never run off the chart
wrap <- function(x, width = 60) paste(strwrap(x, width), collapse = "\n")

source_note <- "Source: NOAA National Centers for Environmental Information, Climate at a Glance: Global Time Series (2026)."

save_chart <- function(plot, file, width = 8, height = 4.8) {
  ggsave(file.path(chart_dir, file), plot, width = width, height = height, dpi = 300, bg = "white")
}

continents <- c(
  africa_land         = "Africa",
  asia_land           = "Asia",
  europe_land         = "Europe",
  north_america_land  = "North America",
  south_america_land  = "South America",
  oceania_land        = "Oceania"
)


# ---- Chart 1: Warming rate by continent since 1980 --------------------------
# Rate = slope of a straight-line fit, 1980 to 2025, in degrees C per decade.

recent <- filter(annual, year >= 1980)

rate <- function(x) unname(coef(lm(x ~ recent$year))[2]) * 10

rates <- tibble(col = names(continents), region = unname(continents)) |>
  mutate(rate = sapply(col, function(cn) rate(recent[[cn]]))) |>
  arrange(rate) |>
  mutate(region = factor(region, levels = region),          # smallest first, so largest plots on top
         highlight = rank(-rate) <= 2)

globe_rate <- rate(recent$globe_land)

c1 <- ggplot(rates, aes(rate, region, fill = highlight)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = sprintf("%.2f", rate)),            # value sits inside the bar end
            hjust = 1.2, size = 3.8, fontface = "bold", colour = "white") +
  annotate("segment", x = globe_rate, xend = globe_rate, y = 0.4, yend = 6.55,
           colour = dark_text, linewidth = 0.5, linetype = "dashed") +
  annotate("text", x = globe_rate, y = 6.62, hjust = 0.5, vjust = 0, size = 3,
           colour = dark_text, label = sprintf("Global land average: %.2f", globe_rate)) +
  scale_fill_manual(values = c(`TRUE` = red, `FALSE` = gray_line)) +
  scale_y_discrete(expand = expansion(add = c(0.6, 0.9))) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.04)), breaks = seq(0, 0.4, 0.1)) +
  coord_cartesian(clip = "off") +
  labs(title = wrap(sprintf("Europe (%.2f) and Asia (%.2f) are warming fastest; Oceania slowest (%.2f)",
                            rates$rate[rates$region == "Europe"],
                            rates$rate[rates$region == "Asia"],
                            rates$rate[rates$region == "Oceania"]), 58),
       subtitle = "Warming rate of land temperature by continent, 1980 to 2025",
       x = paste0("Warming rate (", deg, " per decade)"), y = NULL,
       caption = source_note) +
  theme_swd() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = dark_text, size = 10))

save_chart(c1, "chart1_continent_warming_rate.png")


# ---- Chart 2: Heatmap, month by year, Northern Hemisphere land --------------

heat <- monthly |>
  filter(year >= 1950) |>
  mutate(month_lab = factor(month.abb[month], levels = rev(month.abb)))   # Jan on top

c2 <- ggplot(heat, aes(year, month_lab, fill = nhem_land)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#2c6fbb", mid = "white", high = red, midpoint = 0,
    limits = c(-3, 3), oob = squish,
    breaks = c(-2, -1, 0, 1, 2, 3),
    labels = function(x) ifelse(x > 0, paste0("+", x), x),
    name = paste0("Difference from 20th century average (", deg, ")")
  ) +
  scale_x_continuous(breaks = seq(1950, 2020, 10), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Since the 1990s, nearly every month on northern land has run warm",
       subtitle = "Northern Hemisphere land temperature by month, 1950 to August 2026. 98% of months since 1990 were above average.",
       x = NULL, y = NULL, caption = source_note) +
  theme_swd() +
  theme(
    axis.line = element_blank(), axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(colour = gray_text, size = 9, vjust = 0.85),
    legend.text  = element_text(colour = gray_text, size = 8),
    legend.key.width  = unit(1.6, "cm"),
    legend.key.height = unit(0.3, "cm")
  )

save_chart(c2, "chart2_nhem_land_heatmap.png", width = 9, height = 5.4)


# ---- Chart 3: Slopegraph, 1850-1900 vs 2016-2025 ----------------------------

regions <- c(continents, arctic_land_ocean = "Arctic")

mean_period <- function(y0, y1) {
  annual |> filter(year >= y0, year <= y1) |> summarise(across(all_of(names(regions)), mean))
}

slope <- bind_rows(
  mean_period(1850, 1900) |> mutate(period = "1850 to 1900"),
  mean_period(2016, 2025) |> mutate(period = "2016 to 2025")
) |>
  pivot_longer(-period, names_to = "col", values_to = "value") |>
  mutate(region = unname(regions[col]),
         x = ifelse(period == "1850 to 1900", 1, 2))

change <- slope |>
  select(region, period, value) |>
  pivot_wider(names_from = period, values_from = value) |>
  mutate(change = `2016 to 2025` - `1850 to 1900`,
         highlight = rank(-change) <= 3) |>
  arrange(desc(change))

slope <- left_join(slope, select(change, region, change, highlight), by = "region")

# Spread the right-hand labels so close values (e.g. Europe and North America) don't overlap
spread_labels <- function(y, min_gap = 0.14) {
  o <- order(y); ys <- y[o]
  for (i in seq_along(ys)[-1]) if (ys[i] - ys[i - 1] < min_gap) ys[i] <- ys[i - 1] + min_gap
  ys <- ys - (mean(ys) - mean(y[o]))                      # keep the group centred on the data
  ys[order(o)]
}

left  <- slope |> filter(x == 1) |> mutate(label_y = spread_labels(value))
right <- slope |> filter(x == 2) |> mutate(label_y = spread_labels(value))
text_col <- function(h) ifelse(h, red, gray_text)

c3 <- ggplot(slope, aes(x, value, group = region)) +
  geom_line(aes(colour = highlight, linewidth = highlight)) +
  geom_point(aes(colour = highlight, size = highlight)) +
  # Short leader lines connect each point to its (spread-out) label
  geom_segment(data = left,  aes(x = 0.97, xend = 0.94, y = value, yend = label_y),
               linewidth = 0.3, colour = gray_axis) +
  geom_segment(data = right, aes(x = 2.03, xend = 2.06, y = value, yend = label_y),
               linewidth = 0.3, colour = gray_axis) +
  geom_text(data = left,  aes(x = 0.93, y = label_y, colour = NULL,
                              label = sprintf("%s  %+.2f", region, value)),
            hjust = 1, size = 3.3, colour = text_col(left$highlight)) +
  geom_text(data = right, aes(x = 2.07, y = label_y, colour = NULL,
                              label = sprintf("%+.2f  %s  (%+.2f)", value, region, change)),
            hjust = 0, size = 3.3, colour = text_col(right$highlight)) +
  scale_colour_manual(values = c(`TRUE` = red, `FALSE` = gray_line)) +
  scale_linewidth_manual(values = c(`TRUE` = 1.2, `FALSE` = 0.6)) +
  scale_size_manual(values = c(`TRUE` = 2.4, `FALSE` = 1.6)) +
  scale_x_continuous(limits = c(0.1, 2.9), breaks = c(1, 2),
                     labels = c("1850 to 1900", "2016 to 2025"), position = "top") +
  labs(title = sprintf("The Arctic (%+.2f), Asia (%+.2f), and Europe (%+.2f) have moved the furthest",
                       change$change[change$region == "Arctic"],
                       change$change[change$region == "Asia"],
                       change$change[change$region == "Europe"]),
       subtitle = paste0("Average temperature compared with the 20th century average (", deg,
                         "). Change between the two periods in parentheses."),
       caption = source_note, x = NULL, y = NULL) +
  theme_swd() +
  theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text.y = element_blank(),
        axis.text.x = element_text(colour = dark_text, size = 10, face = "bold"))

save_chart(c3, "chart3_change_slopegraph.png", width = 9, height = 6.2)

print(rates |> arrange(desc(rate)))
print(change)
cat("Charts saved to:", normalizePath(chart_dir), "\n")
