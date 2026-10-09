# DTAN-505 6.4 Group Project (Group 5)
# Charts 1 to 3 from NOAA Climate at a Glance data.
#
# Before running, check that `proj` below points to the project folder.
# Any missing packages are installed automatically the first time.
# Charts are saved to output/charts as PNG files (300 dpi, ready for Word).
#
# Design follows Storytelling With Data (Knaflic):
#   - Line graphs for continuous yearly data. No zero-baseline rule for lines.
#   - Clutter removed: no chart border, no gridlines, no data markers,
#     no legend. Axis labels kept light gray.
#   - Data labeled directly, with the label in the same color as its line.
#   - Color used sparingly: one red for the point of the chart, gray for
#     everything else, and the same red in all three charts.
#   - Text is your friend: a title that states the takeaway, axis titles,
#     and short annotations on the key numbers.
#   - Titles and axis titles aligned to the left edge.

# ---- Install and load packages ---------------------------------------------

packages <- c("ggplot2", "dplyr", "tidyr", "readr", "scales")
missing  <- packages[!packages %in% rownames(installed.packages())]
if (length(missing) > 0) {
  install.packages(missing, repos = "https://cloud.r-project.org")
}
invisible(lapply(packages, library, character.only = TRUE))

proj <- "~/Documents/DTAN-505 6.4 Group Project"

annual <- read_csv(file.path(proj, "data/noaa_annual_anomalies_1850_2025.csv"), show_col_types = FALSE)

chart_dir <- file.path(proj, "output/charts")
dir.create(chart_dir, recursive = TRUE, showWarnings = FALSE)


# ---- Shared style -----------------------------------------------------------

red        <- "#c0392b"   # the one highlight color, used for the story
gray_line  <- "#a6a6a6"   # context lines
gray_text  <- "#7f7f7f"   # axis labels, notes, context labels
gray_axis  <- "#bfbfbf"   # axis lines and the reference line
dark_text  <- "#404040"   # titles

deg <- "°C"

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
      axis.title.y          = element_text(hjust = 1, margin = margin(r = 8)),   # top of the axis
      axis.title.x          = element_text(hjust = 0, margin = margin(t = 8)),   # left of the axis
      legend.position       = "none",
      plot.margin           = margin(14, 20, 10, 14)
    )
}

source_note <- "Source: NOAA National Centers for Environmental Information, Climate at a Glance: Global Time Series (2026)."

save_chart <- function(plot, file, width = 8, height = 4.8) {
  ggsave(file.path(chart_dir, file), plot, width = width, height = height, dpi = 300, bg = "white")
}

# Warming rate in degrees C per decade, from a straight-line fit
trend_per_decade <- function(years, values) {
  unname(coef(lm(values ~ years))[2]) * 10
}

recent <- filter(annual, year >= 1980)

# Gray reference line at zero, labeled like the "GOAL" line in the book
# The label sits at the right end, below the line, where no data crosses it.
baseline_layers <- function() {
  list(
    geom_hline(yintercept = 0, colour = gray_axis, linewidth = 0.5),
    annotate("text", x = 2025, y = 0, label = "20TH CENTURY AVERAGE",
             hjust = 1, vjust = 1.6, size = 2.6, colour = gray_text)
  )
}

x_axis <- function(right_pad) {
  scale_x_continuous(breaks = seq(1850, 2025, 25),
                     expand = expansion(mult = c(0.01, right_pad)))
}


# ---- Chart 1: Global temperature, 1850 to 2025 -----------------------------

record    <- annual |> slice_max(globe_land_ocean, n = 1)
last_cold <- annual |> filter(globe_land_ocean < 0) |> slice_max(year, n = 1)

c1 <- ggplot(annual, aes(year, globe_land_ocean)) +
  baseline_layers() +
  geom_line(data = filter(annual, year <= 2015), colour = gray_line, linewidth = 0.6) +
  geom_line(data = filter(annual, year >= 2015), colour = red, linewidth = 1.3) +
  # Last year below average, in gray because it is context
  geom_point(data = last_cold, colour = gray_text, size = 1.8) +
  annotate("text", x = last_cold$year, y = last_cold$globe_land_ocean - 0.1,
           label = paste0(last_cold$year, ": last year\nbelow average"),
           size = 2.8, colour = gray_text, lineheight = 0.9) +
  # The record year, in red because it is the point
  geom_point(data = record, colour = red, size = 2.6) +
  annotate("text", x = record$year - 2.5, y = record$globe_land_ocean + 0.02, hjust = 1,
           label = paste0(record$year, ": +", record$globe_land_ocean, " ", deg),
           size = 3.6, colour = red, fontface = "bold") +
  annotate("text", x = 2012, y = 0.93, hjust = 1, lineheight = 0.9,
           label = "2015 to 2025:\nthe 11 warmest years on record",
           size = 3.1, colour = red) +
  x_axis(0.02) +
  labs(title = "The last 11 years were the 11 warmest on record",
       subtitle = "Global average temperature (land and ocean), compared with the 20th century average",
       x = "Year", y = paste0("Difference from average (", deg, ")"),
       caption = source_note) +
  theme_swd()

save_chart(c1, "chart1_global_trend.png")


# ---- Chart 2: Land versus ocean ---------------------------------------------

land_rate  <- trend_per_decade(recent$year, recent$globe_land)
ocean_rate <- trend_per_decade(recent$year, recent$globe_ocean)
end2       <- filter(annual, year == max(year))

c2 <- ggplot(annual, aes(year)) +
  baseline_layers() +
  geom_vline(xintercept = 1980, colour = gray_axis, linewidth = 0.4, linetype = "dotted") +
  annotate("text", x = 1981, y = -0.75, label = "Rates measured from 1980",
           hjust = 0, size = 2.6, colour = gray_text) +
  geom_line(aes(y = globe_ocean), colour = gray_line, linewidth = 0.6) +
  geom_line(aes(y = globe_land), colour = red, linewidth = 1.1) +
  annotate("text", x = 2028, y = end2$globe_land, hjust = 0, vjust = 0.3, size = 3.5,
           colour = red, fontface = "bold", label = "Land") +
  annotate("text", x = 2028, y = end2$globe_land - 0.17, hjust = 0, size = 3, colour = red,
           label = sprintf("+%.2f %s per decade", land_rate, deg)) +
  annotate("text", x = 2028, y = end2$globe_ocean, hjust = 0, vjust = 0.3, size = 3.5,
           colour = gray_text, fontface = "bold", label = "Ocean") +
  annotate("text", x = 2028, y = end2$globe_ocean - 0.17, hjust = 0, size = 3, colour = gray_text,
           label = sprintf("+%.2f %s per decade", ocean_rate, deg)) +
  x_axis(0.17) +
  coord_cartesian(clip = "off") +
  labs(title = "Land is warming more than twice as fast as the ocean",
       subtitle = "Global average temperature over land and over the ocean, compared with the 20th century average",
       x = "Year", y = paste0("Difference from average (", deg, ")"),
       caption = source_note) +
  theme_swd()

save_chart(c2, "chart2_land_vs_ocean.png")


# ---- Chart 3: Northern versus Southern Hemisphere ---------------------------

nh_rate <- trend_per_decade(recent$year, recent$nhem_land_ocean)
sh_rate <- trend_per_decade(recent$year, recent$shem_land_ocean)
end3    <- filter(annual, year == max(year))

c3 <- ggplot(annual, aes(year)) +
  baseline_layers() +
  geom_vline(xintercept = 1980, colour = gray_axis, linewidth = 0.4, linetype = "dotted") +
  annotate("text", x = 1981, y = -0.45, label = "Rates measured from 1980",
           hjust = 0, size = 2.6, colour = gray_text) +
  geom_line(aes(y = shem_land_ocean), colour = gray_line, linewidth = 0.6) +
  geom_line(aes(y = nhem_land_ocean), colour = red, linewidth = 1.1) +
  annotate("text", x = 2028, y = end3$nhem_land_ocean, hjust = 0, vjust = 0.3, size = 3.5,
           colour = red, fontface = "bold", label = "Northern") +
  annotate("text", x = 2028, y = end3$nhem_land_ocean - 0.14, hjust = 0, size = 3, colour = red,
           label = sprintf("+%.2f %s per decade", nh_rate, deg)) +
  annotate("text", x = 2028, y = end3$shem_land_ocean, hjust = 0, vjust = 0.3, size = 3.5,
           colour = gray_text, fontface = "bold", label = "Southern") +
  annotate("text", x = 2028, y = end3$shem_land_ocean - 0.14, hjust = 0, size = 3, colour = gray_text,
           label = sprintf("+%.2f %s per decade", sh_rate, deg)) +
  x_axis(0.17) +
  coord_cartesian(clip = "off") +
  labs(title = "The Northern Hemisphere is warming nearly three times as fast as the Southern",
       subtitle = "Average temperature by hemisphere (land and ocean), compared with the 20th century average",
       x = "Year", y = paste0("Difference from average (", deg, ")"),
       caption = source_note) +
  theme_swd()

save_chart(c3, "chart3_north_vs_south.png")

cat("Charts saved to:", normalizePath(chart_dir), "\n")