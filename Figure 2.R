rm(list = ls())
if (!is.null(dev.list())) {
  dev.off()
}
library(eegUtils)
library(dplyr)
library(ggplot2)
library(viridis)
library(gridExtra)
library(patchwork)
library(ggsci)
load("spectral_slope_comparisons_results.RData", envir = .GlobalEnv)
channels <- unique(statistics$Channel)
F30 <- c("AF3", "F5")
M30 <- c("CP3", "TP7", "C5")
F30_and_M30 <- c("FT7", "FC3")
data_to_plot <- data.frame(
  electrode = c(channels, F30, M30, F30_and_M30),
  amplitude = c(
    rep(0, length(channels)),
    rep(0, length(F30)),
    rep(0, length(M30)),
    rep(0, length(F30_and_M30))
  )
)
my_plot <- topoplot(
  data_to_plot,
  contour = FALSE,
  scaling = 1,
  interp_limit = "head",
  fill_title = NULL,
  chan_marker = "point",
) +
  guides(fill = "none")
highlight_df <- electrode_locations(data_to_plot, drop = TRUE) %>%
  filter(electrode %in% c(M30, F30, F30_and_M30)) %>%
  mutate(
    group = case_when(
      electrode %in% F30_and_M30 ~ "F30 and M30",
      electrode %in% M30 ~ "M30",
      electrode %in% F30 ~ "F30",
    )
  )

my_plot <-
  my_plot +
  geom_point(
    data = highlight_df,
    aes(x = x, y = y, colour = group),
    size = 4,
    inherit.aes = FALSE
  ) +
  scale_colour_bmj() +
  theme(legend.title = element_blank())
ggsave(
  plot = my_plot,
  filename = "Figure 2.png",
  width = 6,
  height = 5,
  dpi = 600,
  bg = "white"
)
