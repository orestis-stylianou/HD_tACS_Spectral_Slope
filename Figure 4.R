rm(list = ls())
if (!is.null(dev.list())) {
  dev.off()
}
library(dplyr)
library(ggplot2)
library(viridis)
library(gridExtra)
library(patchwork)
library(ggsci)
library(scales)
options(scipen = 999)
load("spectral_slope_comparisons_results.RData", envir = .GlobalEnv)
channels <- unique(statistics$Channel)
stimulation_types <- unique(statistics$Stimulation_Type)
stimulations <- unique(averaged_spectral_slopes$Stimulation)
subject_average_spectral_slope <- averaged_spectral_slopes %>%
  group_by(Stimulation_Type, Channel, Stimulation) %>%
  summarize(Spectral_Slope = mean(Spectral_Slope))
spectral_slope_range <- range(subject_average_spectral_slope$Spectral_Slope)
colormap <- "magma"
all_spectral_slope_splots <- list()
all_topolots <- list()
stimulation_type <- "F30"
during_stimulation <- subject_average_spectral_slope %>%
  ungroup %>%
  filter(Stimulation_Type == stimulation_type, Stimulation == "During") %>%
  select(Channel, Spectral_Slope)
before_stimulation <- subject_average_spectral_slope %>%
  ungroup %>%
  filter(Stimulation_Type == stimulation_type, Stimulation == "Before") %>%
  select(Channel, Spectral_Slope)
names(during_stimulation) <- c("electrode", "amplitude")
names(before_stimulation) <- names(during_stimulation)
significant_channels <- statistics %>%
  filter(Corrected_p_value <= 0.05, Stimulation_Type == stimulation_type) %>%
  pull(Channel)
for (channel in significant_channels) {
  p_value <- statistics %>%
    filter(Channel == channel, Stimulation_Type == stimulation_type) %>%
    pull(Corrected_p_value)
  if (round(p_value, digits = 3) == 0) {
    p_value <- round(p_value, digits = 4)
  } else {
    p_value <- round(p_value, digits = 3)
  }
  channel_spectral_slope <- averaged_spectral_slopes %>%
    filter(Channel == channel, Stimulation_Type == stimulation_type) %>%
    ungroup() %>%
    select(-c("Channel", "Stimulation_Type"))

  channel_spectral_slope$Stimulation[
    channel_spectral_slope$Stimulation == "Before"
  ] <- "Before F30"
  channel_spectral_slope$Stimulation[
    channel_spectral_slope$Stimulation == "During"
  ] <- "During F30"
  spectral_slope_plot <-
    ggplot(
      channel_spectral_slope,
      aes(x = Stimulation, y = Spectral_Slope, color = Stimulation)
    ) +
    geom_jitter(width = 0.2) +
    scale_color_aaas() +
    theme_classic() +
    ggtitle(channel) +
    geom_boxplot(alpha = 0.3, width = 0.1) +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
    xlab("") +
    ylab("Spectral Slope") +
    annotate(
      "text",
      x = -Inf,
      y = -Inf,
      label = paste0("italic(p) == ", p_value),
      parse = TRUE,
      hjust = -0.1,
      vjust = -0.5,
      size = 2.5,
      inherit.aes = FALSE
    )
  all_spectral_slope_splots <- c(
    all_spectral_slope_splots,
    spectral_slope_plot
  )
}
all_combined <- wrap_plots(all_spectral_slope_splots, ncol = 4, nrow = 2)
ggsave(
  plot = all_combined,
  filename = "Figure 4.png",
  width = 12,
  height = 6,
  dpi = 600,
  bg = "white",
  limitsize = FALSE
)
