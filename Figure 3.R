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
stimulation_types <- unique(statistics$Stimulation_Type)
stimulations <- unique(averaged_spectral_slopes$Stimulation)
subject_average_spectral_slope <- averaged_spectral_slopes %>%
  group_by(Stimulation_Type, Channel, Stimulation) %>%
  summarize(Spectral_Slope = mean(Spectral_Slope))
spectral_slope_range <- range(subject_average_spectral_slope$Spectral_Slope)
colormap <- "magma"
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
difference <- data.frame(
  during_stimulation$electrode,
  during_stimulation$amplitude - before_stimulation$amplitude
)
names(difference) <- names(during_stimulation)
significant_channels <- statistics %>%
  filter(Corrected_p_value <= 0.05, Stimulation_Type == stimulation_type) %>%
  pull(Channel)
topoplot_difference <-
  topoplot(
    difference,
    highlights = significant_channels,
    contour = FALSE,
    scaling = 1,
    interp_limit = "head",
    palette = colormap,
    fill_title = NULL
  )
ggsave(
  plot = topoplot_difference,
  filename = "Figure 3.png",
  width = 6,
  height = 5,
  dpi = 600,
  bg = "white"
)
