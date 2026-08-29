rm(list = ls())
if (!is.null(dev.list())) {
  dev.off()
}
library(dplyr)
library(ggplot2)
library(stringr)
library(ggsci)
library(tidyr)
library(patchwork)
library(ggpp)
options(scipen = 0)
load("spectral_slope_comparisons_results.RData")
significant_channels <- statistics %>%
  filter(Corrected_p_value < 0.05) %>%
  pull(Channel)
significant_stimulation_types <- statistics %>%
  filter(Corrected_p_value < 0.05) %>%
  pull(Stimulation_Type) %>%
  unique()
#### Sorting Files Out ####
subjects <- list.files("Spectral Slope", full.names = TRUE)
subject <- subjects[2]
stimulation_types <- list.files(subject, full.names = TRUE)
stimulation_type <- stimulation_types[2]
segments <- list.files(stimulation_type, full.name = TRUE)
segment <- segments[31]
irasa <- new.env()
load(segment, envir = irasa)
channel <- "O1"
power_spectrum <- irasa$all_irasa_outputs[[channel]]$original
frequencies <- irasa$all_irasa_outputs[[channel]]$frequencies
data_to_plot <- data.frame(
  Frequency = frequencies,
  Power_Spectrum = power_spectrum
)
spectral_plot <- ggplot(
  data_to_plot,
  aes(x = Frequency, y = Power_Spectrum)
) +
  geom_line(key_glyph = draw_key_rect) +
  scale_color_aaas() +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_log10(labels = scales::label_number()) +
  scale_y_log10() +
  guides(alpha = "none") +
  xlab("Frequency (Hz)") +
  ylab("Power")
ggsave(
  plot = spectral_plot,
  filename = "Figure 1.png",
  width = 12,
  height = 6,
  dpi = 600,
  bg = "white",
  limitsize = FALSE
)
