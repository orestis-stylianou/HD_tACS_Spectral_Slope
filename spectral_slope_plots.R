rm(list=ls())
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
load("spectral_slope_comparisons_results.RData",envir = .GlobalEnv)
channels <- unique(statistics$Channel)
stimulation_types <- unique(statistics$Stimulation_Type)
stimulations <- unique(averaged_spectral_slopes$Stimulation)
subject_average_spectral_slope <- averaged_spectral_slopes %>% 
  group_by(Stimulation_Type, Channel, Stimulation) %>% 
  summarize(Spectral_Slope= mean(Spectral_Slope))
spectral_slope_range <- range(subject_average_spectral_slope$Spectral_Slope)
colormap <- "magma"
all_spectral_slope_splots <- list()
all_topolots <- list()
for (stimulation_type in stimulation_types) {
  during_stimulation <- subject_average_spectral_slope %>% ungroup %>% 
    filter(Stimulation_Type == stimulation_type, Stimulation == "During") %>% 
    select(Channel,Spectral_Slope)
  before_stimulation <- subject_average_spectral_slope %>% ungroup %>% 
    filter(Stimulation_Type == stimulation_type, Stimulation == "Before") %>% 
    select(Channel,Spectral_Slope)
  names(during_stimulation) <- c("electrode","amplitude")
  names(before_stimulation) <- names(during_stimulation)
  difference <- data.frame(during_stimulation$electrode,during_stimulation$amplitude-before_stimulation$amplitude)
  names(difference) <- names(during_stimulation)
  significant_channels <- statistics %>% filter(Corrected_p_value <= 0.05, Stimulation_Type == stimulation_type) %>% pull(Channel)
  topoplot_during_stimulation <- 
    topoplot(during_stimulation, highlights = significant_channels,contour = FALSE, scaling = 1,interp_limit = "head",
             fill_title = expression("Spectral Slope"), palette = colormap, limits = spectral_slope_range) +
    theme(plot.title = element_text(hjust = 0.5)) + ggtitle(paste0("During ", stimulation_type, " Stimulation"))
  topoplot_before_stimulation <-
    topoplot(before_stimulation, highlights = significant_channels,contour = FALSE, scaling = 1,interp_limit = "head",
             fill_title = expression("Spectral Slope"),palette = colormap, limits = spectral_slope_range) +
    theme(plot.title = element_text(hjust = 0.5)) + ggtitle(paste0("Before ", stimulation_type, " Stimulation"))
  both_topoplots <- topoplot_before_stimulation + topoplot_during_stimulation
  topoplot_difference <-
    topoplot(difference, highlights = significant_channels,contour = FALSE, scaling = 1,interp_limit = "head",
             fill_title = expression("Spectral Slope"), palette = colormap) +
    theme(plot.title = element_text(hjust = 0.5)) + ggtitle(paste0("During - Before ", stimulation_type))
  all_topolots <- c(all_topolots,both_topoplots,topoplot_difference)
  for (channel in significant_channels) {
    p_value <- statistics %>% filter(Channel == channel, Stimulation_Type == stimulation_type) %>% pull(Corrected_p_value)
    p_value <- p_value %>% round(digits = 3)
    channel_spectral_slope <- averaged_spectral_slopes %>% filter(Channel == channel, Stimulation_Type == stimulation_type) %>% 
      ungroup() %>% select(-c("Channel","Stimulation_Type"))
    spectral_slope_plot <- 
    ggplot(channel_spectral_slope,aes(x = Stimulation,y = Spectral_Slope, color = Stimulation)) + 
      geom_jitter(width = 0.2) + scale_color_aaas() + theme_classic() +
      ggtitle(paste0(channel," p value:",p_value)) + geom_boxplot(alpha=0.3,width=0.1) +
      theme(plot.title = element_text(hjust = 0.5))
    all_spectral_slope_splots <- c(all_spectral_slope_splots,spectral_slope_plot)
  }
}
all_plots <- c(all_topolots,all_spectral_slope_splots)
pdf(file = "Spectral Slope Plots.pdf",  width = 10, height = 10)
for (plot_index in 1:length(all_plots)) {
  plot(all_plots[[plot_index]])
}
dev.off()