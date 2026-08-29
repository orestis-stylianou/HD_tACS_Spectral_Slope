rm(list = ls())
if (!is.null(dev.list())) {
  dev.off()
}
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggsci)
library(nortest)
library(stringr)
load("spectral_slope_comparisons_results.RData", envir = .GlobalEnv)
spectral_slopes <- averaged_spectral_slopes %>%
  filter(Stimulation == "Before") %>%
  select(-Stimulation)
ctt <- readRDS("ctt_data_grouped.rds")
averaged_deviation_per_subject <- ctt %>%
  group_by(Subject, Stimulation_Type, Stimulation) %>%
  summarize("Mean_Deviation" = mean(Mean_Deviation))
averaged_deviation_change_per_subject <- averaged_deviation_per_subject %>%
  pivot_wider(names_from = Stimulation, values_from = Mean_Deviation) %>%
  mutate(Change = During - Before) %>%
  select(-During, -Before)
subjects <- unique(spectral_slopes$Subject)
stimulation_types <- unique(spectral_slopes$Stimulation_Type)
channels <- unique(spectral_slopes$Channel)
combinations <- expand.grid(
  subjects,
  stimulation_types,
  stringsAsFactors = FALSE
)
names(combinations) <- c("Subjects", "Stimulation_Types")
spectral_slope_and_ctt_values <- spectral_slopes
spectral_slope_and_ctt_values$Change <- NA
for (combination in 1:dim(combinations)[1]) {
  subject <- combinations$Subjects[combination]
  stimulation_type <- combinations$Stimulation_Types[combination]
  index <- which(
    spectral_slopes$Subject == subject &
      spectral_slopes$Stimulation_Type == stimulation_type
  )
  deviation <- averaged_deviation_change_per_subject %>%
    filter(Subject == subject, Stimulation_Type == stimulation_type) %>%
    pull(Change)
  spectral_slope_and_ctt_values$Change[index] <- deviation
}
correlations <- data.frame(
  Stimulation_Type = character(),
  Channel = character(),
  p_value = numeric(),
  r_value = numeric()
)
for (stimulation_type in stimulation_types) {
  for (channel in channels) {
    spectral_slopes <- spectral_slope_and_ctt_values %>%
      filter(Stimulation_Type == stimulation_type, Channel == channel) %>%
      pull(Spectral_Slope)
    change <- spectral_slope_and_ctt_values %>%
      filter(Stimulation_Type == stimulation_type, Channel == channel) %>%
      pull(Change)
    if (
      lillie.test(spectral_slopes)$p.value > 0.05 &&
        lillie.test(change)$p.value > 0.05
    ) {
      correlation <- cor.test(
        spectral_slopes,
        change,
        method = "pearson",
        alternative = "two.sided"
      )
    } else {
      correlation <- cor.test(
        spectral_slopes,
        change,
        method = "spearman",
        alternative = "two.sided"
      )
    }
    p_value <- correlation$p.value
    r_value <- unname(correlation$estimate)
    correlations <- correlations %>%
      add_row(
        Stimulation_Type = stimulation_type,
        Channel = channel,
        p_value = p_value,
        r_value = r_value
      )
  }
}
correlations <- correlations %>%
  group_by(Channel) %>%
  mutate(Corrected_p_value = p.adjust(p_value, method = "BH"))
all_plots <- list()
for (stimulation_type in stimulation_types) {
  for (channel in channels) {
    spectral_slopes <- spectral_slope_and_ctt_values %>%
      filter(Stimulation_Type == stimulation_type, Channel == channel) %>%
      pull(Spectral_Slope)
    change <- spectral_slope_and_ctt_values %>%
      filter(Stimulation_Type == stimulation_type, Channel == channel) %>%
      pull(Change)
    p_value <- correlations %>%
      filter(Stimulation_Type == stimulation_type, Channel == channel) %>%
      pull(Corrected_p_value)
    r_value <- correlations %>%
      filter(Stimulation_Type == stimulation_type, Channel == channel) %>%
      pull(r_value)
    if (p_value <= 0.1) {
      correlation_graph <- ggplot(
        data.frame(change, spectral_slopes),
        aes(x = change, y = spectral_slopes)
      ) +
        geom_point() +
        labs(x = "CTT Deviation Change", y = "Spectral Slope") +
        ggtitle(paste(
          stimulation_type,
          channel
        )) +
        theme_classic() +
        theme(plot.title = element_text(hjust = 0.5)) +
        annotate(
          "text",
          x = Inf,
          y = Inf,
          label = paste0(
            "italic(p) == ",
            round(p_value, 3),
            "~italic(r) == ",
            round(r_value, 3)
          ),
          parse = TRUE,
          hjust = 1.1,
          vjust = 1.5,
          size = 2.5,
          inherit.aes = FALSE
        )
      all_plots <- c(all_plots, correlation_graph)
    }
  }
}
design <- "
  112233
  #4455#
"
all_combined <- wrap_plots(all_plots, design = design)
ggsave(
  plot = all_combined,
  filename = "Figure 7.png",
  width = 12,
  height = 6,
  dpi = 600,
  bg = "white",
  limitsize = FALSE
)
