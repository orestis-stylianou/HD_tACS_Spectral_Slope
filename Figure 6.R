rm(list = ls())
if (!is.null(dev.list())) {
  dev.off()
}
library(dplyr)
library(ggplot2)
library(ggsci)
library(nortest)
library(stringr)
library(patchwork)
ctt <- readRDS("ctt_data_grouped.rds")
averaged_deviation_per_subject <- ctt %>%
  group_by(Subject, Stimulation_Type, Stimulation) %>%
  summarize("Mean_Deviation" = mean(Mean_Deviation))
stimulation_types <- unique(averaged_deviation_per_subject$Stimulation_Type)
statistics <- data.frame(Stimulation_Type = character(), p_value = numeric())
all_boxplots <- list()
for (stimulation_type in stimulation_types) {
  before <- averaged_deviation_per_subject %>%
    filter(Stimulation_Type == stimulation_type, Stimulation == "Before") %>%
    pull(Mean_Deviation)
  during <- averaged_deviation_per_subject %>%
    filter(Stimulation_Type == stimulation_type, Stimulation == "During") %>%
    pull(Mean_Deviation)
  if (
    lillie.test(before)$p.value > 0.05 && lillie.test(during)$p.value > 0.05
  ) {
    p <- t.test(before, during, paired = TRUE)$p.value
  } else {
    p <- wilcox.test(before, during, paired = TRUE)$p.value
  }
  statistics <- statistics %>%
    add_row(Stimulation_Type = stimulation_type, p_value = p)
  boxplot <- averaged_deviation_per_subject %>%
    filter(Stimulation_Type == stimulation_type) %>%
    mutate(Stimulation = paste(Stimulation, stimulation_type)) %>%
    ggplot(aes(x = Stimulation, y = Mean_Deviation, colour = Stimulation)) +
    geom_jitter(width = 0.2, aes(colour = Stimulation)) +
    geom_boxplot(alpha = 0.3, width = 0.1) +
    theme_classic() +
    theme(legend.position = "none") +
    scale_color_aaas() +
    ylab("CTT Deviation") +
    annotate(
      "text",
      x = -Inf,
      y = -Inf,
      label = paste0("italic(p) == ", round(p, digits = 3)),
      parse = TRUE,
      hjust = -0.1,
      vjust = -0.5,
      size = 2.5,
      inherit.aes = FALSE
    ) +
    xlab(NULL)
  all_boxplots <- c(all_boxplots, boxplot)
}
all_combined <- wrap_plots(all_boxplots, ncol = 2)
ggsave(
  plot = all_combined,
  filename = "Figure 6.png",
  width = 12,
  height = 6,
  dpi = 600,
  bg = "white",
  limitsize = FALSE
)
