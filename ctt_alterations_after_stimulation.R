rm(list=ls())
if (!is.null(dev.list())) {
  dev.off()
}
pdf(file = "CTT Changes.pdf",  width = 10, height = 10)
library(dplyr)
library(ggplot2)
library(ggsci)
library(nortest)
library(stringr)
ctt <- readRDS("ctt_data_grouped.rds")
averaged_deviation_per_subject <- ctt %>% group_by(Subject,Stimulation_Type,Stimulation) %>%
  summarize("Mean_Deviation" = mean(Mean_Deviation))
stimulation_types <- unique(averaged_deviation_per_subject$Stimulation_Type)
statistics <- data.frame(Stimulation_Type = character(),p_value = numeric())
for (stimulation_type in stimulation_types) {
  before <- averaged_deviation_per_subject %>% filter(Stimulation_Type == stimulation_type,Stimulation == "Before") %>% pull(Mean_Deviation)
  during <- averaged_deviation_per_subject %>% filter(Stimulation_Type == stimulation_type,Stimulation == "During") %>% pull(Mean_Deviation)
  if (lillie.test(before)$p.value > 0.05 && lillie.test(during)$p.value > 0.05) {
    p <- t.test(before,during,paired = TRUE)$p.value
  } else {
    p <- wilcox.test(before,during,paired = TRUE)$p.value
  }
  statistics <- statistics %>% add_row(Stimulation_Type = stimulation_type, p_value = p)
   violin_plot <- averaged_deviation_per_subject %>% filter(Stimulation_Type == stimulation_type) %>%
     ggplot(aes(x = Stimulation,y = Mean_Deviation, fill = Stimulation)) + geom_violin() +
     geom_boxplot(width = 0.1) + theme_classic() + labs(fill = stimulation_type) + ggtitle(paste0(" p value:",p)) + 
     theme(plot.title = element_text(hjust = 0.5)) + scale_fill_aaas()
   print(violin_plot)
}
corrected_p_values <- p.adjust(statistics$p_value, method = "BH")
statistics$BH_corrected_p_values <- corrected_p_values
dev.off()
