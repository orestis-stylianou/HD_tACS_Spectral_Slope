rm(list=ls())
library(dplyr)
library(stringr)
library(nortest)
#### Sorting Files Out ####
subjects <- list.files("Spectral Slope",full.names = TRUE)
all_spectral_slopes <- data.frame(Subject = character(),Stimulation_Type = character(), 
                      Channel = character(), Stimulation = character(), 
                      Spectral_Slope = numeric())
for (subject in subjects) {
  stimulation_types <- list.files(subject, full.names = TRUE)
  for (stimulation_type in stimulation_types) {
    segments <- list.files(stimulation_type, full.name = TRUE)
    for (segment in segments) {
      message(segment)
      irasa <- new.env()
      load(segment,envir = irasa)
      channels <- names(irasa$all_irasa_outputs)
      for (channel in channels) {
        spectral_slope <- irasa$all_irasa_outputs[[channel]]$spectral_slope
        subject_name <- strsplit(subject,'/')[[1]][2]
        stimulation_type_name <- strsplit(stimulation_type,'/')[[1]][3]
        if (str_detect(segment,"No Stim")) {
          stimulation_name <- "Before"
        } else {
          stimulation_name <- "During"
        }
        all_spectral_slopes <- 
          all_spectral_slopes %>% add_row(Subject = subject_name, Stimulation_Type = stimulation_type_name,
                              Channel = channel, Stimulation = stimulation_name, Spectral_Slope = spectral_slope)
      }
    }
  }
}
averaged_spectral_slopes <- all_spectral_slopes %>% group_by(Subject,Stimulation_Type,Channel,Stimulation) %>%     # Summarizing for each subject all Stim and No Stim segments
  summarise_at("Spectral_Slope", mean) 
#### Statistics ####
statistics <- data.frame(Stimulation_Type = character(), Channel = character(),p_value = numeric())
stimulation_types <- unique(averaged_spectral_slopes$Stimulation_Type)
for (stimulation_type in stimulation_types) {
  for (channel in channels) {
    before_stimulation <- averaged_spectral_slopes %>% 
      dplyr::filter(Stimulation_Type == stimulation_type, Channel == channel, Stimulation == "Before") %>%
      pull(Spectral_Slope)
    during_stimulation <- averaged_spectral_slopes %>% 
      dplyr::filter(Stimulation_Type == stimulation_type, Channel == channel, Stimulation == "During") %>%
      pull(Spectral_Slope)
    if (lillie.test(during_stimulation)$p.value > 0.05 && lillie.test(before_stimulation)$p.value > 0.05) {
      p <- t.test(during_stimulation,before_stimulation,paired = TRUE)$p.value
    } else {
      p <- wilcox.test(during_stimulation,before_stimulation,paired = TRUE)$p.value
    }
    statistics <- statistics %>% add_row(Stimulation_Type = stimulation_type, Channel = channel, p_value = p)
  }
}
statistics <- statistics %>% 
  group_by(Channel) %>% 
  mutate(Corrected_p_value = p.adjust(p_value, method = "BH"))
#### Saving Files ####
save(averaged_spectral_slopes, all_spectral_slopes, statistics, file = "spectral_slope_comparisons_results.RData")