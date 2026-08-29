rm(list=ls())
if (!is.null(dev.list())) {
    dev.off()
}
if (file.exists("Spectra Plots.pdf")) {
    file.remove("Spectra Plots.pdf")
}
library(ggplot2)
library(stringr)
library(ggsci)
library(tidyr)
library(patchwork)
library(ggpp)
load("spectral_slope_comparisons_results.RData")
significant_channels <- statistics %>% filter(Corrected_p_value<0.05) %>% pull(Channel)
significant_stimulation_types <- statistics %>% filter(Corrected_p_value<0.05) %>% pull(Stimulation_Type) %>% unique()
#### Sorting Files Out ####
subjects <- list.files("Spectral Slope",full.names = TRUE)
spectra <- list()
for (subject in subjects) {
    stimulation_types <- list.files(subject, full.names = TRUE)
    for (stimulation_type in stimulation_types) {
        if(!str_detect(stimulation_type, significant_stimulation_types)) {next}
        segments <- list.files(stimulation_type, full.name = TRUE)
        for (segment in segments) {
            message(segment)
            irasa <- new.env()
            load(segment,envir = irasa)
            for (channel in significant_channels) {
                power_spectrum <- irasa$all_irasa_outputs[[channel]]$original
                frequencies <- irasa$all_irasa_outputs[[channel]]$frequencies
                scale_free <- irasa$all_irasa_outputs[[channel]]$scale_free
                spectral_slope <- irasa$all_irasa_outputs[[channel]]$spectral_slope
                subject_name <- strsplit(subject,'/')[[1]][2]
                stimulation_type_name <- strsplit(stimulation_type,'/')[[1]][3]
                if (str_detect(segment,"No Stim")) {
                    stimulation_name <- "Before"
                } else {
                    stimulation_name <- "During"
                }
                spectra[[stimulation_type_name]][[channel]][[stimulation_name]][[subject_name]][["power_spectrum"]] <- 
                    cbind(spectra[[stimulation_type_name]][[channel]][[stimulation_name]][[subject_name]][["power_spectrum"]],power_spectrum)
                spectra[[stimulation_type_name]][[channel]][[stimulation_name]][[subject_name]][["frequencies"]] <- 
                    cbind(spectra[[stimulation_type_name]][[channel]][[stimulation_name]][[subject_name]][["frequencies"]],frequencies)
                spectra[[stimulation_type_name]][[channel]][[stimulation_name]][[subject_name]][["scale_free"]] <- 
                    cbind(spectra[[stimulation_type_name]][[channel]][[stimulation_name]][[subject_name]][["scale_free"]],scale_free)
                spectra[[stimulation_type_name]][[channel]][[stimulation_name]][[subject_name]][["spectral_slope"]] <- 
                    cbind(spectra[[stimulation_type_name]][[channel]][[stimulation_name]][[subject_name]][["spectral_slope"]], spectral_slope)
            }
        }
    }
}
stimulations <- names(spectra[[1]][[1]])
subjects <- names(spectra[[1]][[1]][[1]])
frequencies <- spectra[[1]][[1]][[1]][[1]]$frequencies[,1]
all_spectral_plots <- list()
for (stimulation_type in significant_stimulation_types){
    for (channel in significant_channels){
        data_to_plot <- data.frame(Frequency = numeric(),Type = as.character(),Stimulation = as.character(),Power_Spectrum = as.numeric())
        spectral_slope <- list()
        for (stimulation in stimulations){
            power_spectrum <- matrix(nrow=length(frequencies),ncol = 0)
            scale_free <- power_spectrum
            spectral_slope[[stimulation]] <- matrix(nrow=length(subjects),ncol=0)
            for (subject in subjects){
                power_spectrum <- cbind(power_spectrum,rowMeans(spectra[[stimulation_type]][[channel]][[stimulation]][[subject]]$power_spectrum))
                scale_free <- cbind(scale_free,rowMeans(spectra[[stimulation_type]][[channel]][[stimulation]][[subject]]$scale_free))
                spectral_slope[[stimulation]] <- cbind(spectral_slope[[stimulation]],rowMeans(spectra[[stimulation_type]][[channel]][[stimulation]][[subject]]$spectral_slope))
            }
            power_spectrum <- rowMeans(power_spectrum)
            scale_free <- rowMeans(scale_free)
            spectral_slope[[stimulation]] <- mean(spectral_slope[[stimulation]])
            data_to_plot <- data_to_plot %>% 
                add_row(Frequency = frequencies, Type = rep("Oiriginal",length(frequencies)), Stimulation = rep(stimulation,length(frequencies)),
                                                                                                                Power_Spectrum = power_spectrum)
            data_to_plot <- data_to_plot %>% 
                add_row(Frequency = frequencies, Type = rep("Scale Free",length(frequencies)), Stimulation = rep(stimulation,length(frequencies)),
                                                                                                                Power_Spectrum = scale_free)
        }
        spectral_plot <- ggplot(data_to_plot,aes(x=Frequency,y=Power_Spectrum,color = Stimulation, alpha = Type)) + 
            geom_line(key_glyph = draw_key_rect) + scale_color_aaas() + theme_classic() + theme(plot.title = element_text(hjust = 0.5)) + 
            ggtitle(paste(stimulation_type,channel)) + 
            geom_text_npc(data = data.frame(Stimulation = c("Before", "During"), 
                                            Spectral_Slope = c(spectral_slope$Before, spectral_slope$During)),
                          aes(npcx = 0.05, npcy = c(0.10, 0.05),label = paste0("beta == ", round(Spectral_Slope, 3)),
                              color = Stimulation), parse = TRUE, size = 4, show.legend = FALSE) + 
            scale_x_log10(labels = scales::label_number()) + scale_y_log10() + guides(alpha = "none")
        all_spectral_plots <- c(all_spectral_plots,spectral_plot)
    }
}
pdf(file = "Spectra Plots.pdf",  width = 10, height = 10)
for (index in 1:length(all_spectral_plots)){
    plot(all_spectral_plots[[index]])
}
dev.off()
