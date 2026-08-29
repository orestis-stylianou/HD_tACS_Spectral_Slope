rm(list=ls())
library(reticulate)
library(stringr)
library(foreach)
library(doParallel)
library(stats)
source("irasa.R")
cores <- detectCores()
cluster <- makeCluster(cores-1)
registerDoParallel(cluster)
mne <- import("mne")
subjects <- list.files("Clean EEG",full.names = TRUE)
fmin <- 0.5
fmax <- 45
dir.create("Spectral Slope")
for (subject in subjects){
    stimulations <- list.files(subject,full.names = TRUE)
    for (stimulation in stimulations){
      segments <- list.files(stimulation,full.names = TRUE)
      for (segment in segments){
          message(segment)
          eeg_structure <- mne$io$read_raw_fif(segment, verbose = 'error')
          eeg <- eeg_structure$get_data()
          sampling_rate <- eeg_structure$info$sfreq
          channels <- eeg_structure$ch_names
          all_irasa_outputs <- foreach(channel = 1:length(channels)) %dopar% {
              signal <- eeg[channel,]
              output <- irasa(signal,sampling_rate)
              index <- which(output$frequencies>=fmin & output$frequencies<=fmax)
              output$original <- output$original[index]
              output$scale_free <- output$scale_free[index]
              output$oscillatory <- output$oscillatory[index]
              output$frequencies <- output$frequencies[index]
              spectral_slope <- -unname(lm(log(output$scale_free) ~ log(output$frequencies))$coefficients[2])
              output$spectral_slope <- spectral_slope
              return(output)
          }
          names(all_irasa_outputs) <- channels
          file <- str_replace_all(segment,c("Clean EEG" = "Spectral Slope", ".fif" = ".RData"))
          directory <- str_split(file,"/")
          directory <- paste(directory[[1]][1],directory[[1]][2],directory[[1]][3],sep='/')
          dir.create(directory,recursive = TRUE)
          save(all_irasa_outputs,file = file)
      }
    }
}
stopImplicitCluster()