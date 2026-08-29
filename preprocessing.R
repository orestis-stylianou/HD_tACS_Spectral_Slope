rm(list=ls())
library(reticulate)
library(magick)
library(stringr)
library(dplyr)
plt <- import("matplotlib")
plt$use('Qt5Agg')
copy <- import("copy")
mne <- import("mne")
fmin <- 0.5
fmax <- 45
new_sampling_rate <- 250
subjects <- readxl::read_xlsx("Downloaded Data/Experiment_2.xlsx")
mne_iclabel <- import("mne_icalabel")
mne$viz$set_browser_backend("matplotlib")
window_length <- 10
fps <- 1
dir.create("Preprocessing Figures")
dir.create("Clean EEG")
dir.create("CTT Data")
eeg_preprocessing <- function(segment,eeg_channels,new_sampling_rate,fmin,fmax){
  raw_eeg <- copy$deepcopy(segment)
  raw_eeg$pick(eeg_channels)
  clean_eeg <- copy$deepcopy(raw_eeg)
  clean_eeg$resample(new_sampling_rate)
  clean_eeg$filter(fmin,fmax)
  clean_eeg$set_eeg_reference(ref_channels=list("M1","M2"))
  clean_eeg$drop_channels(list("M1","M2"))
  clean_eeg$set_montage("easycap-M1")
  ica <- mne$preprocessing$ICA(method="infomax",fit_params=dict(extended=TRUE))
  ica$fit(clean_eeg)
  iclabel_results <- mne_iclabel$label_components(clean_eeg, ica, method="iclabel")
  iclabel_results$labels[iclabel_results$labels=="brain"] <- "keep"
  segments_to_exclude <- which(iclabel_results$labels != "keep")
  if (length(segments_to_exclude) == 1) {
    ica$apply(clean_eeg, exclude = list(segments_to_exclude))
  }else {
    ica$apply(clean_eeg, exclude = segments_to_exclude)
  }
  clean_eeg <- mne$preprocessing$compute_current_source_density(clean_eeg)
  output <- c(raw_eeg,clean_eeg,list(segments_to_exclude))
  names(output) <- c("raw_eeg","clean_eeg","segments_to_exclude")
  return(output)
}
plot_preprocessing <- function(preprocessing_results,window_length,segment_index,event_names,subjects,file_index){
  temporary_figure_folder <- paste0("Temp_Figures_",file_index)
  dir.create(temporary_figure_folder)
  raw_eeg <- copy$deepcopy(preprocessing_results$raw_eeg)
  clean_eeg <- copy$deepcopy(preprocessing_results$clean_eeg)
  segments_to_exclude <- copy$deepcopy(preprocessing_results$segments_to_exclude)
  segments_to_exclude_number <- length(segments_to_exclude)
  create_gif(raw_eeg,"Raw",window_length,temporary_figure_folder)
  create_gif(clean_eeg,"Clean",window_length,temporary_figure_folder)
  raw_psd <- raw_eeg$plot_psd()
  raw_psd$canvas$manager$full_screen_toggle()
  figure_name <- paste0(temporary_figure_folder,"/","Raw_PSD",".png")
  raw_psd$savefig(figure_name,bbox_inches = 'tight')
  plt$pyplot$close(raw_psd)
  clean_psd <- clean_eeg$plot_psd()
  clean_psd$canvas$manager$full_screen_toggle()
  figure_name <- paste0(temporary_figure_folder,"/","Clean_PSD",".png")
  clean_psd$savefig(figure_name,bbox_inches = 'tight')
  plt$pyplot$close(clean_psd)
  png(filename=paste0(temporary_figure_folder,'/Excluded Segments Number.png'))
  barplot(segments_to_exclude_number,main = paste(segments_to_exclude_number," Segments Excluded"),cex.axis=3)
  dev.off()
  raw_eeg_plot <- image_read(paste0(temporary_figure_folder,"/","Raw_EEG.gif"))
  clean_eeg_plot <- image_read(paste0(temporary_figure_folder,"/","Clean_EEG.gif"))
  raw_eeg_psd <- image_read(paste0(temporary_figure_folder,"/","Raw_PSD.png"))
  clean_eeg_psd <- image_read(paste0(temporary_figure_folder,"/","Clean_PSD.png"))
  excluded_segments_barplot <- image_read(paste0(temporary_figure_folder,"/","Excluded Segments Number.png"))
  raw_eeg_psd <- image_join(replicate(length(raw_eeg_plot), raw_eeg_psd, simplify = FALSE))
  clean_eeg_psd <- image_join(replicate(length(raw_eeg_plot), clean_eeg_psd, simplify = FALSE))
  excluded_segments_barplot <- image_join(replicate(length(raw_eeg_plot), excluded_segments_barplot, simplify = FALSE))
  final_frames <- list()
  for (frame in 1:length(raw_eeg_plot)) {
    row1 <- image_append(c(raw_eeg_plot[frame], raw_eeg_psd[frame], excluded_segments_barplot[frame]))
    row2 <- image_append(c(clean_eeg_plot[frame], clean_eeg_psd[frame]))
    combined_frame <- image_append(c(row1, row2), stack = TRUE)
    final_frames[[frame]] <- combined_frame
  }
  final_gif <- image_join(final_frames)
  dir.create(paste0("Preprocessing Figures/Subject ",subjects$`Sub#`[file_index],"/",subjects$`Stim Type`[file_index]),recursive = TRUE)
  file_name <- paste0("Preprocessing Figures/Subject ",subjects$`Sub#`[file_index],"/",subjects$`Stim Type`[file_index],"/",event_names[segment_index],".gif")
  image_write(final_gif, file_name)
  shell(paste0("rd /s /q \"",temporary_figure_folder,"\""))
}
create_gif <- function(eeg,eeg_name,window_length,temporary_figure_folder){
  segment_length <- max(eeg$times)
  windows_number <- ceiling(segment_length/window_length)
  start <- -window_length
  all_windows <- list()
  for (window in seq(1,windows_number)){
    start <- start + window_length
    if (window == windows_number){
      duration <- max(eeg$times) - (windows_number-1)*window_length 
    }else{
      duration <- window_length
    }
    figure <- eeg$plot(start = start, duration = duration, n_channels = length(eeg$ch_names))
    figure$axes[[1]]$set_ylabel(eeg_name,fontsize = 30)
    figure$canvas$manager$full_screen_toggle()
    window_name <- paste0(temporary_figure_folder,"/window_",window,".png")
    figure$savefig(window_name,bbox_inches = 'tight')
    plt$pyplot$close(figure)
    all_windows[window] <- window_name 
  }
  all_windows_loaded <- lapply(all_windows,image_read)
  all_windows_joined <- image_join(all_windows_loaded)
  gif <- image_animate(all_windows_joined,fps = fps)
  saving_name <- paste0(temporary_figure_folder,"/",eeg_name,"_EEG.gif")
  image_write(gif,saving_name)
}
extract_CTT <- function(CTT_name,event_names,event_durations,event_onsets){
  directory_to_save <- paste0("CTT Data/","Subject ",subjects$`Sub#`[file_index])
  dir.create(directory_to_save)
  directory_to_save <- paste0(directory_to_save,"/",subjects$`Stim Type`[file_index])
  dir.create(directory_to_save)
  CTT_data <- read.csv(CTT_name)
  for (segment_index in 1:length(event_names)){
    duration <- event_durations[segment_index]*1000
    onset <- event_onsets[segment_index]*1000
    stop <- onset + duration
    CTT_segment <- filter(CTT_data,time>=onset & time<=stop)
    file_to_save <- paste0(directory_to_save,"/",event_names[segment_index],".Rdata")
    save(CTT_segment,file=file_to_save)
  }
}
names <- subjects$`Sub#`
duplicates <- which(grepl("^",names,fixed = TRUE))
counter <- 0
row_1 <- vector()
row_2 <- vector()
for (i in 1:length(duplicates)){
  duplicate_1 <- names[duplicates[i]]
  for (j in (i+1):length(duplicates)){
    if (j >length(duplicates)) {next}
    duplicate_2 <- names[duplicates[j]]
    duplicate_1_sign <- strsplit(duplicate_1, "\\^")[[1]]
    duplicate_1_sign <- duplicate_1_sign[2]
    duplicate_2_sign <- strsplit(duplicate_2,"\\^")[[1]]
    duplicate_2_sign <- duplicate_2_sign[2]
    if (duplicate_1_sign == duplicate_2_sign){
      counter <- counter + 1
      row_1[counter] <- which(names == duplicate_2)
      row_2[counter] <- row_1[counter] + 1;
    }
  }
}
subjects <- subjects[-c(row_1,row_2),]
for (recording in 1:dim(subjects)[1]){
  subject <- subjects$`Sub#`[recording]
  if (is.na(subject)){
    subjects$`Sub#`[recording] <- subjects$`Sub#`[recording-1]
  }
  if (grepl("^",subject,fixed = TRUE)){
    subject <- strsplit(subject,"\\^")[[1]]
    subject <- subject[1]
    subjects$`Sub#`[recording] <- subject
  }
}
for(file_index in 1:dim(subjects)[1]) { 
  if (file_index == 13) {next} # Missing file
  data_name <- subjects$`File Num`[file_index]
  CTT_name <- list.files(paste0("Downloaded Data/Exp2_Raw Data/", data_name,"/",data_name),pattern = "csv",full.names = TRUE)
  data_name <- list.files(paste0("Downloaded Data/Exp2_Raw Data/", data_name),pattern = ".cnt",full.names = TRUE)
  data <- mne$io$read_raw_ant(data_name)
  sampling_rate <- data$info["sfreq"]
  eeg_channels_index <- !data$ch_names %in% c("BIP1","BIP2","RESP1")
  eeg_channels <- data$ch_names[eeg_channels_index]
  events <- mne$events_from_annotations(data)
  block_change_marker <- events[[2]][['2']]
  stim_on_marker <- events[[2]][['16']]
  stim_off_marker <- events[[2]][['32']]
  markers_of_interest <- c(block_change_marker, stim_on_marker, stim_off_marker)
  other_markers_index <- !(events[[1]][,3] %in% markers_of_interest)
  data$annotations$delete(other_markers_index)
  events <- mne$events_from_annotations(data)
  events_number <- length(events[[1]][,1])
  block_change_marker <- events[[2]][['2']]
  stim_on_marker <- events[[2]][['16']]
  stim_off_marker <- events[[2]][['32']]
  event_names <- list()
  event_durations <- list()
  event_onsets <- list()
  for (event_index in 1:(events_number-1)){
    description <- events[[1]][event_index,3]
    if (description == block_change_marker) {
      event_names[event_index] <- "No Stim"
    }else if (description == stim_on_marker){
      event_names[event_index] <- "Stim ON"
    }else if (description == stim_off_marker){
      event_names[event_index] <- "Stim OFF"
    } else {next}
    event_durations[event_index] <- events[[1]][event_index+1,1] - events[[1]][event_index,1]
    event_onsets[event_index] <- events[[1]][event_index,1]
  }
  event_names <- unlist(event_names)
  event_durations <- unlist(event_durations)
  event_onsets <- unlist(event_onsets)
  event_durations <- event_durations/sampling_rate
  event_onsets <- event_onsets/sampling_rate
  event_names <- event_names[event_durations>=100]
  event_onsets <- event_onsets[event_durations>100]
  event_durations <- event_durations[event_durations>100]
  event_durations <- event_durations[event_names != "Stim ON"]
  event_onsets <- event_onsets[event_names != "Stim ON"]
  event_names <- event_names[event_names != "Stim ON"]
  event_onsets[event_names == "Stim OFF"] <- event_onsets[event_names == "Stim OFF"] + 10
  event_durations[event_names == "Stim OFF"] <- 100
  event_names[event_names == "Stim OFF"] = paste0("Stim OFF ",seq(1,sum(event_names == "Stim OFF")))
  windows_number <- floor(event_durations[1]/100)
  start <- event_onsets[1] - 100
  for (window in 1:windows_number){
    start <- start + 100
    event_onsets <- c(event_onsets[1:window],start,event_onsets[(window+1):length(event_onsets)])
    event_durations <- c(event_durations[1:window],100,event_durations[(window+1):length(event_durations)])
    event_name <- paste0("No Stim ", window)
    event_names <- c(event_names[1:window],event_name,event_names[(window+1):length(event_names)])
  } 
  event_onsets <- event_onsets[2:length(event_onsets)]
  event_durations <- event_durations[2:length(event_durations)]
  event_names <- event_names[2:length(event_names)]
  annotations <- mne$Annotations(event_onsets,event_durations,event_names)
  data$set_annotations(annotations)
  segments <- data$crop_by_annotations()
  extract_CTT(CTT_name,event_names,event_durations,event_onsets)
  for (segment_index in 1:length(segments)){
    segment <- segments[[segment_index]]
    directory_to_save <- paste0("Clean EEG/Subject ",subjects$`Sub#`[file_index])
    dir.create(directory_to_save)
    directory_to_save <- paste0(directory_to_save,"/",subjects$`Stim Type`[file_index])
    dir.create(directory_to_save)
    file_to_save <- paste0(directory_to_save,"/",event_names[segment_index],".fif")
    figure_name_to_save <- file_to_save %>% 
      str_c(collapse = "---") %>% str_replace_all(c("Clean EEG" = "Preprocessing Figures", "fif" = "gif"))
    if (file.exists(file_to_save) & file.exists(figure_name_to_save)) {next}
    preprocessing_results <- eeg_preprocessing(segment,eeg_channels,new_sampling_rate,fmin,fmax)
    clean_eeg <- preprocessing_results$clean_eeg
    clean_eeg$save(file_to_save,overwrite=TRUE)
    plot_preprocessing(preprocessing_results,window_length,segment_index,event_names,subjects,file_index)
  }
}