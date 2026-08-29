rm(list=ls())
library(tidyr)
ctt_files <- list.files("CTT Data",full.names = TRUE)
ctt_data_grouped <- data.frame(Subject=character(),Stimulation_Type=character(),Stimulation=character(),Mean_Deviation=numeric())
for (ctt_file in ctt_files) {
  stimulation_types <- list.files(ctt_file, full.names = TRUE)
  for (stimulation_type in stimulation_types) {
    segments <- list.files(stimulation_type, full.name = TRUE)
    for (segment in segments) {
      ctt_values <- new.env()
      load(segment,envir = ctt_values)
      CTT_data <- ctt_values$CTT_segment
      deviation <- CTT_data$deviation
      mean_deviation <- mean(deviation)
      if (str_detect(segment,"No Stim")) {
        stimulation <- "Before"
      } else {
        stimulation <- "During"
      }
      subject <- str_split(ctt_file,"/")[[1]][2]
      stimulation_type_name <- str_split(stimulation_type,"/")[[1]][3]
      ctt_data_grouped <- ctt_data_grouped %>% add_row(Subject=subject,Stimulation_Type=stimulation_type_name,
                                                       Stimulation=stimulation,Mean_Deviation=mean_deviation)

    }
  }
}
ctt_data_grouped <- ctt_data_grouped %>% drop_na()
saveRDS(ctt_data_grouped,"ctt_data_grouped.rds")
